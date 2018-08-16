#!/usr/bin/env python
# coding: utf-8

import datetime
import luigi
import os
import random
import subprocess
import logging
import pdb
import yaml

from luigi import six
from os.path import join, dirname
from luigi import configuration
from luigi.contrib import postgres
from luigi.s3 import S3Target, S3Client
from dotenv import load_dotenv, find_dotenv
from luigi.contrib.postgres import PostgresTarget, PostgresQuery

from politica_preventiva.pipelines.utils.pg_sedesol import parse_cfg_string,\
        download_dir
from politica_preventiva.tasks.pipeline_task import DockerTask
from politica_preventiva.pipelines.ingest.tools.ingest_utils import parse_cfg_list,\
    extras, dates_list, get_extra_str, s3_to_pandas, final_dates
from politica_preventiva.pipelines.utils import s3_utils
from politica_preventiva.pipelines.etl.etl_orchestra import ETLPipeline

# Environment Setup
load_dotenv(find_dotenv())

# Logger and Config
conf = configuration.get_config()
logging_conf = configuration.get_config().get("core", "logging_conf_file")

logging.config.fileConfig(logging_conf)
logger = logging.getLogger("dpa-sedesol")

# AWS
aws_access_key_id = os.environ.get('AWS_ACCESS_KEY_ID')
aws_secret_access_key = os.environ.get('AWS_SECRET_ACCESS_KEY')

# Semantic Schema

with open("pipelines/configs/features_dependencies.yaml", "r") as file:
    composition = yaml.load(file)


class FeaturesPipeline(luigi.WrapperTask):

    features = parse_cfg_list(conf.get("FeaturesPipeline", "pipelines"))
    current_date = luigi.DateParameter()
    client = S3Client()
    ptask = luigi.Parameter()

    def requires(self):
        return [UpdateFeaturesDB(features_task, self.current_date)
                for features_task in self.features]

class UpdateFeaturesDB(postgres.PostgresQuery):

    features_task = luigi.Parameter()
    current_date = luigi.DateParameter()
    client = S3Client()
    features_scripts = luigi.Parameter()

    # AWS RDS
    database = os.environ.get("PGDATABASE")
    user = os.environ.get("POSTGRES_USER")
    password = os.environ.get("POSTGRES_PASSWORD")
    host = os.environ.get("PGHOST")

    @property
    def update_id(self):
        return str(self.features_task) + '_features'

    @property
    def table(self):
        return "features." + self.features_task

    @property
    def cmd(self):
        # Read features script
        features_script = self.features_scripts +\
                self.features_task + '.R'

        if not os.path.isfile(features_script):
            return

        command_list = ['Rscript', features_script,
                        '--database', self.database,
                        '--user', self.user,
                        '--password', "'{}'".format(self.password),
                        '--host', self.host]
        cmd = " ".join(command_list)
        return cmd
    @property

    def requires(self):
        # Check table dependencies
        dep_types = [dt for dt in composition[self.features_task].keys()]

        for dt in dep_types:
            if 'features_dependencies' in dep_types:
                features_tables = composition[self.features_task]['features_dependencies']
                yield [FeaturesPipeline(current_Date=self.current_date,
                    feature_task=feature_task) for feature_task in features_tables]

            if 'clean_dependencies' in dep_types:
                clean_tables = composition[self.clean_task]['clean_dependencies']
                yield [ETLPipeline(current_date=self.current_date,
                    pipelines=pipeline_task) for pipeline_task in clean_tables]

            if 'model_dependencies' in dep_types:
                models_tables = composition[self.models_task]['models_dependencies']
                yield [ModelsPipeline(current_date=self.current_date,
                    pipelines=pipeline_task) for pipeline_task in models_tables]

    def output(self):
        return PostgresTarget(host=self.host,
                              database=self.database,
                              user=self.user,
                              password=self.password,
                              table=self.table,
                              update_id=self.update_id)


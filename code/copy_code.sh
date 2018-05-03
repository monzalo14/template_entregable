#!/usr/bin/env bash
while read script; do
scp -i $HOME/.ssh/monizamudio_ec2.pem monizamudio@ec2-52-37-161-172.us-west-2.compute.amazonaws.com:/home/monizamudio/workspace/politica_preventiva/politica_preventiva/pipelines/ingest/classic/source/$script $HOME/Repos/template_entregable/code/$script
done < scripts.txt

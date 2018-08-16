#!/usr/bin/env Rscript

library(optparse)
library(dbplyr)
library(dplyr)
library(dbrsocial)
library(DBI)
source("recode_utils.R")

option_list = list(
  make_option(c("--data_date"), type="character", default="",
              help="data date", metavar="character"),
  make_option(c("--database"), type="character", default="",
              help="database name", metavar="character"),
  make_option(c("--user"), type="character", default="",
              help="database user", metavar="character"),
  make_option(c("--password"), type="character", default="",
              help="password for datbase user", metavar="character"),
  make_option(c("--host"), type="character", default="",
              help="database host name", metavar="character"),
  make_option(c("--pipeline_task"), type="character", default="",
              help="pipeline task", metavar="character"),
  make_option(c("--scripts_dir"), type="character", default="",
              help="scripts directory", metavar="character")
)

opt_parser <- OptionParser(option_list=option_list)

opt <- tryCatch(
  {
    parse_args(opt_parser)
  },
  error=function(cond) {
    message("Error: Provide database connection arguments appropriately.")
    message(cond)
    print_help(opt_parser)
    return(NA)
  },
  warning=function(cond) {
    message("Warning:")
    message(cond)
    return(NULL)
  },
  finally={
    message("Finished attempting to parse arguments.")
  }
)

if(length(opt) > 1){

  if (opt$database=="" | opt$user=="" |
      opt$password=="" | opt$host=="" ){
    print_help(opt_parser)
    stop("Database connection arguments are not supplied.n", call.=FALSE)
  }else{
    PGDATABASE <- opt$database
    POSTGRES_PASSWORD <- opt$password
    POSTGRES_USER <- opt$user
    PGHOST <- opt$host
    PGPORT <- "5432"
  }

  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                   host = PGHOST,
                   port = PGPORT,
                   dbname = PGDATABASE,
                   user = POSTGRES_USER,
                   password = POSTGRES_PASSWORD
  )

  pipeline_task <- opt$pipeline_task
  scripts_dir <- opt$scripts_dir
  raw_table <- pipeline_task

  # clean files
  rfile <- glue::glue('{scripts_dir}/{pipeline_task}.R')
  sqlfile <- glue::glue('{scripts_dir}/{pipeline_task}.sql')

  # If SQL clean script
  if( file.exists(sqlfile) ){
    # Read query
    query <- readLines(sqlfile) %>%
             paste(collapse = "") %>% glue::glue()
    # send query
    dbGetQuery(con, sql(query))
    # ToDo: verificar que este nombre funcione
    temp_name <- glue::glue('temp_{pipeline_task}')
    dbCommit(con)


  # If R clean script
  } else if(file.exists(rfile)) {
    source(rfile)
    clean_table <- make_clean(pipeline_task, con)
    temp_name <- glue::glue('temp_{pipeline_task}')
    copy_to(con, clean_table,
            temp_name,
            temporary = TRUE)
  }

  # Recode
  recoded_table <- recode_vars(table_name = temp_name,
                              db_connection = con)

  # Store
  copy_to(con, recoded_table,
          dbplyr::in_schema("clean", pipeline_task),
          temporary = FALSE, overwrite = TRUE)
  dbDisconnect(con)
}

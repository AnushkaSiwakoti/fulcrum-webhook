#app.R
library(plumber)
library(dplyr)
library(httr)
library(jsonlite)
library(glue)
library( rlang)
library(data.table)
library(rlist)
library(stringr)

port <- Sys.getenv('PORT')

pr <- plumb("plumber.R")

pr$run(
  host = '0.0.0.0',
  port = as.numeric(port)
)


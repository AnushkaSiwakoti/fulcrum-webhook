#app.R

# used to establish the API
# how its hosted on the server and funnels all incoming 
# requests to the plumer.R file
library(plumber)
library(tidyverse)
library(lme4)
library(jsonlite)
library(httr)

port <- Sys.getenv('PORT')

pr <- plumb("plumber.R")

pr$run(
  # host NEEDS to be 0.0.0.0
  host = '0.0.0.0',
  port = as.numeric(port)
)
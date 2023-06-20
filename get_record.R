<<<<<<< HEAD
#' @title Get a single record's JSON by fulcrum_id
#'
#' @author Cody Flagg
#'
#' @description Use to retrieve a single specific record if you know the record's id (fulcrum_id) ahead of time. Uses the REST API, so this will return a JSON object with parserKeys (e.g. 'ab12') rather than the name of the fulcrum field. Deleted records are kept forever in Fulcrum, however their data/JSON are only retrievable using the history endpoint e.g. get_record_history(). Using get_record() on a deleted record will return a 404 http error.
#'
#' @param record_id the fulcrum record id
#' @param api_token a Fulcrum API token
#'
#' @return a list object. Records with child and/or grandchild data will have nested data, which prevents coercion to a simple data.frame.
#'
#' @export
get_record<- function(record_id, api_token = Sys.getenv('FULCRUM_API_NEON')){
  require(httr)
  require(jsonlite)
  url <- paste0("https://api.fulcrumapp.com/api/v2/records/",record_id,".json")
  request <- httr::GET(url, add_headers("X-ApiToken" = api_token, "Accept" = "application/json"))
  content <- httr::content(request, as = 'parsed')
  return(content)
}
#get_record("844b9e53-131f-424b-8f42-48aa8535f77b",Sys.getenv('FULCRUM_API_NEON'))
=======
#' @title Get a single record's JSON by fulcrum_id
#'
#' @author Cody Flagg
#'
#' @description Use to retrieve a single specific record if you know the record's id (fulcrum_id) ahead of time. Uses the REST API, so this will return a JSON object with parserKeys (e.g. 'ab12') rather than the name of the fulcrum field. Deleted records are kept forever in Fulcrum, however their data/JSON are only retrievable using the history endpoint e.g. get_record_history(). Using get_record() on a deleted record will return a 404 http error.
#'
#' @param record_id the fulcrum record id
#' @param api_token a Fulcrum API token
#'
#' @return a list object. Records with child and/or grandchild data will have nested data, which prevents coercion to a simple data.frame.
#'
#' @export
get_record<- function(record_id, api_token = Sys.getenv('FULCRUM_API_NEON')){
  require(httr)
  require(jsonlite)
  url <- paste0("https://api.fulcrumapp.com/api/v2/records/",record_id,".json")
  request <- httr::GET(url, add_headers("X-ApiToken" = api_token, "Accept" = "application/json"))
  content <- httr::content(request, as = 'parsed')
  return(content)
}
#get_record("844b9e53-131f-424b-8f42-48aa8535f77b",Sys.getenv('FULCRUM_API_NEON'))
>>>>>>> e2a294c15498949c17eb4f5fc111e4d4887702ee

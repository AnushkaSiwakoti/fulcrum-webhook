put_record <- function(record_id, 
                       api_token = Sys.getenv('FULCRUM_API_NEON'),
                       body,
                       verbose=FALSE){ 
  url_in = paste0("https://api.fulcrumapp.com/api/v2/records/", record_id, ".json")
  
  request <- httr::PUT(url = url_in,
                       config = add_headers("X-ApiToken" = api_token,
                                            Accept = "application/json",
                                            'Content-Type' = "application/json"),
                       body = body,
                       encode = "json")
  
  if (verbose == TRUE){
    print( 'POSTED?')
    print(content(request))
    print(httr::status_code(request))
  }
  return(request$status_code)
}

#get records function 
get_record<- function(record_id, api_token){
  require(httr)
  require(jsonlite)
  url <- paste0("https://api.fulcrumapp.com/api/v2/records/",record_id,".json")
  request <- httr::GET(url, add_headers("X-ApiToken" = api_token, "Accept" = "application/json"))
  content <- httr::content(request, as = 'parsed')
  return(content)
}

## wrapper for write_csv
write_fxn <- function(input, output){
  readr::write_csv(input, file = output)
}

## function to replace NULL with NA
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}
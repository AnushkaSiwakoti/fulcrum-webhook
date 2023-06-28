uploadFile <- function(api_token = Sys.getenv('FULCRUM_API_NEON'),recordid,filepath,form_values,attachment_key){
  require(httr)
  require(jsonlite)
  url <- paste0("https://api.fulcrumapp.com/api/v2/attachments")
  body <- paste0('{"name": "', basename(filepath) ,'",
  "owners": [
    {
      "type": "record",
      "id": "',recordid,'"
    }
  ]
}')
  
  request <- httr::POST(url,
                        config = add_headers("X-ApiToken" = api_token,
                                             Accept = "application/json",
                                             'Content-Type' = "application/json"), body = body, encoding = "json")
  #print(request)
  content <- httr::content(request, as = 'parsed')
  #content <- content(request)
  
 with(content, {
  url <- url
  id <- id
})
  
  
  
  # put file in box
  body <- list(key=id, value=filepath)
  url <- paste0(url)
  
  request <- httr::PUT(url,
                       config = add_headers(
                         Accept = "application/json",
                         'Content-Type' = "application/json"), body = body, encoding = "json")
  #content <- httr::content(request, as = 'parsed')
  
  
  
  # finalize post
  url <- paste0("https://api.fulcrumapp.com/api/v2/attachments/finalize")
  
  body <- paste0('{
  "owners": [
    {
      "id": "',recordid,'",
      "type": "record"
    }
  ],
  "ID": "',id,'"
}')
  
  #print(body)
  request <- httr::POST(url,
                        config = add_headers("X-ApiToken" = api_token,
                                             Accept = "application/json",
                                             'Content-Type' = "application/json"), body = body, encoding = "json")
  #print(request)
  if (status_code(request) == 201) {
    # Success - AWS box created
    print("Created")
    
  }
  
  
  
  #put file attachment
  
  url <- paste0("https://api.fulcrumapp.com/api/v2/records/", recordid, ".json")
  
  #form_values[[attachmentkey]] <- list(list(name = basename(filepath), attachment_id = id))
  
  form_values <- list(
    attachmentkey = list(
      list(
        name = basename(filepath),
        attachment_id = id
      )
    )
  )
  
  field_name <- attachment_key
  form_values[[field_name]] <- form_values$attachmentkey
  form_values$attachmentkey <- NULL
  
  body <- toJSON(list(record = list(form_values = form_values)), auto_unbox = TRUE)
  
  
  
  request <- httr::PUT(url,
                       config = add_headers("X-ApiToken" = api_token,
                                            Accept = "application/json",
                                            "Content-Type" = "application/json"), body = body, encode = "json")
  
  
  content <- httr::content(request, as = 'parsed')
  
  
  return(content)
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




## A helper function that tests whether an object is either NULL or a list of NULLs
is.NullOb <- function(x) {
  is.null(x) || (is.list(x) && all(sapply(x, is.NullOb)))
}

## Recursively step down into list, removing all such objects 
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

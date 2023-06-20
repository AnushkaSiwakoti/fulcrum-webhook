#' @title Get a form's JSON by fulcrum_id
#'
#' @description Use to upload file in fulcrum
#'
#' 
#' @param api_token a Fulcrum API token
#'
#' @return 
#
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
  
  url <- content$url
  id <- content$id
  
  
  
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

uploadFile(Sys.getenv('FULCRUM_API_NEON'), "a231eafd-d93d-4fea-a2e7-92e0e747ebdf",'C:/Users/siwakoti/Downloads/message.jfif',get_record('a231eafd-d93d-4fea-a2e7-92e0e747ebdf', Sys.getenv('FULCRUM_API_NEON')),"cf80")
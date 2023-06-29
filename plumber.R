# Add required libraries
library(plumber)
library(jsonlite)
library(dplyr)
library(glue)
library(rlist)
library(data.table)
library(httr)
library(stringr)

# Source the required files
source('get_record.R')
source('fulcrum_file_upload.R')

#* @plumber
function(pr) {
  pr %>%
    pr_filter('scs-dev', function(req, res) {
      print(paste0('>>>>>>>>>>>>LOGGER ACTIVE<<<<<<<<<<<<<<<', Sys.time()))
      #print( ls.str(env=req))  #this is used to checkout the request that is being posted to the webhook
      cat( as.character( Sys.time()), "-",
           req$REQUEST_METHOD, req$PATH_INFO, "-",
           req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
      # Retrieve the record ID from the request body
      if (is.null(req$postBody)) {
        res$status <- 404
        return(list(error = "Body not found"))
      } else {
        print('payload found')
        
        record <- jsonlite::parse_json( req$postBody)
        
        
        
        if (!is.null(record$record$type) && record$record$type %in% c('record.create', 'record.update')) {
          record_id <- record$record$id
          form_id <- record$record$form_id
          print(paste0('recordID = ', record_id))
          print(paste0('formID = ', form_id))   
          
          if (!is.null(form_id) && form_id == "555deee5-7e4d-4558-8101-148dfa06d870") {
            print("Shipment creation payload found")
            #print(record)
            
            # Check if shipment manifest is "yes" and no attachment ID exists
            if (!is.null(record$record$form_values$`0ce0`) && record$record$form_values$`0ce0` == "yes" && is.null(record$record$form_values$`cf80`)) {
              print(record$record$form_values$`0ce0`)
              print(record$record$form_values$`cf80`)
              print("Creating draft manifest...")
              api_token <- Sys.getenv('FULCRUM_API_NEON')
              
              # Get the sample array from the shipment creation record
              sample_array <- unlist(record$record$form_values$`88f2`)
              #print(sample_array)
              
              # Parse the sample array
              sample_array <- strsplit(sample_array, "|", fixed = TRUE)[[1]]
              array_parse <- list()
              for (s in 1:length(sample_array)) {
                sample <- jsonlite::parse_json(sample_array[s])
                sample <- rmNullObs(sample)
                array_parse <- list.append(array_parse, sample)
              }
              #print(array_parse)
              
              if (length(array_parse) > 0) {
                draft_manifest <- data.frame(matrix(unlist(array_parse), nrow = length(array_parse), byrow = TRUE), stringsAsFactors = FALSE)
                #print(draft_manifest)
                colnames(draft_manifest) <- names(array_parse[[1]])
                draft_manifest_filename <- glue::glue("{record$record$form_values[['de4e']]}_draft_manifest.csv")
                print(draft_manifest_filename)
                write.csv(draft_manifest, file = draft_manifest_filename, row.names = FALSE)
                form_values <- record$data
                filepath <- draft_manifest_filename
                attachment_key <- "cf80"
                uploadFile(api_token, record_id, filepath, form_values, attachment_key)
                print("Draft manifest created and uploaded.")
                status <- 200
                return(list(status = 200))
              } else {
                print("Draft manifest not created and uploaded.")
                status <- 500
                return(list(status = 500))
              }
            } else {
              print('draft manifest not requested')
              status <- 204
              return(list(error = "MANIFEST NOT REQUESTED"))
            }
          } else {
            ## if it's a record.create or record.update but NOT the target app/form, still return a response
            print('payload not from shipment creation')
            status <- 204
            return(list(error = "NOT SHIPMENT CREATION RECORD"))
          }
        } 
        else {
          #if there's another type of webhook message (e.g. record.delete), still return a response
          print('record type incorrect')
          status <- 204
          return(list(error = "NO RECORD CREATE OR UPDATE"))
        }
      }
    })
}

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
        
        
        
        if (!is.null(record$type) && record$type %in% c('record.create', 'record.update')) {
          record_id <- record$data$id
          form_id <- record$data$form_id
          print(paste0('recordID = ', record_id))
          print(paste0('formID = ', form_id))   
          
          if (!is.null(form_id) && form_id == "555deee5-7e4d-4558-8101-148dfa06d870") {
            print("Shipment creation payload found")
            #print(record)
            
            # Check if shipment manifest is "yes" and no attachment ID exists
            if (!is.null(record$data$form_values$`0ce0`) && record$data$form_values$`0ce0` == "yes" && is.null(record$data$form_values$`cf80`)) {
              # print(record$record$form_values$`0ce0`)
              # print(record$record$form_values$`cf80`)
              print("Creating draft manifest...")
              api_token <- Sys.getenv('FULCRUM_API_NEON')
              
              # Get the sample array from the shipment creation record
              sample_array <- unlist(record$record$form_values$`88f2`)
              #print(sample_array)
               sample_array <- as.character(sample_array)
              
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
                # Create a data frame with the main information
                dat <- data.frame(
                  shipDate = character(),
                  shipmentID = character(),
                  senderID = character(),
                  sentTo = character(),
                  shipmentService = character(),
                  shipmentMethod = character(),
                  trackingNumber = character(),
                  quarantineSamples = character(),
                  stringsAsFactors = FALSE
                )
                
                # Add data to dat
                dat[1, "shipDate"] <- record$record$form_values$`7561`
                dat[1, "shipmentID"] <- record$record$form_values$`de4e`
                dat[1, "senderID"] <- record$record$form_values$`3cf0`
                dat[1, "sentTo"] <- record$record$form_values$`4088`
                dat[1, "shipmentService"] <- record$record$form_values$`d296`$choice_values[[1]]
                dat[1, "shipmentMethod"] <- record$record$form_values$`d7aa`$choice_values[[1]]
                if (!is.null(record$record$form_values$`effd`)) {
                  dat[1, "trackingNumber"] <- record$record$form_values$`effd`
                } else {
                  dat[1, "trackingNumber"] <- ""
                }
                dat[1, "quarantineSamples"] <- "None"
                
                
                # Create an empty data frame with the same column names as dat
                dat2 <- dat[0, ]
                
                for (i in 1:length(array_parse)) {
                  sample <- array_parse[[i]]
                  
                  new_row <- data.frame(
                    
                    shipDate = "",
                    shipmentID = "",
                    senderID = "",
                    sentTo = "",
                    shipmentService = "",
                    shipmentMethod = "",
                    trackingNumber = "",
                    quarantineSamples = "",
                    sampleID = sample$`sample_barcode`,
                    sampleCode = sample$`sample_tag`,
                    sampleClass = sample$`sampleclass`,
                    quarantineStatus = "N",
                    stringsAsFactors = FALSE
                  )
                  
                  dat2 <- bind_rows(dat2, new_row)
                }
                
                # Combine the data frames
                draft_manifest <- cbind(dat, dat2)
                #draft_manifest <- data.frame(dat, dat2)
                
                row.names(draft_manifest) <- NULL
                
                draft_manifest_filename <- glue::glue("{record$record$form_values[['de4e']]}_draft_manifest.csv")
                print(draft_manifest_filename)
                write.csv(draft_manifest, file = draft_manifest_filename, row.names = FALSE)
                form_values <- record$data$form_values
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

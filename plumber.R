# Add required libraries
library(tidyverse)
library(plumber)
library(jsonlite)
library(dplyr)
library(glue)
library(rlist)
library(data.table)
library(httr)
library(stringr)
library(googleCloudStorageR)


# Source the required files
source('functions.R')


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
            if (!is.null(record$data$form_values$`0ce0`) && record$data$form_values$`0ce0` == "yes" && is.null(record$data$form_values$`525a`)) {
              # print(record$record$form_values$`0ce0`)
              # print(record$record$form_values$`cf80`)
              print("Creating draft manifest...")
              my_path_to_gcs_key_json <- "C:/Users/lea/Documents/prod-sa-os-shipping-manifests-writer-jsonkey.json"
              gcs_auth(json_file = my_path_to_gcs_key_json)
              my_bucket_name <- "neon-os-shipping-manifests"
              gcs_global_bucket(my_bucket_name)
              manifest_list <- gcs_list_objects()
              
              # Get the sample array from the shipment creation record
              sample_array <- unlist(record$data$form_values$`88f2`)
              #print(sample_array)

              # Split the sample array
              sample_array <- strsplit(sample_array, "|", fixed = TRUE)[[1]]
              if (length(sample_array) > 0) {
                dat <- data.frame(matrix(NA, nrow = length(sample_array), ncol = 27))
                colnames(dat) <- c("domainID", "dateShipped", "shipmentID", "senderID",
                                   "sentTo", "shipmentService", "shipmentMethod","trackingNumber", 
                                   "quarantineStatus","containerID","sampleID","sampleCode",
                                   "sampleClass","sampleType","numVialsSampleID","analysisType",
                                   "taxonID","wellCoordinates","filterVolume","sampleMass",
                                   "containerMass","preservativeType","preservativeVolume",
                                   "individualCount","namedlocation","collectdate","domainRemarks")
                
                parentVals <- record$record$form_values
                parentFields <- c("72c0","7561","de4e","3cf0","4088","d296","d7aa",
                                  "effd","18c0")
                for (i in 1:length(parentFields)) {
                  if(is.null(parentVals[[parentFields[i]]])){
                    parentVals[parentFields[i]] <- NA
                  }
                }
                for (i in 1:length(sample_array)) {
                  print(i)
                  sample <- jsonlite::parse_json(sample_array[[i]])
                  sample <- map(sample, function(x) ifelse(is.null(x), NA, x))                  
                  if(is.null(sample$storageCode)){
                    sample$storageCode <- NA
                  }
                  
                  dat[i,"domainID"] = parentVals$`72c0`$choice_values[[1]]
                  dat[i,"dateShipped"] = parentVals$`7561`
                  dat[i,"shipmentID"] = parentVals$`de4e`
                  dat[i,"senderID"] = parentVals$`3cf0`
                  dat[i,"sentTo"] = parentVals$`4088`
                  dat[i,"shipmentService"] = parentVals$`d296`$choice_values[[1]]
                  dat[i,"shipmentMethod"] = parentVals$`d7aa`$choice_values[[1]]
                  dat[i,"trackingNumber"] = parentVals$`effd`
                  dat[i,"quarantineStatus"] = parentVals$`18c0`$choice_values[[1]]
                  dat[i,"containerID"] = sample$storageCode
                  dat[i,"sampleID"] = sample$sample_barcode
                  dat[i,"sampleCode"] = sample$sample_tag
                  dat[i,"sampleClass"] = sample$sampleclass
                  dat[i,"sampleType"] = sample$sample_type
                  dat[i,"numVialsSampleID"] = sample$number_containers
                  dat[i,"analysisType"] = sample$analysis_type
                  dat[i,"taxonID"] = sample$taxonomy
                  dat[i,"wellCoordinates"] = sample$well_coordinate
                  dat[i,"filterVolume"] = sample$filter_volume
                  dat[i,"sampleMass"] = sample$sample_mass
                  dat[i,"containerMass"] = sample$container_mass
                  dat[i,"preservativeType"] = sample$preservative_type
                  dat[i,"preservativeVolume"] = sample$preservative_volume
                  dat[i,"individualCount"] = sample$specimen_count
                  dat[i,"namedlocation"] = sample$location
                  dat[i,"collectdate"] = sample$collection_date
                  dat[i,"domainRemarks"] = sample$remarks
                }
              }
              draft_manifest <- Filter(function(y) !all(is.na(y)), dat)
              row.names(draft_manifest) <- NULL
              
              draft_manifest_filename <-paste(record$record$form_values$`de4e`, "_draft_manifest.csv", sep = "")
              print(draft_manifest_filename)
              if(draft_manifest_filename %in% manifest_list$name){
                gcs_delete_object(object_name = draft_manifest_filename)
                
              }
              #write.csv(draft_manifest, file = draft_manifest_filename, row.names = FALSE)
              gcs_upload(
                draft_manifest,
                name = draft_manifest_filename,
                object_function = write_fxn,
              )
              record_out <- list(record = c(id = record_id, list(form_values = 
                                                                   record$record$form_values)))
              url <- gcs_download_url(draft_manifest_filename)
              record_out$record$form_values["525a"] = url
              
              request <- put_record(record_id = record_id, body = record_out)
              print(paste0('>>>>>> REVIEW PUT REQUEST STATUS CODE = ', request))
              res$status <- request
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
        } else {
          #if there's another type of webhook message (e.g. record.delete), still return a response
          print('record type incorrect')
          status <- 204
          return(list(error = "NO RECORD CREATE OR UPDATE"))
        }
      }
    })
}

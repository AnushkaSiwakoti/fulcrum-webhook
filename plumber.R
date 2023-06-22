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

# Define plumber endpoint
#* @plumber
function(pr) {
  # Retrieve the record ID from the request body
  record <- jsonlite::fromJSON(pr$postBody)
  record_id <- record$data$id
  
  api_token <- Sys.getenv('FULCRUM_API_NEON')
  shipment <- get_record(record_id, api_token)
  
  # Check if shipment manifest is "yes" and no attachment ID exists
  if (shipment$data$form_values$`0ce0` == "yes" && is.null(shipment$data$form_values$`cf80`)) {
    if (is.null(pr$postBody)) {
      res$status <- 404
      return(list(error = "Body not found"))
    } else {
      print("Creating draft manifest...")
      
      # Get the sample array from the shipment creation record
      sample_array <- unlist(shipment$data$form_values$`88f2`)
      
      if (any(grepl("storageCode", sample_array))) {
        print(">>>> STORAGE CODE DETECTED >>>>>>>")
        
        # Parse the sample array
        sample_array <- strsplit(sample_array, "|", fixed = TRUE)[[1]]
        array_parse <- list()
        for (s in 1:length(sample_array)) {
          sample <- jsonlite::fromJSON(sample_array[s])
          array_parse <- list.append(array_parse, sample)
        }
        
        # Get unique sample types
        all_types <- unique(lapply(array_parse, `[[`, "sampleclass"))
        
        # Prepare to store inventory samples
        inventory_samples <- list()
        
        # Iterate over sample types
        for (t in 1:length(all_types)) {
          samp_list <- array_parse[lapply(array_parse, `[[`, "sampleclass") == unlist(all_types[t])]
          samp_filter <- rlist::rmNullObs(samp_list)
          samp_po <- names(po)[names(po) %in% names(samp_filter[[1]])]
          sample_mapping <- get_record(idvalue = all_types[t], api_token)
          
          # Iterate over sample mappings
          for (m in 1:length(sample_mapping$rows)) {
            map <- sample_mapping$rows[[m]]
            map_filter <- map[names(map) %in% samp_po]
            map_filter$sampleclass <- NULL
            fields <- unname(unlist(map_filter))
            query_fields <- paste(fields, collapse = ",")
            appname <- stringr::str_replace_all(map$appname, " ", "%20")
            
            if (!is.null(map$repeatablename)) {
              repeatablename <- stringr::str_split(map$repeatablename, "/")[[1]]
              repeatablename <- repeatablename[length(repeatablename)]
            } else {
              repeatablename <- map$repeatablename
            }
            
            # Determine the field to use for ID matching
            if (length(rlist::rmNullObs(lapply(samp_list, `[[`, "sample_barcode"))) == length(samp_list)) {
              id_field <- "sample_barcode"
            } else {
              id_field <- "sample_tag"
            }
            
            parent_data <- get_record(map$fulcrumcode)
            
            if (length(parent_data$rows) > 0) {
              for (row in 1:length(parent_data$rows)) {
                check_data <- parent_data$rows[[row]]
                if (exists("check_data") && !is.null(check_data)) {
                  sample <- list()
                  for (field in fields) {
                    sample[[field]] <- check_data[[field]]
                  }
                  inventory_samples <- rlist::list.append(inventory_samples, sample)
                }
              }
            }
          }
        }
        
        if (length(inventory_samples) > 0) {
          
          draft_manifest <- data.frame(matrix(unlist(inventory_samples), nrow = length(inventory_samples), byrow = TRUE), stringsAsFactors = FALSE)
          
          colnames(draft_manifest) <- names(inventory_samples[[1]])
          
          draft_manifest_filename <- glue::glue("{shipment$data$shipment_id}_draft_manifest.csv")
          
          write.csv(draft_manifest, file = draft_manifest_filename, row.names = FALSE)
          
          form_values <- get_record(record_id, api_token)
          
          filepath <- draft_manifest_filename
          
          attachment_key <- "cf80"
          
          uploadFile(api_token, record_id, filepath, form_values, attachment_key)
          return(res$sendStatus(200))
        }
      }
    }
  }
  
  # Handle other cases if needed
  
  # Return a response if no conditions are met
  res$status <- 200
  return(list(message = "OK"))
}

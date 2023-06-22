# plumber.R
library(plumber)
library(dplyr)
library(httr)
library(jsonlite)
library(glue)
library( rlang)
library(data.table)
library(rlist)
library(stringr)

#source('functions.R')
source('fulcrum_file_upload.R')
source('get_record.R')


#* @plumber 
function(pr){
  pr %>% 
    pr_filter( 'scs-dev', function( req, res){
      #browser()
      print(paste0('>>>>>>>>>>>>LOGGER ACTIVE<<<<<<<<<<<<<<<', Sys.time()))
      #print( ls.str(env=req))  #this is used to checkout the request that is being posted to the webhook
      cat( as.character( Sys.time()), "-",
           req$REQUEST_METHOD, req$PATH_INFO, "-",
           req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
     
      
      
      if (is.null( req$postBody)){
        res$status <- 404 
        return( list( error="Body not found"))
      } else {
        print('payload found')
        shipment <- jsonlite::parse_json( req$postBody)
        record_id <- record$data$id
        print('record_id')
        api_token <- Sys.getenv('FULCRUM_API_NEON')
        
        # Check if shipment manifest is "yes" and no attachment ID exists
        if (shipment$data$form_values$`0ce0` == "yes" && is.null(shipment$data$form_values$`cf80`)) {
          
          
          
          
          ## if the incoming request is a record being created
          if( !is.null( record$type) & record$type %in% c('record.create', 'record.update') ){
            record_type <-  record$type
            print( paste0( 'recordID == ' , record$data$id))
            
            if( (!is.null( record$data$form_id) | !is.null( record$data['form_id'])) ){
              print( 'form id exists')
              #print( record$data$form_id)
              #print( form_ids)
              app_suite <- form_ids[[ record$data$form_id]]
              current_form <- record$data$form_id
            }else{
              print( 'form id does not exist')
              app_suite <- NULL
            }
            
            print( paste0('form_id = ', record$data$form_id,'; type = ', record_type))
            
            print(app_suite)
            ## and it's one of these these forms...
            if( exists('app_suite') & !is.null(app_suite) & record$data$form_values['LOAD_STATUS'] != 'LOADED' & length(record$data$form_values$`88f2`) > 0){ 
              
              
              #this next "if" isn't needed for now as the form_ids object only has shipment creation app id keys
              #if( (current_form %in% c(app_suite$creation_app) | current_form == app_suite$creation_app) ){
              print('>>>> SHIPPING CREATION RECORD DETECTED >>>>')
              recordid <- record$data$id
              coc_num <- record$data$form_values['de4e'] #chain of custody number to link creation and review records
              print(coc_num)
              #get the shipment review record that matches the creation coc number
              
              review_record <- get_record(coc_num)
              review_id <- lapply(review_record$rows,'[[','_record_id') #get the id to query the children
              # only proceed if shipment review record exists
              print(review_id)
              if(length(review_id) > 0){
                print('>>>> PARENT REVIEW RECORD RETRIEVED >>>>>>>')
                
                #shipment creation "passing object" to translate sample_mapping fields to the sample_array fields
                po <- "{\"sample_volume\":\"sample_volume\",\"analysis_type\":\"analysistype\",\"boutnumber\":\"bout_number\",\"caption\":\"caption\",\"collection_date\":\"activityenddate\",\"container_mass\":\"containermass\",\"filter_volume\":\"filtervolume\",\"location\":\"namedlocation\",\"number_containers\":\"numvialssampleid\",\"photoid\":\"photoid\",\"preservative_concentration\":\"preservativeconcentration\",\"preservative_type\":\"preservativetype\",\"preservative_volume\":\"preservativevolume\",\"remarks\":\"remarks\",\"sample_barcode\":\"fulcrumcode\",\"sample_condition\":\"samplecondition\",\"sample_fate\":\"fulcrumfate\",\"sample_mass\":\"samplemass\",\"sample_tag\":\"fulcrumtag\",\"sample_type\":\"sampletype\",\"sampleclass\":\"sampleclass\",\"specimen_count\":\"specimencount\",\"taxonomy\":\"taxonomy\",\"view\":\"view\",\"well_coordinate\":\"well_coordinate\",\"filternumber\":\"filternumber\"}"
                #data.table(keyword=list(            
                #po <- gsub("'", "\"", po)
                po <- jsonlite::parse_json(po)
                
                ##### Branch 1: Inventory Container Update Workflow #####
                #print(paste0('>>> SHIPPING RECORD VALUES ---', record$data$form_values['88f2']))
                
                #grab the sample array
                sample_array <- unlist(record$data$form_values$`88f2`)
                if(any(grepl("storageCode",sample_array))){
                  print('>>>> STORAGE CODE DETECTED >>>>>>>')
                  
                  sample_array <- strsplit(sample_array,"|",fixed=TRUE)[[1]]
                  array_parse <- list() #parse all the sample array jsons and push back into a list
                  for(s in 1:length(sample_array)){ #loop through each sample
                    sample <- jsonlite::parse_json(sample_array[s])
                    array_parse <- list.append(array_parse, sample)
                  }
                  all_types <- unique(lapply(array_parse,'[[','sampleclass')) #get all unique sample types
                  #get the id to query the children
                  
                  #sample_mapping <- strsplit(unlist(record$holder_sql),"|",fixed=TRUE)[[1]]
                  #print(sample_mapping)
                  #print(sample_array[1])
                  inventory_samples <- list()#create a "container" for the inventory samples
                  #all_samples <- list()
                  for(t in 1:length(all_types)){ #loop through each sample type
                    #get list of all samples matching the sample type
                    samp_list <- array_parse[lapply(array_parse,'[[','sampleclass') == unlist(all_types[t])]
                    #print(samp)
                    samp_filter <- rmNullObs(samp_list) 
                    samp_po <- po[names(po) %in% names(samp_filter[[1]])]#filer passing object to the fields present in this sample type
                    sample_mapping <- get_record(all_types[t])
                    #try to find data for each sample mapping query used in the creation record
                    for(m in 1:length(sample_mapping$rows)){#loop through each sample_mapping record that matches the sample type
                      #print(sample_mapping[m])
                      map <- sample_mapping$rows[[m]]
                      map_filter <- map[names(map) %in% samp_po] #make a list of fields for the query
                      map_filter$sampleclass <- NULL
                      fields <- unname(unlist(map_filter))
                      query_fields <- paste(fields, collapse=',')
                      #print(query_fields)
                      appname <- gsub(' ', '%20', map$appname)#html-ify the app name for the query url
                      #get the repeatable name and format it properly
                      if(!is.null(map$repeatablename)){
                        repeatablename <- strsplit(map$repeatablename, '/')[[1]]
                        repeatablename <- repeatablename[length(repeatablename)]
                      }else{
                        repeatablename <- map$repeatablename
                      }
                      # #use barcode if available
                      # if(length(rmNullObs(lapply(samp_list,'[[','sample_barcode'))) == length(samp_list)){
                      #   parent_data <- get_record( idfield = map$fulcrumcode, idvalue = lapply(samp_list,'[[','sample_barcode'))
                      # }else{ #use fulcrumtag if there is no barcode
                      #   parent_data <- get_record(fields = query_fields, appname = appname, repeatable = repeatablename, idfield = map$fulcrumtag, idvalue = lapply(samp_list,'[[','sample_tag'))
                      # }
                      if(length(parent_data$rows) > 0){#only continue of query returned data
                        for(row in 1:length(parent_data$rows)){#loop through each returned record
                          check_data <- parent_data$rows[[row]]
                          if(exists('check_data') && !is.null(check_data) && length(check_data) > 0){
                            #if fulcrumclass exists use that value as the sampleclass field
                            for(f in 1:length(fields)){
                              rename <- names(map_filter)[map_filter == fields[f]]
                              if(rename == 'fulcrumclass'){
                                rename = 'sampleclass'
                              }                          
                              names(check_data)[names(check_data) == fields[f]] <- rename
                            }
                            if(!is.null(check_data$fulcrumcode)){#again use barcode if it exists find the matching sample from the array list
                              samp = samp_filter[lapply(samp_filter,'[[','sample_barcode') == check_data$fulcrumcode][[1]]
                            }else{#if no barcode, use tags
                              samp = samp_filter[lapply(samp_filter,'[[','sample_tag') == check_data$fulcrumtag][[1]]
                            }
                            if(exists('samp') && !is.null(samp) && length(samp) > 0){
                              for(i in 1:length(check_data)){ #go through each field and use the passing object to replace data
                                samp[names(samp_po[samp_po == names(check_data)[i]])] <- check_data[i]
                              }
                            }
                            inventory_samples <- list.append(inventory_samples, samp)
                          }
                          #this next bit was to remake the updated sample array to go back in shipment creation, but that would need an extra field to work so skipping
                          #samp_json <- toJSON(samp, null = 'null', auto_unbox = TRUE)
                          #all_samples <- list.append(all_samples, samp_json)
                        }
                      }
                    }
                  }
                  #print(inventory_samples[1])
                  #print(all_samples[1])
                  if(length(inventory_samples) > 0){
                    print (' >>>>>>> INVENTORY CONTAINER FOUND >>>>>>')
                    
                    
                    
                    #passing object to translate shipment creation sample array names to shipment review field names
                    review_po <- "{\"storageCode\":\"storage_container_barcode\",\"storageWellCoordinates\":\"storage_container_coordinates\",\"sample_volume\":\"sample_volume\",\"analysis_type\":\"analysistype\",\"boutnumber\":\"boutnumber\",\"caption\":\"caption\",\"collection_date\":\"collectdate\",\"container_mass\":\"containermass\",\"filter_volume\":\"filtervolume\",\"location\":\"namedlocation\",\"number_containers\":\"num_containers\",\"photoid\":\"photoid\",\"preservative_concentration\":\"preservativeconcentration\",\"preservative_type\":\"preservativetype\",\"preservative_volume\":\"preservativevolume\",\"remarks\":\"sampleremark\",\"sample_barcode\":\"samplecode\",\"sample_condition\":\"samplecondition\",\"sample_fate\":\"samplefate\",\"sample_mass\":\"samplemass\",\"sample_tag\":\"sampleid\",\"sample_type\":\"sampletype\",\"sampleclass\":\"sampleclass\",\"specimen_count\":\"specimencount\",\"taxonomy\":\"taxonid\",\"view\":\"view\",\"well_coordinate\":\"wellcoordinate\",\"filternumber\":\"filternumber\"}"
                    review_po <- jsonlite::parse_json(review_po)
                    
                    #object to translate field names into fulcrum form value keys for the parent record
                    parent_field_map <- '{"application":"af51","appname_fromcreationapp":"2665","cc_email":"c7b5","cc_list_fops":"d29d","cclist_link":"2f4a","chain_of_custody":"117b","check_this_record_for_accuracy_finalize_the_shipment_and_save_the_finalized_recordafter_the_record_is_finalized_use_the_stork_tool_to_get_a_print_out_of_the_manifest_and_send_notification_of_shipment_to_all_stakeholders_using_the_stork_shipment_verification_tool":"4d65","choosesampletype":"cbb2","cla_only":"8c04","datainvoiced":"f7eb","datareturned":"a0af","dest_city":"f1ab","dest_state":"5f2c","dest_streetaddress":"db0e","dest_zip":"119d","destination_address":"510b","destinationandboutnum_warning":"4ae1","email_who":"320f","error_remarks":"ebf9","filterquerycolumn":"f6b6","filterqueryvalue":"f81c","finalize":"b860","fulcrumversion":"80f3","lab_contact_name":"1581","laboratory_link":"a7d4","list_of_other_sample_classes":"7c8d","load_id":"LOAD_ID","load_status":"LOAD_STATUS","note_one_shipment_record_is_required_for_each_box_to_be_shipped_select_one_sample_type_above_for_inclusion_in_the_shipment_this_choice_will_filter_the_list_of_laboratories_available_to_receive_the_shipment_then_you_will_be_able_to_add_any_sample_to_the_shipment_which_this_laboratory_is_able_to_receive":"fdcf","parent_app_fields":"1a8f","parentappname":"a66c","quarantine_locsbyclass":"bf1d","quarantinedsampleswarning":"d071","quick_links":"e935","repeatablename":"3243","sampleclass_parent":"07c1","samplelist":"988b","samplemappingfields":"b0aa","sampletypes_fromcreationapp":"a010","scientist_1":"af96","scientist_2":"fc76","secondary_email":"a568","sentby_link":"2b69","sentto":"2be7","shipdate_link":"952c","shipment_information":"49ea","sql_finalfields":"2fe2","stork_shipment_verification_tool":"3ef2","use_the_cc_list_field_to_add_additional_email_addresses_that_should_receive_the_final_chain_of_custody_you_can_select_multiple_recipients_one_by_one":"ca30","versioninfo":"1272","domainid":"72c0","shipdate":"7561","coc_number":"de4e","sentby":"3cf0","lab_name":"2540","primary_email":"cc29","carrier":"d296","shipmentmethod":"d7aa","tracking_number":"effd","quarantinestatus":"18c0","holdingtime":"2c74"}'
                    parent_field_map <- jsonlite::parse_json(parent_field_map)
                    parent_form_value <- list()
                    for(pfv in 1:length(parent_field_map)){
                      
                      #new_map <- paste(parent_field_map[names(parent_field_map)[pfv]],pd[names(parent_field_map)[pfv]], sep = '\":\"')
                      #new_map <- paste0('\"', new_map, '\"')
                      if(!is.null(review_record$rows[[1]][names(parent_field_map)[pfv]][[1]])){
                        if(parent_field_map[names(parent_field_map)[pfv]][[1]] %in% c('a7d4','2b69','2f4a')){
                          new_map <- split(x= list(list(record_id = unlist(review_record$rows[[1]][names(parent_field_map)[pfv]][[1]]))), f = parent_field_map[names(parent_field_map)[pfv]][[1]], drop = FALSE)
                        }else if(parent_field_map[names(parent_field_map)[pfv]][[1]] %in% c('af51','72c0','d9f8','cbb2','d296','d7aa','18c0')){
                          new_map <- split(x= list(choice_values = list(review_record$rows[[1]][names(parent_field_map)[pfv]][[1]]), other_values = list()), f = parent_field_map[names(parent_field_map)[pfv]][[1]], drop = FALSE)
                          
                        }else{
                          new_map <- split(x= review_record$rows[[1]][names(parent_field_map)[pfv]][[1]], f = parent_field_map[names(parent_field_map)[pfv]][[1]], drop = FALSE)
                          
                        }
                        parent_form_value <- append(parent_form_value,new_map)
                      }
                    }
                    
                    print('>>>> PARENT FORM VALUES TRANSLATED >>>>>>>')
                    #create a the body to return to the shipment review record
                    record_out <- list(record = c(review_record$rows[[1]]['_record_id'], list(form_values = 
                                                                                                parent_form_value)))
                    #print(record_out)
                    
                    review_children <- get_record(idvalue = review_id)
                    print('>>>> CHILD REVIEW RECORD RETRIEVED >>>>>>>')
                    
                    #object to translate field names into fulcrum form value keys for the child records
                    child_field_map <- '{"boutnumber":"6493","caption":"619f","collectdate":"fe0b","filternumber":"9dd9","namedlocation":"4e15","new_filterquerycolumn":"d3d9","new_filterqueryvalue":"0a85","new_parentappname":"5235","new_repeatablename":"62f5","new_sampleclass":"1a26","new_sqlfinalfields":"8879","photoid":"d567","preservativeconcentration":"50c7","redefined_parent_app_fields":"b7eb","sample_type_child":"b71b","samplecondition":"bd18","sampleremark":"b6c8","storage_container_barcode":"5c93","storage_container_coordinates":"8b40","view":"a797","sampleid":"364e","samplecode":"fb86","samplefate":"1566","sampleclass":"d43d","sampletype":"9a3b","num_containers":"f79e","analysistype":"99e9","taxonid":"8d79","wellcoordinate":"831f","sample_volume":"1b09","filtervolume":"88ce","samplemass":"049f","containermass":"8d77","preservativetype":"6902","preservativevolume":"50ae","specimencount":"eb96"}'
                    child_field_map <- jsonlite::parse_json(child_field_map)
                    
                    ## modify fields
                    combine <- list()
                    for(child_sample in 1:length(review_children$rows)){
                      if(review_children$rows[[child_sample]]$samplecode %in% lapply(inventory_samples,'[[','sample_barcode')){
                        inventory_sample <- inventory_samples[lapply(inventory_samples,'[[','sample_barcode') == review_children$rows[[child_sample]]$samplecode]
                      }else if(review_children$rows[[child_sample]]$sampleid %in% lapply(inventory_samples,'[[','sample_tag')){
                        inventory_sample <- inventory_samples[lapply(inventory_samples,'[[','sample_tag') == review_children$rows[[child_sample]]$sampleid]
                      }
                      for(i in 1:length(inventory_sample[[1]])){
                        rename <- review_po[names(inventory_sample[[1]][i])][[1]]
                        names(inventory_sample[[1]])[names(inventory_sample[[1]]) == names(inventory_sample[[1]][i])] <- rename
                        review_children$rows[[child_sample]][rename] <- inventory_sample[[1]][rename]
                      }
                      child_form_value <- list()
                      for(cfv in 1:length(child_field_map)){
                        if(!is.null(review_children$rows[[child_sample]][names(child_field_map)[cfv]][[1]])){
                          new_map <- split(x= as.character(review_children$rows[[child_sample]][names(child_field_map)[cfv]][[1]]), f = child_field_map[names(child_field_map)[cfv]][[1]], drop = FALSE)
                          child_form_value <- append(child_form_value,new_map)
                        }
                      }
                      child <- c(list(id = review_children$rows[[child_sample]]['_child_record_id'][[1]]), list(form_values = 
                                                                                                                  child_form_value))
                      combine <- list.append(combine, child)
                    }
                    print('>>>> CHILD FORM VALUES TRANSLATED >>>>>>>')
                    #print(combine[1])
                    #add child form values to parent record
                    record_out$record$form_values['3dfd'] <- list(combine)
                    
                    print('updating review record')
                    #body1 = toJSON(record_out, auto_unbox = TRUE, null = 'null')
                    request <- put_record(record_id = review_id, body = record_out)
                    print(paste0('>>>>>> REVIEW PUT REQUEST STATUS CODE = ', request))
                    res$status <- request
                    
                  }else{
                    print( 'no samples processed')
                    res$status <- 204
                    return( list( error="NO SAMPLES FOUND"))
                  }
                }else{
                  print( 'no inventory containers')
                  res$status <- 204
                  return( list( error="NO INVENTORY CONTAINERS FOUND"))
                }
              } else {
                print('no review record exists')
                res$status <- 204
                return( list( error="REVIEW RECORD DOES NOT EXIST"))
              }
              
            }else{
              ## if it's a record.create or record.update but NOT the target app/form, still return a response
              print('payload not from shipment creation')
              res$status <- 204
              return( list( error="NOT SHIPMENT CREATION RECORD"))
            }
            
            # }
          }else{
            ## if there's another type of webhook message (e.g. record.delete), still return a response
            print('record type incorrect')
            res$status <- 204
            return( list( error="NO RECORD CREATE OR UPDATE"))
          }
        }
        
        output$download_manifest <- downloadHandler(
          filename = function() {
            # Make the file name for the downloaded manifest
            if(!is.null(data_parent())){
              # Grab dataset
              dat = data_parent()
              
              # Filter to records from selected fulcrum_id
              dat = dat[unique(input$display_table_rows_selected), ]
              
              # Format e.g. 'D0320190806151244701_manifest_on_2019-08-01.csv'
              unique(paste0(dat$shipmentID[1], "_manifest_on_", Sys.Date(),".csv"))
            }
          },
          content = function(file){
            # Make downloaded manifest
            if(!is.null(data_child())){
              # Grab dataset based on fulcrumID of selected record
              dat = data_child()
              
              # Don't include fulcrum or receipt columns
              dat = dat[, which(!colnames(dat)%in%c(extra_cols, rec_cols))]
              
              # Make sure excel can't screw up the printed tracking number
              dat$trackingNumber = paste("#", dat$trackingNumber)
              
              ## If the data are quarantine sampleclasses, add county information
              ## Then filter by 'quarantine_locations' for each quarantineable sampleclass
              ## To revise 'quarantine status' field in the manifest
              # Default quarantineStatus to 'N'
              dat$quarantineStatus = 'N' # by default, nothing is quarantined
              dat$quarantineSamples = 'none' # by default, nothing is quarantined
              # Per the samp_map table, these sample classes 'quar' are under quarantine 
              quar = samp_map$sampleclass[samp_map$quarantine_status%in%'Y']
              # Check for eligible samples
              if(any(dat$sampleClass%in%quar)){
                # If there is an eligible sample class in the shipment
                # Join all county information by default to the manifest
                dat = dplyr::left_join(dat,plots[,c('plotID', 'county')],
                                       by = c('locationID' = 'plotID'))
                # Get site from locationID
                dat$siteID = substr(dat$locationID,1,4)
                
                # Loop through the list of sites where that sampleClass 
                # is under quarantine and revise manifest accordingly
                for (cls in unique(dat$sampleClass[dat$sampleClass%in%quar])){
                  # sites under quarantine for that class
                  sites = trimws(unlist(strsplit(samp_map$quarantine_locations[samp_map$sampleclass%in%cls],
                                                 split = '\\|')),'both')
                  dat[dat$siteID%in%sites,"quarantineStatus"] = "Y"
                  dat[!dat$siteID%in%sites,"county"] = NA
                }
                if(any(dat$quarantineStatus%in%'Y')){
                  dat$quarantineSamples = 'present'
                }
              }
              
              # Make a Manifest that is easier to print
              colnames(dat)[which(colnames(dat)%in%'namedLocation')] = 'domainID' 
              if(any(samp_map$minified_manifest[samp_map$sampleclass%in%unique(dat$sampleClass)]%in%'yes')){
                dat = pretty_manifest(dat = dat, small = T)
              } else {
                dat = pretty_manifest(dat = dat, small = F)
              }
              
              ## convert fulcrum_data to a matrix or else server will crash
              write.table(x = as.matrix(dat[, colSums(!is.na(dat)) > 0 &!colnames(dat)%in%'fulcrum_id']), file,
                          sep = ",", row.names = FALSE, fileEncoding = "UTF-8",
                          na = '')
            }
          },
          contentType = "text/csv"
        )
        form_values <- get_record(record_id, api_token)
        
        filepath <- draft_manifest_filename
        
        attachment_key <- "cf80"
        
        uploadFile(api_token, record_id, filepath, form_values, attachment_key)
        
        
        
      }
    })
  
}

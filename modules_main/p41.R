# Situation Report Module

# QUESTIONS TO INCLUDE
# nationality
# iar

# UI ****************************************** ================================
p41_edit_UI <- function(id) {
  ns <- NS(id)
  
  # UI Data Entry --------------------------------------------------------------
  fluidPage(
    
    #### Hide errors from displaying
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    # ** Top Panel -------------------------------------------------------------
    fluidRow(column(1,
                    actionButton("go_to_report_list","Back to Report List")
    ),
    column(7,
           ""),
    column(2,
           #actionButton("show", "Delete Form")
    )
    ),
    
    # ** Tabset Panel ----------------------------------------------------------
    tabPanel("SR",
             uiOutput(ns("sr_tabs_ui")),    
    ),
    
    # ** Bottom Panel ----------------------------------------------------------
    hr(),
    fluidRow(
      column(3,
             ""),
      column(1,
             " "
      ),
      column(2,
             "")
    ),
    
    fluidRow(
      column(12, ".")
    ),
    
    # Add Extra space at bottum of page
    fluidRow(
      column(12, "."),
      column(12, "."),
      column(12, ".")
    )
    
  )
  
}

# Submit to HQ Popup -----------------------------------------------------------
# https://stackoverflow.com/questions/48127459/using-modal-window-in-shiny-module
# https://stackoverflow.com/questions/52042747/shiny-modularized-inputs-inside-pop-up-modal-arent-being-written-to-reactiveval

# SERVER ****************************************** ============================
p41_edit_Server <- function(input, output, session, data_i, dataset, dataset_variables,
                            sr_variables, tab_types) {
  
  # OTHER ********************* ------------------------------------------------
  # ** Download P41 ------------------------------------------------------------
  output$p41_pdf = downloadHandler(
    filename = paste0('p41_OB',data_i$ob_no,'.pdf'),
    
    content = function(file) {
      out = knit2pdf(file.path('modules_gen_reports', 'generate_p41_pdf_parent.Rnw'), clean = TRUE)
      #file.rename(out, file) # move pdf to file for downloading
      file.copy(out, file)
    },
    
    contentType = 'application/pdf'
  )
  
  # ** Download Crash Sketch ---------------------------------------------------
  ## Button
  output$download_crash_sketch_review_page <- downloadHandler(
    filename = function() {
      paste0("crash_sketch_OB", data_i$ob_no, ".png")
    },
    content = function(file) {
      
      image_i <- readPNG(file.path("data", 
                                   "crash_sketches",
                                   paste0(data_i$uid, ".png")))
      
      writePNG(image_i, file)
    }
  )
  
  ## UI
  # If no crash sketch uploaded, then no download button
  output$download_crash_sketch_review_page_ui <- renderUI({
    
    img_file_name <- file.path("data", 
                               "crash_sketches",
                               paste0(data_i$uid, ".png"))
    
    if(file.exists(img_file_name)){
      out <- downloadButton(session$ns("download_crash_sketch_review_page"), "Download Crash Sketch")
    } else{
      out <- NULL
    }
    
    out
  })
  
  # # FUNCTIONS TO GENERATE SIT REPORT ********************* ---------------------
  source(file.path("modules_main", "functions.R"))
  
  
  # # AUTOSAVE **************************************---------------------------
  observe({
    
    tmp <- uploaded_crash_sketch_r$value # observe whether crash sketch uploaded
    uploaded_crash_sketch_r$value <<- 2
    
    # ** Save ------------------------------------------------------------------
    for(var in dataset_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character()
    }
    
    data_i$last_updated <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character
    
    for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
    for(var in names(dataset)) dataset[[var]] <- dataset[[var]] %>% as.character()
    
    dataset <- bind_rows(dataset, data_i) %>%
      arrange(desc(last_updated)) %>%
      distinct(uid, report_type, amend_report_id, .keep_all = TRUE)
    
    dataset <- as.data.frame(dataset)
    if(TESTING_SYSTEM){
      saveRDS(dataset, file.path("data", "police_data_test.Rds"), version=2)
    } else{
      saveRDS(aes_cbc_encrypt(serialize(dataset, NULL), key = data_key), file.path("data", "police_data.Rds"), version=2)
    }
    
    #### All data 
    all_data_r <- reactive({
      dataset
    })
    all_data_r <<- all_data_r
    
    # ** Download Sit Report ---------------------------------------------------
    data_i <<- data_i
    #callModule(download_one_sit_report_Server, "d_sr_1", data_i)
    
    # ** Checks ----------------------------------------------------------------
    
    
    # **** General Information -------------------------------------------------
    general_info_vars <- bind_rows(
      data.frame(var="police_division", label="Police Division", default=".", stringsAsFactors=F),
      data.frame(var="police_station", label="Police Station", default=".", stringsAsFactors=F),
      data.frame(var="ob_no", label="O.B. Num", default="", stringsAsFactors=F),
      data.frame(var="iar_no", label="IAR Num", default="", stringsAsFactors=F),
      data.frame(var="accident_type", label="What is the accident type?", default=".", stringsAsFactors=F),
      data.frame(var="hit_and_run", label="Was the crash a hit and run?", default=".", stringsAsFactors=F),
      data.frame(var="accident_date", label="Date of Accident", default=NA, stringsAsFactors=F),
      data.frame(var="accident_time_hour", label="Accident Time [Hour]", default=NA, stringsAsFactors=F),
      data.frame(var="accident_time_minute", label="Accident Time [Minute]", default=NA, stringsAsFactors=F),
      # data.frame(var="accident_reported_date", label="Data Accident Reported", default="", stringsAsFactors=F),
      # data.frame(var="accident_reported_time_hour", label="Time Accident Reported [Hour]", default="", stringsAsFactors=F),
      # data.frame(var="accident_reported_time_minute", label="Time Accident Reported [Minute]", default="", stringsAsFactors=F),
      # data.frame(var="road_name", label="Road Name", default="", stringsAsFactors=F),
      data.frame(var="landmark", label="Neary Landmark", default="", stringsAsFactors=F),
      data.frame(var="officer_rspnd_crash_title", label="Officer Name who Responded to Crash - Title", default="", stringsAsFactors=F),
      data.frame(var="officer_rspnd_crash_firstname", label="Officer Name who Responded to Crash - First Name", default="", stringsAsFactors=F),
      data.frame(var="officer_rspnd_crash_surname", label="Officer Name who Responded to Crash - Surname", default="", stringsAsFactors=F),
      data.frame(var="officer_filled_form_title", label="Officer Name who Filled out Form - Title", default="", stringsAsFactors=F),
      data.frame(var="officer_filled_form_firstname", label="Officer Name who Filled out Form - First Name", default="", stringsAsFactors=F),
      data.frame(var="officer_filled_form_surname", label="Officer Name who Filled out Form - Surname", default="", stringsAsFactors=F),
      data.frame(var="road_authority", label="Road Authority", default=".", stringsAsFactors=F),
      data.frame(var="urban_rural", label="Urban/Rural", default=".", stringsAsFactors=F),
      data.frame(var="speed_limit", label="Speed Limit", default="", stringsAsFactors=F),
      data.frame(var="accident_location", label="Accident Location", default="", stringsAsFactors=F),
      data.frame(var="roadno", label="Road Number", default="", stringsAsFactors=F),
      data.frame(var="ch_reg_no", label="Ch-Reg No.", default="", stringsAsFactors=F)
    )
    
    general_info_missing <- lapply(1:nrow(general_info_vars), function(i){
      
      general_info_vars_i <- general_info_vars[i,]
      
      if(length(data_i[[general_info_vars_i$var]]) %in% 0){
        out <- general_info_vars_i$label # paste0('<p style="color:red;">',general_info_vars_i$label,'</p>')
      } else if(general_info_vars_i$default %in% data_i[[general_info_vars_i$var]]){
        out <- general_info_vars_i$label # paste0('<p style="color:red;">',general_info_vars_i$label,'</p>') # general_info_vars_i$label
      } else{
        out <- NULL
      }
      
      return(out)
    }) %>%
      unlist()
    
    if(length(general_info_missing) > 0){
      # Header
      header <- "<h4>General Information</h4>"
      
      # List of missing variables
      general_info_missing <- general_info_missing %>%
        paste(collapse = "<br>")
      general_info_missing <- paste0('<p style="color:red;">',general_info_missing,'</p>')
      general_info_missing <- paste0(header, general_info_missing)
      
    } else{
      general_info_missing <- NULL
    }
    
    # **** Conditions ----------------------------------------------------------
    conditions_vars <- bind_rows(
      data.frame(var="road_surface", label="Road Surface", default=".", stringsAsFactors=F),
      data.frame(var="surface_width", label="Surface Width", default=NA, stringsAsFactors=F),
      data.frame(var="surface_wet_dry", label="Surface - Wet/Dry", default=NA, stringsAsFactors=F),
      data.frame(var="weather_conditions", label="Weather Conditions", default=NA, stringsAsFactors=F),
      data.frame(var="road_damaged_yn", label="Road Condition", default=".", stringsAsFactors=F)
    )
    
    if("damaged" %in% data_i$road_damaged_yn){
      # Select multiple
      if(is_na_null(data_i$road_damaged_type_potholes) &
         is_na_null(data_i$road_damaged_type_damagededges) & 
         is_na_null(data_i$road_damaged_type_corrugated) & 
         is_na_null(data_i$road_damaged_type_loosestones)){
        
        conditions_vars <- bind_rows(
          conditions_vars,
          data.frame(var="road_damaged_type_potholes", label="How road damanged?", default=NA, stringsAsFactors=F)
        )
        
      } 
    }
    
    conditions_vars <- bind_rows(
      conditions_vars,
      data.frame(var="junction_yn", label="Accident at Junction?", default=".", stringsAsFactors=F)
    )
    
    if("Yes" %in% data_i$junction_yn){
      conditions_vars <- bind_rows(conditions_vars,
                                   data.frame(var="junction_type", label="Junction Type", default=NA, stringsAsFactors=F),
                                   data.frame(var="junction_signs_signals", label="Signs/Signals at Junction?", default=NA, stringsAsFactors=F),
                                   data.frame(var="junction_signals_operating", label="Traffic light operating?", default=NA, stringsAsFactors=F))
    }
    
    conditions_vars <- bind_rows(
      conditions_vars,
      data.frame(var="time_of_day", label="Illumination", default=NA, stringsAsFactors=F),
      data.frame(var="street_lights", label="Street Lights", default=NA, stringsAsFactors=F),
      data.frame(var="railway_crossing", label="Railway level crossing", default=NA, stringsAsFactors=F),
      data.frame(var="railway_crossing", label="Railway level crossing", default=".", stringsAsFactors=F),
      data.frame(var="road_works", label="Road works", default=".", stringsAsFactors=F)
    )
    
    conditions_missing <- lapply(1:nrow(conditions_vars), function(i){
      
      conditions_vars_i <- conditions_vars[i,]
      
      if(length(data_i[[conditions_vars_i$var]]) %in% 0){
        out <- conditions_vars_i$label # paste0('<p style="color:red;">',general_info_vars_i$label,'</p>')
      } else if(conditions_vars_i$default %in% data_i[[conditions_vars_i$var]]){
        out <- conditions_vars_i$label # paste0('<p style="color:red;">',general_info_vars_i$label,'</p>') # general_info_vars_i$label
      } else{
        out <- NULL
      }
      
      return(out)
    }) %>%
      unlist()
    
    if(length(conditions_missing) > 0){
      # Header
      header <- "<h4>Conditions</h4>"
      
      # List of missing variables
      conditions_missing <- conditions_missing %>%
        paste(collapse = "<br>")
      conditions_missing <- paste0('<p style="color:red;">',conditions_missing,'</p>')
      conditions_missing <- paste0(header, conditions_missing)
      
    } else{
      conditions_missing <- NULL
    }
    
    # **** Participants --------------------------------------------------------
    N_participants <- ifelse(is.null(data_i$N_participants), 1, data_i$N_participants)
    N_participants <- ifelse(is.na(N_participants), 1, N_participants)
    participant_missing <- lapply(1:N_participants, function(i){
      
      #### Variables 
      participant_vars <- bind_rows(
        data.frame(var = paste0("prtcpnt_vehpart_type_", i), label = "Vehicle/Participant Type", default = "", stringsAsFactors=F),
        data.frame(var = paste0("prtcpnt_name_", i), label = "Name", default = "", stringsAsFactors=F),
        data.frame(var = paste0("prtcpnt_address_", i), label = "Address", default = "", stringsAsFactors=F),
        data.frame(var = paste0("prtcpnt_telephone_", i), label = "Telephone Number", default = "", stringsAsFactors=F),
        data.frame(var = paste0("prtcpnt_nationality_", i), label = "Nationality", default = "", stringsAsFactors=F)
      )
      
      ## Vehicle questions
      if(!is.null(data_i[[paste0("prtcpnt_vehpart_type_", i)]])){
        if(data_i[[paste0("prtcpnt_vehpart_type_", i)]] %in% c("Saloon Car",
                                                               "Pickup van",
                                                               "Lorry",
                                                               "Lorry + trailer",
                                                               "Bus",
                                                               "Matatu",
                                                               "Motor-cycle",
                                                               "Other Vehicle")){
          participant_vars <- bind_rows(
            participant_vars,
            data.frame(var = paste0("prtcpnt_owner_driver_", i), label = "Owner or Driver?", default = NA, stringsAsFactors=F),
            data.frame(var = paste0("prtcpnt_vehicle_adn_info_", i), label = "Additional information on type of vehicle", default = NA, stringsAsFactors=F)
          )
        }
        
        ## Pedestrian questions
        if(data_i[[paste0("prtcpnt_vehpart_type_", i)]] %in% c("Pedestrian")){
          
          # Only needs to fill on one of them
          if(!is.null((data_i[[paste0("prtcpnt_ped_crossing_", i)]])) & 
             !is.null((data_i[[paste0("prtcpnt_ped_along_", i)]]))){
            if((data_i[[paste0("prtcpnt_ped_crossing_", i)]] %in% ".") & 
               (data_i[[paste0("prtcpnt_ped_along_", i)]] %in% ".")){
              
              participant_vars <- bind_rows(
                participant_vars,
                data.frame(var = paste0("prtcpnt_ped_crossing_", i), label = "Pedestrian crossing or walking along direction", default = ".", stringsAsFactors=F)
              )
              
            }
          }
          
          participant_vars <- bind_rows(
            participant_vars,
            data.frame(var = paste0("prtcpnt_ped_comments_", i), label = "Comments about pedestrian movement", default = "", stringsAsFactors=F)
          )
        }
      }
      
      participant_vars <- bind_rows(
        participant_vars,
        data.frame(var = paste0("prtcpnt_vehicle_damage_", i), label = "Brief details of damage", default = "", stringsAsFactors=F)
      )
      
      ## Reg/Liscence Numbers
      if(!(TRUE %in% (c("Pedestrian", "Bicycle", "Unknown") %in% data_i[[paste0("prtcpnt_vehpart_type_", i)]]))){
        participant_vars <- bind_rows(
          participant_vars,
          data.frame(var = paste0("prtcpnt_regnumber_", i), label = "Registration Number", default = "", stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_drivinglic_", i), label = "Driving License No", default = "", stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_drivinglic_valid_", i), label = "Driving License No - Valid?", default = ".", stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_roadlic_", i), label = "Road License No", default = "", stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_roadlic_valid_", i), label = "Road License No - Valid?", default = ".", stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_insurance_", i), label = "Insurance Company", default = "", stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_inscert_", i), label = "Ins Certificate No", default = "", stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_inscert_valid_", i), label = "Ins Certificate No - Valid?", default = ".", stringsAsFactors=F),
        )
        
        ## PSV Number
        # Only include if doesn't mark "not applicable"
        if(!("Not Applicable" %in% data_i[[paste0("prtcpnt_psv_valid_", i)]])){
          participant_vars <- bind_rows(
            participant_vars,
            data.frame(var = paste0("prtcpnt_psv_", i), label = "P.S.V Lisence No", default = "", stringsAsFactors=F),
            data.frame(var = paste0("prtcpnt_psv_valid_", i), label = "P.S.V Lisence No - Valid?", default = ".", stringsAsFactors=F),
          )
        }
        
      }
      

      
      ## Injured
      participant_vars <- bind_rows(
        participant_vars,
        data.frame(var = paste0("prtcpnt_injured_yn_", i), label = "Was the participant injured?", default = ".", stringsAsFactors=F)
      )
      
      if("Yes" %in% data_i[[paste0("prtcpnt_injured_yn_", i)]]){
        participant_vars <- bind_rows(
          participant_vars,
          data.frame(var = paste0("prtcpnt_injurytype_", i), label = "Injury Type", default = "", stringsAsFactors=F),
          
          data.frame(var = paste0("prtcpnt_personclass_", i), label = "Class of Person", default = NA, stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_age_years_", i), label = "Age", default = NA, stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_gender_", i), label = "Sex", default = NA, stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_position_", i), label = "Position in Vehicle", default = ".", stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_seatbelt_", i), label = "Safety Belt in Use", default = ".", stringsAsFactors=F),
          
          data.frame(var = paste0("prtcpnt_hospital_taken_", i), label = "Hospital/Mortuary Taken To", default = "", stringsAsFactors=F),
          data.frame(var = paste0("prtcpnt_hospital_admno_", i), label = "Hospital/Mortuary Admin No", default = "", stringsAsFactors=F)
        )
      }
      
      participant_missing <- lapply(1:nrow(participant_vars), function(var_i){
        
        participant_vars_i <- participant_vars[var_i,]
        
        if(length(data_i[[participant_vars_i$var]]) %in% 0){
          out <- participant_vars_i$label
        } else if(participant_vars_i$default %in% data_i[[participant_vars_i$var]]){
          out <- participant_vars_i$label
        } else if(is.na(data_i[[participant_vars_i$var]])){
          out <- participant_vars_i$label
        } else{
          out <- NULL
        }
        
        return(out)
        
      }) %>%
        unlist()
      
      if(length(participant_missing) > 0){
        participant_missing <- participant_missing %>%
          paste(collapse = "<br>")
        participant_missing <- paste0('<p style="color:red;">',participant_missing,'</p>')
        participant_missing <- paste0("<h5>Participant ", i, "</h5>", 
                                      participant_missing)
      } else{
        participant_missing <- NULL
      }
      
      return(participant_missing)
      
    }) %>%
      unlist()
    
    if(length(participant_missing) %in% 0){
      participant_missing <- NULL
    } else{
      participant_missing <- paste0("<h4>Participants</h4>", 
                                    paste(participant_missing,
                                          collapse = ""))
    }
    
    # **** Victims -------------------------------------------------------------
    N_victims <- ifelse(is.null(data_i$N_victims), 1, data_i$N_victims)
    N_victims <- ifelse(is.na(N_victims), 1, N_victims)
    
    if(N_victims %in% 0){
      victim_missing <- NULL
    } else{
      
      victim_missing <- lapply(1:N_victims, function(i){
        
        #### Variables
        victim_vars <- bind_rows(
          data.frame(var = paste0("victim_name_", i), label = "Name", default = "", stringsAsFactors=F),
          data.frame(var = paste0("victim_address_", i), label = "Address", default = "", stringsAsFactors=F),
          data.frame(var = paste0("victim_refnum_", i), label = "Part/Veh Ref No", default = "", stringsAsFactors=F),
          data.frame(var = paste0("victim_age_years_", i), label = "Age", default = NA, stringsAsFactors=F),
          data.frame(var = paste0("victim_gender_", i), label = "Gender", default = ".", stringsAsFactors=F),
          data.frame(var = paste0("victim_telephone_", i), label = "Telephone Number", default = "", stringsAsFactors=F),
          data.frame(var = paste0("victim_personclass_", i), label = "Class of Person", default = NA, stringsAsFactors=F),
          data.frame(var = paste0("victim_position_", i), label = "Position in Vehicle", default = ".", stringsAsFactors=F),
          data.frame(var = paste0("victim_seatbelt_", i), label = "Seat Belt Use", default = ".", stringsAsFactors=F),
          data.frame(var = paste0("victim_injurytype_", i), label = "Injury Type", default = NA, stringsAsFactors=F),
          data.frame(var = paste0("victim_hospital_taken_", i), label = "Hospital/Mortuary Taken To", default = "", stringsAsFactors=F),
          data.frame(var = paste0("victim_hospital_admno_", i), label = "Hospital/Mortuary Admission No.", default = "", stringsAsFactors=F)
        )
        
        #### Vector of missing variables
        victim_missing <- lapply(1:nrow(victim_vars), function(var_i){
          
          victim_vars_i <- victim_vars[var_i,]
          
          if(length(data_i[[victim_vars_i$var]]) %in% 0){
            out <- victim_vars_i$label
          } else if(victim_vars_i$default %in% data_i[[victim_vars_i$var]]){
            out <- victim_vars_i$label
          } else if(is.na(data_i[[victim_vars_i$var]])){
            out <- victim_vars_i$label
          } else{
            out <- NULL
          }
          
          return(out)
          
        }) %>%
          unlist()
        
        #### To Text
        if(length(victim_missing) > 0){
          victim_missing <- victim_missing %>%
            paste(collapse = "<br>")
          victim_missing <- paste0('<p style="color:red;">',victim_missing,'</p>')
          victim_missing <- paste0("<h5>Victim ", i, "</h5>", 
                                   victim_missing)
        } else{
          victim_missing <- NULL
        }
        
        return(victim_missing)
        
      }) %>%
        unlist()
      
      #### Text with Header
      if(length(victim_missing) %in% 0){
        victim_missing <- NULL
      } else{
        victim_missing <- paste0("<h4>Victim</h4>", 
                                 paste(victim_missing,
                                       collapse = ""))
      }
      
    }
    
    # **** 0 Victim Check ------------------------------------------------------
    # TODO: Need to take into account participants AND victims for this check.
    #zero_victim_check <- NULL
    
    #if(data_i$N_victims %in% 0){
    #  if(data_i$accident_type %in% c("Slight", "Serious", "Fatal")){
    #    zero_victim_check <- paste0("You indicated t")
    #  }
    #}
    
    # **** Crash Location ------------------------------------------------------
    crash_loc_missing_txt <- '<h4>Crash Location</h4><p style="color:red;">Place pin on location where crash occured</p>'
    
    if(is.null(data_i$crash_lat)){
      crash_loc_missing <- crash_loc_missing_txt
    } else if (is.na(data_i$crash_lat)){
      crash_loc_missing <- crash_loc_missing_txt
    } else if (data_i$crash_lat %in% ""){
      crash_loc_missing <- crash_loc_missing_txt
    } else{
      crash_loc_missing <- NULL
    }
    
    # **** Cause Code ----------------------------------------------------------
    if(is_na_null(data_i$cause_code_category)){
      cause_code_cat_missing <- "Cause Code Category"
    } else{
      cause_code_cat_missing <- NULL
    }
    
    if(is_na_null(data_i$cause_code_value)){
      cause_code_missing <- "Cause Code"
    } else{
      cause_code_missing <- NULL
    }
    
    if("." %in% data_i$alcohol_involved){
      alcohol_missing <- "Alcohol involved?"
    } else{
      alcohol_missing <- NULL
    }
    
    if("" %in% data_i$primarily_responsible){
      primarily_responsible_missing <- "Primarily responsible?"
    } else{
      primarily_responsible_missing <- NULL
    }
    
    cause_code_vars_missing <- list(alcohol_missing,
                                    primarily_responsible_missing,
                                    cause_code_cat_missing,
                                    cause_code_missing) %>%
      unlist()
    
    if(length(cause_code_vars_missing) %in% 0){
      cause_code_vars_missing <- NULL
    } else{
      cause_code_header <- "<h4>Cause and responsibility</h4>"
      cause_code_vars_missing <- paste0('<p style="color:red;">',
                                        cause_code_vars_missing %>% paste(collapse = "<br>"),
                                        '</p>')
      
      cause_code_vars_missing <- paste0(cause_code_header,
                                        cause_code_vars_missing)
    }
    
    # **** Remarks and Prosecution ---------------------------------------------
    if("" %in% data_i$remarks){
      remarks_missing <- "Remarks of the investigating officer"
    } else{
      remarks_missing <- NULL
    }
    
    if(is_na_null(data_i$intended_prosecution)){
      intended_prosecution_missing <- "Notice of intended prosecution been served?"
    } else{
      intended_prosecution_missing <- NULL
    }
    
    remarks_prosec_vars_missing <- list(remarks_missing,
                                        intended_prosecution_missing) %>%
      unlist()
    
    if(length(remarks_prosec_vars_missing) %in% 0){
      remarks_prosec_vars_missing <- NULL
    } else{
      remarks_prosec_header <- "<h4>Remarks and Prosecution</h4>"
      remarks_prosec_vars_missing <- paste0('<p style="color:red;">',
                                            remarks_prosec_vars_missing %>% paste(collapse = "<br>"),
                                            '</p>')
      
      remarks_prosec_vars_missing <- paste0(remarks_prosec_header,
                                            remarks_prosec_vars_missing)
    }
    
    # **** Crash Sketch --------------------------------------------------------
    png_name <- file.path("data", "crash_sketches", paste0(data_i$uid[1], ".png"))
    #if(!("yes" %in% data_i$crash_sketch_uploaded)){
    if(!file.exists(png_name)){  
      crash_sketch_missing <- "Crash Sketch"
    } else{
      crash_sketch_missing <- NULL
    }
    
    crash_sketch_vars_missing <- crash_sketch_missing
    
    if(length(crash_sketch_missing) %in% 0){
      crash_sketch_vars_missing <- NULL
    } else{
      crash_sketch_header <- "<h4>Crash Sketch</h4>"
      crash_sketch_vars_missing <- paste0('<p style="color:red;">',
                                          crash_sketch_vars_missing %>% paste(collapse = "<br>"),
                                          '</p>')
      
      crash_sketch_vars_missing <- paste0(crash_sketch_header,
                                          crash_sketch_vars_missing)
    }
    
    # **** Render Text Output --------------------------------------------------
    #### Prepp Output
    sr_checks_txt <- paste(general_info_missing,
                           crash_loc_missing,
                           participant_missing,
                           victim_missing,
                           conditions_missing,
                           cause_code_vars_missing,
                           remarks_prosec_vars_missing,
                           crash_sketch_vars_missing)
    allow_submission <- F
    if(length(sr_checks_txt) %in% 0) allow_submission <- T
    
    if(allow_submission){
      sr_checks_txt <- "<h3>All variables filled in</h3>"
    } else{
      sr_checks_txt <- paste0("<h2>Missing Variables</h2>",
                              "<h3>Enter data for missing variables then you will be able to submit</h3>",
                              "<hr>",
                              sr_checks_txt)
    }
    
    output$sr_report_checks <- renderText({
      sr_checks_txt
    })
    
    output$submit_to_hq_button_ui <- renderUI({
      #actionButton(session$ns("submit_to_hq_button"), "Submit to HQ")
      actionButton(session$ns("submit_to_hq_button"), "Submit to HQ")
      
      # if(allow_submission){
      #   
      # } else{
      #   NULL
      # }
      
    })
    
    # ** Required Variables ----------------------------------------------------
    # disable_submit_hq <- T
    # 
    # if(is.null(input$police_division)){
    #   disable_submit_hq <- T
    # } else if ("." %in% input$police_division){
    #   disable_submit_hq <- T
    # } else{
    #   disable_submit_hq <- F
    # }
    # 
    # 
    # output$fill_in_rqr_vars_warning <- renderText({
    #   if(disable_submit_hq){
    #     out <- "<h5 style='color:red;'><b>In order to submit, fill in required variables.</b></h5>"
    #     
    #   } else{
    #     out <- ""
    #   }
    #   out
    # })
    # 
    # 
    # if(disable_submit_hq){
    #   shinyjs::disable("submit_to_hq_button")
    # } else{
    #   shinyjs::enable("submit_to_hq_button")
    # }
    
    allow_feedback_finalize <- !("" %in% input$officer_feedback_title) &
      !("" %in% input$officer_feedback_firstname) & 
      !("" %in% input$officer_feedback_surname)
    
    #  submit_for_feedback
    if(allow_feedback_finalize){
      shinyjs::enable("finalize")
      shinyjs::enable("submit_for_feedback")
    } else{
      shinyjs::disable("finalize")
      shinyjs::disable("submit_for_feedback")
    }
    
    
  })
  
  # 
  
  
  
  # TABPANEL ******************** ----------------------------------------------
  # Sit Report Data Entry UI ---------------------------------------------------
  ui_data_entry_qs <- navlistPanel(
    widths=c(2,10),
    "Crash Details",
    
    # ** Basic Crash Information 
    tabPanel("General Information",
             uiOutput(session$ns("crash_info_sr")),
    ),
    # ** Location of Crash
    tabPanel("Crash Location",
             column(12, align="center",
                    h4("Click on the location where the crash occurred")
             ),
             
             leafletOutput(session$ns("map")),
             
             column(12, align="center",
                    uiOutput(session$ns("crash_latlon_inputs"))
             )
             
    ),
    # ** Participants 
    tabPanel("Participants",
             
             fluidRow(column(4,
                             ""),
                      uiOutput(session$ns("N_participant_qs"))
             ),
             hr(),
             uiOutput(session$ns("participant_main_qs_sr")),
             
    ),
    # ** Crash Description 
    tabPanel("Crash Description",
             uiOutput(session$ns("crash_description")),
    ),
    # ** Victims 
    tabPanel("Victims",
             
             fluidRow(column(4,
                             ""),
                      #column(4,
                      #       numericInput("N_victims","How many victims are there that are not participants?",0,min=0,max=10,step=1))
                      uiOutput(session$ns("N_victims_qs"))
             ),
             uiOutput(session$ns("victim_1_qs_sr")),
             uiOutput(session$ns("victim_2_qs_sr")),
             uiOutput(session$ns("victim_3_qs_sr")),
             uiOutput(session$ns("victim_4_qs_sr")),
             uiOutput(session$ns("victim_5_qs_sr")),
             uiOutput(session$ns("victim_6_qs_sr")),
             uiOutput(session$ns("victim_7_qs_sr")),
             uiOutput(session$ns("victim_8_qs_sr")),
             uiOutput(session$ns("victim_9_qs_sr")),
             uiOutput(session$ns("victim_10_qs_sr"))
             
    ),
    # ** Cause Code 
    tabPanel("Cause Code",
             
             # TODO: Put into UI
             fluidRow(column(4,
                             uiOutput(session$ns("cause_code_category_ui"))   
             ),
             uiOutput(session$ns("cause_code_ui"))
             )
             
    )
    
  )
  
  gen_sr_tabs <- function(tab_type){
    renderUI({
      
      div(
        # ** Enter Data --------------------------------------------------------
        if(tab_type %in% "Enter Data"){
          
          fluidRow(
            column(12,
                   ui_data_entry_qs
            ),
          )
          
          # ** Submit to HQ ----------------------------------------------------
        } else if(tab_type %in% "Submit to HQ"){
          fluidRow(
            column(6,
                   uiOutput(session$ns("sr_report_checks"))
            ),
            column(6,
                   fluidRow(
                     column(12, align = "center",
                            h3("Provide Comments to HQ")
                     )
                   ),
                   fluidRow(
                     column(12, align = "center",
                            textAreaInput(session$ns("p41_station_comments"), "If needed, provide comments for HQ"),
                     )
                   ),
                   fluidRow(
                     column(12, align = "center", strong("Title and Name")),
                   ),
                   fluidRow(
                     column(3, textInput(session$ns("station_p41_feedback_title"), "Title", "")),
                     column(3, textInput(session$ns("station_p41_feedback_firstname"), "First Name", "")),
                     column(3, textInput(session$ns("station_p41_feedback_surname"), "Surname", "")),
                     column(3, textInput(session$ns("station_p41_feedback_snumber"), "Service No.", ""))
                   ),
                   br(),
                   br(),
                   br(),
                   br(),
                   fluidRow(
                     column(12, align = "center",
                            #disabled(
                            actionButton(session$ns("submit_to_hq_button"), "Submit to HQ")
                            #)
                     )
                   ),
                   fluidRow(
                     column(8, offset = 2, align = "center",
                            h5(htmlOutput(session$ns("fill_in_rqr_vars_warning")))
                     )
                   ),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   fluidRow(
                     column(12, align = "center",
                            downloadButton(session$ns('p41_pdf'), label="Download P41"),
                     )
                   ),
                   fluidRow(
                     column(12, align = "center",
                            uiOutput(session$ns('download_crash_sketch_review_page_ui'))
                     )
                   )
                   
            )
            
          )
          
          # ** Review Sit Report - hq ------------------------------------------
        } else if(tab_type %in% "Review P41" & USER_ROLE %in% "hq"){
          fluidRow(
            column(12, align = "center",
                   
                   fluidRow(
                     column(12,  offset = 2, align = "left",
                            htmlOutput(session$ns("p41_station_comments_txt")),
                     )
                   ),
                   br(),
                   fluidRow(
                     column(12,  offset = 2, align = "left",
                            htmlOutput(session$ns("station_p41_feedback_title_name_txt")),
                     )
                   ),
                   
                   fluidRow(
                     column(12, align = "center",
                            h3("Provide Feedback")
                     )
                   ),
                   fluidRow(
                     column(12, align = "center",
                            textAreaInput(session$ns("sr_hq_comments"), "If needed, provide comments for changes to the p41."),
                     )
                   ),
                   br(),
                   fluidRow(
                     column(12, align = "center", strong("Title and Name")),
                   ),
                   fluidRow(
                     column(6, offset = 3,
                            fluidRow(
                              column(3, textInput(session$ns("officer_feedback_title"), "Title", "")),
                              column(3, textInput(session$ns("officer_feedback_firstname"), "First Name", "")),
                              column(3, textInput(session$ns("officer_feedback_surname"), "Surname", "")),
                              column(3, textInput(session$ns("officer_feedback_snumber"), "Service No.", ""))
                            )
                     )
                   ),
                   fluidRow(
                     column(12, align = "center",
                            #column(6,
                            actionButton(session$ns("submit_for_feedback"), "Send Comments to Officer for Edits"),
                            #        ),
                            #column(6,
                            actionButton(session$ns("finalize"), div(tags$b("Approve", style = "color: red;")))
                            # )
                     )
                   ),
                   fluidRow(
                     column(12, align = "center",
                            h6("Fill in title, name and surname to enable buttons")
                     )
                   ),
                   br(),
                   br(),
                   br(),
                   fluidRow(
                     column(12, align = "center",
                            downloadButton(session$ns('p41_pdf'), label="Download P41")
                     )
                   ),
                   fluidRow(
                     column(12, align = "center",
                            uiOutput(session$ns('download_crash_sketch_review_page_ui'))
                     )
                   )
            )
          )
          # ** Review Sit Report - officer -------------------------------------
        } else if(tab_type %in% "Review P41" & USER_ROLE %in% "officer"){
          fluidRow(
            column(12, align = "center",
                   h3("Submitted to HQ"),
                   fluidRow(
                     column(12, align = "center",
                            
                            strong("The situation report has been submitted to HQ 
                                   and is pending approval. HQ will either approve or send back for edits.")
                            
                     )
                   ),
                   br(),
                   br(),
                   br(),
                   br(),
                   fluidRow(
                     column(12, align = "center",
                            downloadButton(session$ns('p41_pdf'), label="Download P41")
                     )
                   )
            )
          )
          # ** Review Feedback from HQ -----------------------------------------
        } else if (tab_type %in% "Review Feedback from HQ"){
          fluidRow(
            fluidRow(
              column(12, align = "center",
                     h4("Feedback from HQ"),
                     br(),
                     column(8,  offset = 2, align = "left",
                            textOutput(session$ns("sr_hq_comments_txt")),
                     )
              )
            ),
            br(),
            fluidRow(
              column(12, offset = 2, align = "left",
                     htmlOutput(session$ns("officer_feedback_title_name_txt"))
              )
            )
          )
        } else if (tab_type %in% "Finalized"){
          fluidRow(
            h3("finalized")
          )
        }
        
      )
      
    })
  }
  
  output$sr_tabs_ui <- renderUI({
    do.call(tabsetPanel,
            lapply(tab_types,
                   function(i) tabPanel(i, gen_sr_tabs(i) )))
  })
  
  
  # [Tabset]: Editing **************************** ---------------------------
  source(file.path("modules_main", "edit_p41_server.R"), local=T)
  
  # [Tabset]: Submit SR to HQ **************************** ---------------------
  
  # ** Submit to HQ Button -----------------------------------------------------
  
  submit_to_hq_modal <- function(session) {
    #modalDialog(actionButton(session$ns("submit_to_hq_button"), "Close Modal"))
    
    modalDialog(
      
      ifelse(curl::has_internet(),
             "Please click \"Yes\" to confirm the submission; the submission will then be sent to HQ.",
             "Submit to HQ. Aftering clicking \"Yes\", you will need to connect to internet and click \"Sync Data with Server\" for HQ to receive the report"),
      footer = tagList(
        actionButton(session$ns("submit_to_hq_no"), "No"),
        actionButton(session$ns("submit_to_hq_yes"), div(tags$b("Yes", style = "color: red;")))
      )
    )
    
    
  }
  
  # open modal on button click
  observeEvent(input$submit_to_hq_button,
               ignoreNULL = T,   # Show modal on start up
               showModal(submit_to_hq_modal(session))
  )
  
  # Yes - Submit
  observeEvent(input$submit_to_hq_yes, { 
    
    # ** Change Status - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    data_i$stage_p41 <- "submitted_to_hq"
    data_i$stage_p41_num <- as.numeric(data_i$stage_p41_num) + 1
    
    # ** Save - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    for(var in dataset_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character()
    }
    
    data_i$last_updated <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character
    
    for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
    for(var in names(dataset)) dataset[[var]] <- dataset[[var]] %>% as.character()
    
    dataset <- bind_rows(dataset, data_i) %>%
      arrange(desc(last_updated)) %>%
      distinct(uid, report_type, amend_report_id, .keep_all = TRUE)
    
    dataset <- as.data.frame(dataset)
    if(TESTING_SYSTEM){
      saveRDS(dataset, file.path("data", "police_data_test.Rds"), version=2)
    } else{
      saveRDS(aes_cbc_encrypt(serialize(dataset, NULL), key = data_key), file.path("data", "police_data.Rds"), version=2)
    }
    
    #### All data 
    all_data_r <- reactive({
      dataset
    })
    all_data_r <<- all_data_r
    
    # ** Back to report list - - - - - - - - - - - - - - - - - - - - - - - - - - 
    go_to_report_list_r$value <<- 1
    update_figures_r$value <<- 1
    update_map_r$value <<- 1
    update_p69_r$value <<- 1
    update_summary_table_r$value <<- 1
    sync_server_trigger_r$value <<- 21
    
    removeModal() 
  })
  
  # Yes - Submit
  observeEvent(input$submit_to_hq_no, { 
    removeModal() 
  })
  
  
  # [Tabset]: Review Sit Report **************************** -------------------
  output$p41_station_comments_txt <- renderText({
    
    out <- ""
    
    if(!is.null(data_i$p41_station_comments)){
      if(!is.na(data_i$p41_station_comments)){
        if(nchar(data_i$p41_station_comments) > 1){
          
          out <- paste("<h3>Message from Officer:</h3>", 
                       data_i$p41_station_comments)
          
        }
      }
    }
    
    out
    
  })
  
  output$station_p41_feedback_title_name_txt <- renderText({
    
    out <- ""
    
    if(!is.null(data_i$p41_station_comments)){
      if(!is.na(data_i$p41_station_comments)){
        if(nchar(data_i$p41_station_comments) > 1){
          
          out <- paste("<strong>Message from:</strong>", 
                       data_i$station_p41_feedback_title, 
                       data_i$station_p41_feedback_firstname, 
                       data_i$station_p41_feedback_surname,
                       "<br>", 
                       "<strong>Service Number:</strong>", data_i$station_p41_feedback_snumber,
                       "<hr>")
          
        }
      }
    }
    
    out
    
  })
  
  # ** Submit for Feedback -----------------------------------------------------
  observeEvent(input$submit_for_feedback,{
    
    # ** Change Status - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    data_i$stage_p41 <- "hq_provided_feedback"
    data_i$stage_p41_num <- as.numeric(data_i$stage_p41_num) + 1
    
    # ** Save - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    for(var in c("sr_hq_comments")){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character()
    }
    
    data_i$last_updated <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character
    
    for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
    for(var in names(dataset)) dataset[[var]] <- dataset[[var]] %>% as.character()
    
    dataset <- bind_rows(dataset, data_i) %>%
      arrange(desc(last_updated)) %>%
      distinct(uid, report_type, amend_report_id, .keep_all = TRUE)
    
    dataset <- as.data.frame(dataset)
    if(TESTING_SYSTEM){
      saveRDS(dataset, file.path("data", "police_data_test.Rds"), version=2)
    } else{
      saveRDS(aes_cbc_encrypt(serialize(dataset, NULL), key = data_key), file.path("data", "police_data.Rds"), version=2)
    }
    
    #### All data 
    all_data_r <- reactive({
      dataset
    })
    all_data_r <<- all_data_r
    
    # ** Back to report list - - - - - - - - - - - - - - - - - - - - - - - - - - 
    go_to_report_list_r$value <<- 1
    update_figures_r$value <<- 1
    update_map_r$value <<- 1
    update_p69_r$value <<- 1
    update_summary_table_r$value <<- 1
    sync_server_trigger_r$value <<- 51
    
    removeModal() 
    
    shinyalert(title = ifelse(curl::has_internet(), 
                              "Feedback Sent to Station", 
                              "Feedback Recorded"),
               text = ifelse(curl::has_internet(),
                             "The P41 has been sent back to the station for edits.",
                             "For the station to receive the feedback, connect to internet and click the \"Sync Data with Server\" button."),
               type = "success")
    
  })
  
  # ** Finalize ----------------------------------------------------------------
  observeEvent(input$finalize,{
    
    # ** Change Status - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    data_i$stage_p41 <- "finalized"
    data_i$stage_p41_num <- as.numeric(data_i$stage_p41_num) + 1
    
    # ** Save - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    for(var in dataset_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character()
    }
    
    data_i$last_updated <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character
    
    for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
    for(var in names(dataset)) dataset[[var]] <- dataset[[var]] %>% as.character()
    
    dataset <- bind_rows(dataset, data_i) %>%
      arrange(desc(last_updated)) %>%
      distinct(uid, report_type, amend_report_id, .keep_all = TRUE)
    
    dataset <- as.data.frame(dataset)
    if(TESTING_SYSTEM){
      saveRDS(dataset, file.path("data", "police_data_test.Rds"), version=2)
    } else{
      saveRDS(aes_cbc_encrypt(serialize(dataset, NULL), key = data_key), file.path("data", "police_data.Rds"), version=2)
    }
    
    #### All data 
    all_data_r <- reactive({
      dataset
    })
    all_data_r <<- all_data_r
    
    # ** Back to report list - - - - - - - - - - - - - - - - - - - - - - - - - - 
    go_to_report_list_r$value <<- 1
    update_figures_r$value <<- 1
    update_map_r$value <<- 1
    update_p69_r$value <<- 1
    update_summary_table_r$value <<- 1
    sync_server_trigger_r$value <<- 81
    
    removeModal() 
    
    shinyalert(title = "Report Finalized",
               text = ifelse(curl::has_internet(),
                             "The P41 has been finalized.",
                             "The P41 has been finalized. For all stations to see the P41 as finalized, connect to internet and click the \"Sync Data with Server\" button."),
               type = "success")
    
  })
  
  
  
  
  
  
  # [Tabset]: Review Comments from HQ *********************** ------------------
  output$sr_hq_comments_txt <- renderText({
    data_i$sr_hq_comments
  })
  
  output$officer_feedback_title_name_txt <- renderText({
    
    paste("<strong>Feedback provided by:</strong>", 
           data_i$officer_feedback_title, 
           data_i$officer_feedback_firstname, 
           data_i$officer_feedback_surname,
          "<br>", 
          "<strong>Service Number:</strong>", data_i$officer_feedback_snumber)
    
  })
  

  
  
  
  
}


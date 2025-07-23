# Situation Report Module

# UI ##################################################################### -----
sitreport_UI <- function(id) {
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
           uiOutput(ns("delete_report_button_ui"))
    )
    ),
    
    # ** Tabset Panel ----------------------------------------------------------
    tabPanel("SR",
             uiOutput(ns("sr_tabs_ui")),    
    ),
    
    # ** Bottom Panel ----------------------------------------------------------
    hr(),
    br(),
    br(),
    br(),
    br(),
    br()
  )
}

# SERVER ################################################################# -----
sitreport_Server <- function(input, output, session, data_i, dataset, dataset_variables,
                             sr_variables, tab_types) {
  
  source(file.path("modules_main", "functions.R"))
  
  #shinyjs::disable(session$ns("ob_no"))
  
  # AUTOSAVE **************************************-----------------------------
  observe({
    
    # ** Save ------------------------------------------------------------------
    #### Add variables to data_i
    for(var in dataset_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character()
    }
    
    for(var in sr_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character() %>% toupper()
    }
    
    
    #### Check number of particiapnts and victims
    # If number of victims (for eg) is n, delete all data for victims n+1, n+2. If 
    # user changes from n+1 to n victims, data for n+1 victims will still be there
    
    # Victims
    if(!is.null(data_i$N_victims)){
      if(!is.na(as.numeric(data_i$N_victims))){
        
        varnames <- names(data_i)
        
        ## Grab number at end (eg, victim_age_3 --> 3)
        varname_ending_num <- varnames %>% str_replace_all(".*_", "") %>% as.numeric()
        varname_victim <- varnames %>% str_detect("^victim_")
        
        varname_to_na <- (((varname_ending_num) > as.numeric(data_i$N_victims)) %in% TRUE) & varname_victim
        
        data_i <- data_i %>%
          mutate_at(varnames[varname_to_na], ~NA)
        
      }
    }
    
    ## Participants
    if(!is.null(data_i$N_participants)){
      if(!is.na(as.numeric(data_i$N_participants))){
        
        varnames <- names(data_i)
        
        ## Grab number at end (eg, victim_age_3 --> 3)
        varname_ending_num <- varnames %>% str_replace_all(".*_", "") %>% as.numeric()
        varname_prtcpnt <- varnames %>% str_detect("^prtcpnt_")
        
        varname_to_na <- (((varname_ending_num) > as.numeric(data_i$N_participants)) %in% TRUE) & varname_prtcpnt
        
        data_i <- data_i %>%
          mutate_at(varnames[varname_to_na], ~NA)
        
      }
    }
    
    #### Add last updated
    data_i$last_updated <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character
    
    #### All as character to avoid variable type conflicts
    for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
    for(var in names(dataset)) dataset[[var]] <- dataset[[var]] %>% as.character()
    
    #### Add to full dataset
    dataset <- bind_rows(dataset, data_i) %>%
      arrange(desc(last_updated)) %>%
      distinct(uid, report_type, amend_report_id, .keep_all = TRUE)
    
    #### Save
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
    
    # ** Auto Generated Sit Report ---------------------------------------------
    autogen_sit_report <- function(data_i){
      fluidRow(
        fluidRow(
          column(4, sr_row1_c1_fun(data_i)),
          column(4, sr_row1_c2_fun(data_i)),
          column(4, sr_row1_c3_fun(data_i))
        ),
        fluidRow(column(12, sr_row2_fun(data_i))),
        fluidRow(column(12, sr_row3_fun(data_i))),
        fluidRow(column(12, sr_row4_fun(data_i))),
        fluidRow(column(12, sr_row5_fun(data_i))),
        fluidRow(
          column(4, sr_row6_c1_fun(data_i)),
          column(4, sr_row6_c2_fun(data_i)),
          column(4, sr_row6_c3_fun(data_i))
        ),
        br(),
        fluidRow(
          column(12,
                 main_text_fun(data_i,
                               participant_text_fun(data_i),
                               victim_text_fun(data_i))
          )
        ),
        br(),
        fluidRow(column(12, sr_row7_fun(data_i))),
        fluidRow(column(12, sr_row8_fun(data_i))),
        fluidRow(column(12, sr_row9_fun(data_i))),
        fluidRow(column(12, sr_row10_fun(data_i)))
      )
    }
    
    output$sr_report_auto <- renderUI(autogen_sit_report(data_i))
    output$sr_report_auto_2 <- renderUI(autogen_sit_report(data_i))
    
    # ** Manual Sit Report 1 ---------------------------------------------------
    # For some reason need to generate two (v1 and v2); they're identical, but
    # they're used in different tabsets
    
    manualgen_sit_report <- function(data_i){
      fluidRow(
        fluidRow(
          column(4, data_i$sr_row1_c1 %>% toupper()),
          column(4, data_i$sr_row1_c2 %>% toupper()),
          column(4, data_i$sr_row1_c3 %>% toupper())
        ),
        fluidRow(column(12,data_i$sr_row2 %>% toupper())),
        fluidRow(column(12,data_i$sr_row3 %>% toupper())),
        fluidRow(column(12,data_i$sr_row4 %>% toupper())),
        fluidRow(column(12,data_i$sr_row5 %>% toupper())),
        fluidRow(
          column(4, data_i$sr_row6_c1 %>% toupper()),
          column(4, data_i$sr_row6_c2 %>% toupper()),
          column(4, data_i$sr_row6_c3 %>% toupper())
        ),
        br(),
        fluidRow(column(12, data_i$sr_main_text %>% toupper())),
        br(),
        fluidRow(column(12, data_i$sr_row7 %>% toupper())),
        fluidRow(column(12, data_i$sr_row8 %>% toupper())),
        fluidRow(column(12, data_i$sr_row9 %>% toupper())),
        fluidRow(column(12, data_i$sr_row10 %>% toupper()))
      )
    }
    
    output$sr_report_manual_v1 <- renderUI(manualgen_sit_report(data_i))
    output$sr_report_manual_v2 <- renderUI(manualgen_sit_report(data_i))
    output$sr_report_manual_v3 <- renderUI(manualgen_sit_report(data_i))
    output$sr_report_manual_v4 <- renderUI(manualgen_sit_report(data_i))
    
    # ** Variables Not Entered -------------------------------------------------
    
    # **** General Information -------------------------------------------------
    general_info_vars <- bind_rows(
      data.frame(var="police_division", label="Police Division", default=".", stringsAsFactors=F),
      data.frame(var="ob_no", label="O.B. Num", default="", stringsAsFactors=F),
      #data.frame(var="iar_no", label="IAR Num", default="", stringsAsFactors=F),
      data.frame(var="accident_type", label="What is the accident type?", default=".", stringsAsFactors=F),
      data.frame(var="hit_and_run", label="Was the crash a hit and run?", default=".", stringsAsFactors=F),
      data.frame(var="accident_date", label="Date of Accident", default=NA, stringsAsFactors=F),
      data.frame(var="accident_time_hour", label="Accident Time [Hour]", default=NA, stringsAsFactors=F),
      data.frame(var="accident_time_minute", label="Accident Time [Minute]", default=NA, stringsAsFactors=F),
      data.frame(var="accident_reported_date", label="Data Accident Reported", default="", stringsAsFactors=F),
      data.frame(var="accident_reported_time_hour", label="Time Accident Reported [Hour]", default=NA, stringsAsFactors=F),
      data.frame(var="accident_reported_time_minute", label="Time Accident Reported [Minute]", default=NA, stringsAsFactors=F),
      data.frame(var="road_name", label="Road Name", default="", stringsAsFactors=F),
      data.frame(var="landmark", label="Neary Landmark", default="", stringsAsFactors=F),
      data.frame(var="officer_rspnd_crash_title", label="Officer Name who Responded to Crash - Title", default="", stringsAsFactors=F),
      data.frame(var="officer_rspnd_crash_firstname", label="Officer Name who Responded to Crash - First Name", default="", stringsAsFactors=F),
      data.frame(var="officer_rspnd_crash_surname", label="Officer Name who Responded to Crash - Surname", default="", stringsAsFactors=F),
      data.frame(var="officer_filled_form_title", label="Officer Name who Filled out Form - Title", default="", stringsAsFactors=F),
      data.frame(var="officer_filled_form_firstname", label="Officer Name who Filled out Form - First Name", default="", stringsAsFactors=F),
      data.frame(var="officer_filled_form_surname", label="Officer Name who Filled out Form - Surname", default="", stringsAsFactors=F)
    )
    
    general_info_missing <- lapply(1:nrow(general_info_vars), function(i){
      
      general_info_vars_i <- general_info_vars[i,]
      
      if(length(data_i[[general_info_vars_i$var]]) %in% 0){
        out <- general_info_vars_i$label
      } else if(general_info_vars_i$default %in% data_i[[general_info_vars_i$var]]){
        out <- general_info_vars_i$label 
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
    
    # **** Participants --------------------------------------------------------
    N_participants <- ifelse(is.null(data_i$N_participants), 1, data_i$N_participants)
    N_participants <- ifelse(is.na(N_participants), 1, N_participants)
    participant_missing <- lapply(1:N_participants, function(i){
      
      #### Variables
      participant_vars <- bind_rows(
        data.frame(var = paste0("prtcpnt_vehpart_type_", i), label = "Vehicle/Participant Type", default = "", stringsAsFactors=F),
        data.frame(var = paste0("prtcpnt_name_", i), label = "Name", default = "", stringsAsFactors=F),
        data.frame(var = paste0("prtcpnt_telephone_", i), label = "Telephone Number", default = "", stringsAsFactors=F),
        data.frame(var = paste0("prtcpnt_nationality_", i), label = "Nationality", default = "", stringsAsFactors=F),
        data.frame(var = paste0("prtcpnt_injured_yn_", i), label = "Was the participant injured?", default = ".", stringsAsFactors=F)
      )
      
      if(!(TRUE %in% (c("Pedestrian", "Bicycle", "Unknown") %in% data_i[[paste0("prtcpnt_vehpart_type_", i)]]))){
        participant_vars <- bind_rows(
          participant_vars,
          data.frame(var = paste0("prtcpnt_regnumber_", i), label = "Registration Number", default = "", stringsAsFactors=F),
        )
      }
      
      if("Yes" %in% data_i[[paste0("prtcpnt_injured_yn_", i)]]){
        participant_vars <- bind_rows(
          participant_vars,
          data.frame(var = paste0("prtcpnt_injurytype_", i), label = "Injury Type", default = "", stringsAsFactors=F),
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
          data.frame(var = paste0("victim_age_years_", i), label = "Age", default = NA, stringsAsFactors=F),
          data.frame(var = paste0("victim_gender_", i), label = "Gender", default = ".", stringsAsFactors=F),
          data.frame(var = paste0("victim_personclass_", i), label = "Class of Person", default = NA, stringsAsFactors=F),
          data.frame(var = paste0("victim_injurytype_", i), label = "Injury Type", default = "", stringsAsFactors=F),
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
    
    # **** Crash Description ---------------------------------------------------
    crash_descrip_missing_txt <- '<h4>Crash Description</h4><p style="color:red;">Enter crash description</p>'
    
    if(is.null(data_i$crash_description)){
      crash_descrip_missing <- crash_descrip_missing_txt
    } else if (is.na(data_i$crash_description)){
      crash_descrip_missing <- crash_descrip_missing_txt
    } else if (data_i$crash_description %in% "IT HAPPENED THAT"){
      crash_descrip_missing <- crash_descrip_missing_txt
    } else if (data_i$crash_description %in% ""){
      crash_descrip_missing <- crash_descrip_missing_txt
    } else{
      crash_descrip_missing <- NULL
    }
    
    # **** Cause Code ----------------------------------------------------------
    if(is.null(data_i$cause_code_category)){
      cause_code_cat_missing <- "Cause Code Category"
    } else if (is.na(data_i$cause_code_category)){
      cause_code_cat_missing <- "Cause Code Category"
    } else if (data_i$cause_code_category %in% ""){
      cause_code_cat_missing <- "Cause Code Category"
    } else{
      cause_code_cat_missing <- NULL
    }
    
    if(is.null(data_i$cause_code_value)){
      cause_code_missing <- "Cause Code"
    } else if (is.na(data_i$cause_code_value)){
      cause_code_missing <- "Cause Code"
    } else if (data_i$cause_code_value %in% ""){
      cause_code_missing <- "Cause Code"
    } else{
      cause_code_missing <- NULL
    }
    
    cause_code_vars_missing <- list(cause_code_cat_missing,
                                    cause_code_missing) %>%
      unlist()
    
    if(length(cause_code_vars_missing) %in% 0){
      cause_code_vars_missing <- NULL
    } else{
      cause_code_header <- "<h4>Cause Code</h4>"
      cause_code_vars_missing <- paste0('<p style="color:red;">',
                                        cause_code_vars_missing %>% paste(collapse = "<br>"),
                                        '</p>')
      
      cause_code_vars_missing <- paste0(cause_code_header,
                                        cause_code_vars_missing)
    }
    
    # **** Render Text Output --------------------------------------------------
    #### Prepp Output
    sr_checks_txt <- paste(general_info_missing,
                           crash_loc_missing,
                           participant_missing,
                           crash_descrip_missing,
                           victim_missing,
                           cause_code_vars_missing)
    allow_submission <- F
    if(length(sr_checks_txt) %in% 0) allow_submission <- T
    
    if(allow_submission){
      sr_checks_txt <- "<h3>All variables filled in</h3>"
    } else{
      sr_checks_txt <- paste0("<h2>Missing Variables</h2>",
                              # "<h3>Enter data for missing variables then you will be able to submit</h3>",
                              "<hr>",
                              sr_checks_txt)
    }
    
    output$sr_report_checks <- renderText({
      sr_checks_txt
    })
    
    # ** Checking Inputs -------------------------------------------------------
    
    # **** OB Already Exists ---------------------------------------------------
    output$ob_already_exits <- renderText({
      
      out <- NULL
      
      dataset_noti <- dataset[!(dataset$uid %in% data_i$uid),]
      ob_exists <- data_i$ob_no %in% dataset_noti$ob_no 
      
      if(!is.null(data_i$ob_no)){
        if(ob_exists & nchar(data_i$ob_no[1]) > 1){
          #out <- "<strong style='color:red'>This O.B. Number Already Exists</strong>"
        }
      }
      
      out
    })
    
    # **** IAR Already Exists --------------------------------------------------
    output$iar_already_exits <- renderText({
      
      dataset_noti <- dataset[!(dataset$uid %in% data_i$uid),]
      iar_exists <- data_i$iar_no %in% dataset_noti$iar_no 
      
      out <- NULL
      
      if(!is.null(data_i$iar_no)){
        if(iar_exists & nchar(data_i$iar_no[1]) > 1){
          out <- "<strong style='color:red'>This IAR Number Already Exists</strong>"
        }
      }
      
      out
    })
    
    # ** Required Variables ----------------------------------------------------
    
    # **** Allow Feedback/Finalize -----
    allow_feedback_finalize <- !("" %in% input$officer_feedback_title) &
      !("" %in% input$officer_feedback_firstname) & 
      !("" %in% input$officer_feedback_surname)
    
    if(allow_feedback_finalize){
      shinyjs::enable("finalize")
      shinyjs::enable("submit_for_feedback")
    } else{
      shinyjs::disable("finalize")
      shinyjs::disable("submit_for_feedback")
    }
    
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
    
    #### Allow submit to HQ
    # disable_submit_hq <- T
    #shinyjs::disable("submit_to_hq_button")
    # shinyjs::disable(session$ns("submit_to_hq_button_ui"))
    # if(disable_submit_hq){
    #   shinyjs::disable(session$ns("submit_to_hq_button_ui"))
    # } else{
    #   #shinyjs::disable(session$ns("submit_to_hq_button_ui"))
    # }
    
    # ** Show submit to hq button or required variables ------------------------
    output$submit_button_or_revise <- renderUI({
      # Default to submit button
      # Go through checks; if one not met, shows warning instead of button
      
      out <- actionButton(session$ns("submit_to_hq_button"), "Submit to HQ")
      use_warning <- F
      warning_manually_edit_tf <- F
      warning_enter_data_tf <- F
      
      warning <- "<strong><span style='color:red'>THE FOLLOWING IS REQUIRED BEFORE SUBMITTING</span><strong>"
      warning_manually_edit <- "<br><br><strong><span style='color:red'><u>In the \"Manually Edit\" tab, the following is required</u></span><strong>"
      warning_enter_data <- "<br><br><strong><span style='color:red'><u>In the \"Enter Data\" tab, please fill in the following required variables</u></span><strong>"
      
      #### MANUALLY EDIT CHECKS
      if(TRUE %in% c(is.null(data_i$sr_main_text), (nchar(data_i$sr_main_text) <= 4))){
        use_warning <- T
        warning_manually_edit_tf <- T
        
        warning_manually_edit <- paste0(warning_manually_edit, "<br>",
                                        "<strong><span style='color:red'>-- PLEASE FILL OUT
      THE SITUATION REPORT BEFORE CONTINUING. Before submitting, go to the \"Manually Edit\" tab and fill
    in the report. First, click the \"Fill Situation Report with Entered Data\" button 
    to have the system draft the report. Second, manually edit the report if needed. 
    Once this is done, you will see the text of the report on this page.</span><strong>")
      }
      
      drafter_not_entered <- nchar(data_i$sr_row7) %in% c(0,1) | data_i$sr_row7 %in% "DRAFTER"
      if(TRUE %in% c(is.null(data_i$sr_row7), drafter_not_entered)){
        use_warning <- T
        warning_manually_edit_tf <- T
        
        warning_manually_edit <- paste0(warning_manually_edit, "<br>",
                                        "<strong><span style='color:red'>-- PLEASE FILL IN DRAFTER OF THE REPORT. 
                            Before submitting, go to the \"Manually Edit\" tab and fill in the name
                            of the drafter of the report.</span><strong>")
      }
      
      
      #### ENTER DATA CHECKS
      if(TRUE %in% c(is.null(data_i$cause_code_category), is.na(data_i$cause_code_category))){
        use_warning <- T
        warning_enter_data_tf <- T
        
        warning_enter_data <- paste0(warning_enter_data, "<br>",
                                     "<strong><span style='color:red'>-- Cause Code Category</span><strong>")
      }
      
      if(TRUE %in% c(is.null(data_i$cause_code_value), is.na(data_i$cause_code_value))){
        use_warning <- T
        warning_enter_data_tf <- T
        
        warning_enter_data <- paste0(warning_enter_data, "<br>",
                                     "<strong><span style='color:red'>-- Cause Code Value</span><strong>")
      }
      
      if(use_warning){
        
        if(warning_enter_data_tf)    warning <- paste0(warning, warning_enter_data)
        if(warning_manually_edit_tf) warning <- paste0(warning, warning_manually_edit)
        
        out <- div(
          fluidRow(
            column(12, align = "left",
                   HTML(warning)
            )
          )
        )
        
      } 
      
      
      out
      
    })
    
  })
  
  # DOWNLOAD HANDLER ********************* -------------------------------------
  output$sr_pdf = downloadHandler(
    filename = paste0('situation_report_OB',data_i$ob_no,'.pdf'),
    
    content = function(file) {
      out = knit2pdf(file.path('modules_gen_reports', 'generate_sr_pdf_parent.Rnw'), clean = TRUE)
      #file.rename(out, file) # move pdf to file for downloading
      file.copy(out, file) 
    },
    
    contentType = 'application/pdf'
  )
  
  # DELETE REPORT **************************** ---------------------------------
  
  # ** Delete Report Button ----------------------------------------------------
  # **** Box ####
  delete_report_modal <- function(session) {
    
    modalDialog(
      "Delete Report",
      footer = tagList(
        actionButton(session$ns("delete_report_no"), "No"),
        actionButton(session$ns("delete_report_yes"), div(tags$b("Yes", style = "color: red;")))
      )
    )
  }
  
  # **** Open Box ####
  # open modal on button click
  observeEvent(input$delete_report_button,
               ignoreNULL = T,   # Show modal on start up
               showModal(delete_report_modal(session))
  )
  
  # **** Yes ####
  observeEvent(input$delete_report_yes, { 
    
    #### Grab uid of report to delete
    uid_delete <- data_i$uid[1]
    
    #### Delete Report
    dataset <- dataset[dataset$uid != uid_delete,]
    
    #### Save (1) list of uids to delete and (2) dataset
    dataset <- as.data.frame(dataset)
    if(TESTING_SYSTEM){
      
      # Updating uid_delete_list doesn't matter for testing, as list only
      # used when syncing with server and can't do that when testing.
      
      ## Data
      saveRDS(dataset, file.path("data", "police_data_test.Rds"), version=2)
    } else{
      ## Reports to delete
      uid_delete_list <- readRDS(file.path("data", "sr_to_delete.Rds"))
      uid_delete_list <- c(uid_delete, uid_delete_list)
      saveRDS(uid_delete_list, file.path("data", "sr_to_delete.Rds"), version=2)
      
      ## Data
      saveRDS(aes_cbc_encrypt(serialize(dataset, NULL), key = data_key), file.path("data", "police_data.Rds"), version=2)
    }
    
    #### All data 
    all_data_r <- reactive({
      dataset
    })
    all_data_r <<- all_data_r
    
    #### Back to report list 
    go_to_report_list_r$value <<- 1
    update_figures_r$value <<- 10
    update_map_r$value <<- 10
    update_p69_r$value <<- 10
    update_summary_table_r$value <<- 10
    sync_server_trigger_r$value <<- 4
    
    
    removeModal() 
  })
  
  # **** No ####
  observeEvent(input$delete_report_no, { 
    removeModal() 
  })
  
  # **** UI ####
  output$delete_report_button_ui <- renderUI({
    if(data_i$stage %in% "editing"){
      out <- actionButton(session$ns("delete_report_button"), "Delete Report")
    } else{
      out <- NULL
    }
    
    out
  })
  
  # TABPANEL ******************** ----------------------------------------------
  # Sit Report Data Entry UI ---------------------------------------------------
  ui_data_entry_qs <- navlistPanel(
    widths=c(2,10),
    "Crash Details",
    
    # ** Basic Crash Information 
    tabPanel("General Information",
             uiOutput(session$ns("crash_info_sr"))
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
             uiOutput(session$ns("participant_main_qs_sr"))
             
    ),
    # ** Crash Description 
    tabPanel("Crash Description",
             uiOutput(session$ns("crash_description"))
    ),
    # ** Victims 
    tabPanel("Victims",
             
             fluidRow(column(4,
                             ""),
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
            column(8,
                   ui_data_entry_qs
            ),
            column(4,
                   
                   fluidRow(
                     column(12, align = "center",
                            h4("Auto-Generated Situation Report")
                     )
                   ),
                   fluidRow(column(11, offset = 1, 
                                   uiOutput(session$ns("sr_report_auto"))))
                   
            )
          )
          
          # ** Data Checks -----------------------------------------------------
        } else if(tab_type %in% "Data Checks"){
          
          fluidRow(
            column(6,
                   htmlOutput(session$ns("sr_report_checks"))
            ),
            column(6,
                   fluidRow(
                     column(12,
                            h4("Auto-Generated Situation Report", align = "center")
                     ),
                     column(12,
                            uiOutput(session$ns("sr_report_auto_2"))
                     )
                   )
            )
          )
          
          # ** Manually Edit ---------------------------------------------------
        } else if(tab_type %in% "Manually Edit"){
          
          fluidRow(
            column(6,
                   uiOutput(session$ns("edit_sit_report_ui"))
            ),
            column(6,
                   fluidRow(
                     column(12,
                            h4("Situation Report", align = "center")
                     ),
                     column(12,
                            uiOutput(session$ns("sr_report_manual_v2"))
                     )
                   )
            )
          )
          
          # ** Submit to HQ ----------------------------------------------------
        } else if(tab_type %in% "Submit to HQ"){
          fluidRow(
            column(6,
                   fluidRow(
                     column(12, align = "center",
                            h3("Provide Comments to HQ")
                     )
                   ),
                   fluidRow(
                     column(12, align = "center",
                            textAreaInput(session$ns("sr_station_comments"), "If needed, provide comments for HQ"),
                     )
                   ),
                   fluidRow(
                     column(12, align = "center", strong("Title and Name")),
                   ),
                   fluidRow(
                     column(3, textInput(session$ns("station_feedback_title"), "Title", "")),
                     column(3, textInput(session$ns("station_feedback_firstname"), "First Name", "")),
                     column(3, textInput(session$ns("station_feedback_surname"), "Surname", "")),
                     column(3, textInput(session$ns("station_feedback_snumber"), "Service No.", ""))
                   ),
                   # fluidRow(
                   #   column(12, align = "center",
                   #          h6("Fill in title, name and surname to enable buttons")
                   #   )
                   # ),
                   br(),
                   br(),
                   #br(),
                   #br(),
                   fluidRow(
                     column(12, align = "center",
                            uiOutput(session$ns("submit_button_or_revise"))
                            #actionButton(session$ns("submit_to_hq_button"), "Submit to HQ")
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
                   #br(),
                   #br(),
                   #br(),
                   fluidRow(
                     column(12, align = "center",
                            downloadButton(session$ns('sr_pdf'), label="Download Situation Report"),
                     )
                   )
                   
            ),
            column(6,
                   fluidRow(
                     column(12, align = "center",
                            h3("Situation Report")
                     )
                   ),
                   fluidRow(
                     column(12, align = "left",
                            uiOutput(session$ns("sr_report_manual_v3")))
                   )
            )
            
          )
          
          # ** Review Sit Report - hq ------------------------------------------
        } else if(tab_type %in% "Review Situation Report" & USER_ROLE %in% "hq"){
          fluidRow(
            column(6,
                   
                   fluidRow(
                     column(8,  offset = 2, align = "left",
                            htmlOutput(session$ns("sr_station_comments_txt")),
                     )
                   ),
                   br(),
                   fluidRow(
                     column(8,  offset = 2, align = "left",
                            htmlOutput(session$ns("station_feedback_title_name_txt")),
                     )
                   ),
                   
                   fluidRow(
                     column(12, align = "center",
                            h3("Provide Feedback")
                     )
                   ),
                   fluidRow(
                     column(12, align = "center",
                            textAreaInput(session$ns("sr_hq_comments"), "If needed, provide comments for changes to the situation report."),
                     )
                   ),
                   fluidRow(
                     column(12, align = "center", strong("Title and Name")),
                   ),
                   fluidRow(
                     column(3, textInput(session$ns("officer_feedback_title"), "Title", "")),
                     column(3, textInput(session$ns("officer_feedback_firstname"), "First Name", "")),
                     column(3, textInput(session$ns("officer_feedback_surname"), "Surname", "")),
                     column(3, textInput(session$ns("officer_feedback_snumber"), "Service No.", ""))
                   ),
                   
                   fluidRow(
                     column(12, align = "center",
                            actionButton(session$ns("submit_for_feedback"), "Send Comments to Officer for Edits"),
                            actionButton(session$ns("finalize"), div(tags$b("Approve", style = "color: red;")))
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
                   br(),
                   fluidRow(
                     column(12, align = "center",
                            downloadButton(session$ns('sr_pdf'), label="Download Situation Report")
                     )
                   )
            ),
            column(6,
                   uiOutput(session$ns("sr_report_manual_v3")))
          )
          # ** Review Sit Report - officer -------------------------------------
        } else if(tab_type %in% "Review Situation Report" & USER_ROLE %in% "officer"){
          fluidRow(
            column(6, align = "center",
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
                            downloadButton(session$ns('sr_pdf'), label="Download Situation Report")
                     )
                   )
            ),
            column(6,
                   uiOutput(session$ns("sr_report_manual_v3")))
          )
          # ** Review Feedback from HQ -----------------------------------------------
        } else if (tab_type %in% "Review Feedback from HQ"){
          fluidRow(
            fluidRow(
              column(6, align = "center",
                     fluidRow(
                       h4("Feedback from HQ")
                     ),
                     br(),
                     fluidRow(
                       column(8,  offset = 2, align = "left",
                              textOutput(session$ns("sr_hq_comments_txt")),
                       )
                     ),
                     br(),
                     fluidRow(
                       column(8,  offset = 2, align = "left",
                              htmlOutput(session$ns("officer_feedback_title_name_txt")),
                       )
                     )
              ),
              column(6,
                     fluidRow(
                       column(12, align = "center",
                              h4("Situation Report")
                       )
                     ),
                     fluidRow(
                       column(12,
                              uiOutput(session$ns("sr_report_manual_v4"))
                       )
                     )
                     
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
  
  # UIs FOR TABSETS ********************** -------------------------------------
  
  # [Tabset UIs]: Editing **************************** ---------------------------
  source(file.path("modules_main", "edit_sit_report_server.R"), local=T)
  
  # [Tabset UIs] Manually Edit Sit Report ************** -------------------
  
  # ** Edit Sit Report ---------------------------------------------------------
  
  output$edit_sit_report_ui <- renderUI({
    
    div(
      fluidRow(
        column(12, align = "center",
               HTML("<strong>Click the button to have the system draft the situation report, then manually edit as needed. <span style='color:red'>This step is required before continuing.</span></strong>")
        )
      ),
      
      fluidRow(
        column(12, align = "center",
               actionButton(session$ns("fill_sr_with_data"),"Fill Situation Report with Entered Data")
        )
      ),
      br(),
      
      fluidRow(
        column(4,
               textInput(session$ns("sr_row1_c1"), NULL, data_i$sr_row1_c1)
        ),
        column(4,
               textInput(session$ns("sr_row1_c2"), NULL, data_i$sr_row1_c2)
        ),
        column(4,
               textInput(session$ns("sr_row1_c3"), NULL, data_i$sr_row1_c3)
        )
      ),
      fluidRow(column(12, textInput(session$ns("sr_row2"), NULL, data_i$sr_row2, width = "120%"))),
      fluidRow(column(12, textAreaInput(session$ns("sr_row3"), NULL, data_i$sr_row3, width="130%", height = 100))),
      fluidRow(column(12, textInput(session$ns("sr_row4"), NULL, data_i$sr_row4, width = "120%"))),
      fluidRow(column(12, textInput(session$ns("sr_row5"), NULL, data_i$sr_row5, width = "120%"))),
      fluidRow(
        column(4,
               textInput(session$ns("sr_row6_c1"), NULL, data_i$sr_row6_c1)
        ),
        column(4,
               textInput(session$ns("sr_row6_c2"), NULL, data_i$sr_row6_c2)
        ),
        column(4,
               textInput(session$ns("sr_row6_c3"), NULL, data_i$sr_row6_c3)
        )
      ),
      fluidRow(column(12, textAreaInput(session$ns("sr_main_text"), NULL,  data_i$sr_main_text,
                                        width="130%", height=200))),
      fluidRow(column(12, textInput(session$ns("sr_row7"), NULL, data_i$sr_row7, width = "120%"))),
      fluidRow(column(12, textInput(session$ns("sr_row8"), NULL, data_i$sr_row8, width = "120%"))),
      fluidRow(column(12, textInput(session$ns("sr_row9"), NULL, data_i$sr_row9, width = "120%"))),
      fluidRow(column(12, textInput(session$ns("sr_row10"), NULL, data_i$sr_row10, width = "120%")))
      
    )
    
  })
  
  # ** Fill SR -----------------------------------------------------------------
  observeEvent(input$fill_sr_with_data, {
    
    for(var in dataset_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character()
    }
    
    for(var in sr_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character() %>% toupper()
    }
    
    data_i$sr_row1_c1 <- sr_row1_c1_fun(data_i) %>% toupper()
    data_i$sr_row1_c2 <- sr_row1_c2_fun(data_i) %>% toupper()
    data_i$sr_row1_c3 <- sr_row1_c3_fun(data_i) %>% toupper()
    
    data_i$sr_row2 <- sr_row2_fun(data_i) %>% toupper()
    data_i$sr_row3 <- sr_row3_fun(data_i) %>% toupper()
    data_i$sr_row4 <- sr_row4_fun(data_i) %>% toupper()
    data_i$sr_row5 <- sr_row5_fun(data_i) %>% toupper()
    
    data_i$sr_row6_c1 <- sr_row6_c1_fun(data_i) %>% toupper()
    data_i$sr_row6_c2 <- sr_row6_c2_fun(data_i) %>% toupper()
    data_i$sr_row6_c3 <- sr_row6_c3_fun(data_i) %>% toupper()
    
    data_i$sr_row7 <- sr_row7_fun(data_i) %>% toupper()
    data_i$sr_row8 <- sr_row8_fun(data_i) %>% toupper()
    data_i$sr_row9 <- sr_row9_fun(data_i) %>% toupper()
    data_i$sr_row10 <- sr_row10_fun(data_i) %>% toupper()
    
    participant_text <- participant_text_fun(data_i)
    victim_text <- victim_text_fun(data_i)
    
    data_i$sr_main_text <- main_text_fun(data_i,
                                         participant_text,
                                         victim_text) %>% toupper()
    
    #### Update what user sees
    for(sr_var in sr_variables){
      updateTextInput(session, sr_var, value = data_i[[sr_var]])
    }
    
  })
  
  # [Tabset UIs]: Submit SR to HQ **************************** -----------------
  
  # ** Submit to HQ Button -----------------------------------------------------
  submit_to_hq_modal <- function(session) {
    
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
    data_i$stage <- "submitted_to_hq"
    data_i$stage_sr_num <- as.numeric(data_i$stage_sr_num) + 1
    
    # ** Save - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    for(var in dataset_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character()
    }
    
    for(var in sr_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character() %>% toupper()
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
    
    #### Sync Data
    #tmp <- input$sync_with_server # so "input$sync_with_server" is observerd
    
    # ** Back to report list - - - - - - - - - - - - - - - - - - - - - - - - - - 
    go_to_report_list_r$value <<- 1
    update_figures_r$value <<- 10
    update_map_r$value <<- 10
    update_p69_r$value <<- 10
    update_summary_table_r$value <<- 10
    sync_server_trigger_r$value <<- 10
    
    removeModal() 
  })
  
  # No - Submit
  observeEvent(input$submit_to_hq_no, { 
    removeModal() 
  })
  
  
  # [Tabset UIs]: Review Sit Report **************************** -------------------
  output$sr_station_comments_txt <- renderText({
    
    out <- ""
    
    if(!is.null(data_i$sr_station_comments)){
      if(!is.na(data_i$sr_station_comments)){
        if(nchar(data_i$sr_station_comments) > 1){
          
          out <- paste("<h3>Message from Officer:</h3>", 
                       data_i$sr_station_comments)
          
        }
      }
    }
    
    out
    
  })
  
  output$station_feedback_title_name_txt <- renderText({
    
    out <- ""
    
    if(!is.null(data_i$sr_station_comments)){
      if(!is.na(data_i$sr_station_comments)){
        if(nchar(data_i$sr_station_comments) > 1){
          
          out <- paste("<strong>Message from:</strong>", 
                       data_i$station_feedback_title, 
                       data_i$station_feedback_firstname, 
                       data_i$station_feedback_surname,
                       "<br>", 
                       "<strong>Service Number:</strong>", data_i$station_feedback_snumber,
                       "<hr>")
          
        }
      }
    }
    
    out
    
  })
  
  
  # ** Submit for Feedback -----------------------------------------------------
  observeEvent(input$submit_for_feedback,{
    
    # ** Change Status - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    data_i$stage <- "hq_provided_feedback"
    data_i$stage_sr_num <- as.numeric(data_i$stage_sr_num) + 1
    
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
    update_figures_r$value <<- 10
    update_map_r$value <<- 10
    update_p69_r$value <<- 10
    update_summary_table_r$value <<- 10
    sync_server_trigger_r$value <<- 11
    
    removeModal() 
    
    shinyalert(title = ifelse(curl::has_internet(), 
                              "Feedback Sent to Station", 
                              "Feedback Recorded"),
               text = ifelse(curl::has_internet(),
                             "The situation report has been sent back to the station for edits.",
                             "For the station to receive the feedback, connect to internet and click the \"Sync Data with Server\" button."),
               type = "success")
    
  })
  
  # ** Finalize ----------------------------------------------------------------
  observeEvent(input$finalize,{
    
    # ** Change Status - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    data_i$stage <- "finalized"
    data_i$stage_sr_num <- as.numeric(data_i$stage_sr_num) + 1
    
    # ** Save - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    for(var in dataset_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character()
    }
    
    for(var in sr_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character() %>% toupper()
    }
    
    data_i$last_updated <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character
    
    for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
    for(var in names(dataset)) dataset[[var]] <- dataset[[var]] %>% as.character()
    
    dataset <- bind_rows(dataset, data_i) %>%
      arrange(desc(last_updated)) %>%
      distinct(uid, report_type, amend_report_id, .keep_all = TRUE)
    
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
    update_figures_r$value <<- 10
    update_map_r$value <<- 10
    update_p69_r$value <<- 10
    update_summary_table_r$value <<- 10
    sync_server_trigger_r$value <<- 30
    
    removeModal() 
    
    shinyalert(title = "Report Finalized",
               text = ifelse(curl::has_internet(),
                             "The situation report has been finalized.",
                             "The situation report has been finalized. For all stations to see the report as finalized, connect to internet and click the \"Sync Data with Server\" button."),
               type = "success")
    
  })
  
  # [Tabset UIs]: Review Comments from HQ *********************** ------------------
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


# Situation Report Module

# UI ##################################################################### -----
amendreport_UI <- function(id) {
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
             uiOutput(ns("ar_tabs_ui")),    
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
amendreport_Server <- function(input, output, session, data_i, dataset, dataset_variables,
                             sr_variables, tab_types) {
  
  source(file.path("modules_main", "functions.R"))
  
  # AUTOSAVE **************************************-----------------------------
  observe({
    
    # ** Save ------------------------------------------------------------------
    #### Add variables to data_i
    for(var in dataset_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character()
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
    
    #### Update Data in SR/Main Data
    data_same_uid <- dataset[dataset$uid %in% data_i$uid,]
    
    vars_to_update <- names(data_i)
    vars_to_update <- vars_to_update[!(vars_to_update %in% c("report_type", "amend_report_id", "followup_report_id"))]
    vars_to_update <- vars_to_update[!grepl("^stage|^sr_", vars_to_update)]
    for(var in vars_to_update) data_same_uid[[var]] <- data_i[[var]] %>% as.character()
    
    dataset <- bind_rows(data_same_uid, dataset) %>%
      distinct(uid, report_type, amend_report_id, followup_report_id, .keep_all = TRUE)
    
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
    
    manualgen_amend_report <- function(data_i){
      fluidRow(
        fluidRow(
          column(4, data_i$ar_row1_c1),
          column(4, data_i$ar_row1_c2),
          column(4, data_i$ar_row1_c3)
        ),
        fluidRow(column(12,data_i$ar_row2)),
        fluidRow(column(12,data_i$ar_row3)),
        fluidRow(column(12,data_i$ar_row4)),
        fluidRow(column(12,data_i$ar_row5)),
        fluidRow(
          column(4, data_i$ar_row6_c1),
          column(4, data_i$ar_row6_c2),
          column(4, data_i$ar_row6_c3)
        ),
        br(),
        fluidRow(column(12, data_i$ar_main_text)),
        br(),
        fluidRow(column(12, data_i$ar_row7)),
        fluidRow(column(12, data_i$ar_row8)),
        fluidRow(column(12, data_i$ar_row9)),
        fluidRow(column(12, data_i$ar_row10))
      )
    }
    
    output$ar_report_manual_v1 <- renderUI(manualgen_amend_report(data_i))
    output$ar_report_manual_v2 <- renderUI(manualgen_amend_report(data_i))
    output$ar_report_manual_v3 <- renderUI(manualgen_amend_report(data_i))
    output$ar_report_manual_v4 <- renderUI(manualgen_amend_report(data_i))
    
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
    
  })
  
  # DOWNLOAD HANDLER ********************* -------------------------------------
  output$ar_pdf = downloadHandler(
    filename = paste0('amendment_report_OB',data_i$ob_no,'.pdf'),
    
    content = function(file) {
      out = knit2pdf(file.path('modules_gen_reports', 'generate_ar_pdf_parent.Rnw'), clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
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
    sync_server_trigger_r$value <<- 24
    
    
    removeModal() 
  })
  
  # **** No ####
  observeEvent(input$delete_report_no, { 
    removeModal() 
  })
  
  # **** UI ####
  output$delete_report_button_ui <- renderUI({
    if(data_i$stage_ar %in% "editing"){
      out <- actionButton(session$ns("delete_report_button"), "Delete Form")
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
  
  gen_ar_tabs <- function(tab_type){
    renderUI({
      
      div(
        # ** Enter Data --------------------------------------------------------
        if(tab_type %in% "Enter Data"){
          
          fluidRow(
            column(12,
                   ui_data_entry_qs
            )
          )
          
        } else if(tab_type %in% "Write Amendment Report"){
          
          fluidRow(
            column(6,
                   uiOutput(session$ns("edit_amend_report_ui"))
            ),
            column(6,
                   fluidRow(
                     column(12,
                            h4("Amendment Report", align = "center")
                     ),
                     column(12,
                            uiOutput(session$ns("ar_report_manual_v1"))
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
                            textAreaInput(session$ns("ar_station_comments"), "If needed, provide comments for HQ"),
                     )
                   ),
                   fluidRow(
                     column(12, align = "center", strong("Title and Name")),
                   ),
                   fluidRow(
                     column(3, textInput(session$ns("station_ar_feedback_title"), "Title", "")),
                     column(3, textInput(session$ns("station_ar_feedback_firstname"), "First Name", "")),
                     column(3, textInput(session$ns("station_ar_feedback_surname"), "Surname", "")),
                     column(3, textInput(session$ns("station_ar_feedback_snumber"), "Service No.", ""))
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
                            downloadButton(session$ns('ar_pdf'), label="Download Amendment Report"),
                     )
                   )
                   
            ),
            column(6,
                   uiOutput(session$ns("ar_report_manual_v2")))
          )
          
          # ** Review Sit Report - hq ------------------------------------------
        } else if(tab_type %in% "Review Amendment Report" & USER_ROLE %in% "hq"){
          fluidRow(
            column(6,
                   
                   fluidRow(
                     column(12,  offset = 2, align = "left",
                            htmlOutput(session$ns("ar_station_comments_txt")),
                     )
                   ),
                   br(),
                   fluidRow(
                     column(12,  offset = 2, align = "left",
                            htmlOutput(session$ns("station_ar_feedback_title_name_txt")),
                     )
                   ),
                   
                   
                   fluidRow(
                     column(12, align = "center",
                            h3("Provide Feedback")
                     )
                   ),
                   fluidRow(
                     column(12, align = "center",
                            textAreaInput(session$ns("sr_hq_comments"), "If needed, provide comments for changes to the amendment report."),
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
                   br(),
                   fluidRow(
                     column(12, align = "center",
                            downloadButton(session$ns('ar_pdf'), label="Download Amendment Report")
                     )
                   )
            ),
            column(6,
                   uiOutput(session$ns("ar_report_manual_v3")))
          )
          # ** Review Sit Report - officer -------------------------------------
        } else if(tab_type %in% "Review Amendment Report" & USER_ROLE %in% "officer"){
          fluidRow(
            column(6, align = "center",
                   h3("Submitted to HQ"),
                   fluidRow(
                     column(12, align = "center",
                            strong("The amendment report has been submitted to HQ 
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
            column(6, align = "center",
                   h4("Feedback from HQ"),
                   br(),
                   column(8,  offset = 2, align = "left",
                          textOutput(session$ns("sr_hq_comments_txt")),
                   )
            ),
            column(6,
                   uiOutput(session$ns("sr_report_manual_v4"))
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
  
  output$ar_tabs_ui <- renderUI({
    do.call(tabsetPanel,
            lapply(tab_types,
                   function(i) tabPanel(i, gen_ar_tabs(i) )))
  })
  
  # UIs FOR TABSETS ********************** -------------------------------------
  
  # [Tabset UIs]: Editing **************************** ---------------------------
  source(file.path("modules_main", "edit_sit_report_server.R"), local=T)
  
  # [Tabset UIs] Manually Edit Sit Report ************** -------------------
  
  # ** Edit Sit Report ---------------------------------------------------------
  
  output$edit_amend_report_ui <- renderUI({
    
    div(
      fluidRow(
        column(4,
               textInput(session$ns("ar_row1_c1"), NULL, data_i$ar_row1_c1)
        ),
        column(4,
               textInput(session$ns("ar_row1_c2"), NULL, data_i$ar_row1_c2)
        ),
        column(4,
               textInput(session$ns("ar_row1_c3"), NULL, data_i$ar_row1_c3)
        )
      ),
      fluidRow(column(12, textInput(session$ns("ar_row2"), NULL, data_i$ar_row2, width = "120%"))),
      fluidRow(column(12, textAreaInput(session$ns("ar_row3"), NULL, data_i$ar_row3, width="130%", height = 100))),
      fluidRow(column(12, textInput(session$ns("ar_row4"), NULL, data_i$ar_row4, width = "120%"))),
      fluidRow(column(12, textInput(session$ns("ar_row5"), NULL, data_i$ar_row5, width = "120%"))),
      fluidRow(
        column(4,
               textInput(session$ns("ar_row6_c1"), NULL, data_i$ar_row6_c1)
        ),
        column(4,
               textInput(session$ns("ar_row6_c2"), NULL, data_i$ar_row6_c2)
        ),
        column(4,
               textInput(session$ns("ar_row6_c3"), NULL, data_i$ar_row6_c3)
        )
      ),
      fluidRow(column(12, textAreaInput(session$ns("ar_main_text"), NULL,  data_i$ar_main_text,
                                        width="130%", height=200))),
      fluidRow(column(12, textInput(session$ns("ar_row7"), NULL, data_i$ar_row7, width = "120%"))),
      fluidRow(column(12, textInput(session$ns("ar_row8"), NULL, data_i$ar_row8, width = "120%"))),
      fluidRow(column(12, textInput(session$ns("ar_row9"), NULL, data_i$ar_row9, width = "120%"))),
      fluidRow(column(12, textInput(session$ns("ar_row10"), NULL, data_i$ar_row10, width = "120%")))
      
    )
    
  })
  
  # ** Fill SR -----------------------------------------------------------------
  observeEvent(input$fill_sr_with_data, {
    
    for(var in dataset_variables){
      if(length(input[[var]]) > 0) data_i[[var]] <- input[[var]] %>% as.character()
    }
    
    data_i$sr_row1_c1 <- sr_row1_c1_fun(data_i)
    data_i$sr_row1_c2 <- sr_row1_c2_fun(data_i)
    data_i$sr_row1_c3 <- sr_row1_c3_fun(data_i)
    
    data_i$sr_row2 <- sr_row2_fun(data_i)
    data_i$sr_row3 <- sr_row3_fun(data_i)
    data_i$sr_row4 <- sr_row4_fun(data_i)
    data_i$sr_row5 <- sr_row5_fun(data_i)
    
    data_i$sr_row6_c1 <- sr_row6_c1_fun(data_i)
    data_i$sr_row6_c2 <- sr_row6_c2_fun(data_i)
    data_i$sr_row6_c3 <- sr_row6_c3_fun(data_i)
    
    data_i$sr_row7 <- sr_row7_fun(data_i)
    data_i$sr_row8 <- sr_row8_fun(data_i)
    data_i$sr_row9 <- sr_row9_fun(data_i)
    data_i$sr_row10 <- sr_row10_fun(data_i)
    
    participant_text <- participant_text_fun(data_i)
    victim_text <- victim_text_fun(data_i)
    
    data_i$sr_main_text <- main_text_fun(data_i,
                                         participant_text,
                                         victim_text)
    
    #### Update what user sees
    for(sr_var in sr_variables){
      updateTextInput(session, sr_var, value = data_i[[sr_var]])
    }
    
  })
  
  # [Tabset UIs]: Submit SR to HQ **************************** ---------------------
  
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
    data_i$stage_ar <- "submitted_to_hq"
    data_i$stage_ar_num <- as.numeric(data_i$stage_ar_num) + 1

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
    update_figures_r$value <<- 10
    update_map_r$value <<- 10
    update_p69_r$value <<- 10
    update_summary_table_r$value <<- 10
    sync_server_trigger_r$value <<- 20
    
    removeModal() 
  })
  
  # No - Submit
  observeEvent(input$submit_to_hq_no, { 
    removeModal() 
  })
  
  
  # [Tabset UIs]: Review Sit Report **************************** -------------------
  output$ar_station_comments_txt <- renderText({
    
    out <- ""
    
    if(!is.null(data_i$ar_station_comments)){
      if(!is.na(data_i$ar_station_comments)){
        if(nchar(data_i$ar_station_comments) > 1){
          
          out <- paste("<h3>Message from Officer:</h3>", 
                       data_i$ar_station_comments)
          
        }
      }
    }
    
    out
    
  })
  
  output$station_ar_feedback_title_name_txt <- renderText({
    
    out <- ""
    
    if(!is.null(data_i$ar_station_comments)){
      if(!is.na(data_i$ar_station_comments)){
        if(nchar(data_i$ar_station_comments) > 1){
          
          out <- paste("<strong>Message from:</strong>", 
                       data_i$station_ar_feedback_title, 
                       data_i$station_ar_feedback_firstname, 
                       data_i$station_ar_feedback_surname,
                       "<br>", 
                       "<strong>Service Number:</strong>", data_i$station_ar_feedback_snumber,
                       "<hr>")
          
        }
      }
    }
    
    out
    
  })
  
  # ** Submit for Feedback -----------------------------------------------------
  observeEvent(input$submit_for_feedback,{
    
    # ** Change Status - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    data_i$stage_ar <- "hq_provided_feedback"
    data_i$stage_ar_num <- as.numeric(data_i$stage_ar_num) + 1
    
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
    update_figures_r$value <<- 10
    update_map_r$value <<- 10
    update_p69_r$value <<- 10
    update_summary_table_r$value <<- 10
    sync_server_trigger_r$value <<- 41
    
    
    removeModal() 
    
    shinyalert(title = ifelse(curl::has_internet(), 
                              "Feedback Sent to Station", 
                              "Feedback Recorded"),
               text = ifelse(curl::has_internet(),
                             "The amendment report has been sent back to the station for edits.",
                             "For the station to receive the feedback, connect to internet and click the \"Sync Data with Server\" button."),
               type = "success")
    
  })
  
  # ** Finalize ----------------------------------------------------------------
  observeEvent(input$finalize,{
    
    # ** Change Status - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    data_i$stage_ar <- "finalized"
    data_i$stage_ar_num <- as.numeric(data_i$stage_ar_num) + 1
    
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
    update_figures_r$value <<- 10
    update_map_r$value <<- 10
    update_p69_r$value <<- 10
    update_summary_table_r$value <<- 10
    sync_server_trigger_r$value <<- 73

    removeModal() 
    
    shinyalert(title = "Report Finalized",
               text = ifelse(curl::has_internet(),
                             "The amendment report has been finalized.",
                             "The amendment report has been finalized. For all stations to see the report as finalized, connect to internet and click the \"Sync Data with Server\" button."),
               type = "success")
    
  })
  
  # [Tabset UIs]: Review Comments from HQ *********************** ------------------
  output$sr_hq_comments_txt <- renderText({
    data_i$sr_hq_comments
  })
  
}


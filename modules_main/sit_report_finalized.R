# Situation Report Module


# UI ****************************************** ================================
sitreport_finalized_UI <- function(id) {
  ns <- NS(id)
  
  
  # UI Data Entry --------------------------------------------------------------
  fluidPage(
    useShinyalert(),
    
    #### Hide errors from displaying
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    # ** Top Panel -------------------------------------------------------------
    fluidRow(
      fluidRow(
        column(6, align = "left", actionButton("go_to_report_list","Back to Report List")),
        column(6, align = "right", uiOutput(ns("delete_report_button_ui")))
      ),
      
      
      fluidRow(
        column(12, align = "center",
               titlePanel("Crash Reports")
        )
      ),
      
      fluidRow(
        column(12, align = "center",
               h4(textOutput(ns("ob_no_text")))
        )
      ),
      
      fluidRow(
        column(4, offset = 4, align = "center",
               uiOutput(ns("iar_number_text"))
        ),
        column(3, align = "left",
               br(),
               uiOutput(ns("iar_number_button")))
      ),
      fluidRow(
        column(6, offset = 3, align = "center",
               uiOutput(ns("iar_number_warning"))
        )
      ),
      
      hr(),
      
      fluidRow(
        column(6, offset = 3,
               fluidRow(column(12, align = "center", h4("Situation, Follow Up & Amendment Reports"))),
               fluidRow(column(12, align = "center", uiOutput(ns("sr_options_ui")))),
               fluidRow(column(12, align = "center", uiOutput(ns("email_sr_button_ui")) )),
               
               br(), 
               
               uiOutput(ns("sr_report_text")),
               hr(),
               fluidRow(column(12, align = "center", uiOutput(ns("fu_options_ui")))),
               hr(),
               fluidRow(column(12, align = "center", uiOutput(ns("ar_options_ui")))),
               
        ),
        column(3, align = "center",
               
               br(),
               br(),
               br(),
               br(),
               
               fluidRow(
                 column(12, align = "center",
                        ""
                        #actionButton('gen_p41_from_sit_report', label="Generate P41")
                 )
               ),
               
               br(),
               br(),
               
               fluidRow(
                 column(12, align = "center",
                        ""
                        #actionButton('gen_amend_report', "Generate Amendment Report")
                 )
               )
               
        )
        
      ),
      
      br(),
      
      fluidRow(
        column(12, 
               uiOutput(ns("ar_text_1"))
               # uiOutput(ns("ar_text_2")),
               # uiOutput(ns("ar_text_3")),
               # uiOutput(ns("ar_text_4")),
               # uiOutput(ns("ar_text_5"))
        )
      ),
      
      fluidRow(column(6, offset = 3, align = "center", hr())),
      
      fluidRow(column(12, align = "center", h4("P41"))),
      fluidRow(column(12, align = "center", uiOutput(ns("p41_options_ui")))),
      fluidRow(column(12, align = "center", uiOutput(ns("download_crash_sketch_finalized_page_ui")))),
      fluidRow(column(12, align = "center", uiOutput(ns("email_p41_button_ui")) )),
      
      #fluidRow(column(6, offset = 3, align = "center", hr())),
      
      br(),
      fluidRow(
        column(12, align = "center",
               ""
        )
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
      
    )
  )
  
  
}

# SERVER ****************************************** ============================
sitreport_finalized_Server <- function(input, output, session, data_i, dataset, dataset_variables,
                                       sr_variables, ar_data, fu_data) {
  
  # Source ---------------------------------------------------------------------
  source(file.path("modules_main", "functions.R"))
  source(file.path("modules_main", "sit_report.R"))
  
  # Needed to pass data to main server then to generate amend report
  data_i <<- data_i
  
  sr_data_i <<- dataset %>%
    filter(uid %in% data_i$uid) %>%
    filter(report_type %in% "sit_report") 
  
  p41_data_i <<- dataset %>%
    filter(uid %in% data_i$uid) %>%
    filter(report_type %in% "p41") 
  
  ar_data_i <<- dataset %>%
    filter(uid %in% data_i$uid) %>%
    filter(report_type %in% "amendment") 
  
  fu_data_i <<- dataset %>%
    filter(uid %in% data_i$uid) %>%
    filter(report_type %in% "followup") 
  
  # DELETE LATER
  if(nrow(p41_data_i) > 1) p41_data_i <- p41_data_i[1,]
  if(nrow(ar_data_i) > 1) ar_data_i <- ar_data_i[1,]
  if(nrow(fu_data_i) > 1) fu_data_i <- fu_data_i[1,]
  
  # Manual Sit Report 1 --------------------------------------------------------
  output$sr_report_text <- renderUI({
    
    out <- NULL
    
    if(!is.null(data_i$sr_row1_c1)){
      if(is.na(data_i$sr_row1_c1)){
        
      } else{
        
        out <- fluidRow(
          fluidRow(column(12, align = "center", strong("Situation Report"))),
          br(),
          fluidRow(
            column(4, data_i$sr_row1_c1),
            column(4, data_i$sr_row1_c2),
            column(4, data_i$sr_row1_c3)
          ),
          fluidRow(column(12,data_i$sr_row2)),
          fluidRow(column(12,data_i$sr_row3)),
          fluidRow(column(12,data_i$sr_row4)),
          fluidRow(column(12,data_i$sr_row5)),
          fluidRow(
            column(4, data_i$sr_row6_c1),
            column(4, data_i$sr_row6_c2),
            column(4, data_i$sr_row6_c3)
          ),
          br(),
          fluidRow(
            column(12,
                   data_i$sr_main_text
            )
          ),
          br(),
          fluidRow(column(12, data_i$sr_row7)),
          fluidRow(column(12, data_i$sr_row8)),
          fluidRow(column(12, data_i$sr_row9)),
          fluidRow(column(12, data_i$sr_row10))
        )
      }
    }
    
    out
    
  })
  
  # View Follow Up Report ------------------------------------------------------
  gen_followup_report_text <- function(i){
    renderUI({
      
      if(!("finalized" %in% fu_data$stage_fu[i])){
        return(NULL) } 
      else {
        
        div(
          
          #fluidRow(column(6, offset = 3, align = "center", hr())),
          fluidRow(
            column(12, align = "center",
                   strong("Folow Up Report")
            ),
          ),
          br(),
          fluidRow(
            column(6, offset = 3,
                   fu_data$fu_main_text[i]
            )
          )
          
        ) # end div
        
      }
      
    })
  }
  
  output$fu_text_1 <- gen_followup_report_text(1)
  
  # View Amendment Report ------------------------------------------------------
  gen_amend_report_text <- function(i){
    renderUI({
      
      if(!("finalized" %in% ar_data$stage_ar[i])){
        return(NULL) } 
      else {
        
        div(
          
          #fluidRow(column(6, offset = 3, align = "center", hr())),
          fluidRow(
            column(12, align = "center",
                   strong("Amendment Report")
            ),
          ),
          br(),
          fluidRow(
            column(6, offset = 3,
                   ar_data$ar_main_text[i]
            )
          )
          
        ) # end div
        
      }
      
    })
  }
  
  output$ar_text_1 <- gen_amend_report_text(1)
  # output$ar_text_2 <- gen_amend_report_text(2)
  # output$ar_text_3 <- gen_amend_report_text(3)
  # output$ar_text_4 <- gen_amend_report_text(4)
  # output$ar_text_5 <- gen_amend_report_text(5)
  # 
  # Download Handler: SR -----------------------------------------------------------
  output$print_all_situationreports = downloadHandler(
    filename = 'situation_report_merged.pdf',
    
    content = function(file) {
      shiny::withProgress(
        message = paste0("Downloading Situation Reports"),
        value = 0,
        {
          shiny::incProgress(1/10)
          #Sys.sleep(1)
          shiny::incProgress(5/10)
          
          data_temp = bind_rows(data_i, ar_data, fu_data)
          out = knit2pdf(file.path('modules_gen_reports','generate_sr_pdf_loop_parent.Rnw'), clean = TRUE)
          #file.rename(out, file) # move pdf to file for downloading
          file.copy(out, file) 
        }
        
      )
    },
    
    contentType = 'application/pdf'
  )
  
  # Download Handler: P41 -----------------------------------------------------------
  output$p41_pdf = downloadHandler(
    filename = 'p41.pdf',
    
    content = function(file) {
      out = knit2pdf(file.path('modules_gen_reports', 'generate_p41_pdf_parent.Rnw'), clean = TRUE)
      #file.rename(out, file) # move pdf to file for downloading
      file.copy(out, file) 
    },
    
    contentType = 'application/pdf'
  )
  
  # Download Handler: Crash Sketch ---------------------------------------------
  ## Button
  output$download_crash_sketch_finalized_page <- downloadHandler(
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
  output$download_crash_sketch_finalized_page_ui <- renderUI({
    
    img_file_name <- file.path("data", 
                               "crash_sketches",
                               paste0(data_i$uid, ".png"))
    
    if(file.exists(img_file_name)){
      out <- downloadButton(session$ns("download_crash_sketch_finalized_page"), "Download Crash Sketch")
    } else{
      out <- NULL
    }
    
    out
  })
  
  # Download Title -------------------------------------------------------------
  # Download Sit Report OR 'Download Sit Report and Amend Reports'
  
  # output$download_button <- renderUI({
  #   
  #   label <- "Download Situation Report"
  #   
  #   if(!is.null(ar_data)){
  #     if(nrow(ar_data) > 0){
  #       label <- "Download Situation Report and Amendment Reports"
  #     }
  #   }
  #   
  #   #downloadButton(session$ns('print_all_situationreports'), label=label)
  #   
  # })
  # 
  
  # P41 Generate/View/Edit -----------------------------------------------------
  output$p41_options_ui <- renderUI({
    
    if(nrow(p41_data_i) %in% 0){
      actionButton('gen_p41_from_sit_report', label="Generate P41")
    } else if ("editing" %in% p41_data_i$stage_p41){
      actionButton('edit_p41_from_sit_report', label="Edit P41")
    } else if ("submitted_to_hq" %in% p41_data_i$stage_p41){
      actionButton('edit_p41_from_sit_report', label="Review P41")
    } else if ("hq_provided_feedback" %in% p41_data_i$stage_p41){
      actionButton('edit_p41_from_sit_report', label="Edit P41 Based on HQ Feedback")
    } else if ("finalized" %in% p41_data_i$stage_p41){
      downloadButton(session$ns('p41_pdf'), label="Download P41")
    } else{
      "P41?"
    }
    
  })
  
  # SR Generate/View/Edit -----------------------------------------------------
  output$sr_options_ui <- renderUI({
    
    ## Download Label
    label <- "Download Situation Report"
    
    if(!is.null(ar_data)){
      if(nrow(ar_data) > 0){
        label <- "Download Situation and Amendment Reports"
      }
    }
    
    if(!is.null(fu_data)){
      if(nrow(fu_data) > 0){
        label <- "Download Situation and Follow Up Reports"
      }
    }
    
    if(!is.null(ar_data) & !is.null(fu_data)){
      if((nrow(ar_data) > 0) & (nrow(fu_data) > 0)){
        label <- "Download Situation, Amendment and Follow Up Reports"
      }
    }
    
    if("editing" %in% sr_data_i$stage){
      actionButton('edit_sit_report', label="Edit Situation Report")
    } else if ("submitted_to_hq" %in% sr_data_i$stage){
      actionButton('edit_sit_report', label="Review Sit Report")
    } else if ("hq_provided_feedback" %in% sr_data_i$stage){
      actionButton('edit_sit_report', label="Edit Sit Report Based on HQ Feedback")
    } else if ("finalized" %in% sr_data_i$stage){
      downloadButton(session$ns('print_all_situationreports'), label=label)
    } else{
      "SR?"
    }
    
  })
  
  # Amend Report Generate/View/Edit --------------------------------------------
  output$ar_options_ui <- renderUI({
    
    if(nrow(ar_data_i) %in% 0){
      actionButton('gen_amend_report', label="Generate Amendment Report")
    } else if ("editing" %in% ar_data_i$stage_ar){
      actionButton('edit_amend_report', label="Edit Amendment Report")
    } else if ("submitted_to_hq" %in% ar_data_i$stage_ar){
      actionButton('edit_amend_report', label="Review Amendment Report")
    } else if ("hq_provided_feedback" %in% ar_data_i$stage_ar){
      actionButton('edit_amend_report', label="Edit Amendment Report Based on HQ Feedback")
    } else if ("finalized" %in% ar_data_i$stage_ar){
      #downloadButton(session$ns('ar_pdf'), label="Download Amendment Report")
      NULL
    } else{
      "Amendment Report?"
    }
    
  })
  
  # Follow Up Report Generate/View/Edit --------------------------------------------
  output$fu_options_ui <- renderUI({
    
    if(nrow(fu_data_i) %in% 0){
      actionButton('gen_followup_report', label="Generate Follow Up Report")
    } else if ("editing" %in% fu_data_i$stage_fu){
      actionButton('edit_followup_report', label="Edit Follow Up Report")
    } else if ("submitted_to_hq" %in% fu_data_i$stage_fu){
      actionButton('edit_followup_report', label="Review Follow Up Report")
    } else if ("hq_provided_feedback" %in% fu_data_i$stage_fu){
      actionButton('edit_followup_report', label="Edit Follow Up Report Based on HQ Feedback")
    } else if ("finalized" %in% fu_data_i$stage_fu){
      #downloadButton(session$ns('fu_pdf'), label="Download Follow Up Report")
      NULL
    } else{
      "Follow Up Report?"
    }
    
  })
  
  # [EMAIL LOGIC] : Sit report -------------------------------------------------
  
  ## Button for "Email Sit Report"
  output$email_sr_button_ui <- renderUI({
    
    out <- NULL
    
    ## Download Label
    label <- "Email Situation Report"
    
    if(!is.null(ar_data)){
      if(nrow(ar_data) > 0){
        label <- "Email Situation and Amendment Reports"
      }
    }
    
    if(!is.null(fu_data)){
      if(nrow(fu_data) > 0){
        label <- "Email Situation and Follow Up Reports"
      }
    }
    
    if(!is.null(ar_data) & !is.null(fu_data)){
      if((nrow(ar_data) > 0) & (nrow(fu_data) > 0)){
        label <- "Email Situation, Amendment and Follow Up Reports"
      }
    }
    
    if(nrow(sr_data_i) > 0){
      if("finalized" %in% sr_data_i$stage){
        out <- actionButton(session$ns('email_sr_button'), label=label,
                            icon = icon("envelope", lib = "font-awesome"))
      } 
    }
    
    if(ONLINE_SYSTEM %in% FALSE){
      if(curl::has_internet() %in% FALSE){
        out <- NULL
      }
    }
    
    out
    
  })
  
  ## Open Model
  email_sr_modal <- function(session) {
    
    modalDialog(
      #"Submit to HQ. After submitting, you will need to click 'Sync Data with Server'
      #for HQ to receive the report.",
      
      tagList(
        h3("Email Situation Report"),
        
        textInput(session$ns("email_sr_emailaddress"), "Email Address(es)", value = ""),
        h6("To send to multiple email addresses, separate by a semicolon (;). All emails are sent via bcc."),
        
        textInput(session$ns("email_sr_subject"), "Subject", 
                  value = paste0("Situation Report - O.B. ", sr_data_i$ob_no)),
        
        textAreaInput(session$ns("email_sr_body"), "Body", 
                      value = paste0("Please see attached for the situation report for O.B. ",
                                     sr_data_i$ob_no,". --Nairobi Traffic eCrash System"),
                      width = "150%",
                      height = "300%")
        
      ),
      footer = tagList(
        actionButton(session$ns("email_sr_button_no"), "Cancel"),
        actionButton(session$ns("email_sr_button_yes"), div(tags$b("Send", style = "color: green;")))
      )
    )
  }
  
  observeEvent(input$email_sr_button,
               ignoreNULL = T,   # Show modal on start up
               showModal(email_sr_modal(session))
  )
  
  # Cancel Send
  observeEvent(input$email_sr_button_no, { 
    removeModal() 
  })
  
  # Send
  observeEvent(input$email_sr_button_yes, { 
    
    sr_pdf_fileame <- sr_data_i$ob_no %>% str_replace_all("/", "_") %>% paste0(".tex")
    sr_pdf_fileame <- paste0("sitreport_ob_", sr_pdf_fileame)
    
    data_temp = bind_rows(data_i, ar_data, fu_data)
    out = knit2pdf(file.path('modules_gen_reports',
                             'generate_sr_pdf_loop_parent.Rnw'), 
                   output = sr_pdf_fileame,
                   clean = TRUE)
    
    to_email_address <- input$email_sr_emailaddress %>% str_squish()
    
    email_worked_message <- tryCatch(
      {
        time_response <- withTimeout({
          
          gm_auth_configure(path = "keys_passwords/gmail_credentials/credentials.json")
          gm_auth(cache = "keys_passwords/gmail_credentials",
                  email = "nairobi.ecrash.system@gmail.com")
          
          tmp_threads <- gm_threads(num_results=1)
          
          my_email_message <- gm_mime() %>%
            gm_bcc(to_email_address) %>%
            gm_from("nairobi.ecrash.system@gmail.com") %>%
            gm_subject(input$email_sr_subject) %>%
            gm_text_body(input$email_sr_body) %>%
            gm_attach_file(out)
          
          gm_send_message(my_email_message)
          
          NULL
        },
        timeout = 7,
        onTimeout = "warning"
        )
        
        if(!is.null(time_response)){
          out_message <- "Timed Out"
        } else{
          out_message <- "Worked"
        }
        
        out_message
        
      },
      error = function(e){
        return("Failed")
      }
    )
    
    removeModal() 
    
    ## Message
    if(email_worked_message %in% "Worked"){
      shinyalert(title = "Sent!",
                 text = "Email successfully sent",
                 type = "success")
    } else if(email_worked_message %in% "Failed"){
      #shinyalert(title = "Emails Not Sent",
      #           text = "Emails not successfully sent. Make sure the email addresses are valid and make sure you're connected to the internet.",
      #           type = "error")
      shinyalert(title = "Issue with Gmail",
                 text = "Sorry, there's an issue with Gmail. Please try again later.",
                 type = "error")
    } else if(email_worked_message %in% "Timed Out"){
      shinyalert(title = "Issue with Gmail",
                 text = "Sorry, there's an issue with Gmail. Please try again later.",
                 type = "error")
    } 
    
  })
  
  # [EMAIL LOGIC] : P41 --------------------------------------------------------
  
  ## Button for "Email Sit Report"
  output$email_p41_button_ui <- renderUI({
    
    out <- NULL
    
    if(nrow(sr_data_i) > 0){
      if("finalized" %in% p41_data_i$stage_p41){
        out <- actionButton(session$ns('email_p41_button'), label="Email P41",
                            icon = icon("envelope", lib = "font-awesome"))
      } 
    }
    
    if(ONLINE_SYSTEM %in% FALSE){
      if(curl::has_internet() %in% FALSE){
        out <- NULL
      }
    }
    
    out
    
  })
  
  ## Open Model
  email_p41_modal <- function(session) {
    
    modalDialog(
      #"Submit to HQ. After submitting, you will need to click 'Sync Data with Server'
      #for HQ to receive the report.",
      
      tagList(
        h3("Email P41"),
        
        textInput(session$ns("email_p41_emailaddress"), "Email Address(es)", value = ""),
        h6("To send to multiple email addresses, separate by a semicolon (;). All emails are sent via bcc."),
        
        textInput(session$ns("email_p41_subject"), "Subject", 
                  value = paste0("P41 - O.B. ", sr_data_i$ob_no)),
        
        textAreaInput(session$ns("email_p41_body"), "Body", 
                      value = paste0("Please see attached for the P41 for O.B. ",
                                     sr_data_i$ob_no,". --Nairobi Traffic eCrash System"),
                      width = "150%",
                      height = "300%")
        
      ),
      footer = tagList(
        actionButton(session$ns("email_p41_button_no"), "Cancel"),
        actionButton(session$ns("email_p41_button_yes"), div(tags$b("Send", style = "color: green;")))
      )
    )
  }
  
  observeEvent(input$email_p41_button,
               ignoreNULL = T,   # Show modal on start up
               showModal(email_p41_modal(session))
  )
  
  # Cancel Send
  observeEvent(input$email_p41_button_no, { 
    removeModal() 
  })
  
  # Send
  observeEvent(input$email_p41_button_yes, { 
    
    p41_pdf_fileame <- sr_data_i$ob_no %>% str_replace_all("/", "_") %>% paste0(".tex")
    p41_pdf_fileame <- paste0("p41_ob_", p41_pdf_fileame)
    
    out = knit2pdf(file.path('modules_gen_reports', 
                             'generate_p41_pdf_parent.Rnw'), 
                   output = p41_pdf_fileame,
                   clean = TRUE)
    
    to_email_address <- input$email_p41_emailaddress %>% str_squish()
    
    email_worked_message <- tryCatch(
      {
        time_response <- withTimeout({
          
          gm_auth_configure(path = "keys_passwords/gmail_credentials/credentials.json")
          gm_auth(cache = "keys_passwords/gmail_credentials",
                  email = "nairobi.ecrash.system@gmail.com")
          
          tmp_threads <- gm_threads(num_results=1)
          
          my_email_message <- gm_mime() %>%
            gm_bcc(to_email_address) %>%
            gm_from("nairobi.ecrash.system@gmail.com") %>%
            gm_subject(input$email_p41_subject) %>%
            gm_text_body(input$email_p41_body) %>%
            gm_attach_file(out)
          
          gm_send_message(my_email_message)
          
          NULL
          
        },
        timeout = 7,
        onTimeout = "warning"
        )
        
        if(!is.null(time_response)){
          out_message <- "Timed Out"
        } else{
          out_message <- "Worked"
        }
        
        out_message
        
      },
      error = function(e){
        return("Failed")
      }
    )
    
    removeModal() 
    
    ## Message
    if(email_worked_message %in% "Worked"){
      shinyalert(title = "Sent!",
                 text = "Email successfully sent",
                 type = "success")
    } else if(email_worked_message %in% "Failed"){
      #shinyalert(title = "Emails Not Sent",
      #           text = "Emails not successfully sent. Make sure the email addresses are valid and make sure you're connected to the internet.",
      #           type = "error")
      shinyalert(title = "Issue with Gmail",
                 text = "Sorry, there's an issue with Gmail. Please try again later.",
                 type = "error")
    } else if(email_worked_message %in% "Timed Out"){
      shinyalert(title = "Issue with Gmail",
                 text = "Sorry, there's an issue with Gmail. Please try again later.",
                 type = "error")
    } else{
      shinyalert(title = "Issue with Gmail",
                 text = "Sorry, there's an issue with Gmail. Please try again later.",
                 type = "error")
    }
    
  })
  
  
  # OB Number Text -------------------------------------------------------------
  output$ob_no_text <- renderText({
    paste0("O.B. Number: ", sr_data_i$ob_no)
  })
  
  # IAR Number -----------------------------------------------------------------
  ##### ** IAR Text -----------------------
  output$iar_number_text <- renderUI({
    
    ## Initial Outputs
    out <- HTML(paste0("<h4>IAR Number: ", sr_data_i$iar_no, "</h4>"))
    
    ## Check if IAR Has Been Entered
    if(is.na(sr_data_i$iar_no) | "" %in% sr_data_i$iar_no){
      
      out <- textInput(session$ns('iar_no_finalized'), 
                       label="Enter IAR Number",
                       value = "")
      
    }
    
    out
    
  })
  
  ##### ** IAR Button -----------------------
  output$iar_number_button <- renderUI({
    
    ## Initial Outputs
    out <- NULL
    
    ## Check if IAR Has Been Entered
    if(is.na(sr_data_i$iar_no) | "" %in% sr_data_i$iar_no){
      
      out <- actionButton(session$ns('submit_iar_no'), 
                          label="Submit IAR Number")
      
    }
    
    
    out
    
  })
  
  ##### ** Enable/Disable IAR Submit Button -----------------------
  observe({
    
    shinyjs::disable("submit_iar_no")
    
    ## Check if IAR Has Been Entered
    if(is.na(sr_data_i$iar_no) | "" %in% sr_data_i$iar_no){
      
      if(!is.null(input$iar_no_finalized)){
        if(!is.na(input$iar_no_finalized)){
          if(input$iar_no_finalized != ""){
            
            if(input$iar_no_finalized %in% dataset$iar_no){
              # out <- HTML("<strong><span style='color:red'>This IAR Number Already Exists</span><strong>")
              shinyjs::disable("submit_iar_no")
              
            } else{
              #out <- actionButton(session$ns('submit_iar_no'), 
              #                    label="Submit IAR Number")
              shinyjs::enable("submit_iar_no")
            }
            
          }
        }
      }
    }
    
  })
  
  ##### ** IAR duplicate Warning -----------------------
  observe({
    
    output$iar_number_warning <- renderUI({
      iar_warning <- NULL
      
      ## Check if IAR Has Been Entered
      if(is.na(sr_data_i$iar_no) | "" %in% sr_data_i$iar_no){
        
        if(!is.null(input$iar_no_finalized)){
          if(!is.na(input$iar_no_finalized)){
            if(input$iar_no_finalized != ""){
              
              if(input$iar_no_finalized %in% dataset$iar_no){
                iar_warning <- HTML("<strong><span style='color:red'>This IAR Number Already Exists</span><strong>")
              } 
              
            }
          }
        }
      }
      
      iar_warning
      
    })
  })
  
  ##### ** Action when click submit -----------------------
  observeEvent(input$submit_iar_no, {
    
    ## Add IAR to Crash Data
    data_i$iar_no <- input$iar_no_finalized %>% as.character()
    
    #### Add last updated
    data_i$last_updated <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character
    
    ## All as character to avoid variable type conflicts
    for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
    for(var in names(dataset)) dataset[[var]] <- dataset[[var]] %>% as.character()
    
    ## Add to full dataset
    dataset <- bind_rows(dataset, data_i) %>%
      arrange(desc(last_updated)) %>%
      distinct(uid, report_type, amend_report_id, .keep_all = TRUE)
    
    ## Save
    dataset <- as.data.frame(dataset)
    if(TESTING_SYSTEM){
      saveRDS(dataset, file.path("data", "police_data_test.Rds"), version=2)
    } else{
      saveRDS(aes_cbc_encrypt(serialize(dataset, NULL), key = data_key), file.path("data", "police_data.Rds"), version=2)
    }
    
    ## All data 
    all_data_r <- reactive({
      dataset
    })
    all_data_r <<- all_data_r
    
    ## Change UI
    output$iar_number_text <- renderUI({
      HTML(paste0("<h4>IAR Number: ", data_i$iar_no, "</h4>"))
    })
    
    output$iar_number_button <- renderUI({
      NULL
    })
    
    output$iar_number_warning <- renderUI({
      NULL
    })
    
    
  })
  
  # ** Delete Report Button ----------------------------------------------------
  # **** Box ####
  delete_report_modal <- function(session) {
    
    modalDialog(
      "Are you sure you want to delete the report?",
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
    if(Username %in% "dime"){
      out <- actionButton(session$ns("delete_report_button"), "Delete Report")
    } else{
      #out <- actionButton(session$ns("delete_report_button"), "Delete Report")
      out <- NULL
    }
    
    out
  })
  
  
  
  
}







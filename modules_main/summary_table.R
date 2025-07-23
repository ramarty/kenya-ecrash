# Summary Table

sum_table_UI <- function(id) {
  ns <- NS(id)
  
  div(
    
    fluidRow(
      column(12, align = "center",
             uiOutput(ns("table_cntrl_dates_ui"))
      )
    ),
    
    tabsetPanel(
      
      # Tab: Crashes and Victims -----------------------------------------------
      tabPanel("Crashes and Victims", 
               
               fluidRow(
                 column(6, align = "right",
                        downloadButton(ns('victim_summary_pdf'), label="Download Table")
                 ),
                 column(6, align = "left",
                        actionButton(ns('email_t1_button'), label="Email Table",
                                     icon = icon("envelope", lib = "font-awesome"))
                 )
               ),
               fluidRow(
                 column(12, align = "center",
                        reactableOutput(ns("crash_table"),
                                        width = "95%")
                 )
               ),
               fluidRow(
                 column(12, align = "left",
                        HTML("<small><strong>Victims include both injured participants and those injured that were not participants.</strong></small>")
                 )
               ),
               br(),
               br(),
               br(),
               br()
      ),
      
      # Tab: Crashes Weekly Report ---------------------------------------------
      tabPanel("Summary of Crashes", 
               
               
               #fluidRow(
               #   column(12, align = "center",
               #          uiOutput(ns("table2_cntrl_dates_ui"))
               #   )
               # ),
               fluidRow(
                 column(6, align = "right",
                        downloadButton(ns('crash_summary_pdf'), label="Download Table")
                 ),
                 column(6, align = "left",
                        actionButton(ns('email_t2_button'), label="Email Table",
                                     icon = icon("envelope", lib = "font-awesome"))
                 )
               ),
               fluidRow(
                 column(12, align = "center",
                        tableOutput(ns("weekly_report_table"))
                 )
               ),
               
               
      )
      
      
    )
    
  )
  
}

# SERVER ****************************************** ============================
sum_table_Server <- function(input, output, session) {
  
  # ** Control/Inputs -------------------------------------------------------------
  output$table_cntrl_dates_ui <- renderUI({
    
    dateRangeInput(session$ns("table_cntrl_dates"), 
                   "Choose Date Range",
                   start = as.Date("2021-01-01"),
                   end = Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.Date(),
                   label = NULL,
                   language = "en-GB",
                   format = "dd/mm/yyyy")
    
  })
  
  # output$table2_cntrl_dates_ui <- renderUI({
  #   
  #   dateRangeInput(session$ns("table2_cntrl_dates"), 
  #                  "Choose Date Range",
  #                  start = as.Date("2020-01-01"),
  #                  end = Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.Date(),
  #                  label = NULL,
  #                  language = "en-GB",
  #                  format = "dd/mm/yyyy")
  #   
  # })
  
  # ** Download Buttons --------------------------------------------------------
  output$crash_summary_pdf = downloadHandler(
    filename = 'crash_summary.pdf',
    
    content = function(file) {
      out = knit2pdf(file.path('modules_gen_reports', 'generate_crash_summary_table.Rnw'), clean = TRUE)
      #file.rename(out, file) # move pdf to file for downloading
      file.copy(out, file)
    },
    
    contentType = 'application/pdf'
  )
  
  output$victim_summary_pdf = downloadHandler(
    filename = 'crash_victim_summary.pdf',
    
    content = function(file) {
      out = knit2pdf(file.path('modules_gen_reports', 'generate_victim_summary_table.Rnw'), clean = TRUE)
      #file.rename(out, file) # move pdf to file for downloading
      file.copy(out, file)
    },
    
    contentType = 'application/pdf'
  )
  
  # ** Email Buttons -----------------------------------------------------------

  # **** Table 1 ---------------------------------------------------------------
  
  ## Open Model
  email_t1_modal <- function(session) {
    
    modalDialog(
      #"Submit to HQ. After submitting, you will need to click 'Sync Data with Server'
      #for HQ to receive the report.",
      
      tagList(
        h3("Email Accident Victims Summary"),
        
        textInput(session$ns("email_t1_emailaddress"), "Email Address(es)", value = ""),
        h6("To send to multiple email addresses, separate by a semicolon (;). All emails are sent via bcc."),
        
        textInput(session$ns("email_t1_subject"), "Subject", 
                  value = paste0("Accident Victims Summary - ", 
                                 input$table_cntrl_dates[1] %>% as.Date() %>% format('%d/%m/%Y'),
                                 " to ", 
                                 input$table_cntrl_dates[2] %>% as.Date() %>% format('%d/%m/%Y'))),
        
        textAreaInput(session$ns("email_t1_body"), "Body", 
                      value = paste0("Please see attached for the summary of accident victims. --Nairobi Traffic eCrash System"),
                      width = "150%",
                      height = "300%")
        
      ),
      footer = tagList(
        actionButton(session$ns("email_t1_button_no"), "Cancel"),
        actionButton(session$ns("email_t1_button_yes"), div(tags$b("Send", style = "color: green;")))
      )
    )
  }
  
  observeEvent(input$email_t1_button,
               ignoreNULL = T,   # Show modal on start up
               showModal(email_t1_modal(session))
  )
  
  # Cancel Send
  observeEvent(input$email_t1_button_no, { 
    removeModal() 
  })
  
  # Send
  observeEvent(input$email_t1_button_yes, { 
    
    t1_pdf_fileame <- "accident_victim_summary.tex"
    
    print(t1_pdf_fileame)
    out = knit2pdf(file.path('modules_gen_reports', 
                             'generate_victim_summary_table.Rnw'), 
                   output = t1_pdf_fileame,
                   clean = TRUE)
    
    to_t1_email_address <- input$email_t1_emailaddress %>% str_squish()
    
    email_worked_message <- tryCatch(
      {
        time_response <- withTimeout({
          
          gm_auth_configure(path = "keys_passwords/gmail_credentials/credentials.json")
          gm_auth(cache = "keys_passwords/gmail_credentials",
                  email = "nairobi.ecrash.system@gmail.com")
          
          tmp_threads <- gm_threads(num_results=1)
          
          my_email_message <- gm_mime() %>%
            gm_bcc(to_t1_email_address) %>%
            gm_from("nairobi.ecrash.system@gmail.com") %>%
            gm_subject(input$email_t1_subject) %>%
            gm_text_body(input$email_t1_body) %>%
            gm_attach_file(out)
          
          aaaa <<- my_email_message
          
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
  
  # **** Table 2 ---------------------------------------------------------------
  ## Open Model
  email_t2_modal <- function(session) {
    
    modalDialog(
      #"Submit to HQ. After submitting, you will need to click 'Sync Data with Server'
      #for HQ to receive the report.",
      
      tagList(
        h3("Email Accident Summary"),
        
        textInput(session$ns("email_t2_emailaddress"), "Email Address(es)", value = ""),
        h6("To send to multiple email addresses, separate by a semicolon (;). All emails are sent via bcc."),
        
        textInput(session$ns("email_t2_subject"), "Subject", 
                  value = paste0("Accident Summary - ", 
                                 input$table_cntrl_dates[1] %>% as.Date() %>% format('%d/%m/%Y'),
                                 " to ", 
                                 input$table_cntrl_dates[2] %>% as.Date() %>% format('%d/%m/%Y'))),
        
        textAreaInput(session$ns("email_t2_body"), "Body", 
                      value = paste0("Please see attached for the accident summary. --Nairobi Traffic eCrash System"),
                      width = "150%",
                      height = "300%")
        
      ),
      footer = tagList(
        actionButton(session$ns("email_t2_button_no"), "Cancel"),
        actionButton(session$ns("email_t2_button_yes"), div(tags$b("Send", style = "color: green;")))
      )
    )
  }
  
  observeEvent(input$email_t2_button,
               ignoreNULL = T,   # Show modal on start up
               showModal(email_t2_modal(session))
  )
  
  # Cancel Send
  observeEvent(input$email_t2_button_no, { 
    removeModal() 
  })
  
  # Send
  observeEvent(input$email_t2_button_yes, { 
    
    t2_pdf_fileame <- "accident_summary.tex"
    
    print(t2_pdf_fileame)
    out = knit2pdf(file.path('modules_gen_reports', 
                             'generate_crash_summary_table.Rnw'), 
                   output = t2_pdf_fileame,
                   clean = TRUE)
    
    to_t2_email_address <- input$email_t2_emailaddress %>% str_squish()
    
    email_worked_message <- tryCatch(
      {
        time_response <- withTimeout({
          
          gm_auth_configure(path = "keys_passwords/gmail_credentials/credentials.json")
          gm_auth(cache = "keys_passwords/gmail_credentials",
                  email = "nairobi.ecrash.system@gmail.com")

          tmp_threads <- gm_threads(num_results=1)
          
          my_email_message <- gm_mime() %>%
            gm_bcc(to_t2_email_address) %>%
            gm_from("nairobi.ecrash.system@gmail.com") %>%
            gm_subject(input$email_t2_subject) %>%
            gm_text_body(input$email_t2_body) %>%
            gm_attach_file(out)
          
          aaaa <<- my_email_message

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
  
  # ** Prep Data ---------------------------------------------------------------
  observe({
    tmp <- update_summary_table_r$value
    update_summary_table_r$value <<- NULL
    
    # Load Data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    if(TESTING_SYSTEM){
      
      # Need to check file exists; when first create app without data throws an error
      if(!file.exists(file.path("data", "police_data_test.Rds")) | !exists("data_key")){
        data_table <- data.frame(NULL)
      } else{
        data_table <- readRDS(file.path("data", "police_data_test.Rds")) %>%
          arrange(desc(last_updated)) %>%
          distinct(uid, .keep_all = T)
      }
      
    } else{
      if(!file.exists(file.path("data", "police_data.Rds"))  | !exists("data_key")){
        data_table <- data.frame(NULL)
      } else{
        data_table <- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data.Rds")), key = data_key)) %>%
          arrange(desc(last_updated)) %>%
          distinct(uid, .keep_all = T)
      } 
      
    }
    
    ## Prep variables
    if(nrow(data_table) > 0){
      
      # Subset - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      data_table$accident_date <- data_table$accident_date %>% ymd()
      
      ## Remove crashes based on controls
      data_table <- data_table[!is.na(data_table$accident_date),]
      data_table <- data_table[data_table$accident_date >= as.character(input$table_cntrl_dates[1]),]
      data_table <- data_table[data_table$accident_date <= as.character(input$table_cntrl_dates[2]),]
      
      # Prep Data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      data_table_list <- prep_data_for_figures(data_table)
      data_table <- data_table_list$dataset
      victim_df <- data_table_list$victim_df
      #prtcpnt_df <- data_table_list$prtcpnt_df
    }  
    
    if(nrow(data_table) %in% 0){
      data_table <- data.frame(NULL) %>%
        dplyr::mutate(uid = "",
               worst_injury = "",
               worst_injury_color = "",
               accident_date = "",
               accident_date = "",
               cause_code_category = "",
               cause_code = "",
               police_division = "",
               cause_code_value = "")
      
      victim_df <- data.frame(NULL) %>%
        dplyr::mutate(uid = "",
               injurytype = "")
      
    }
    
    if(nrow(victim_df) %in% 0){
      
      victim_df <- data.frame(NULL) %>%
        dplyr::mutate(uid = "",
               injurytype = "")
    }
    
    # ** Table: Crashes and Victims by Station ---------------------------------
    output$crash_table <- renderReactable({
      req(input$table_cntrl_dates)
      
      data_station_sum <<- prep_data_victim_summary_table(data_table, victim_df)

      # #### Number of victims per type per crash
      # data_victims <- victim_df %>%
      #   dplyr::select(uid, injurytype) %>%
      #   group_by(uid, injurytype) %>%
      #   dplyr::summarise(N = n()) %>%
      #   pivot_wider(id_cols = uid,
      #               values_from = N,
      #               names_from = injurytype)
      # 
      # if(is.null(data_victims$Fatal))   data_victims$Fatal   <- 0
      # if(is.null(data_victims$Serious)) data_victims$Serious <- 0
      # if(is.null(data_victims$Slight))  data_victims$Slight  <- 0
      # 
      # data_victims <- data_victims %>%
      #   dplyr::mutate(Victims = Slight + Serious + Fatal)
      # 
      # ## Worst Type
      # data_victims$worst_injury <- NA
      # data_victims$worst_injury[data_victims$Slight > 0]  <- "Slight"
      # data_victims$worst_injury[data_victims$Serious > 0] <- "Serious"
      # data_victims$worst_injury[data_victims$Fatal > 0]   <- "Fatal"
      # 
      # #### Merge with Crashes, Prep, Collapse
      # data_station_sum <- data_table %>%
      #   
      #   ## Merge
      #   dplyr::select(uid, police_division) %>%
      #   left_join(data_victims, by = "uid") %>%
      #   
      #   ## Prep
      #   mutate(police_division = police_division %>%
      #            na_if(".")) %>%
      #   filter(!is.na(police_division)) %>%
      #   
      #   ## Collapse
      #   group_by(police_division) %>%
      #   dplyr::summarise(
      #     #"Total Victims" = sum(Victims, na.rm=T),
      #     "Slight Victims" = sum(Slight, na.rm=T),
      #     "Serious Victims" = sum(Serious, na.rm=T),
      #     "Fatal Victims" = sum(Fatal, na.rm=T),
      #     
      #     "Total Crashes" = n(),
      #     "Slight Crashes" = sum(worst_injury %in% "Slight", na.rm=T),
      #     "Serious Crashes" = sum(worst_injury %in% "Serious", na.rm=T),
      #     "Fatal Crashes" = sum(worst_injury %in% "Fatal", na.rm=T)
      #   ) %>%
      #   
      #   dplyr::mutate(`Total Victims` = `Slight Victims` + `Serious Victims` + `Fatal Victims`) %>%
      #   
      #   ## Final Prep
      #   dplyr::rename(Station = police_division) 
      # 
      # ## Nrow 0 check and fill variables
      # if(nrow(data_station_sum) %in% 0){
      #   data_station_sum <- data.frame(matrix(nrow = 1, ncol=0)) %>%
      #     mutate(Station = "Buruburu",
      #            "Total Victims" = 0,
      #            "Slight Victims" = 0,
      #            "Serious Victims" = 0,
      #            "Fatal Victims" = 0,
      #            "Total Crashes" = 0,
      #            "Slight Crashes" = 0,
      #            "Serious Crashes" = 0,
      #            "Fatal Crashes" = 0)
      # }
      # 
      # data_station_sum <- data_station_sum %>%
      #   mutate(Station = Station %>%
      #            factor(levels = c("Buruburu",
      #                              "Central",
      #                              "Dagoretti",
      #                              "Embakasi",
      #                              "Gigiri",
      #                              "GVAIS",
      #                              "Industrial Area",
      #                              "Karen",
      #                              "Kasarani",
      #                              "Kayole",
      #                              "Kilimani",
      #                              "Langata",
      #                              "Makongeni",
      #                              "Starehe"))) %>%
      #   complete(Station, 
      #            fill = list(`Total Victims` = 0,
      #                        `Slight Victims` = 0,
      #                        `Serious Victims` = 0,
      #                        `Fatal Victims` = 0,
      #                        `Total Crashes` = 0,
      #                        `Slight Crashes` = 0,
      #                        `Serious Crashes` = 0,
      #                        `Fatal Crashes` = 0)) %>%
      #   
      #   # Order
      #   dplyr::select("Station", 
      #                 "Total Crashes",
      #                 "Slight Crashes",
      #                 "Serious Crashes",
      #                 "Fatal Crashes",
      #                 "Total Victims",
      #                 "Slight Victims",
      #                 "Serious Victims",
      #                 "Fatal Victims") %>%
      #   
      #   # As Integer
      #   mutate_if(is.numeric, . %>% as.integer) 
      
      #### Make table
      GnYlRd <- function(x) rgb(colorRamp(c("#C6F6C6", "#F6F6C6", "#EFB8AF"))(x), maxColorValue = 255)
      
      crashes_num <- data_station_sum[,c("Total Crashes", "Slight Crashes", "Serious Crashes", "Fatal Crashes")]
      victims_num <- data_station_sum[,c("Total Victims", "Slight Victims", "Serious Victims", "Fatal Victims")]
      
      crashes_num_max <- ifelse(max(crashes_num) %in% 0, 1, max(crashes_num))
      victims_num_max <- ifelse(max(victims_num) %in% 0, 1, max(victims_num))
      
      #max_value <- ifelse(max(data_station_sum_num) %in% 0, 1, max(data_station_sum_num))
      reactable(
        data_station_sum,
        defaultColDef = colDef(
          header = function(value) gsub(".", " ", value, fixed = TRUE),
          cell = function(value) format(value, nsmall = 1),
          align = "center",
          headerStyle = list(background = "#f7f7f8"),
          
          style = function(value) {
            if (!is.numeric(value)) return()
            normalized <- (value - min(victims_num)) / (victims_num_max - min(victims_num))
            color <- GnYlRd(normalized)
            list(background = color)
          },
          format = colFormat(digits = 0),
          minWidth = 50
        ),
        columns = list(
          .rownames = colDef(name = "Station", sortable = TRUE, align = "left"),
          `Total Crashes`   = colDef(name = "Total",
                                     style = function(value) {
                                       if (!is.numeric(value)) return()
                                       normalized <- (value - min(crashes_num)) / (crashes_num_max - min(crashes_num))
                                       color <- GnYlRd(normalized)
                                       list(background = color)
                                     }),
          `Slight Crashes`  = colDef(name = "Slight",
                                     style = function(value) {
                                       if (!is.numeric(value)) return()
                                       normalized <- (value - min(crashes_num)) / (crashes_num_max - min(crashes_num))
                                       color <- GnYlRd(normalized)
                                       list(background = color)
                                     }),
          `Serious Crashes` = colDef(name = "Serious",
                                     style = function(value) {
                                       if (!is.numeric(value)) return()
                                       normalized <- (value - min(crashes_num)) / (crashes_num_max - min(crashes_num))
                                       color <- GnYlRd(normalized)
                                       list(background = color)
                                     }),
          `Fatal Crashes`   = colDef(name = "Fatal",
                                     style = function(value) {
                                       if (!is.numeric(value)) return()
                                       normalized <- (value - min(crashes_num)) / (crashes_num_max - min(crashes_num))
                                       color <- GnYlRd(normalized)
                                       list(background = color)
                                     }),
          `Total Victims`   = colDef(name = "Total"),
          `Slight Victims`  = colDef(name = "Slight"),
          `Serious Victims` = colDef(name = "Serious"),
          `Fatal Victims`   = colDef(name = "Fatal")
        ),
        columnGroups = list(
          colGroup(name = "Crashes", columns = c("Total Crashes", "Slight Crashes", "Serious Crashes", "Fatal Crashes")),
          colGroup(name = "Victims", columns = c("Total Victims", "Slight Victims", "Serious Victims", "Fatal Victims"), headerStyle = list(background = "black", color = "white"))
        ),
        bordered = TRUE,
        pagination = FALSE, 
        highlight = TRUE,
        rowStyle = function(index) {
          list(fontWeight = "bold")
        }
      )
      
      
    })
    
    # ** Table: Weekly Crash Report --------------------------------------------
    output$weekly_report_table <- function() {
      req(input$table_cntrl_dates)
      
      data_table <- cause_code_text_to_value(data_table)
      
      df_table <<- prep_data_weekly_report_table(data_table, victim_df)
      
      #### Prep Data
      
      # ## Add UID to victim_df
      # uid_station_df <- data_table %>%
      #   dplyr::filter(police_division != ".") %>%
      #   dplyr::select(uid, police_division) %>%
      #   distinct()
      # 
      # victim_df <- victim_df %>%
      #   left_join(uid_station_df, by = "uid")
      # 
      # ## Add worst injury to data_table
      # wi_df <- victim_df
      # wi_df$worst_injury <- NA
      # wi_df$worst_injury[wi_df$injurytype %in% "Slight"]  <- 1
      # wi_df$worst_injury[wi_df$injurytype %in% "Serious"]  <- 2
      # wi_df$worst_injury[wi_df$injurytype %in% "Fatal"]  <- 3
      # 
      # wi_df <- wi_df %>%
      #   group_by(uid) %>%
      #   mutate(worst_injury = max(worst_injury)) %>%
      #   ungroup() %>%
      #   distinct(worst_injury, uid) %>% 
      #   mutate(worst_injury =
      #            case_when(worst_injury %in% 1 ~ "Slight",
      #                      worst_injury %in% 2 ~ "Serious",
      #                      worst_injury %in% 3 ~ "Fatal")) 
      # 
      # data_table$worst_injury <- NULL
      # 
      # data_table <- data_table %>%
      #   left_join(wi_df, by = "uid")
      # 
      # #### NO. OF ACCIDENTS
      # no_accidents <- data_table %>%
      #   dplyr::filter(worst_injury %in% c("Slight", "Serious", "Fatal"),
      #                 police_division != ".") %>%
      #   dplyr::group_by(police_division, worst_injury) %>%
      #   dplyr::summarise(N = n()) %>%
      #   dplyr::rename('NO. OF ACCIDENTS' = N) %>%
      #   dplyr::rename(injurytype = worst_injury)
      # 
      # #### NO. OF VICTIMS
      # no_victims <- victim_df %>%
      #   dplyr::filter(injurytype %in% c("Slight", "Serious", "Fatal"),
      #                 police_division != ".") %>%
      #   dplyr::group_by(police_division, injurytype) %>%
      #   dplyr::summarise(N = n()) %>%
      #   dplyr::rename('NO. OF VICTIMS' = N)
      # 
      # #### VICTIM CLASS
      # ## Prep victim class variable
      # # From "victim" tab
      # victim_df$victim_class <- victim_df$personclass 
      # 
      # # From "participant" tab
      # victim_df$victim_class[is.na(victim_df$victim_class)] <- victim_df$vehpart_type[is.na(victim_df$victim_class)]
      # 
      # # Cleanup
      # victim_df$victim_class[victim_df$victim_class %in% c("Saloon Car",
      #                                                      "Pickup van",
      #                                                      "Lorry",
      #                                                      "Lorry + trailer",
      #                                                      "Bus",
      #                                                      "Matatu",
      #                                                      "Motor-cycle", ## TODO: in this category??
      #                                                      "Other Vehicle")] <- "Driver"
      # 
      # victim_df$victim_class[victim_df$victim_class %in% c("Bicycle", "Cyclist")] <- "Pedal Cyclist"
      # 
      # ## Make Data
      # victim_class <- victim_df %>%
      #   filter(injurytype %in% c("Slight", "Serious", "Fatal"),
      #          police_division != ".") %>%
      #   filter(!is.na(victim_class)) %>%
      #   dplyr::group_by(police_division, injurytype, victim_class) %>%
      #   dplyr::summarise(N = n()) %>%
      #   dplyr::mutate(victim_class = 
      #                   case_when((victim_class == "Pedestrian" & N >= 2) ~ "Pedestrians",
      #                             (victim_class == "Driver" & N >= 2) ~ "Drivers",
      #                             (victim_class == "Passenger" & N >= 2) ~ "Passengers",
      #                             (victim_class == "Pedal Cyclist" & N >= 2) ~ "Pedal Cyclists",
      #                             TRUE ~ victim_class)) %>%
      #   dplyr::mutate(victim_class = paste(N, victim_class)) %>%
      #   dplyr::select(-N) %>%
      #   dplyr::rename('CLASS OF VICTIM' = victim_class) %>%
      #   group_by(police_division, injurytype) %>%
      #   dplyr::summarise_all( ~ paste(., collapse="<br>"))
      # 
      # #### Crash Details
      # # Date, time, code, road
      # 
      # data_table$accident_time_hour[nchar(data_table$accident_time_hour) %in% 1] <-
      #   paste0("0", data_table$accident_time_hour[nchar(data_table$accident_time_hour) %in% 1])
      # 
      # data_table$accident_time_minute[nchar(data_table$accident_time_minute) %in% 1] <-
      #   paste0("0", data_table$accident_time_minute[nchar(data_table$accident_time_minute) %in% 1])
      # 
      # data_table$accident_time <- paste0(data_table$accident_time_hour, data_table$accident_time_minute, " HRS")
      # data_table$accident_time[is.na(data_table$accident_time_hour)] <- "UNKNOWN"
      # 
      # crash_details <- data_table %>%
      #   dplyr::filter(worst_injury %in% c("Slight", "Serious", "Fatal"),
      #                 police_division != ".") %>%
      #   dplyr::select(police_division, worst_injury, accident_date, accident_time, cause_code_number, road_name) %>%
      #   dplyr::mutate(accident_date = accident_date %>% as.Date() %>% format("%d/%m/%Y")) %>%
      #   dplyr::rename(injurytype = worst_injury,
      #                 DATE = accident_date,
      #                 TIME = accident_time,
      #                 CODE = cause_code_number,
      #                 ROAD = road_name) %>%
      #   group_by(police_division, injurytype) %>%
      #   dplyr::summarise_all( ~ paste(., collapse="<br>"))
      # 
      # #### Merge
      # df_table <- victim_class %>%
      #   merge(no_victims, by = c("police_division", "injurytype"), all = T) %>%
      #   merge(no_accidents, by = c("police_division", "injurytype"), all = T) %>%
      #   merge(crash_details, by = c("police_division", "injurytype"), all = T) %>%
      #   dplyr::rename('STATION' = police_division,
      #                 'TYPE OF ACCIDENT' = injurytype) %>%
      #   mutate(`TYPE OF ACCIDENT` = `TYPE OF ACCIDENT` %>% factor(levels = c("Fatal", "Serious", "Slight"))) %>%
      #   complete(STATION, `TYPE OF ACCIDENT`) %>%
      #   arrange(STATION, `TYPE OF ACCIDENT`) %>%
      #   mutate(CODE = CODE %>% str_replace_all("NA", ""))
      
      options(knitr.kable.NA = '')
      
      df_table %>%
        dplyr::select("STATION", "TYPE OF ACCIDENT",
                      "NO. OF ACCIDENTS", "NO. OF VICTIMS",
                      "CLASS OF VICTIM", "DATE",
                      "TIME", "CODE", "ROAD") %>%
        kable("html", 
              escape = FALSE,
              align = "lllllllll") %>%
        column_spec(1:9,border_left = T, border_right = T) %>%
        row_spec(3, hline_after = T) %>%
        kable_styling(full_width = F,
                      fixed_thead = F) %>%
        column_spec(1:2, width = "7em") %>%
        column_spec(3:4, width = "5em") %>%
        column_spec(5, width = "8em") %>%
        collapse_rows(columns = 1:2, valign = "top") 
      
    }
    
  })
  
}




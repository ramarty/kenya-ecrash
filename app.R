# Nairobi Police eCrash System

# Resources
# https://rstudio.github.io/shinydashboard/

rm(list = ls()) # App depends on globals, so clear everything beforehand

ONLINE_SYSTEM <<- T
BUCKET_NAME <<- "wb-smarttrans-police-pilot-test"
VERSION_NUMBER <<- "1.15"

TESTING_CAPTION <- "TEST SYSTEM"
SYSTEM_SHORT_NAME <- " TEST"
TEST_SYSTEM_TF <- T

ALLOWED_IP_ADDRESSES <- ""

source(file.path("modules_main", "setup.R"))
source(file.path("modules_main", "functions.R"))
source(file.path("modules_main", "new_report.R"))
source(file.path("modules_main", "sit_report.R"))
source(file.path("modules_main", "sit_report_finalized.R"))
source(file.path("modules_main", "summary_figures.R"))
source(file.path("modules_main", "summary_table.R"))
source(file.path("modules_main", "amendment_report.R"))
source(file.path("modules_main", "followup_report.R"))
source(file.path("modules_main", "p41.R"))
source(file.path("modules_gen_reports", "generate_p41_pdf_prepdata.R"))

TESTING_SYSTEM <<- T
INCORRECT_PASSWORD <<- T

# Popups **************************************** ------------------------------

# ** Delete Popup --------------------------------------------------------------
# Display an important message that can be dismissed only by clicking the
# dismiss button.
shinyApp(
  ui = basicPage(
    actionButton("show", "Delete Form")
  ),
  server = function(input, output) {
    observeEvent(input$show, {
      showModal(modalDialog(
        title = "Important message",
        "This is an important message!"
      ))
    })
  }
)


# Display a message that can be dismissed by clicking outside the modal dialog,
# or by pressing Esc.
shinyApp(
  ui = basicPage(
    actionButton("show", "Delete Form")
  ),
  server = function(input, output) {
    observeEvent(input$show, {
      showModal(modalDialog(
        title = "Somewhat important message",
        "This is a somewhat important message.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
  }
)


# 3. POPUPS ************************************************* -------------------------------



# # ** Update System Popup - ORIG -----------------------------------------------------
# 
# 
# 
# 
# 
# # ** Update System Popup -------------------------------------------------------
# # Display an important message that can be dismissed only by clicking the
# # dismiss button.
# shinyApp(
#   ui = basicPage(
#     actionButton("updatesystem", "Update System")
#   ),
#   server = function(input, output) {
#     observeEvent(input$updatesystem, {
#       showModal(modalDialog(
#         title = "Important message",
#         "This is an important message!"
#       ))
#     })
#   }
# )
# 
# # Display a message that can be dismissed by clicking outside the modal dialog,
# # or by pressing Esc.
# shinyApp(
#   ui = basicPage(
#     actionButton("updatesystem", "Update System")
#   ),
#   server = function(input, output) {
#     observeEvent(input$updatesystem, {
#       showModal(modalDialog(
#         title = "Somewhat important message",
#         "This is a somewhat important message.",
#         easyClose = TRUE,
#         footer = NULL
#       ))
#     })
#   }
# )

# UIs **************************************** ---------------------------------

# ui blank ---------------------------------------------------------------------
#ui = (htmlOutput("page"))

# ui: invalid IP ---------------------------------------------------------------
ui_invalid_ip <- function(){
  tagList(
    div(id = "ipauthorization",
        h1(""),
    ),
    
    tags$style(type="text/css", "#login {font-size:12px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

# ui: password landing page ----------------------------------------------------
ui_password <- function(){
  tagList(
    div(id = "login",
        #h3(TESTING_CAPTION),
        div(
          style = "max-width: 300px; word-wrap: break-word;",
          HTML("<strong>NOTE: This is a test eCrash system and contains fake data for illustrating the functionality of the system. <span style='color:red'>Do not enter any real data into this system.</span></strong>"),
        ),
        br(),
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),
                  actionButton("Login", "Log in")),
        HTML("<strong><u>Username and passwords</strong></u>"),
        br(),
        div(
          style = "max-width: 300px; word-wrap: break-word;",
          HTML("Username and passwords for different roles. <strong>hq_officer</strong> is used for an officer at headquarters, and has roles for reviewing and approving reports; <strong>police_officer</strong> is used for police officers that fill out reports."),
        ),
        br(),
        HTML("<strong>username:</strong> hq_officer, <strong>password:</strong> roadsafety"),
        br(),
        HTML("<strong>username:</strong> police_officer, <strong>password:</strong> roadsafety"),
        br(),
        br(),
        HTML(paste("<strong><small>eCrash System - Version ", VERSION_NUMBER, "</small></strong>")),
        br(),
        br(),
        htmlOutput("passwordwarning_ui")
        #actionButton("Login_Test", "Test System")
    ),
    
    tags$style(type="text/css", "#login {font-size:12px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui_password2 <- div(
  
  wellPanel(textInput("userName", "Username"),
            passwordInput("passwd", "Password"),
            br(),
            actionButton("Login", "Log in")),
  actionButton("Login_Test", "Test System")
  
)

# ui main page -----------------------------------------------------------------
main_sidebar <- div(
  sidebarMenu(
    id = "tab",
    
    menuItem("Crash Reports", tabName = "sit_report", icon = icon("file-alt"), selected = TRUE),
    menuItem("P69", tabName = "p69", icon = icon("file-alt")),
    menuItem("Summary Tables", tabName = "summary_table", icon = icon("table")),
    menuItem("Summary Figures", tabName = "summary_figs", icon = icon("chart-line")),
    menuItem("Crash Map", tabName = "summary_map", icon = icon("chart-line")),
    menuItem("Information", tabName = "information", icon = icon("question-circle"))
    
  )
)

main_body <- div(    
  # ** Top----------------------------------------------------------------------
  fluidRow(
    column(12,
           fluidRow(
             column(4,
                    #h1(textOutput("tab_title")), style = "font-family: 'BentonSans Book'; font-weight:bold;"),
                    h1(textOutput("tab_title"))),
             column(3,
                    h4(TESTING_CAPTION)),
             column(5, align = "right",
                    uiOutput("download_raw_data_ui"),
                    #downloadButton("download_raw_data", "Download Data"),
                    actionButton("sync_with_server", "Sync Data with Server"),
                    uiOutput("updatesystem_ui"),
                    HTML(paste("<strong><small>Version ", VERSION_NUMBER, "</small></strong>"))
                    #actionButton("updatesystem", "Update System")
             )
           )
    )
  ),
  fluidRow(
    column(6, uiOutput("testing_system_text")),
    column(6, uiOutput("sync_notification"))
  ),
  fluidRow(
    column(12, align = "center",
           htmlOutput("version_warning")
    )
  ),
  
  shinyjs::useShinyjs(),
  #js function to reset a button, variableName is the button name whose value we want to reset
  tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
              Shiny.onInputChange(variableName, null);
              });
              "),
  
  hr(),
  tabItems(
    
    # ** Sit Report ------------------------------------------------------------
    tabItem(tabName = "sit_report",
            fluidRow(
              column(12, align = "center",
                     actionButton("gen_new_report","Generate New Crash Report"))
            ),
            fluidRow(
              column(3,
                     wellPanel(
                       
                       fluidRow(column(12, align="center", h3("Filter Reports"))),
                       
                       fluidRow(
                         selectInput("dt_filter_injury_type", "Injury Type", 
                                     choices = c("All",
                                                 "Non-Injury",
                                                 "Slight",
                                                 "Serious",
                                                 "Fatal"),
                                     selected="All")
                         
                       ),
                       
                       fluidRow(
                         selectInput("dt_filter_station", "Station", 
                                     choices = c("All",
                                                 "Buruburu",
                                                 "Central",
                                                 "Dagoretti",
                                                 "Embakasi",
                                                 "Gigiri",
                                                 "GVAIS",
                                                 "Industrial Area",
                                                 "Karen",
                                                 "Kasarani",
                                                 "Kayole",
                                                 "Kilimani",
                                                 "Langata",
                                                 "Makongeni",
                                                 "Starehe"),
                                     selected="All")
                         
                       ),
                       fluidRow(
                         dateRangeInput("dt_filter_daterange", "Date Range",
                                        start = as.Date("2021-01-01"),
                                        end = Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% substring(1,10),
                                        language = "en-GB",
                                        format = "dd/mm/yyyy")
                       ),
                       fluidRow(
                         # selectInput("dt_filter_final_edit", "Finalized / To be Completed", 
                         #             choices = c("All",
                         #                         "Finalized",
                         #                         "To be Completed"),
                         #             selected="All")
                         uiOutput("dt_filter_final_edit_ui")
                       ),
                       fluidRow(
                         uiOutput("dt_filter_p41_final_edit_ui")
                       ),
                       fluidRow(
                         uiOutput("dt_filter_ar_final_edit_ui")
                       ),
                       fluidRow(
                         uiOutput("dt_filter_fu_final_edit_ui")
                       ),
                       fluidRow(
                         column(12, align = "center",
                                downloadButton("print_all_situationreports","Print All Situation Reports")
                         )
                       ),
                       br(),
                       fluidRow(
                         column(12, align = "center",
                                downloadButton("print_all_p41s","Print All P41s")
                         )
                       ),
                       fluidRow(
                         h6("Only finalized reports are printed. Amendment reports included with situation reports.")
                       )
                       #style = "background-color:#F7FDFF;"
                     )
                     
              ),
              column(9,
                     DT::dataTableOutput("table")
                     
              )
            )
            
    ),
    
    # ** P69 -----------------------------------------------------------------
    tabItem(tabName = "p69",
            fluidRow(
              column(5,  offset = 1,
                     fluidRow(
                       column(12,
                              infoBoxOutput("N_crashes_p69", width=10)
                       )
                     ),
                     fluidRow(
                       column(12, offset = 1,
                              radioButtons("p69_all_or_finalized", 
                                           "Use all reports or only completed ones?",
                                           choices = c("All Reports",
                                                       "Only Completed Reports"),
                                           inline = T,
                                           selected = "All Reports")
                       )
                     ),
                     br(),
                     fluidRow(
                       column(12, offset = 1,
                              textInput("p69_officer_name", 
                                        "Officer Submitting P69",
                                        value = "")
                       )
                     ),
                     fluidRow(
                       column(12, offset = 1,
                              selectInput("p69_station",
                                          "Station",
                                          choices = c("Nairobi County",
                                                      "Buruburu",
                                                      "Central",
                                                      "Dagoretti",
                                                      "Embakasi",
                                                      "Gigiri",
                                                      "GVAIS",
                                                      "Industrial Area",
                                                      "Karen",
                                                      "Kasarani",
                                                      "Kayole",
                                                      "Kilimani",
                                                      "Langata",
                                                      "Makongeni",
                                                      "Starehe"),
                                          selected = "Nairobi County"),
                              
                       )
                     )
                     
              ),
              
              column(4, offset = -2,
                     wellPanel(
                       #style = "height: 400px;",
                       column(12, align = "center",
                              h4("Generate P69 Report"),
                       ),
                       
                       selectInput("summary_date_type",
                                   "Summary Type",
                                   choices = c("Monthly",
                                               "Quarterly",
                                               "Annually",
                                               "Date Range"),
                                   selected = "Quarterly"),
                       uiOutput("ui_summary_year_selection"),
                       uiOutput("ui_summary_date_selection"),
                       
                       fluidRow(
                         useShinyalert(),
                         column(12, align = "center",
                                downloadButton('p69_pdf', label="Download P69 PDF")
                         ),
                         br(),
                         column(12, align = "center",
                                actionButton('email_p69_button', label="Email P69",
                                             icon = icon("envelope", lib = "font-awesome"))
                         ),
                         
                         fluidRow(
                           column(12, align = "left",
                                  h6("To download or email, enter the officer name, station and select
                                 a time period with one or more crashes.")) 
                         )
                         
                         
                         
                       )
                       
                     )
              )
            )
    ),
    
    # ** Summary Figures -----------------------------------------------------
    tabItem(tabName = "summary_table",
            sum_table_UI("tab1")
    ),
    
    # ** Summary Figures -----------------------------------------------------
    tabItem(tabName = "summary_figs",
            sum_figures_UI("fig1")
    ),
    
    # ** Summary Figures -----------------------------------------------------
    tabItem(tabName = "summary_map",
            
            fluidRow(align = "center",
                     dateRangeInput("map_cntrl_dates", 
                                    "Choose Date Range",
                                    start = as.Date("2020-01-01"),
                                    end = Sys.Date(),
                                    label = NULL,
                                    language = "en-GB",
                                    format = "dd/mm/yyyy")
            ),
            fluidRow(
              leafletOutput("crash_map",
                            height = 700)
            )
            
    ),
    
    # ** Information -----------------------------------------------------------
    tabItem(tabName = "information",
            
            fluidRow(
              column(12, align = "center",
                     h3("User Manual")
              )
            ),
            fluidRow(
              column(12, align = "center",
                     downloadButton("download_user_manual", "Download User Manual")
              )
            ),
            br(),
            fluidRow(
              column(8, align = "center", offset = 2,
                     
                     fluidRow(
                       column(12, align = "center", h3("Frequently Asked Questions"))
                     ),
                     
                     br(),
                     fluidRow(
                       column(12, align = "left",
                              strong("How do I upload a crash sketch?"))
                     ),
                     fluidRow(
                       column(12, align = "left",
                              "Take a photo of the crash sketch and upload the crash sketch to the computer. Once the crash sketch image is on the computer, you will then be able to upload the image into the system.")
                     ),
                     br(),
                     fluidRow(
                       column(12, align = "left",
                              strong("How do I log out of the system?"))
                     ),
                     fluidRow(
                       column(12, align = "left",
                              "To log out, just close the web browser. Doing so will automatically log you out.")
                     )
                     
                     
                     
              )
            )
            
            
    )
    
  )
)

# Render Page -------------
logo_blue_gradient <- shinyDashboardLogoDIY(
  
  boldText = "Nairobi Police"
  ,mainText = paste0("eCrash", SYSTEM_SHORT_NAME, " System")
  ,textSize = 16
  ,badgeText = "BETA"
  ,badgeTextColor = "black"
  ,badgeTextSize = 2
  ,badgeBackColor = "lightcyan"
  ,badgeBorderRadius = 3
  
)


ui <- dashboardPage(
  
  dashboardHeader(title = logo_blue_gradient,
                  titleWidth = 350
                  # dropdownMenuOutput("messageMenu")
  ),
  dashboardSidebar(uiOutput("ui_main_sidebar")),
  dashboardBody(  
    
    shinyDashboardThemes(
      theme = "grey_light" # https://github.com/nik01010/dashboardthemes
    ),
    uiOutput("ui_main_body")
  )
  
)

# SERVER ************************************* ---------------------------------


server = (function(input, output, session) {
  
  # Loggin In ------------------------------------------------------------------
  login = FALSE
  USER <- reactiveValues(Logged = Logged)
  
  ## Check IP Address
  VALID_IP_ADDRESS <- reactiveValues(valid_ip = FALSE)
  
  ALLOWED_IP_ADDRESSES_RX <- paste0("^", ALLOWED_IP_ADDRESSES) %>% paste(collapse = "|")
  if(ONLINE_SYSTEM %in% FALSE){
    VALID_IP_ADDRESS$valid_ip <- TRUE # If using non-online system, say has valid IP
  } else{
    VALID_IP_ADDRESS$valid_ip <- grepl(ALLOWED_IP_ADDRESSES_RX, session$request$HTTP_X_FORWARDED_FOR)
  }
  
  VALID_IP_ADDRESS$valid_ip <- TRUE # REMOVE!!!
  
  ## ** If clicks "Test System" ------------------------------------------------
  observe({
    if(USER$Logged == FALSE){
      if(!is.null(input$Login_Test)){
        if (input$Login_Test > 0) {
          
          USER$Logged <- TRUE
          TESTING_SYSTEM <<- TRUE
          USER_ROLE <<- "hq"
          
          #### Generate blank test data file if needed 
          if(!dir.exists("data")) dir.create("data")
          if(!file.exists(file.path("data", "police_data_test.Rds"))){
            df_null <- data.frame(matrix(nrow=0, ncol=24))
            names(df_null) <- c("uid", "report_type", "accident_type", "amend_report_id", "followup_report_id", "last_updated", "accident_date", "accident_time_hour", "accident_time_minute", "ob_no", "iar_no", "police_division", "stage_p41_clean", "stage_sr_clean", "stage", "stage_p41", "stage_ar", "stage_ar_clean", "stage_fu", "stage_fu_num", "stage_fu_clean", "stage_sr_num", "stage_ar_num", "stage_p41_num")
            
            saveRDS(df_null, file.path("data", "police_data_test.Rds"), version=2)
          } 
          
          #### Load Data
          if(TESTING_SYSTEM){
            
            dataset <- readRDS(file.path("data", "police_data_test.Rds")) %>%
              dplyr::arrange(desc(last_updated)) %>%
              filter(!(uid %in% uids_delete))
            
          } else{
            dataset <- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data.Rds")), key = data_key)) %>%
              dplyr::arrange(desc(last_updated)) %>%
              filter(!(uid %in% uids_delete))
          }
          
          #### If no observations, add column name
          if(nrow(dataset) %in% 0){
            df_null <- data.frame(matrix(nrow=0, ncol=24))
            names(df_null) <- c("uid", "report_type", "accident_type", "amend_report_id", "followup_report_id", "last_updated", "accident_date", "accident_time_hour", "accident_time_minute", "ob_no", "iar_no", "police_division", "stage_p41_clean", "stage_sr_clean", "stage", "stage_p41", "stage_ar", "stage_ar_clean", "stage_fu", "stage_fu_num", "stage_fu_clean", "stage_sr_num", "stage_ar_num", "stage_p41_num")
            
          }
          
          dataset <<- dataset
          
        }
      }
    }
  })
  
  ## ** If clicks "Login" ------------------------------------------------------
  observe({ 
    # If not logged in, go to password landing page
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          
          TESTING_SYSTEM <<- FALSE
          update_figures_r$value <<- 1
          update_map_r$value <<- 1
          update_p69_r$value <<- 1
          update_summary_table_r$value <<- 1
          
          
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          
          ## Global Username parameter
          Username <<- Username
          
          INCORRECT_PASSWORD <<- TRUE
          
          if(Username %in% passwords_df$username){
            passwords_df_i <- passwords_df[passwords_df$username %in% Username,]
            USER_ROLE <<- passwords_df$role[passwords_df$username %in% Username]
            
            if(checkpw(Password, passwords_df_i$password_hashed) %in% TRUE){
              
              INCORRECT_PASSWORD <<- FALSE
              
              USER$Logged <- TRUE
              
              # ** If username and password match, load data - - - - - - - - - -
              #### Grab key
              user_key <- sha256(charToRaw(Password))
              data_encrypt_key <- unserialize(aes_cbc_decrypt(readRDS(file.path("keys_passwords",
                                                                                "secured",
                                                                                "users",
                                                                                paste0(Username, ".Rds"))), 
                                                              key = user_key)) 
              
              data_key <<- sha256(charToRaw(data_encrypt_key))
              
              #### Load data
              # If none exists, create blank dataframe
              if(!file.exists(file.path("data", "police_data.Rds"))){
                df_null <- data.frame(matrix(nrow=0, ncol=24))
                names(df_null) <- c("uid", "report_type", "accident_type", "amend_report_id", "followup_report_id", "last_updated", "accident_date", "accident_time_hour", "accident_time_minute", "ob_no", "iar_no", "police_division", "stage_p41_clean", "stage_sr_clean", "stage", "stage_p41", "stage_ar", "stage_ar_clean", "stage_fu", "stage_fu_num", "stage_fu_clean", "stage_sr_num", "stage_ar_num", "stage_p41_num")
                
                #saveRDS(df_null, file.path("data", "police_data_test.Rds"), version=2)
                saveRDS(aes_cbc_encrypt(serialize(df_null, NULL), key = data_key), file.path("data", "police_data.Rds"), version=2)
              } 
              
              if(!file.exists(file.path("data", "police_data_test.Rds"))){
                df_null <- data.frame(matrix(nrow=0, ncol=24))
                names(df_null) <- c("uid", "report_type", "accident_type", "amend_report_id", "followup_report_id", "last_updated", "accident_date", "accident_time_hour", "accident_time_minute", "ob_no", "iar_no", "police_division", "stage_p41_clean", "stage_sr_clean", "stage", "stage_p41", "stage_ar", "stage_ar_clean", "stage_fu", "stage_fu_num", "stage_fu_clean", "stage_sr_num", "stage_ar_num", "stage_p41_num")
                
                saveRDS(df_null, file.path("data", "police_data_test.Rds"), version=2)
              } 
              
              
              
              if(TESTING_SYSTEM){
                dataset <- readRDS(file.path("data", "police_data_test.Rds")) %>%
                  dplyr::arrange(desc(last_updated)) %>%
                  filter(!(uid %in% uids_delete))
              } else{
                dataset <- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data.Rds")), key = data_key)) %>%
                  dplyr::arrange(desc(last_updated)) %>%
                  filter(!(uid %in% uids_delete))
              }
              
              if(nrow(dataset) %in% 0){
                df_null <- data.frame(matrix(nrow=0, ncol=24))
                names(df_null) <- c("uid", "report_type", "accident_type", "amend_report_id", "followup_report_id", "last_updated", "accident_date", "accident_time_hour", "accident_time_minute", "ob_no", "iar_no", "police_division", "stage_p41_clean", "stage_sr_clean", "stage", "stage_p41", "stage_ar", "stage_ar_clean", "stage_fu", "stage_fu_num", "stage_fu_clean", "stage_sr_num", "stage_ar_num", "stage_p41_num")
              }
              
              # Add variables added later if needed
              if(is.null(dataset$stage_fu)) dataset$stage_fu <- NA
              if(is.null(dataset$stage_fu_num)) dataset$stage_fu_num <- NA
              if(is.null(dataset$stage_fu_clean)) dataset$stage_fu_clean <- NA
              
              dataset <<- dataset
              
              #### API Keys
              api_keys <- unserialize(aes_cbc_decrypt(readRDS(file.path("keys_passwords", "secured", "api_keys.Rds")), key = data_key))
              
              Sys.setenv("AWS_ACCESS_KEY_ID" = api_keys$Key[(api_keys$Service %in% "AWS_ACCESS_KEY_ID")],
                         "AWS_SECRET_ACCESS_KEY" = api_keys$Key[(api_keys$Service %in% "AWS_SECRET_ACCESS_KEY")],
                         "AWS_DEFAULT_REGION" = "us-east-2")
              
            }
          }
          
          
          # ** Incorrect Password Warning ----------------------------------------------
          output$passwordwarning_ui <- renderText({
            
            if(INCORRECT_PASSWORD){
              out <- HTML("<h3><span style='color:red'>Incorrect Username or Password</span></h3>")
            } else{
              out <- ""
            }
            
            out
          })
          
        } 
      }
    }    
  })
  
  # ** Going to Main Page [Loggging in --> Main page] --------------------------
  observeEvent(USER$Logged, {
    
    #### Add edit button
    dataset <- add_edit_button(dataset)
    dataset <- mk_stage_sr_clean_var(dataset)
    dataset <- mk_stage_p41_clean_var(dataset)
    dataset <- mk_stage_ar_clean_var(dataset)
    dataset <- mk_stage_fu_clean_var(dataset)
    
    #### Define reactive value of full dataset
    # Needed to grab specific rows later on
    vals <- reactiveValues()
    vals$Data <- data.table(dataset)
    
    #### All data (sit report, amend, P41)
    all_data_r <- reactive({
      filter_dataset(dataset, input)
    })
    
    #### Make data table - Sit Report
    filterData <- reactive({
      filter_dataset(dataset[dataset$report_type %in% "sit_report",], 
                     input)
    })
    
    output$table = DT::renderDataTable({
      DT = filterData()
      
      DT <- DT[,c("accident_date", "accident_time_hour",	"accident_time_minute", "accident_type", "ob_no", "police_division", "stage_sr_clean", "stage_ar_clean", "stage_p41_clean", "Edit")] %>%
        dplyr::mutate(accident_time_minute = case_when(nchar(accident_time_minute) %in% 1 ~ paste0(0, accident_time_minute),
                                                       TRUE ~ as.character(accident_time_minute)),
                      accident_date = accident_date %>% as.Date() %>% format('%d/%m/%Y'),
                      accident_time = paste0(accident_time_hour, ":", accident_time_minute) %>%
                        str_replace_all("NA:NA", "")) %>%
        dplyr::select(-c(accident_time_hour, accident_time_minute)) %>%
        dplyr::select(accident_date, accident_time, everything()) %>%
        dplyr::rename("Accident Date" = accident_date,
                      "Accident Time" = accident_time,
                      "Injury Type" = accident_type,
                      "O.B. Num" = ob_no,
                      "Sit Report" = stage_sr_clean,
                      "P41" = stage_p41_clean,
                      "Amend Report" = stage_ar_clean,
                      "Police Station" = police_division,
                      "Crash Report" = Edit) 
      
      datatable(DT, escape = F, selection="none",
                options = list(scrollX = T))
      
    })
    
    
    
    #### Make data table - P41
    p41_data_r <- reactive({
      filter_dataset(dataset[dataset$report_type %in% "p41",] %>%
                       add_edit_button("stage_p41"), 
                     input)
    })
    
    output$table_p41 = DT::renderDataTable({
      DT = p41_data_r()
      
      DT <- DT[,c("last_updated", "accident_date", "ob_no", "iar_no", "police_division", "Edit")] %>%
        dplyr::rename("Last Updated" = last_updated,
                      "Accident Date" = accident_date,
                      "O.B. Num" = ob_no,
                      "IAR Num" = iar_no,
                      "Police Station" = police_division)
      
      datatable(DT, escape = F, selection="none",
                options = list(scrollX = T))
    })
    
    # Allow values to work outside of this environment
    filterData <<- filterData # need to do so works in downloadHandler
    p41_data_r <<- p41_data_r # need to do so works in downloadHandler
    all_data_r <<- all_data_r # need to do so works in downloadHandler
    vals <<- vals
    
  })
  
  # ** Initially Selected Sit Report -------------------------------------------
  observeEvent(USER$Logged == TRUE, {
    updateTabItems(session, "tab", selected = "sit_report")
  })
  
  # Render Main Page -----------------------------------------------------------
  output$ui_main_body <- renderUI(
    
    if((VALID_IP_ADDRESS$valid_ip %in% TRUE)){
      if (USER$Logged == TRUE){ 
        main_body
      } else{
        div(class="outer",do.call(bootstrapPage,c("",ui_password())))
      }
    } else{
      div(class="outer",do.call(bootstrapPage,c("",ui_invalid_ip())))
    }
    
  )
  
  output$ui_main_sidebar <- renderUI(
    if (USER$Logged == TRUE ){ 
      main_sidebar
    }
  )
  
  # Render ---------------------------------------------------------------------
  
  # ** Title -------------------------------------------------------------------
  output$tab_title <- renderText({
    req(input$tab)
    if(input$tab %in% "sit_report"){
      txt <- "Crash Reports"
    } else if (input$tab %in% "p41"){
      txt <- "P41"
    } else if (input$tab %in% "p69"){
      txt <- "P69"
    } else if (input$tab %in% "summary_table"){
      txt <- "Summary Tables"
    } else if (input$tab %in% "summary_figs"){
      txt <- "Summary Figures"
    } else if (input$tab %in% "summary_map"){
      txt <- "Crash Map"
    } else if (input$tab %in% "information"){
      txt <- "Information"
    }
    
  })
  
  # ** Notification Message ----------------------------------------------------
  
  output$messageMenu <- renderMenu({
    req(USER$Logged)
    
    d_p41 <- dataset[dataset$report_type %in% "p41",]
    d_sr <- dataset[dataset$report_type %in% "sit_report",]
    
    n_sr_review <- nrow(d_sr[d_sr$stage %in% c("submitted_to_hq",
                                               "hq_provided_feedback"),])
    
    n_p41_review <- nrow(d_p41[d_p41$stage_p41 %in% c("submitted_to_hq",
                                                      "hq_provided_feedback"),])
    
    dropdownMenu(type = "messages",
                 notificationItem(
                   text = paste(n_sr_review, "Situation Reports to review"),
                   icon("file-alt"),
                   status = "warning"
                 ),
                 notificationItem(
                   text = paste(n_p41_review, "P41s to review"),
                   icon = icon("file-alt"),
                   status = "warning"
                 )
    )
    
  })
  
  # ** Testing System Text -----------------------------------------------------
  output$testing_system_text <- renderText({
    if(TESTING_SYSTEM){
      "<b><font color='red'>TESTING THE SYSTEM ONLY:</font> Do not enter any sensitive information.</b>"
    } else{
      ""
    }
  })
  
  # ** Filter Table Type Choices - SR ------------------------------------------
  output$dt_filter_final_edit_ui <- renderUI({
    
    # admin_stages <- c("To Review",
    #                   "Finalized")
    # 
    # officer_stages <- c("To Edit",
    #                     "Incorporate Feedback from HQ",
    #                     "Finalized")
    
    all_stages <- c("Edit",
                    "Pending HQ approval",
                    "Pending officer revision (HQ feedback)",
                    "Completed")
    
    out <- selectInput("dt_filter_final_edit", "Situation Report Stage", 
                       choices = c("All", all_stages),
                       selected="All")
    
    out 
    
  })
  
  # ** Filter Table Type Choices - P41 -----------------------------------------
  
  output$dt_filter_p41_final_edit_ui <- renderUI({
    
    # admin_stages <- c("To Review",
    #                   "Finalized")
    # 
    # officer_stages <- c("To Edit",
    #                     "Incorporate Feedback from HQ",
    #                     "Finalized")
    
    all_stages <- c("Not Started",
                    "Edit",
                    "Pending HQ approval",
                    "Pending officer revision (HQ feedback)",
                    "Completed")
    
    out <- selectInput("dt_filter_p41_final_edit", "P41 Stage", 
                       choices = c("All", all_stages),
                       selected="All")
    
    out 
    
  })
  
  output$dt_filter_ar_final_edit_ui <- renderUI({
    
    # admin_stages <- c("To Review",
    #                   "Finalized")
    # 
    # officer_stages <- c("To Edit",
    #                     "Incorporate Feedback from HQ",
    #                     "Finalized")
    
    all_stages <- c("Not Started",
                    "Edit",
                    "Pending HQ approval",
                    "Pending officer revision (HQ feedback)",
                    "Completed")
    
    out <- selectInput("dt_filter_ar_final_edit", "Amend Report Stage", 
                       choices = c("All", all_stages),
                       selected="All")
    
    out 
    
  })
  
  output$dt_filter_fu_final_edit_ui <- renderUI({
    
    # admin_stages <- c("To Review",
    #                   "Finalized")
    # 
    # officer_stages <- c("To Edit",
    #                     "Incorporate Feedback from HQ",
    #                     "Finalized")
    
    all_stages <- c("Not Started",
                    "Edit",
                    "Pending HQ approval",
                    "Pending officer revision (HQ feedback)",
                    "Completed")
    
    out <- selectInput("dt_filter_fu_final_edit", "Follow Up Report Stage", 
                       choices = c("All", all_stages),
                       selected="All")
    
    out 
    
  })
  
  # ** Year Selection [P69] ----------------------------------------------------
  output$ui_summary_year_selection <- renderUI({
    # For Monthly and Quarterly need to first select year
    
    out <- ""
    
    if(!is.null(input$summary_date_type)){
      if(input$summary_date_type %in% c("Annually", "Monthly", "Quarterly")){
        out <- selectInput("summary_year_selection",
                           "Choose Year",
                           choices = c("2010",
                                       "2011",
                                       "2012",
                                       "2013",
                                       "2014",
                                       "2015",
                                       "2016",
                                       "2017",
                                       "2018",
                                       "2019",
                                       "2020",
                                       "2021",
                                       "2022",
                                       "2023",
                                       "2024",
                                       "2025"),
                           selected = "2025")
      }
    }
    
    out
    
  })
  
  # ** Date Selection [P69] ----------------------------------------------------
  output$ui_summary_date_selection <- renderUI({
    
    #### Default
    out <- ""
    
    #### Dynamically Select
    if(!is.null(input$summary_date_type)){
      
      #### Quarterly
      if(input$summary_date_type %in% "Quarterly"){
        out <- selectInput("summary_date_selection",
                           "Choose Quarter",
                           choices = c("January - March",
                                       "April - June",
                                       "July - September",
                                       "October - December"),
                           selected = "January - March")
      }
      
      #### Monthly
      if(input$summary_date_type %in% "Monthly"){
        out <- selectInput("summary_date_selection",
                           "Choose Month",
                           choices = c("January",
                                       "February",
                                       "March",
                                       "April",
                                       "May",
                                       "June",
                                       "July",
                                       "August",
                                       "September",
                                       "October",
                                       "November",
                                       "December"))
      }
      
      #### Date Range
      if(input$summary_date_type %in% "Date Range"){
        out <- dateRangeInput("summary_date_selection", 
                              "Choose Date Range",
                              start = as.Date("2020-01-01"),
                              end = Sys.Date(),
                              language = "en-GB",
                              format = "dd/mm/yyyy")
      }
      
      
    }
    
    out
    
  })
  
  # ** Crash Map ---------------------------------------------------------------
  observe({
    
    tmp <- update_map_r$value
    update_map_r$value <<- NULL
    
    output$crash_map <- renderLeaflet({
      
      # Load Data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(TESTING_SYSTEM){
        data_map <- readRDS(file.path("data", "police_data_test.Rds")) %>%
          dplyr::arrange(desc(last_updated)) %>%
          distinct(uid, .keep_all = T)
      } else{
        data_map <- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data.Rds")), key = data_key)) %>%
          dplyr::arrange(desc(last_updated)) %>%
          distinct(uid, .keep_all = T)
      }
      
      if(nrow(data_map) > 0){
        if(!is.null(data_map$accident_date)){
          data_map <- data_map[data_map$accident_date >= input$map_cntrl_dates[1],]
          data_map <- data_map[data_map$accident_date <= input$map_cntrl_dates[2],]
        }
      }
      
      if(nrow(data_map) > 0){
        
        data_map_list <- prep_data_for_figures(data_map)
        
        N_injuries_df <- data_map_list$victim_df %>%
          dplyr::group_by(uid) %>%
          dplyr::summarise(N_injuries = n())
        
        data_map <- data_map_list$dataset
        
        data_map <- data_map %>% 
          left_join(N_injuries_df, by = "uid") %>%
          dplyr::mutate(N_injuries = N_injuries %>% replace_na(0))
        
      } else{
        data_map <- data.frame(NULL) %>%
          mutate(crash_long = "",
                 crash_lat = "",
                 accident_date = "",
                 accident_time_hour = "",
                 accident_time_minute = "",
                 accident_type = "",
                 police_division = "",
                 worst_injury_color = "",
                 N_injuries = "")
      }
      
      
      #internet_connection <- curl::has_internet()
      
      if(ONLINE_SYSTEM){
        l <- leaflet() %>%
          addTiles() %>% 
          setView(lng = 36.817, lat = -1.29, zoom=11) 
      } else{
        addResourcePath("mytiles", "basemap")
        
        l <- leaflet() %>%
          addTiles(urlTemplate = "/mytiles/{z}_{x}_{y}.png") %>% 
          setView(lng = 36.817, lat = -1.29, zoom=11) 
      }
      
      data_map <- data_map %>%
        mutate(crash_long = crash_long %>% as.numeric(),
               crash_lat = crash_lat %>% as.numeric()) %>%
        filter(!is.na(crash_long)) 
      
      #### Leaflet
      time_padded_0s <- case_when(data_map$accident_time_minute %in% as.character(0:9) ~ paste0("0", data_map$accident_time_minute),
                                  TRUE ~ data_map$accident_time_minute)
      
      data_map <- data_map %>%
        mutate(leaflet_popup = paste0("<b>Police Station:</b> ", police_division %>% toTitleCase(), "<br/>",
                                      "<b>Crash Date:</b> ", accident_date %>% as.Date() %>% format('%d/%m/%Y'), "<br/>",
                                      "<b>Crash Time:</b> ", data_map$accident_time_hour, ":", time_padded_0s, "<br/>",
                                      "<b>Injury Type:</b> ", data_map$accident_type, "<br/>",
                                      "<b>Number of Victims:</b> ", data_map$N_injuries
        ))
      
      if(nrow(data_map) > 0){
        l <- l %>% 
          addCircles(data=data_map, 
                     lng=~crash_long,
                     lat=~crash_lat,
                     color=~worst_injury_color,
                     #color="red",
                     opacity=1,
                     fillOpacity = 1,
                     weight = 10,
                     label = ~ lapply(leaflet_popup, htmltools::HTML),
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal",
                                    padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto")) 
      }
      
      l %>%
        addLegend(position = "topright",
                  colors = c("#f24224", "#264b96", "#27b376", "black"),
                  labels = c("Fatal", "Serious", "Slight", "Non-Injury"),
                  opacity = 1)
      
      
    })
  })
  
  # ** Call Figures Module -----------------------------------------------------
  callModule(sum_figures_Server, "fig1")
  callModule(sum_table_Server, "tab1")
  
  # Other ObserveEvents --------------------------------------------------------
  
  # ** Generate New Report ---------------------------------------------------
  observeEvent(input$gen_new_report, {
    
    data_i <- data.frame(matrix(nrow=1, ncol=0))
    
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    update_figures_r$value <<- NULL
    update_map_r$value <<- NULL
    update_p69_r$value <<- NULL
    update_summary_table_r$value <<- NULL
    
    callModule(newreport_Server, id_temp, data_i, dataset, dataset_variables, sr_variables, tab_types)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          newreport_UI(id_temp)
        )
      )
    })
    
    #### Make uid
    ## make username numeric
    # usr_num <- Username %>% LETTER2num %>% paste(collapse = "")
    # 
    # uid <- Sys.time() %>% 
    #   as.numeric %>% 
    #   substring(1,16) %>% 
    #   as.character() %>% 
    #   str_replace_all("[[:punct:]]", "") %>% 
    #   paste0(usr_num) %>%
    #   paste0(sample(c(1:999999), size=1))
    # uid <- paste0("uid", uid)
    # data_i$uid <- uid
    # data_i$stage <- "editing"
    # data_i$stage_p41 <- "none"
    # data_i$stage_ar <- "none"
    # data_i$stage_sr_num <- "1"
    # data_i$stage_p41_num <- "1"
    # data_i$stage_ar_num <- "1"
    # data_i$stage_fu_num <- "1"
    # data_i$report_type <- "sit_report"
    # data_i$iar_no <- ""
    # data_i$police_division <- "."
    # data_i$amend_report_id <- NA
    # data_i$p41_id <- NA
    # for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
    
    
    #### Update Sr Inputs
    # Problem before where (1) generate SR, (2) generage a NEW report
    # (3) click save, then PREVIOUS SR report was saved to new report,
    # but not other variables. Consequently, when generate a new report
    # update SR variables to blank.
    #for(var in sr_variables) updateTextInput(session, var, value = "")
    
    # if(data_i$stage[1] %in% "editing"){
    #   tab_types <- c("Enter Data", "Data Checks", "Manually Edit", "Submit to HQ")
    # } else if (data_i$stage[1] %in% "submitted_to_hq"){
    #   tab_types <- c("Review Situation Report")
    # } else if (data_i$stage[1] %in% "hq_provided_feedback"){
    #   tab_types <- c("Review Feedback from HQ", "Enter Data", "Data Checks", "Manually Edit", "Submit to HQ")
    # } else if (data_i$stage[1] %in% " "){
    #   tab_types <- c("Finalized")
    # }
    
    
    
    
    
    # Grab relevant amendment reports for viewing
    # ar_data <- data_all[data_all$uid %in% data_i$uid,]
    # ar_data <- ar_data[ar_data$report_type %in% "amendment",]
    # 
    # # Call UI/Server
    # callModule(sitreport_finalized_Server, 
    #            id_temp, 
    #            data_i, dataset, dataset_variables, sr_variables, ar_data)
    # 
    # output$ui_main_body <- renderUI({
    #   div(
    #     fluidPage(
    #       sitreport_finalized_UI(id_temp)
    #     )
    #   )
    # })
    
    
    
    
  })
  
  # ** Create New Report --> Go to Report Landing Page ----------------------------
  observeEvent(input$create_report_button, {
    
    data_i <- data.frame(matrix(nrow=1, ncol=0))
    
    ## Add O.B. Numbers
    data_i$ob_no_1 <- ob_no_1 
    data_i$ob_no_2 <- ob_no_2 
    data_i$ob_no_3 <- ob_no_3 
    data_i$ob_no_4 <- ob_no_4 
    data_i$ob_no <- ob_no 
    
    ## Add other data needed for new report
    usr_num <- Username %>% LETTER2num %>% paste(collapse = "")
    
    uid <- Sys.time() %>% 
      as.numeric %>% 
      substring(1,16) %>% 
      as.character() %>% 
      str_replace_all("[[:punct:]]", "") %>% 
      paste0(usr_num) %>%
      paste0(sample(c(1:999999), size=1))
    uid <- paste0("uid", uid)
    data_i$uid <- uid
    data_i$stage <- "editing"
    data_i$stage_p41 <- "none"
    data_i$stage_ar <- "none"
    data_i$stage_fu <- "none"
    data_i$stage_sr_num <- "1"
    data_i$stage_p41_num <- "1"
    data_i$stage_ar_num <- "1"
    data_i$stage_fu_num <- "1"
    data_i$report_type <- "sit_report"
    data_i$iar_no <- ""
    data_i$police_division <- "."
    data_i$accident_type <- "."
    data_i$amend_report_id <- NA
    data_i$followup_report_id <- NA
    data_i$p41_id <- NA
    data_i$accident_time_hour <- NA
    data_i$accident_time_minute <- NA
    for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
    
    if(Username %in% "embakasi") data_i$police_division <- "Embakasi"   
    if(Username %in% "langata") data_i$police_division <- "Langata"  
    
    #data_i$report_type <- "sit_report"
    data_i$accident_date          <- as.Date(NA) #Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.Date() %>% as.character
    data_i$accident_reported_date <- as.Date(NA) #Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.Date() %>% as.character
    
    ## Add last updated
    data_i$last_updated <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character
    
    for(var in names(data_i))  data_i[[var]]  <- data_i[[var]] %>% as.character()
    for(var in names(dataset)) dataset[[var]] <- dataset[[var]] %>% as.character()
    
    ## Add to full dataset
    if(nrow(dataset) > 0){
      
      dataset <- bind_rows(dataset, data_i) %>%
        arrange(desc(last_updated)) %>%
        distinct(uid, report_type, amend_report_id, .keep_all = TRUE)
    } else{
      dataset <- data_i
      
      # Give all variables that need
      
      #v[!(v %in% names(aaaa))]
      
      #names(dataset) <- c("uid", "last_updated", "accident_date", "ob_no", "iar_no", "police_division", "stage_p41_clean", "stage_sr_clean", "stage", "stage_p41", "stage_ar", "stage_ar_clean", "stage_sr_num", "stage_ar_num", "stage_p41_num")
      #aaaa <<- dataset
    }
    
    ## Save
    dataset <- as.data.frame(dataset)
    dataset <<- dataset
    
    if(TESTING_SYSTEM){
      saveRDS(dataset, file.path("data", "police_data_test.Rds"), version=2)
    } else{
      saveRDS(aes_cbc_encrypt(serialize(dataset, NULL), key = data_key), file.path("data", "police_data.Rds"), version=2)
    }
    
    ## Update All data 
    all_data_r <- reactive({
      dataset
    })
    all_data_r <<- all_data_r
    
    ## NEW ---
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    ar_data <- data.frame(NULL)
    fu_data <- data.frame(NULL)
    
    # Call UI/Server
    callModule(sitreport_finalized_Server,
               id_temp,
               data_i, dataset, dataset_variables, sr_variables, ar_data, fu_data)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          sitreport_finalized_UI(id_temp)
        )
      )
    })
    
  })
  
  
  
  # * Generate New Report 
  # observeEvent(input$go_to_data_entry, {
  #   
  #   data_i <- data.frame(matrix(nrow=1, ncol=0))
  #   
  #   #### Make uid
  #   ## make username numeric
  #   usr_num <- Username %>% LETTER2num %>% paste(collapse = "")
  #   
  #   uid <- Sys.time() %>% 
  #     as.numeric %>% 
  #     substring(1,16) %>% 
  #     as.character() %>% 
  #     str_replace_all("[[:punct:]]", "") %>% 
  #     paste0(usr_num) %>%
  #     paste0(sample(c(1:999999), size=1))
  #   uid <- paste0("uid", uid)
  #   data_i$uid <- uid
  #   data_i$stage <- "editing"
  #   data_i$stage_sr_num <- 1
  #   data_i$stage_p41_num <- 1
  #   data_i$stage_ar_num <- 1
  #   data_i$report_type <- "sit_report"
  #   data_i$amend_report_id <- NA
  #   data_i$p41_id <- NA
  #   
  #   #data_i <- data_i
  #   #data_i$temp <- 1
  #   
  #   #### Update Sr Inputs
  #   # Problem before where (1) generate SR, (2) generage a NEW report
  #   # (3) click save, then PREVIOUS SR report was saved to new report,
  #   # but not other variables. Consequently, when generate a new report
  #   # update SR variables to blank.
  #   #for(var in sr_variables) updateTextInput(session, var, value = "")
  #   
  #   if(data_i$stage[1] %in% "editing"){
  #     tab_types <- c("Enter Data", "Data Checks", "Manually Edit", "Submit to HQ")
  #   } else if (data_i$stage[1] %in% "submitted_to_hq"){
  #     tab_types <- c("Review Situation Report")
  #   } else if (data_i$stage[1] %in% "hq_provided_feedback"){
  #     tab_types <- c("Review Feedback from HQ", "Enter Data", "Data Checks", "Manually Edit", "Submit to HQ")
  #   } else if (data_i$stage[1] %in% " "){
  #     tab_types <- c("Finalized")
  #   }
  #   
  #   update_figures_r$value <<- NULL
  #   update_map_r$value <<- NULL
  #   update_p69_r$value <<- NULL
  #   update_summary_table_r$value <<- NULL
  #   
  #   id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
  #   
  #   callModule(sitreport_Server, id_temp, data_i, dataset, dataset_variables, sr_variables, tab_types)
  #   
  #   output$ui_main_body <- renderUI({
  #     div(
  #       fluidPage(
  #         sitreport_UI(id_temp)
  #       )
  #     )
  #   })
  #   
  #   #output$page <- renderUI({
  #   #  div(ui_dataentry)
  #   #})
  #   
  #   ##Reset the select_button
  #   # session$sendCustomMessage(type = 'resetInputValue', message =  "select_button")
  # })
  
  # ** Go to specific entry & pre-fill [sit report] ----------------------------
  observeEvent(input$lastClick, {
    
    new_report <- FALSE
    selectedRow <- as.numeric(strsplit(input$lastClick, "_")[[1]][2])
    data_i <- vals$Data[selectedRow,]
    data_i <- data_i %>% as.data.frame()
    
    if(data_i$stage[1] %in% "editing"){
      tab_types <- c("Enter Data", "Data Checks", "Manually Edit", "Submit to HQ")
    } else if (data_i$stage[1] %in% "submitted_to_hq"){
      tab_types <- c("Review Situation Report")
    } else if (data_i$stage[1] %in% "hq_provided_feedback"){
      tab_types <- c("Review Feedback from HQ", "Enter Data", "Data Checks", "Manually Edit", "Submit to HQ")
    } else if (data_i$stage[1] %in% "finalized"){
      tab_types <- c("Finalized")
    }
    
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    # Reload all data
    if(TESTING_SYSTEM){
      data_all <<- readRDS(file.path("data", "police_data_test.Rds")) %>%
        dplyr::arrange(desc(last_updated)) %>%
        filter(!(uid %in% uids_delete))
    } else{
      data_all <<- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data.Rds")), key = data_key)) %>%
        dplyr::arrange(desc(last_updated)) %>%
        filter(!(uid %in% uids_delete))
    }
    
    # Grab relevant amendment reports for viewing
    ar_data <- data_all[data_all$uid %in% data_i$uid,]
    ar_data <- ar_data[ar_data$report_type %in% "amendment",]
    
    # Grab relevant follow up reports for viewing
    fu_data <- data_all[data_all$uid %in% data_i$uid,]
    fu_data <- fu_data[fu_data$report_type %in% "followup",]
    
    # Call UI/Server
    callModule(sitreport_finalized_Server, 
               id_temp, 
               data_i, dataset, dataset_variables, sr_variables, ar_data, fu_data)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          sitreport_finalized_UI(id_temp)
        )
      )
    })
    
    ## Reset the select_button
    session$sendCustomMessage(type = 'resetInputValue', message =  "lastClick")
    
  })
  
  
  
  # ** Back to Finalized Sit Report --------------------------------------------
  submit_amendment_r <<- reactiveValues(value = NULL)
  
  observeEvent(submit_amendment_r$value, {
    
    # Reload all data
    if(TESTING_SYSTEM){
      data_all <<- readRDS(file.path("data", "police_data_test.Rds")) %>%
        dplyr::arrange(desc(last_updated)) %>%
        filter(!(uid %in% uids_delete))
    } else{
      data_all <<- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data.Rds")), key = data_key)) %>%
        dplyr::arrange(desc(last_updated)) %>%
        filter(!(uid %in% uids_delete))
    }
    
    # Grab relevant amendment reports for viewing
    ar_data <- data_all[data_all$uid %in% data_i$uid,]
    ar_data <- ar_data[ar_data$report_type %in% "amendment",]
    
    fu_data <- data_all[data_all$uid %in% data_i$uid,]
    fu_data <- fu_data[fu_data$report_type %in% "followup",]
    
    
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    callModule(sitreport_finalized_Server, 
               id_temp, 
               data_i, dataset, dataset_variables, sr_variables, ar_data, fu_data)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          sitreport_finalized_UI(id_temp)
        )
      )
    })
    
    submit_amendment_r$value <<- NULL
    
  })
  
  
  # ** Update Figures ----------------------------------------------------------
  update_figures_r <<- reactiveValues(value = NULL)
  update_map_r <<- reactiveValues(value = NULL)
  update_p69_r <<- reactiveValues(value = NULL)
  update_summary_table_r <<- reactiveValues(value = NULL)
  uploaded_crash_sketch_r <<- reactiveValues(value = NULL)
  sync_server_trigger_r <<- reactiveValues(value = NULL)
  
  # ** Observe All Data Reactive -----------------------------------------------
  # When filters are changed, update reactive of all data
  observe({
    
    ## Observe below filters
    tmp1 <- input$dt_filter_station
    tmp2 <- input$dt_filter_daterange 
    tmp3 <- input$dt_filter_final_edit_ui 
    tmp4 <- input$dt_filter_p41_final_edit_ui 
    tmp5 <- input$dt_filter_ar_final_edit_ui
    tmp6 <- input$dt_filter_fu_final_edit_ui
    tmp7 <- input$dt_filter_injury_type
    
    all_data_r <- reactive({
      filter_dataset(dataset, 
                     input)
    })
    all_data_r <<- all_data_r
  })
  
  # ** Back to Report List [Data Entry --> Main Page] ----------------------
  go_to_report_list_r <<- reactiveValues(value = NULL)
  
  observeEvent(input$go_to_report_list, {
    
    # Regenerate table for report - - - - - - - - - - - - - - - - - - - - -
    ## Load Data
    if(TESTING_SYSTEM){
      dataset <<- readRDS(file.path("data", "police_data_test.Rds")) %>%
        dplyr::arrange(desc(last_updated)) %>%
        filter(!(uid %in% uids_delete))
      
    } else{
      dataset <<- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data.Rds")), key = data_key)) %>%
        dplyr::arrange(desc(last_updated)) %>%
        filter(!(uid %in% uids_delete))
    }
    
    #### All data (sit report, amend, P41)
    all_data_r <- reactive({
      filter_dataset(dataset, 
                     input)
    })
    all_data_r <<- all_data_r
    
    #### Prep sit report data
    dataset <- mk_stage_sr_clean_var(dataset)
    dataset <- mk_stage_p41_clean_var(dataset)
    dataset <- mk_stage_ar_clean_var(dataset)
    dataset <- mk_stage_fu_clean_var(dataset)
    
    dataset_sitreport <- dataset[dataset$report_type %in% "sit_report",]
    dataset_sitreport <- add_edit_button(dataset_sitreport)
    vals$Data <- data.table(dataset_sitreport)
    
    filterData <- reactive({
      filter_dataset(dataset_sitreport, input)
    })
    
    dataset_subset <<- filterData()
    
    output$table = DT::renderDataTable({
      DT = filterData()
      
      DT <- DT[,c("accident_date", "accident_time_hour",	"accident_time_minute", "accident_type", "ob_no", "police_division", "stage_sr_clean", "stage_ar_clean", "stage_p41_clean", "Edit")] %>%
        dplyr::mutate(accident_time_minute = case_when(nchar(accident_time_minute) %in% 1 ~ paste0(0, accident_time_minute),
                                                       TRUE ~ as.character(accident_time_minute)),
                      accident_date = accident_date %>% as.Date() %>% format('%d/%m/%Y'),
                      accident_time = paste0(accident_time_hour, ":", accident_time_minute) %>%
                        str_replace_all("NA:NA", "")) %>%
        dplyr::select(-c(accident_time_hour, accident_time_minute)) %>%
        dplyr::select(accident_date, accident_time, everything()) %>%
        dplyr::rename("Accident Date" = accident_date,
                      "Accident Time" = accident_time,
                      "Injury Type" = accident_type,
                      "O.B. Num" = ob_no,
                      "Sit Report" = stage_sr_clean,
                      "P41" = stage_p41_clean,
                      "Amend Report" = stage_ar_clean,
                      "Police Station" = police_division,
                      "Crash Report" = Edit) 
      
      datatable(DT, escape = F, selection="none",
                options = list(scrollX = T))
    })
    
    #### Prep P41 Data
    dataset_p41 <- dataset[dataset$report_type %in% "p41",]
    dataset_p41 <- add_edit_button(dataset_p41, "stage_p41")
    #vals$Data <- data.table(dataset_p41)
    
    p41_data_r <- reactive({
      filter_dataset(dataset_p41, input)
    })
    
    output$table_p41 = DT::renderDataTable({
      DT = p41_data_r()
      
      DT <- DT[,c("last_updated", "accident_date", "ob_no", "iar_no", "police_division", "Edit")] %>%
        dplyr::rename("Last Updated" = last_updated,
                      "Accident Date" = accident_date,
                      "O.B. Num" = ob_no,
                      "IAR Num" = iar_no,
                      "Police Station" = police_division)
      
      datatable(DT, escape = F, selection="none",
                options = list(scrollX = T))
    })
    
    # Allow values to work outside of this environment
    filterData <<- filterData # need to do so works in downloadHandler
    p41_data_r <<- p41_data_r # need to do so works in downloadHandler
    all_data_r <<- all_data_r # need to do so works in downloadHandler
    vals <<- vals
    
    go_to_report_list_r$value <<- NULL
    update_figures_r$value <<- 11
    update_map_r$value <<- 11
    update_p69_r$value <<- 11
    update_summary_table_r$value <<- 11
    
    # output$ui_main_body <- renderUI({
    #   div(ui_main)
    # })
    
    updateTabItems(session, "tab", selected = "sit_report") # may not need?
    
    output$ui_main_body <- renderUI(
      main_body
    )
    
  }, ignoreNULL = T)
  
  # For some reason, table doesn't appear - so go to p69, then sit_report
  observeEvent(input$go_to_report_list, {
    updateTabItems(session, "tab", selected = "p69")
  }, priority = -10)
  
  observeEvent(input$go_to_report_list, {
    updateTabItems(session, "tab", selected = "sit_report")
  }, priority = -20)
  
  # ** Download Crash Sketch ---------------------------------------------------
  output$download_raw_data_ui <- renderUI({
    
    if(USER_ROLE %in% "hq"){
      out <- downloadButton("download_raw_data", "Download Data")
    } else{
      out <- NULL
    }
    
    out
  })
  
  
  
  # ** Generate Amendment Report --------------------------------------------------
  observeEvent(input$gen_amend_report, {
    
    ## ID
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    ## Initialize report time/ id / stage
    data_i$report_type <- "amendment"
    data_i$amend_report_id <- paste0("ar", id_temp, sample(100000:999999,size=1))
    data_i$stage_ar <- "editing"
    
    ## Default AR Text
    data_i$ar_main_text <- paste("SUBJECT: AMENDMENT PD PLEASE REFER TO MY EARLIER SIGNAL",
                                 data_i$ob_no,
                                 "DATED",
                                 data_i$accident_date,
                                 "PLEASE AMEND THE SAME TO READ",
                                 "------INPUT AMENDMENT TEXT HERE--------")
    for(var in c("_row1_c1", "_row1_c2", "_row1_c3", 
                 "_row2", "_row3", "_row4", "_row5", 
                 "_row6_c1", "_row6_c2", "_row6_c3", 
                 "_row7", "_row8", "_row9", "_row10")){
      data_i[[paste0("ar", var)]] <- data_i[[paste0("sr", var)]]
    }
    
    tab_types <- c("Enter Data", "Write Amendment Report", "Submit to HQ")
    
    data_i <- as.data.frame(data_i)
    
    callModule(amendreport_Server, id_temp, data_i, dataset, dataset_variables, 
               sr_variables, tab_types)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          amendreport_UI(id_temp)
        )
      )
    })
    
  })
  
  # ** Edit Amendment Report ---------------------------------------------------
  observeEvent(input$edit_amend_report, {
    
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    if(ar_data_i$stage_ar %in% "editing"){
      tab_types <- c("Enter Data", "Write Amendment Report", "Submit to HQ")
    } else if (ar_data_i$stage_ar %in% "submitted_to_hq"){
      tab_types <- c("Review Amendment Report")
    } else if (ar_data_i$stage_ar %in% "hq_provided_feedback"){
      tab_types <- c("Review Feedback from HQ", "Enter Data", "Write Amendment Report", "Submit to HQ")
    }
    
    ar_data_i <- as.data.frame(ar_data_i)
    
    callModule(amendreport_Server, id_temp, ar_data_i, dataset, dataset_variables, 
               sr_variables, tab_types)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          amendreport_UI(id_temp)
        )
      )
    })
    
  })
  
  # ** Generate Follow Up Report --------------------------------------------------
  observeEvent(input$gen_followup_report, {
    
    ## ID
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    ## Initialize report time/ id / stage
    data_i$report_type <- "followup"
    data_i$followup_report_id <- paste0("fu", id_temp, sample(100000:999999,size=1))
    data_i$stage_fu <- "editing"
    
    ## Default AR Text
    data_i$fu_main_text <- paste("SUBJECT: FOLLOW UP PD PLEASE REFER TO MY EARLIER SIGNAL",
                                 data_i$ob_no,
                                 "DATED",
                                 data_i$accident_date,
                                 "PLEASE AMEND THE SAME TO READ",
                                 "------INPUT FOLLOW UP TEXT HERE--------")
    for(var in c("_row1_c1", "_row1_c2", "_row1_c3", 
                 "_row2", "_row3", "_row4", "_row5", 
                 "_row6_c1", "_row6_c2", "_row6_c3", 
                 "_row7", "_row8", "_row9", "_row10")){
      data_i[[paste0("fu", var)]] <- data_i[[paste0("sr", var)]]
    }
    
    tab_types <- c("Enter Data", "Write Follow Up Report", "Submit to HQ")
    
    data_i <- as.data.frame(data_i)
    
    callModule(followupreport_Server, id_temp, data_i, dataset, dataset_variables, 
               sr_variables, tab_types)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          followupreport_UI(id_temp)
        )
      )
    })
    
  })
  
  # ** Edit Follow Up Report ---------------------------------------------------
  observeEvent(input$edit_followup_report, {
    
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    if(fu_data_i$stage_fu %in% "editing"){
      tab_types <- c("Enter Data", "Write Follow Up Report", "Submit to HQ")
    } else if (fu_data_i$stage_fu %in% "submitted_to_hq"){
      tab_types <- c("Review Follow Up Report")
    } else if (fu_data_i$stage_fu %in% "hq_provided_feedback"){
      tab_types <- c("Review Feedback from HQ", "Enter Data", "Write Follow Up Report", "Submit to HQ")
    }
    
    fu_data_i <- as.data.frame(fu_data_i)
    
    callModule(followupreport_Server, id_temp, fu_data_i, dataset, dataset_variables, 
               sr_variables, tab_types)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          followupreport_UI(id_temp)
        )
      )
    })
    
  })
  
  # ** Edit situation Report ---------------------------------------------------
  observeEvent(input$edit_sit_report, {
    
    #new_report <- FALSE
    #selectedRow <- as.numeric(strsplit(input$lastClick, "_")[[1]][2])
    #data_i <- vals$Data[selectedRow,]
    data_i <- data_i %>% as.data.frame()
    
    if(data_i$stage[1] %in% "editing"){
      tab_types <- c("Enter Data", "Data Checks", "Manually Edit", "Submit to HQ")
    } else if (data_i$stage[1] %in% "submitted_to_hq"){
      tab_types <- c("Review Situation Report")
    } else if (data_i$stage[1] %in% "hq_provided_feedback"){
      tab_types <- c("Review Feedback from HQ", "Enter Data", "Data Checks", "Manually Edit", "Submit to HQ")
    } else if (data_i$stage[1] %in% "finalized"){
      tab_types <- c("Finalized")
    }
    
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    callModule(sitreport_Server,
               id_temp,
               data_i, dataset, dataset_variables, sr_variables, tab_types)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          sitreport_UI(id_temp)
        )
      )
    })
    
  })
  
  # ** P41: Generate from Sit Report --------------------------------------------
  observeEvent(input$gen_p41_from_sit_report, {
    
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    data_i$report_type <- "p41"
    data_i$p41_report_id <- paste0("p41", id_temp, sample(100000:999999,size=1))
    data_i$stage_p41 <- "editing"
    
    tab_types <- c("Enter Data", "Submit to HQ")
    
    data_i <- as.data.frame(data_i)
    
    callModule(p41_edit_Server, id_temp, data_i, dataset, dataset_variables,
               sr_variables, tab_types)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          p41_edit_UI(id_temp)
        )
      )
    })
    
  })
  
  # ** P41: Edit from Sit Report --------------------------------------------
  observeEvent(input$edit_p41_from_sit_report, {
    
    id_temp <- Sys.time() %>% str_replace_all("[[:punct:]]| ", "")
    
    if(p41_data_i$stage_p41 %in% "editing"){
      tab_types <- c("Enter Data", "Submit to HQ")
    } else if (p41_data_i$stage_p41 %in% "submitted_to_hq"){
      tab_types <- c("Review P41")
    } else if (p41_data_i$stage_p41 %in% "hq_provided_feedback"){
      tab_types <- c("Review Feedback from HQ", "Enter Data", "Submit to HQ")
    }
    
    p41_data_i <- as.data.frame(p41_data_i)
    
    callModule(p41_edit_Server, id_temp, p41_data_i, dataset, dataset_variables,
               sr_variables, tab_types)
    
    output$ui_main_body <- renderUI({
      div(
        fluidPage(
          p41_edit_UI(id_temp)
        )
      )
    })
    
  })
  
  
  
  # ** Subset Data for P69 -----------------------------------------------------
  observe({
    req(input$summary_date_type)
    tmp <- update_p69_r$value
    
    #### Load Data
    if(TESTING_SYSTEM){
      data_temp <- readRDS(file.path("data", "police_data_test.Rds")) %>%
        dplyr::arrange(desc(last_updated))
    } else{
      data_temp <- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data.Rds")), key = data_key)) %>%
        dplyr::arrange(desc(last_updated)) 
    }
    
    # Subset Data ------------------------------------------------------------------
    if(!is.null(input$p69_all_or_finalized)){
      if(input$p69_all_or_finalized %in% "Only Completed Reports"){
        data_temp <- data_temp[data_temp$stage %in% "finalized",]
      }
    }
    
    # TODO: For each uid, take latest entry (so could be P41 or amendment)
    data_temp <- data_temp %>%
      arrange(desc(last_updated)) %>%
      distinct(uid, .keep_all = TRUE)
    
    #data_temp <- data_temp[data_temp$report_type %in% "sit_report",]
    
    if(input$p69_station != "Nairobi County"){
      data_temp <- data_temp[data_temp$police_division %in% input$p69_station,]
    }
    
    if(nrow(data_temp) > 0){
      #### Define variables to help subset
      data_temp <- data_temp %>%
        mutate(accident_year = accident_date %>% year(),
               accident_month = accident_date %>% 
                 month(label=T, abbr=F) %>%
                 as.character(),
               accident_quarter = case_when(accident_month %in% c("January", "February", "March")     ~ "January - March",
                                            accident_month %in% c("April",   "May",      "June")      ~ "April - June",
                                            accident_month %in% c("July",    "August",   "September") ~ "July - September",
                                            accident_month %in% c("October", "November", "December")  ~ "October - December"))
      
      #### Restrict by Year
      if(input$summary_date_type %in% c("Annually", "Quarterly", "Monthly")){
        data_temp <- data_temp[data_temp$accident_year %in% as.numeric(input$summary_year_selection),]
      }
      
      
      #### Restrict by type
      if (input$summary_date_type %in% "Quarterly"){
        data_temp <- data_temp[data_temp$accident_quarter %in% input$summary_date_selection,]
        title_type <- "QUARTERLY ACCIDENT SUMMARY"
        type_label <- "Quarter ending"
        type_text_1 <- input$summary_date_selection %>% 
          str_replace_all(".*-", "") %>% 
          str_squish() %>%
          paste0(",")
        type_text_2 <- input$summary_year_selection
        
      } else if (input$summary_date_type %in% "Monthly"){
        data_temp <- data_temp[data_temp$accident_month %in% input$summary_date_selection,]
        title_type <- "MONTHLY ACCIDENT SUMMARY"
        type_label <- "Month"
        type_text_1 <- input$summary_date_selection
        type_text_2 <- input$summary_year_selection
        
      } else if (input$summary_date_type %in% "Date Range"){
        data_temp <- data_temp[(data_temp$accident_date >= min(input$summary_date_selection)) & 
                                 (data_temp$accident_date <= max(input$summary_date_selection)),]
        title_type <- "ACCIDENT SUMMARY"
        type_label <- "Date range"
        type_text_1 <- paste0(min(input$summary_date_selection), " to ", max(input$summary_date_selection))
        type_text_2 <- ""
      }
      
      if (input$summary_date_type %in% "Annually"){
        title_type <- "ANNUAL ACCIDENT SUMMARY"
        type_label <- "Year"
        type_text_1 <- input$summary_year_selection
        type_text_2 <- ""
      }
      
    }
    
    # **** Box Indicating Number of Crashes --------------------------------------
    output$N_crashes_p69 <- renderInfoBox({
      valueBox(
        nrow(data_temp), "Crashes in selected time period", icon = icon("car-crash"),
        color = "red"
      )
    })
    
    # **** Download Button -----------------------------------------------------
    if(nrow(data_temp) > 0 & 
       !(input$p69_officer_name %in% "") & 
       !(input$p69_station %in% "")){
      shinyjs::enable("p69_pdf")
    } else{
      shinyjs::disable("p69_pdf")
    }
    
    if(nrow(data_temp) > 0 & 
       !(input$p69_officer_name %in% "") & 
       !(input$p69_station %in% "")){
      shinyjs::enable("email_p69_button")
    } else{
      shinyjs::disable("email_p69_button")
    }
    
  })
  
  # UIs ------------------------------------------------------------------------
  
  # ** Warning to Update Version -----------------------------------------------
  
  observe({
    
    tmp <- input$sync_with_server # so "input$sync_with_server" is observerd
    tmp <- input$updatesystem_yes # so "input$sync_with_server" is observerd
    
    output$version_warning <- renderText({
      version_latest <- readRDS(file.path("data", "version_latest.Rds"))
      version_local_system <- readRDS(file.path("data", "version_local_system.Rds"))
      
      if((version_latest != version_local_system) & ONLINE_SYSTEM %in% F){
        out <- "<h4><br><span style='color:red'><strong>
            ***The system needs to be updated! 
            Connect to internet and click the 'Update System' button on the top right.***
            </strong></span></h4>"
      } else{
        out <- NULL
      }
      
      out
      
    })
  })
  
  # ** Download Raw Data Button ------------------------------------------------
  output$download_raw_data_ui <- renderUI({
    
    if(USER_ROLE %in% "hq"){
      out <- downloadButton("download_raw_data", "Download Data")
    } else{
      out <- NULL
    }
    
    out
  })
  
  # ** Update System Button ------------------------------------------------
  output$updatesystem_ui <- renderUI({
    
    if(ONLINE_SYSTEM %in% F){
      out <- actionButton("updatesystem", "Update System")
    } else{
      out <- NULL
    }
    
    # DELETE ME
    out <- actionButton("updatesystem", "Update System")
    
    out
  })
  
  
  
  
  # Downloading ----------------------------------------------------------------
  
  # ** Download all data -------------------------------------------------------
  observe({
    output$download_raw_data <- downloadHandler(
      filename = function() {
        paste0("crash_data", ".csv")
      },
      content = function(file) {
        write.csv(all_data_r(), file, row.names = FALSE)
      }
    )
  })
  
  # ** Print all Situation Reports ---------------------------------------------
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
          
          data_temp = all_data_r()
          out = knit2pdf(file.path('modules_gen_reports','generate_sr_pdf_loop_parent.Rnw'), clean = TRUE)
          #file.rename(out, file) # move pdf to file for downloading
          file.copy(out, file) # move pdf to file for downloading
        }
        
      )
    },
    
    contentType = 'application/pdf'
  )
  
  # ** Print all P41s ----------------------------------------------------------
  output$print_all_p41s = downloadHandler(
    filename = 'p41s_merged.pdf',
    
    content = function(file) {
      shiny::withProgress(
        message = paste0("Downloading P41s"),
        value = 0,
        {
          shiny::incProgress(1/10)
          #Sys.sleep(1)
          shiny::incProgress(5/10)
          
          data_temp = all_data_r()
          out = knit2pdf(file.path('modules_gen_reports', 'generate_p41_pdf_loop_parent.Rnw'), clean = TRUE)
          #file.rename(out, file) # move pdf to file for downloading
          file.copy(out, file)
        }
        
      )
    },
    
    contentType = 'application/pdf'
  )
  
  # ** Print P69 ---------------------------------------------------------------
  output$p69_pdf = downloadHandler(
    filename = 'p69.pdf',
    
    content = function(file) {
      out = knit2pdf(file.path('modules_gen_reports', 'generate_p69_pdf_child.Rnw'), clean = TRUE)
      #file.rename(out, file) # move pdf to file for downloading
      file.copy(out, file)
    },
    
    contentType = 'application/pdf'
  )
  
  
  # ** Print User Manual -------------------------------------------------------
  output$download_user_manual <- downloadHandler(
    filename = "eCrash System User Manual.pdf",
    content = function(file) {
      file.copy("user_manual/ecrash_manual.pdf", file)
    },
    contentType = 'application/pdf'
  )
  
  # Email P69 ------------------------------------------------------------------
  
  ## Open Model
  email_p69_modal <- function(session) {
    
    modalDialog(
      #"Submit to HQ. After submitting, you will need to click 'Sync Data with Server'
      #for HQ to receive the report.",
      
      tagList(
        h3("Email P69"),
        
        textInput("email_p69_emailaddress", "Email Address(es)", value = ""),
        h6("To send to multiple email addresses, separate by a semicolon (;). All emails are sent via bcc."),
        
        textInput("email_p69_subject", "Subject", 
                  value = paste0("P69 - ", input$p69_station)),
        
        textAreaInput("email_p69_body", "Body", 
                      value = paste0("Please see attached for the P69 for ",
                                     input$p69_station,". --Nairobi Traffic eCrash System"),
                      width = "150%",
                      height = "300%")
        
      ),
      footer = tagList(
        actionButton("email_p69_button_no", "Cancel"),
        actionButton("email_p69_button_yes", div(tags$b("Send", style = "color: green;")))
      )
    )
  }
  
  observeEvent(input$email_p69_button,
               ignoreNULL = T,   # Show modal on start up
               showModal(email_p69_modal(session))
  )
  
  # Cancel Send
  observeEvent(input$email_p69_button_no, { 
    removeModal() 
  })
  
  # Send
  observeEvent(input$email_p69_button_yes, { 
    
    p69_pdf_fileame <- input$p69_station %>% tolower() %>% str_replace_all(" ", "") %>% paste0(".tex")
    p69_pdf_fileame <- paste0("report_", p69_pdf_fileame)
    
    print(p69_pdf_fileame)
    out = knit2pdf(file.path('modules_gen_reports', 
                             'generate_p69_pdf_child.Rnw'), 
                   output = p69_pdf_fileame,
                   clean = TRUE)
    
    to_p69_email_address <- input$email_p69_emailaddress %>% str_squish()
    
    email_worked_message <- tryCatch(
      {
        time_response <- withTimeout({
          
          
          gm_auth_configure(path = "keys_passwords/gmail_credentials/credentials.json")
          gm_auth(cache = "keys_passwords/gmail_credentials",
                  email = "nairobi.ecrash.system@gmail.com")
          
          tmp_threads <- gm_threads(num_results=1)
          
          my_email_message <- gm_mime() %>%
            gm_bcc(to_p69_email_address) %>%
            gm_from("nairobi.ecrash.system@gmail.com") %>%
            gm_subject(input$email_p69_subject) %>%
            gm_text_body(input$email_p69_body) %>%
            gm_attach_file(out)
          
          gm_send_message(my_email_message)
          
          NULL
          
        },
        timeout = 4,
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
  
  
  # Back to report list, after approving ---------------------------------------
  observeEvent(go_to_report_list_r$value, {
    
    if(!is.null(go_to_report_list_r$value)){
      
      # Regenerate table for report - - - - - - - - - - - - - - - - - - - - -
      ## Load Data
      if(TESTING_SYSTEM){
        dataset <<- readRDS(file.path("data", "police_data_test.Rds")) %>%
          dplyr::arrange(desc(last_updated)) %>%
          filter(!(uid %in% uids_delete))
      } else{
        dataset <<- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data.Rds")), key = data_key)) %>%
          dplyr::arrange(desc(last_updated)) %>%
          filter(!(uid %in% uids_delete))
      }
      
      ## Add edit button
      dataset <- add_edit_button(dataset)
      dataset <- mk_stage_sr_clean_var(dataset)
      dataset <- mk_stage_p41_clean_var(dataset)
      dataset <- mk_stage_ar_clean_var(dataset)
      dataset <- mk_stage_fu_clean_var(dataset)
      
      ## Reactive value of full dataset
      vals$Data <- data.table(dataset)
      
      #### Filter data
      filterData <- reactive({
        filter_dataset(dataset[dataset$report_type %in% "sit_report",], 
                       input)
      })
      
      dataset_subset <<- filterData()
      
      output$table = DT::renderDataTable({
        DT = filterData()
        
        DT <- DT[,c("accident_date", "accident_time_hour",	"accident_time_minute", "accident_type", "ob_no", "police_division", "stage_sr_clean", "stage_ar_clean", "stage_p41_clean", "Edit")] %>%
          dplyr::mutate(accident_time_minute = case_when(nchar(accident_time_minute) %in% 1 ~ paste0(0, accident_time_minute),
                                                         TRUE ~ as.character(accident_time_minute)),
                        accident_date = accident_date %>% as.Date() %>% format('%d/%m/%Y'),
                        accident_time = paste0(accident_time_hour, ":", accident_time_minute) %>%
                          str_replace_all("NA:NA", "")) %>%
          dplyr::select(-c(accident_time_hour, accident_time_minute)) %>%
          dplyr::select(accident_date, accident_time, everything()) %>%
          dplyr::rename("Accident Date" = accident_date,
                        "Accident Time" = accident_time,
                        "Injury Type" = accident_type,
                        "O.B. Num" = ob_no,
                        "Sit Report" = stage_sr_clean,
                        "P41" = stage_p41_clean,
                        "Amend Report" = stage_ar_clean,
                        "Police Station" = police_division,
                        "Crash Report" = Edit) 
        
        
        datatable(DT, escape = F, selection="none",
                  options = list(scrollX = T))
      })
      
      #### Make data table - P41
      p41_data_r <- reactive({
        filter_dataset(dataset[dataset$report_type %in% "p41",] %>%
                         add_edit_button("stage_p41"), 
                       input)
      })
      
      output$table_p41 = DT::renderDataTable({
        DT = p41_data_r()
        
        DT <- DT[,c("last_updated", "accident_date", "ob_no", "iar_no", "police_division", "Edit")] %>%
          dplyr::rename("Last Updated" = last_updated,
                        "Accident Date" = accident_date,
                        "O.B. Num" = ob_no,
                        "IAR Num" = iar_no,
                        "Police Station" = police_division)
        
        datatable(DT, escape = F, selection="none",
                  options = list(scrollX = T))
      })
      
      # Allow values to work outside of this environment
      filterData <<- filterData # need to do so works in downloadHandler
      p41_data_r <<- p41_data_r # need to do so works in downloadHandler
      all_data_r <<- all_data_r # need to do so works in downloadHandler
      vals <<- vals
      
      updateTabItems(session, "tab", selected = "sit_report") # may not need?
      
      output$ui_main_body <- renderUI(
        main_body
      )
      
    }
    
  }, ignoreNULL = T)
  
  observeEvent(go_to_report_list_r$value, {
    updateTabItems(session, "tab", selected = "p69")
  }, priority = -10)
  
  observeEvent(go_to_report_list_r$value, {
    updateTabItems(session, "tab", selected = "sit_report")
    
    go_to_report_list_r$value <<- NULL
    update_figures_r$value <<- NULL
    update_map_r$value <<- NULL
    update_p69_r$value <<- NULL
    update_summary_table_r$value <<- NULL
    
  }, priority = -20)
  
  # ** Sync Data with Server ---------------------------------------------------
  observeEvent(input$sync_with_server | sync_server_trigger_r$value, {
    # input$sync_with_server
    # input$sync_with_server | sync_server_trigger_r$value
    
    #### Check if has internet connection
    internet_connection <- curl::has_internet()
    
    #### Print sync notification
    # Can only sink if connected to internet AND not in test mode
    output$sync_notification <- renderText({
      
      
      #language = "en-GB",
      #format = "dd/mm/yyyy"
      
      if((internet_connection %in% TRUE) & (TESTING_SYSTEM %in% FALSE)){
        paste0("<b>Synced at ", Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% format('%d/%m/%Y %H:%M:%OS') %>% substring(1,19), "</b>")
      } else if((internet_connection %in% FALSE) & (TESTING_SYSTEM %in% FALSE)){
        "<font color=\"#FF0000\"><b> Unable to Sync: No Internet Connection </b></font>"
      } else if(TESTING_SYSTEM %in% TRUE){
        "<b>Cannot Sync in Test Mode</b>"
      }
      
    })
    
    #### If syncing (internet connecting AND not testing system)
    if(internet_connection & !TESTING_SYSTEM){
      
      ## **** Setup --------------------------------------------------------------
      ## Load API Keys
      api_keys <- unserialize(aes_cbc_decrypt(readRDS(file.path("keys_passwords", "secured", "api_keys.Rds")), key = data_key))
      
      Sys.setenv("AWS_ACCESS_KEY_ID" = api_keys$Key[(api_keys$Service %in% "AWS_ACCESS_KEY_ID")],
                 "AWS_SECRET_ACCESS_KEY" = api_keys$Key[(api_keys$Service %in% "AWS_SECRET_ACCESS_KEY")],
                 "AWS_DEFAULT_REGION" = "us-east-2")
      
      ## **** Reports to Delete ------------------------------------------------
      #### Load Local
      uid_delete_list <- readRDS(file.path("data", "sr_to_delete.Rds"))
      
      #### Add in data from server
      # Sync this first, as use this list when syncing main data with server
      aws_bucket_sr_to_delete <- get_bucket(bucket = BUCKET_NAME, prefix="data_to_delete/sr_to_delete.Rds", check_region = F)
      if(length(aws_bucket_sr_to_delete) > 0){
        uid_delete_list_server <- s3read_using(readRDS, 
                                               bucket = BUCKET_NAME,
                                               object = aws_bucket_sr_to_delete[1]$Contents$Key)
        
        uid_delete_list <- c(uid_delete_list, uid_delete_list_server) %>% unique()
      }
      
      #### Push to server
      s3write_using(uid_delete_list, 
                    FUN = saveRDS_version2,
                    bucket = BUCKET_NAME,
                    object = "data_to_delete/sr_to_delete.Rds")
      
      #### Save locally
      saveRDS(uid_delete_list, file.path("data", "sr_to_delete.Rds"))
      
      ## **** Main Data ----------------------------------------------------------
      
      #### Pull Data from Server - - - - - - - - - - - - - - - - - - - - - - - - 
      
      ## Load Data from AWS s3 Bucket
      aws_bucket_df <- get_bucket(bucket = BUCKET_NAME, prefix="data/police_data.Rds", check_region = F)
      if(length(aws_bucket_df) > 0){
        
        dataset_server <- s3read_using(readRDS, 
                                       bucket = BUCKET_NAME,
                                       object = aws_bucket_df[1]$Contents$Key) %>%
          aes_cbc_decrypt(key = data_key) %>%
          unserialize
        
        for(var in c(textInput_variables, textAreaInput_variables)){
          if(length(dataset_server[[var]]) > 0) dataset_server[[var]] <- dataset_server[[var]] %>% as.character()
        }
        
      } else{
        dataset_server <- data.frame(NULL)
      }
      
      #### Append Server Data with Local Data; Keep Most Updated
      ## Convert all variables to character to avoid type conflicts
      for(var in names(dataset))        dataset[[var]]        <- dataset[[var]]        %>% as.character()
      for(var in names(dataset_server)) dataset_server[[var]] <- dataset_server[[var]] %>% as.character()
      
      ## Bind rows and keep latest updated
      #if(!is.null(dataset$report_type))     dataset$report_type <- NA
      #if(!is.null(dataset$amend_report_id)) dataset$amend_report_id <- NA
      
      dataset <- bind_rows(dataset, dataset_server) %>%
        
        ## Ensure numeric so sorts/orders correctly 
        mutate(stage_sr_num = stage_sr_num %>% as.numeric,
               stage_ar_num = stage_ar_num %>% as.numeric,
               stage_fu_num = stage_fu_num %>% as.numeric,
               stage_p41_num = stage_p41_num %>% as.numeric) %>%
        arrange(desc(stage_sr_num), desc(stage_ar_num), desc(stage_fu_num), desc(stage_p41_num), desc(last_updated)) %>%
        distinct(uid, report_type, amend_report_id, .keep_all = TRUE) %>%
        arrange(desc(last_updated)) %>%
        mutate(stage_sr_num = stage_sr_num %>% as.character,
               stage_ar_num = stage_ar_num %>% as.character,
               stage_fu_num = stage_fu_num %>% as.character,
               stage_p41_num = stage_p41_num %>% as.character)
      
      # For testing
      # dataset <- data.frame(last_updated = seq(from = as.Date("2020-01-01"), to = as.Date("2020-01-10"), by=1),
      #                       stage_sr_num = c(2,10,1,9,10,9,6,9,8,10),
      #                       stage_ar_num = c(10,1,10,1,2,3,4,5,6,7),
      #                       stage_p41_num = c(5,6,10,7,3,8,10,9,10,1))
      # dataset <- dataset[order(runif(10)),]
      # dataset %>%
      #   arrange(desc(stage_sr_num), desc(stage_ar_num), desc(stage_p41_num), desc(last_updated)) #%>%
      #   #distinct(stage_sr_num, .keep_all = TRUE)
      
      
      
      
      
      
      ## Remove if deleted
      if(length(uid_delete_list) > 0){
        dataset <- dataset[!(dataset$uid %in% uid_delete_list),]
      }
      
      #### Send data to server - - - - - - - - - - - - - - - - - - - - - - - - -
      s3write_using(aes_cbc_encrypt(serialize(dataset, NULL), key = data_key), 
                    FUN = saveRDS_version2,
                    bucket = BUCKET_NAME,
                    object = "data/police_data.Rds")
      
      #### *** Save data locally ***
      dataset <- as.data.frame(dataset)
      saveRDS(aes_cbc_encrypt(serialize(dataset, NULL), key = data_key), file.path("data", "police_data.Rds"), version=2)
      
      dataset <<- dataset
      
      ## **** Crash Sketches ---------------------------------------------------
      # TODO: Does this take care of versioning? E.g., later version uploaded?
      #       Could keep all and date them all, and in data include the 
      #       name of the one to actually use. So the name would be:
      #       [uid]_[datetime]. Then could check and delete the older ones.
      # TODO: Files to big. Either don't encrypt or encrypt in another way.
      #       For example, password protect the Rds file? If don't encrypt,
      #       just use the PNG
      # TODO: Upload works; not download
      s3sync(
        path = file.path("data", "crash_sketches"),
        bucket = BUCKET_NAME,
        prefix = "crash_sketches/uid",
        direction = c("upload", "download")
      )
      
      ## **** Amendment Data 
      # _ba = _beforeamendments
      #### Pull Data from Server - - - - - - - - - - - - - - - - - - - - - - - - 
      
      # ## Load Data from AWS s3 Bucket
      # aws_bucket_df_ba <- get_bucket(bucket = "wb-smarttrans-police-pilot", prefix="data/police_data_beforeamendments.Rds", check_region = F)
      # if(length(aws_bucket_df_ba) > 0){
      #   
      #   dataset_server_ba <- s3read_using(readRDS, 
      #                                     bucket = "wb-smarttrans-police-pilot",
      #                                     object = aws_bucket_df[1]$Contents$Key) %>%
      #     aes_cbc_decrypt(key = data_key) %>%
      #     unserialize
      #   
      #   for(var in c(textInput_variables, textAreaInput_variables)){
      #     if(length(dataset_server_ba[[var]]) > 0) dataset_server_ba[[var]] <- dataset_server_ba[[var]] %>% as.character()
      #   }
      #   
      # } else{
      #   dataset_server_ba <- data.frame(NULL)
      # }
      # 
      # #### Append Data - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      # 
      # #### Load Data
      # dataset_ba <- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data_beforeamendments.Rds")), key = data_key)) %>%
      #   dplyr::arrange(desc(last_updated)) %>%
      #   filter(!(uid %in% uids_delete))
      # 
      # #### Append Server Data with Local Data; Keep Most Updated
      # ## Convert all variables to character to avoid type conflicts
      # for(var in names(dataset_ba))        dataset_ba[[var]]        <- dataset_ba[[var]] %>% as.character()
      # for(var in names(dataset_server_ba)) dataset_server_ba[[var]] <- dataset_server_ba[[var]] %>% as.character()
      # 
      # ## Bind rows [here we don't keep last updated]
      # dataset_ba <- bind_rows(dataset_ba, dataset_server_ba) %>%
      #   dplyr::arrange(desc(last_updated)) %>%
      #   distinct(uid, report_type, amend_report_id, .keep_all = TRUE) %>%
      #   filter(!(uid %in% uids_delete))
      # 
      # #### Send data to server - - - - - - - - - - - - - - - - - - - - - - - - -
      # s3write_using(aes_cbc_encrypt(serialize(dataset_ba, NULL), key = data_key), 
      #               FUN = saveRDS_version2,
      #               bucket = "wb-smarttrans-police-pilot",
      #               object = "data/police_data_beforeamendments.Rds")
      # 
      # #### *** Save data locally ***
      # if(TESTING_SYSTEM){
      #   saveRDS(dataset_ba, file.path("data", "police_data_test_beforeamendments.Rds"), version=2)
      # } else{
      #   saveRDS(aes_cbc_encrypt(serialize(dataset_ba, NULL), key = data_key), file.path("data", "police_data_beforeamendments.Rds"), version=2)
      # }
      
      ## **** Version ----------------------------------------------------------
      #### Download Rds file that indicates latest version of the system
      version_bucket <- get_bucket(bucket = BUCKET_NAME, prefix="version/version_latest.Rds")
      
      version_latest <- s3read_using(readRDS, 
                                     bucket = BUCKET_NAME,
                                     object = version_bucket[1]$Contents$Key) 
      
      saveRDS(version_latest, file.path("data", "version_latest.Rds"))
      
      ## **** Regenerate table for report ----------------------------------------
      
      #### Add edit button
      dataset <- add_edit_button(dataset)
      dataset <- mk_stage_sr_clean_var(dataset)
      dataset <- mk_stage_p41_clean_var(dataset)
      dataset <- mk_stage_ar_clean_var(dataset)
      dataset <- mk_stage_fu_clean_var(dataset)
      
      #### Define reactive value of full dataset
      # Needed to grab specific rows later on
      vals$Data <- data.table(dataset)
      
      #### Filter data
      filterData <- reactive({
        filter_dataset(dataset, input) %>%
          filter(report_type %in% "sit_report")
      })
      
      #### Make data table
      output$table = DT::renderDataTable({
        DT = filterData()
        
        DT <- DT[,c("accident_date", "accident_time_hour",	"accident_time_minute", "accident_type", "ob_no", "police_division", "stage_sr_clean", "stage_ar_clean", "stage_p41_clean", "Edit")] %>%
          dplyr::mutate(accident_time_minute = case_when(nchar(accident_time_minute) %in% 1 ~ paste0(0, accident_time_minute),
                                                         TRUE ~ as.character(accident_time_minute)),
                        accident_date = accident_date %>% as.Date() %>% format('%d/%m/%Y'),
                        accident_time = paste0(accident_time_hour, ":", accident_time_minute) %>%
                          str_replace_all("NA:NA", "")) %>%
          dplyr::select(-c(accident_time_hour, accident_time_minute)) %>%
          dplyr::select(accident_date, accident_time, everything()) %>%
          dplyr::rename("Accident Date" = accident_date,
                        "Accident Time" = accident_time,
                        "Injury Type" = accident_type,
                        "O.B. Num" = ob_no,
                        "Sit Report" = stage_sr_clean,
                        "P41" = stage_p41_clean,
                        "Amend Report" = stage_ar_clean,
                        "Police Station" = police_division,
                        "Crash Report" = Edit) 
        
        
        datatable(DT, escape = F, selection="none",
                  options = list(scrollX = T))
      })
      
      
      #### Make data table - P41
      # p41_data_r <- reactive({
      #   filter_dataset(dataset[dataset$report_type %in% "p41",] %>%
      #                    add_edit_button("stage_p41"), 
      #                  input)
      # })
      
      # output$table_p41 = DT::renderDataTable({
      #   DT = p41_data_r()
      #   
      #   DT <- DT[,c("last_updated", "accident_date", "ob_no", "iar_no", "police_division", "Edit")] %>%
      #     dplyr::rename("Last Updated" = last_updated,
      #                   "Accident Date" = accident_date,
      #                   "O.B. Num" = ob_no,
      #                   "IAR Num" = iar_no,
      #                   "Police Station" = police_division)
      #   
      #   datatable(DT, escape = F, selection="none")
      # })
      
      # Allow values to work outside of this environment
      filterData <<- filterData # need to do so works in downloadHandler
      # p41_data_r <<- p41_data_r # need to do so works in downloadHandler
      
      # Update data pages
      update_figures_r$value <<- 2000
      update_map_r$value <<- 2000
      update_p69_r$value <<- 2000
      update_summary_table_r$value <<- 2000
      
      vals <<- vals
      
      #### Update Page
      output$page <- renderUI({
        div(ui_main)
      })
      
      
    }
    
  })
  
  # Update System [Git Pull] ---------------------------------------------------
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  dataModal_updatesystem <- function(failed = FALSE) {
    modalDialog(
      
      ifelse(curl::has_internet(), "Update the system. Doing so will cause the application to stop. After clicking yes, please exit the system and re-start.", "You are not connected to internet. You will need to connect to internet to update the system"),
      #div(tags$b("Are you sure you want to finalize the form? Finalizing means that you will not be able to edit the data, P41 or situation report. You will only be able to download the P41 and Situation Report.", style = "color: red;")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("updatesystem_yes", div(tags$b("Yes", style = "color: red;")))
      )
    )
  }
  
  # Show modal when button is clicked.
  observeEvent(input$updatesystem, {
    showModal(dataModal_updatesystem())
  })
  
  # Confirm finalize
  observeEvent(input$updatesystem_yes, {
    
    if(curl::has_internet()){
      
      #### Grab latest version name
      ## Set API Keys
      api_keys <- unserialize(aes_cbc_decrypt(readRDS(file.path("keys_passwords", "secured", "api_keys.Rds")), key = data_key))
      
      Sys.setenv("AWS_ACCESS_KEY_ID" = api_keys$Key[(api_keys$Service %in% "AWS_ACCESS_KEY_ID")],
                 "AWS_SECRET_ACCESS_KEY" = api_keys$Key[(api_keys$Service %in% "AWS_SECRET_ACCESS_KEY")],
                 "AWS_DEFAULT_REGION" = "us-east-2")
      
      ## Download Version Name
      version_bucket <- get_bucket(bucket = BUCKET_NAME, prefix="version/version_latest.Rds", check_region = F)
      version_latest <- s3read_using(readRDS, 
                                     bucket = BUCKET_NAME,
                                     object = version_bucket[1]$Contents$Key) 
      saveRDS(version_latest, file.path("data", "version_latest.Rds"))
      saveRDS(version_latest, file.path("data", "version_local_system.Rds"))
      
      #### Sync System
      if(Sys.info()['sysname'] %in% "Windows") shell(paste0("cd ", getwd(), " & git stash     "))
      if(Sys.info()['sysname'] %in% "Darwin") system(paste0("cd ", getwd(), " ; git stash     "))
      
      if(Sys.info()['sysname'] %in% "Windows") shell(paste0("cd ", getwd(), " & git pull     "))
      if(Sys.info()['sysname'] %in% "Darwin") system(paste0("cd ", getwd(), " ; git pull     "))
      
      Sys.sleep(1)
      stopApp()
      
    } else{
      removeModal()
    }
    
  })
  
  
  
})

## END -------------------------------------------------------------------------
shinyApp(ui, server)







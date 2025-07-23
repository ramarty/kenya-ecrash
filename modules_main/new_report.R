# Situation Report Module

# UI ##################################################################### -----
newreport_UI <- function(id) {
  ns <- NS(id)
  
  # UI Data Entry --------------------------------------------------------------
  fluidPage(
    
    #### Hide errors from displaying
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    # ** Top Panel -------------------------------------------------------------
    fluidRow(
      column(1,
             actionButton("go_to_report_list","Back to Report List")
      )
    ),
    
    # ** Tabset Panel ----------------------------------------------------------
    fluidRow(
      column(10, align = "center",
             h3("Create an OB Number")
      )
    ),
    fluidRow(
      column(3, align = "center",
             numericInput(ns("ob_no_1"),
                       label = "OB #",
                       value = NA)  
      ),
      column(3, align = "center",
             numericInput(ns("ob_no_2"),
                       label = "Day",
                       value = Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% substring(9,10) %>% as.numeric())  
      ),
      column(3, align = "center",
             numericInput(ns("ob_no_3"),
                       label = "Month", 
                       value = Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% substring(6,7) %>% as.numeric())  
      ),
      column(3, align = "center",
             numericInput(ns("ob_no_4"),
                       label = "Year", 
                       value = Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% substring(1,4) %>% as.numeric())  
      )
      
    ),
    
    fluidRow(
      column(10, align = "center",
             strong(textOutput(ns("ob_no_full")))
      )
    ),
    br(),
    fluidRow(
      column(10, align = "center",
             uiOutput(ns("create_report_button_ui"))
      )
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
newreport_Server <- function(input, output, session, data_i, dataset, dataset_variables,
                             sr_variables, tab_types) {
  
  source(file.path("modules_main", "functions.R"))
  
  observe({
    
    ## To pass back to "input$create_report_button" in app.R
    ob_no_1 <<- input$ob_no_1 %>% as.numeric %>% as.character() # to convert "03" to "3" (for example)
    ob_no_2 <<- input$ob_no_2 %>% as.numeric %>% as.character()
    ob_no_3 <<- input$ob_no_3 %>% as.numeric %>% as.character()
    ob_no_4 <<- input$ob_no_4 %>% as.numeric %>% as.character()
    ob_no <<- paste(input$ob_no_1, input$ob_no_2, input$ob_no_3, input$ob_no_4, sep = "/")
    
    output$ob_no_full <- renderText({
      ob_no <- paste(input$ob_no_1, input$ob_no_2, input$ob_no_3, input$ob_no_4, sep = "/")
      paste("O.B. Number:", ob_no)
    })
    
    output$create_report_button_ui <- renderUI({
      
      out <- NULL
      
      if(!is.null(input$ob_no_1)){
        
        ## If all ob values entered, create button
        if((!is.na(input$ob_no_1)) & 
           (!is.na(input$ob_no_2)) & 
           (!is.na(input$ob_no_3)) & 
           (!is.na(input$ob_no_4))){
          #out <- actionButton(session$ns("create_report_button"), "Create New Report")
          out <- actionButton("create_report_button", "Create New Report")
          
        }
        
        ## But if ob already exists, then return a warning
        ob_new    <- paste(input$ob_no_1 %>% as.numeric,   
                           input$ob_no_2 %>% as.numeric,   
                           input$ob_no_3 %>% as.numeric,   
                           input$ob_no_4 %>% as.numeric, sep = "/")
        #ob_exists <- paste(dataset$ob_no_1, dataset$ob_no_2, dataset$ob_no_3, dataset$ob_no_4) # What they originally entered, but use below as could have changed. Below is from sit report
        ob_exists <- dataset$ob_no

        
        if(!is.null(dataset$ob_no_1)){
          if(ob_new %in% ob_exists){
            
            out <- HTML("<h4><span style='color:red'>This O.B. number already exists.</span></h4>")
          }
        }
        
      }
      
      out
      
    })
    
  })
  
  
}


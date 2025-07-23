# Situation Report Module


sum_figures_UI <- function(id) {
  ns <- NS(id)
  
  div(
    
    fluidRow(
      column(12, align = "center",
             uiOutput(ns("fig_cntrl_dates_ui"))
      )
    ),
    
    fluidRow(infoBoxOutput(ns("N_crashes"), width = 3),
             infoBoxOutput(ns("N_fatal_injuries"), width = 3),
             infoBoxOutput(ns("N_serious_injuries"), width = 3),
             infoBoxOutput(ns("N_slight_injuries"), width = 3)
    ),
    
    fluidRow(
      box(title = "Number of Crashes by Month", 
          plotlyOutput(ns("crashes_by_day_type"),
                       height = "300px")),
      box(title = "Number of Crashes by Station", 
          plotlyOutput(ns("crashes_by_station"),
                       height = "300px")),
    ),
    
    fluidRow(
      box(title = "Percent of Crashes by Cause Codes", 
          plotOutput(ns("crashes_by_causecode"),
                     height = "500px"),
          width = 12),
    ),
    fluidRow(
      column(10, offset = 1,
             box(title = "Crashes by Time of Day", 
                 fluidRow(
                   column(width = 4, offset = 4, 
                          selectInput(ns("crashes_by_time_station"),
                                      "Station",
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
                                      selected = "All")
                   )
                 ),
                 
                 plotOutput(ns("crashes_by_time"),
                            height = "550px"),
                 width = 12),
      )
    )
  )
  
  
  
  
}

# SERVER ****************************************** ============================
sum_figures_Server <- function(input, output, session) {
  
  # ** Control/Inputs -------------------------------------------------------------
  output$fig_cntrl_dates_ui <- renderUI({
    
    dateRangeInput(session$ns("fig_cntrl_dates"), 
                   "Choose Date Range",
                   start = as.Date("2020-01-01"),
                   end = Sys.Date(),
                   label = NULL,
                   language = "en-GB",
                   format = "dd/mm/yyyy")
    
  })
  
  # ** Prep Data ---------------------------------------------------------------
  observe({
    tmp <- update_figures_r$value
    update_figures_r$value <<- NULL
    
    # Load Data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    if(TESTING_SYSTEM){
      
      # Need to check file exists; when first create app without data throws an error
      if(!file.exists(file.path("data", "police_data_test.Rds")) | !exists("data_key")){
        data_figs <- data.frame(NULL) %>%
          mutate(uid = "")
      } else{
        data_figs <- readRDS(file.path("data", "police_data_test.Rds")) %>%
          arrange(desc(last_updated)) %>%
          distinct(uid, .keep_all = T)
      }
      
      
      
    } else{
      if(!file.exists(file.path("data", "police_data.Rds")) | !exists("data_key")){
        data_figs <- data.frame(NULL) %>%
          mutate(uid = "")
      } else{
        data_figs <- unserialize(aes_cbc_decrypt(readRDS(file.path("data", "police_data.Rds")), key = data_key)) %>%
          arrange(desc(last_updated)) %>%
          distinct(uid, .keep_all = T)
      } 
      
      
    }
    
    ## Only Use Sit Reports
    #data_figs <- data_figs[data_figs$report_type %in% "sit_report",]
    
    ## Prep variables
    if(nrow(data_figs) > 0){
      
      # Subset - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ## Remove crashes if reported happening after today's date
      #data_figs <- data_figs[data_figs$accident_date <= as.character(Sys.Date()),]
      data_figs$accident_date <- data_figs$accident_date %>% ymd()
      
      ## Remove crashes based on controls
      data_figs <- data_figs[!is.na(data_figs$accident_date),]
      data_figs <- data_figs[data_figs$accident_date >= input$fig_cntrl_dates[1],]
      data_figs <- data_figs[data_figs$accident_date <= input$fig_cntrl_dates[2],]
      
      # Prep Data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      data_figs_list <- prep_data_for_figures(data_figs)
      data_figs <- data_figs_list$dataset
      victim_df <- data_figs_list$victim_df
      prtcpnt_df <- data_figs_list$prtcpnt_df
      
    } else{
      data_figs <- data.frame(NULL) %>%
        dplyr::mutate(worst_injury = "",
                      worst_injury_color = "",
                      accident_date = "",
                      accident_date = "",
                      cause_code_category = "",
                      cause_code = "",
                      police_division = "",
                      cause_code_value = "")
      
      victim_df <- data.frame(NULL) %>%
        dplyr::mutate(injurytype = "")
      
      prtcpnt_df <- data.frame(NULL) %>%
        dplyr::mutate(uid = "")
    }
    
    # ** Info Boxes --------------------------------------------------------------
    output$N_crashes <- renderInfoBox({
      valueBox(
        nrow(data_figs), "Crashes", icon = icon("car-crash"),
        color = "black"
      )
    })
    
    output$N_fatal_injuries <- renderInfoBox({
      valueBox(
        sum(victim_df$injurytype %in% "Fatal"), 
        "Fatal Injuries", icon = icon("car-crash"),
        color = "red"
      )
    })
    
    output$N_serious_injuries <- renderInfoBox({
      valueBox(
        sum(victim_df$injurytype %in% "Serious"), 
        "Serious Injuries", icon = icon("car-crash"),
        color = "blue"
      )
    })
    
    output$N_slight_injuries <- renderInfoBox({
      valueBox(
        sum(victim_df$injurytype %in% "Slight"), 
        "Slight Injuries", icon = icon("car-crash"),
        color = "green"
      )
    })
    
    # ** Figure: Crashes by Day and Type ------------------------------------------
    output$crashes_by_day_type <- renderPlotly({
      req(input$fig_cntrl_dates)
      
      #### Prep Data
      data_figs_dates <- data_figs %>%
        
        dplyr::filter(!is.na(worst_injury)) %>%
        
        mutate(accident_year = accident_date %>% year(),
               accident_month = accident_date %>% 
                 month(label=T, abbr=F),
               accident_quarter = case_when(accident_month %in% c("January", "February", "March")     ~ "January - March",
                                            accident_month %in% c("April",   "May",      "June")      ~ "April - June",
                                            accident_month %in% c("July",    "August",   "September") ~ "July - September",
                                            accident_month %in% c("October", "November", "December")  ~ "October - December")) %>%
        
        group_by(accident_year, accident_month, worst_injury, worst_injury_color) %>%
        summarise(N = n()) %>%
        ungroup() %>%
        
        dplyr::select(accident_month, N, worst_injury, worst_injury_color) %>%
        dplyr::rename(Type = worst_injury)
      
      ### Order for worst_injury_color vector
      data_figs_dates$ordervar <- NA
      data_figs_dates$ordervar[data_figs_dates$Type %in% "Fatal"] <- 1
      data_figs_dates$ordervar[data_figs_dates$Type %in% "Serious"] <- 2
      data_figs_dates$ordervar[data_figs_dates$Type %in% "Slight"] <- 3
      data_figs_dates$ordervar[data_figs_dates$Type %in% "Non-Injury"] <- 4
      
      data_figs_dates <- data_figs_dates %>%
        arrange(ordervar)
      
      ### Figure
      p <- ggplot() +
        geom_col(data=data_figs_dates,
                 aes(x=accident_month, y=N, group=Type, fill=Type),
                 position = position_dodge2(width = 1, preserve = "single"),
                 width=.5) +
        theme_minimal() +
        labs(x="",
             y="",
             title = "",
             fill = "") +
        scale_fill_manual(values = unique(data_figs_dates$worst_injury_color)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      ggplotly(p, tooltip = c("N")) %>%
        layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>%
        plotly::config(displayModeBar = F)
      
    })
    
    # ** Figure: Crashes by Cause Code --------------------------------------------
    output$crashes_by_causecode <- renderPlot({
      req(input$fig_cntrl_dates)
      
      df <- data_figs %>%
        mutate(one = 1) %>%
        filter(!is.na(cause_code_value) & !is.na(cause_code_category))
      
      # Shorten some cause codes
      df <- df %>%
        mutate(cause_code_value = cause_code_value %>% as.character) %>%
        mutate(cause_code_value = case_when(cause_code_value %in% "Pulling out from near side or from one traffic lane (not from parking area) to another without due care)" ~ 
                                              "Pulling out from near side or from one traffic lane without due care",
                                            TRUE ~ cause_code_value))
      
      p <- ggplot(df,
                  aes(x = cause_code_value)) +
        geom_bar(
          aes(y = (..count..)/sum(..count..),
              #group = cause_code_category,
              fill = cause_code_category)) +
        geom_text(aes(group = one, label = scales::percent(..prop..),
                      y= ..prop.. ), stat= "count", hjust = 1,
                  size = 6) +
        coord_flip() +
        labs(title = "",
             x="",
             y="",
             fill="Cause Code\nCategory") +
        #scale_fill_manual(values = c("chocolate1", "dodgerblue2", "gray61")) +
        theme_minimal() +
        theme(axis.text.y = element_text(face = "bold", size = 14, color = "black"),
              axis.text.x = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 14, face = "bold")) +
        scale_y_continuous(labels = scales::percent) 
      
      p 
      #ggplotly(p, tooltip = c(NULL)) %>%
      #  layout(legend = list(orientation = "v", x = 1, y = 0.5)) %>%
      #  layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>%
      #  config(displayModeBar = F)
      
    }, bg="transparent")
    
    # ** Figure: Crashes by Station --------------------------------------------
    output$crashes_by_station <- renderPlotly({
      req(input$fig_cntrl_dates)
      
      df <- data_figs %>%
        filter(!is.na(worst_injury)) %>%
        filter(!is.na(police_division)) %>%
        filter(police_division != ".") %>%
        group_by(police_division, worst_injury, worst_injury_color) %>%
        summarise(N = n()) %>%
        ungroup() %>%
        group_by(police_division) %>%
        mutate(N_division = n()) %>%
        ungroup()
      
      color_df <- df %>%
        distinct(worst_injury, worst_injury_color) %>%
        mutate(worst_injury = factor(worst_injury, 
                                     levels = c("Fatal", "Serious", "Slight", "Non-Injury"), ordered=T)) %>%
        arrange(worst_injury)
      
      p <- ggplot(df) +
        geom_bar(aes(x = reorder(police_division, N_division),
                     y = N,
                     fill = worst_injury),
                 stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = color_df$worst_injury_color) +
        labs(x = "",
             y = "",
             fill = "") +
        theme_minimal()
      
      ggplotly(p, tooltip = c(NULL)) %>%
        layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>%
        plotly::config(displayModeBar = F)
      
    })
    
    # ** Figure: Crashes by Time of Day ----------------------------------------
    output$crashes_by_time <- renderPlot({
      req(input$fig_cntrl_dates)
      req(input$crashes_by_time_station)
      
      if(input$crashes_by_time_station != "All"){
        data_figs <- data_figs[data_figs$police_division %in% input$crashes_by_time_station,]
      }
      
      ####
      data_figs <- data_figs[data_figs$worst_injury_color != "",]
      
      df_type <- data_figs %>%
        dplyr::filter(!is.na(accident_time_hour)) %>%
        dplyr::mutate(worst_injury = accident_type) %>%
        #dplyr::mutate(accident_time_hour = accident_time_hour %>% factor(levels = as.character(0:23))) %>%
        dplyr::group_by(accident_time_hour, worst_injury, worst_injury_color) %>%
        dplyr::summarise(N = n()) %>%
        dplyr::ungroup() %>%
        # tidyr::complete(nesting(accident_time_hour = as.character(0:23),
        #                         worst_injury,
        #                         worst_injury_color),
        #                 fill = list(N = 0)) %>%
        tidyr::complete(accident_time_hour = as.character(0:23),
                        nesting(worst_injury,
                                worst_injury_color),
                        fill = list(N = 0)) %>%
        dplyr::mutate(accident_time_hour = accident_time_hour %>% as.numeric %>% factor(levels = as.character(0:23))) %>%
        distinct()
      
      df_all <- data_figs %>%
        dplyr::filter(!is.na(accident_time_hour)) %>%
        #dplyr::mutate(accident_time_hour = accident_time_hour %>% factor(levels = as.character(0:23))) %>%
        dplyr::group_by(accident_time_hour) %>%
        dplyr::summarise(N = n()) %>%
        #tidyr::complete(nesting(accident_time_hour = as.character(0:23)),
        #                fill = list(N = 0)) %>%
        mutate(accident_time_hour = accident_time_hour %>% as.numeric %>% factor(levels = as.character(0:23))) 
      
      # Order colors, but account for circumstances where not all crash
      # types present in data
      color_order <- c("black",
                       "#27b376",
                       "#264b96",
                       "#f24224")
      colors_in_data <- unique(df_type$worst_injury_color)
      colors_in_data <- colors_in_data[order(match(colors_in_data, color_order))]
      
      rect_data_day <- data.frame(min = factor("6", levels = as.character(0:23)),
                                  max = factor("19", levels = as.character(0:23)))
      
      bkgnd_df1 <- data.frame(x = factor(as.character(0:6), levels = as.character(0:23)),
                              y = Inf)
      bkgnd_df2 <- data.frame(x = factor(as.character(19:23), levels = as.character(0:23)),
                              y = Inf)
      
      p <- ggplot() +
        geom_col(data = df_type,
                 aes(x = accident_time_hour,
                     y = N,
                     group = worst_injury,
                     fill = worst_injury)) +
        geom_col(data = bkgnd_df1,
                 aes(x = x,
                     y = y),
                 fill = "gray30",
                 width = 1,
                 alpha = 0.1) +
        geom_col(data = bkgnd_df2,
                 aes(x = x,
                     y = y),
                 fill = "gray30",
                 width = 1,
                 alpha = 0.1) +
        geom_rect(data = rect_data_day,
                  aes(xmin=min,
                      xmax=max,
                      ymin=0, ymax=Inf),
                  fill = "yellow",
                  alpha = 0.1) +
        geom_col(data = df_type,
                 aes(x = accident_time_hour,
                     y = N,
                     group = worst_injury,
                     fill = worst_injury)) +
        scale_fill_manual(values = rev(colors_in_data)) +
        geom_text(data = df_all,
                  aes(label = N,
                      x = accident_time_hour,
                      y = N),
                  fontface = "bold",
                  size = 5,
                  nudge_y = 0.25) +
        labs(x = NULL,
             y = NULL,
             fill = "Crash Type") +
        coord_polar() +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.text.x = element_text(size = 14),
              legend.title = element_text(size = 14, face = "bold"), 
              legend.text = element_text(size = 14))
      
      
      
      p
      
    }, bg="transparent")
    
    
    
    
    
    
    
    
    
    
  })
  
}




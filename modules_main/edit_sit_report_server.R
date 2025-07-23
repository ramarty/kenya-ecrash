
## ** General ----------------------------------------------------------------
output$crash_info_sr <- renderUI({
  
  fluidRow(
    
    fluidRow(
      column(6,
             selectInput(session$ns("police_division"), "Police Division", 
                         choices = c(".",
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
                         selected=check_selected_period(data_i[["police_division"]])),
      ),
      column(6,
             textInput(session$ns("ob_no"), "O.B. Num", check_selected_blanktext(data_i[["ob_no"]]))
      )
    ),
    fluidRow(
      column(6, offset = 6,
             htmlOutput(session$ns("ob_already_exits"))
      )
    ),
    #fluidRow(
    #  column(6,
    #         textInput(session$ns("iar_no"), "IAR Num", check_selected_blanktext(data_i[["iar_no"]]))
    #  )
    #),
    #fluidRow(
    #  column(6,
    #         htmlOutput(session$ns("iar_already_exits"))
    #  )
    #),
    
    fluidRow(
      
      column(6,
             selectInput(session$ns("accident_type"), "What is the accident type?", 
                         choices = c(".",
                                     "Non-Injury",
                                     "Slight",
                                     "Serious",
                                     "Fatal"),
                         selected=check_selected_period(data_i[["accident_type"]]))
      ),
      column(6,
             selectInput(session$ns("hit_and_run"), "Was the crash a hit and run?", 
                         choices = c(".",
                                     "Yes",
                                     "No"),
                         selected=check_selected_period(data_i[["hit_and_run"]]))
      )
    ),
    
    fluidRow(
      column(6,
             dateInput(session$ns("accident_date"), "Date of Accident", value=check_selected_date(data_i[["accident_date"]]),
                       language = "en-GB",
                       format = "dd/mm/yyyy")
      ),
      column(6,
             fluidRow(
               column(6,
                      numericInput(session$ns("accident_time_hour"),"Accident Time - Hour, 0-23",min=0,max=23,step=1, value=check_selected_numeric(data_i[["accident_time_hour"]]), width="100%")
               ),
               column(6,
                      numericInput(session$ns("accident_time_minute"),"Minute",min=0,max=59,step=1, value=check_selected_numeric(data_i[["accident_time_minute"]]), width="100%")
               )
             ),
      )
    ),
    
    fluidRow(
      column(6,
             dateInput(session$ns("accident_reported_date"), 
                       HTML("Date Accident Reported <em>(This is the date the Sit Report is entered)</em>"), 
                       value=check_selected_date(data_i[["accident_reported_date"]]),
                       language = "en-GB",
                       format = "dd/mm/yyyy")
      ),
      column(6,
             fluidRow(
               column(6,
                      numericInput(session$ns("accident_reported_time_hour"),"Accident Reported Time - Hour, 0-23",min=0,max=23,step=1, value=check_selected_numeric(data_i[["accident_reported_time_hour"]]), width="100%")
               ),
               column(6,
                      numericInput(session$ns("accident_reported_time_minute"),"Minute",min=0,max=59,step=1, value=check_selected_numeric(data_i[["accident_reported_time_minute"]]), width="100%")
               )
             ),
      )
    ),
    
    fluidRow(
      column(6,
             textInput(session$ns("road_name"), "Road Name", check_selected_blanktext(data_i[["road_name"]]))
      ),
      column(6,
             textInput(session$ns("landmark"), "Nearby Landmark (e.g, store, matatu stage)", check_selected_blanktext(data_i[["landmark"]]))
      )
    ),
    
    fluidRow(
      column(12, strong("Officer who Responded to Crash")),
    ),
    fluidRow(
      column(3, textInput(session$ns("officer_rspnd_crash_title"), "Title", check_selected_blanktext(data_i[["officer_rspnd_crash_title"]]))),
      column(3, textInput(session$ns("officer_rspnd_crash_firstname"), "First Name", check_selected_blanktext(data_i[["officer_rspnd_crash_firstname"]]))),
      column(3, textInput(session$ns("officer_rspnd_crash_surname"), "Surname", check_selected_blanktext(data_i[["officer_rspnd_crash_surname"]]))),
      column(3, textInput(session$ns("officer_rspnd_crash_snumber"), "Service No.", check_selected_blanktext(data_i[["officer_rspnd_crash_surname"]])))
    ),
    
    fluidRow(
      column(12, strong("Officer who Filled Out Form")),
    ),
    fluidRow(
      column(3, textInput(session$ns("officer_filled_form_title"), "Title", check_selected_blanktext(data_i[["officer_filled_form_title"]]))),
      column(3, textInput(session$ns("officer_filled_form_firstname"), "First Name", check_selected_blanktext(data_i[["officer_filled_form_firstname"]]))),
      column(3, textInput(session$ns("officer_filled_form_surname"), "Surname", check_selected_blanktext(data_i[["officer_filled_form_surname"]]))),
      column(3, textInput(session$ns("officer_filled_form_snumber"), "Service No.", check_selected_blanktext(data_i[["officer_filled_form_surname"]])))
      
    )
  )
  
})

# ** Participants ------------------------------------------------------------
gen_participant_qs_sr <- function(i){
  renderUI({
    if (!(input$N_participants >= i)) return(NULL) else {
      
      div(
        fluidRow(column(4,
                        ""),
                 column(5,
                        h3(paste0("Pariticipant ", i)))
        ),
        fluidRow(
          column(4,
                 radioButtons(session$ns(paste0("prtcpnt_vehpart_type_", i)), "Vehicle/Participant Type",
                              choices = c("Saloon Car",
                                          "Pickup van",
                                          "Lorry",
                                          "Lorry + trailer",
                                          "Bus",
                                          "Matatu",
                                          "Motor-cycle",
                                          "Bicycle",
                                          "Other Vehicle",
                                          "Pedestrian",
                                          "Unknown"),
                              selected = check_selected_character0(data_i[[paste0("prtcpnt_vehpart_type_", i)]]) ) 
          ),
          
          column(8,
                 
                 column(12,
                        textInput(session$ns(paste0("prtcpnt_name_", i)), "Name", check_selected_blanktext(data_i[[paste0("prtcpnt_name_", i)]]))
                 ),
                 column(12,
                        numericInput(session$ns(paste0("prtcpnt_age_years_", i)), "Age", check_selected_numeric(data_i[[paste0("prtcpnt_age_years_", i)]]))
                 ),
                 
                 column(12,
                        textInput(session$ns(paste0("prtcpnt_telephone_", i)), "Telephone", check_selected_blanktext(data_i[[paste0("prtcpnt_telephone_", i)]]))
                 ),
                 uiOutput(session$ns(paste0("telephone_length_warning_", i))),
                 
                 # column(12,
                 #        textInput(session$ns(paste0("prtcpnt_regnumber_", i)), "Registration Number", check_selected_blanktext(data_i[[paste0("prtcpnt_regnumber_", i)]]))
                 # ),
                 
                 uiOutput(session$ns(paste0("prtcpnt_reg_no_qs_sr_", i))),
                 
                 column(12,
                        textInput(session$ns(paste0("prtcpnt_nationalid_", i)), "National ID Number", check_selected_blanktext(data_i[[paste0("prtcpnt_nationalid_", i)]]))
                 ),
                 column(12,
                        selectInput(session$ns(paste0("prtcpnt_nationality_", i)), "Nationality",
                                    choices = c(".","Kenyan","Other"),
                                    selected = check_selected_period(data_i[[paste0("prtcpnt_nationality_", i)]]))
                 ),
                 uiOutput(session$ns(paste0("prtcpnt_nationality_other_qs_sr_", i))),
                 
                 uiOutput(session$ns(paste0("prtcpnt_ped_pushcart_qs_sr_", i))),
                 
                 
                 column(12,
                        radioButtons(session$ns(paste0("prtcpnt_gender_", i)), "Gender",
                                     choices = c("Male","Female"),
                                     selected = check_selected_character0(data_i[[paste0("prtcpnt_gender_", i)]]),
                                     inline=T)
                 ),
                 column(12,
                        radioButtons(session$ns(paste0("prtcpnt_police_officer_", i)), "Is the participant a police officer?",
                                     choices = c("Yes","No"),
                                     selected = check_selected_character0(data_i[[paste0("prtcpnt_police_officer_", i)]]),
                                     inline=T)
                 ),
                 uiOutput(session$ns(paste0("prtcpnt_part_officer_sn_qs_sr_", i))),

          ),
          
          uiOutput(session$ns(paste0("prtcpnt_sacco_qs_sr_", i))),
          uiOutput(session$ns(paste0("prtcpnt_veh_make_model_qs_sr_", i))),
          
          column(8,
                 selectInput(session$ns(paste0("prtcpnt_injured_yn_", i)), "Was the participant injured?",
                             choices = c(".","Yes","No"),
                             selected = check_selected_period(data_i[[paste0("prtcpnt_injured_yn_", i)]]))
          )
          
        ),
        
        uiOutput(session$ns(paste0("prtcpnt_injured_qs_sr_", i))),
        uiOutput(session$ns(paste0("prtcpnt_injured_fatal_qs_sr_", i)))
      )
    }
  })
}

output$participant_main_qs_sr <- renderUI({
  
  if(is.null(input$N_participants)){
    N_participants_vector <- 1
  } else{
    N_participants_vector <- 1:input$N_participants
  }
  
  do.call(tabsetPanel, 
          lapply(N_participants_vector, 
                 function(i) tabPanel(i, gen_participant_qs_sr(i) )))
})

# ** Participant Injuried ------------------------------------------------------

#### Participant Injury Questions
gen_part_injury_qs_sr <- function(i){
  renderUI({
    
    part_i_injured_yn <- input[[paste0("prtcpnt_injured_yn_", i)]]
    
    if (!part_i_injured_yn %in% 'Yes') return(NULL) else {
      div(
        fluidRow(
          column(12,
                 radioButtons(session$ns(paste0("prtcpnt_injurytype_", i)), "Injury Type",
                              choices = c("Fatal","Serious", "Slight"),
                              selected=check_selected_character0(data_i[[paste0("prtcpnt_injurytype_", i)]]),
                              inline=T)
          )
        ),
        fluidRow(
        ),
        fluidRow(
          column(6,
                 textInput(session$ns(paste0("prtcpnt_hospital_taken_", i)), "Hospital Taken To", check_selected_blanktext(data_i[[paste0("prtcpnt_hospital_taken_", i)]]))
          ),
          column(6,
                 textInput(session$ns(paste0("prtcpnt_hospital_admno_", i)), "Hospital Admission No.", check_selected_blanktext(data_i[[paste0("prtcpnt_hospital_admno_", i)]]))
          ),
        ),
      )
    }
  })
}

output$prtcpnt_injured_qs_sr_1 <- gen_part_injury_qs_sr(1)
output$prtcpnt_injured_qs_sr_2 <- gen_part_injury_qs_sr(2)
output$prtcpnt_injured_qs_sr_3 <- gen_part_injury_qs_sr(3)
output$prtcpnt_injured_qs_sr_4 <- gen_part_injury_qs_sr(4)
output$prtcpnt_injured_qs_sr_5 <- gen_part_injury_qs_sr(5)
output$prtcpnt_injured_qs_sr_6 <- gen_part_injury_qs_sr(6)
output$prtcpnt_injured_qs_sr_7 <- gen_part_injury_qs_sr(7)
output$prtcpnt_injured_qs_sr_8 <- gen_part_injury_qs_sr(8)
output$prtcpnt_injured_qs_sr_9 <- gen_part_injury_qs_sr(9)
output$prtcpnt_injured_qs_sr_10 <- gen_part_injury_qs_sr(10)
output$prtcpnt_injured_qs_sr_11 <- gen_part_injury_qs_sr(11)
output$prtcpnt_injured_qs_sr_12 <- gen_part_injury_qs_sr(12)
output$prtcpnt_injured_qs_sr_13 <- gen_part_injury_qs_sr(13)
output$prtcpnt_injured_qs_sr_14 <- gen_part_injury_qs_sr(14)
output$prtcpnt_injured_qs_sr_15 <- gen_part_injury_qs_sr(15)
output$prtcpnt_injured_qs_sr_16 <- gen_part_injury_qs_sr(16)
output$prtcpnt_injured_qs_sr_17 <- gen_part_injury_qs_sr(17)
output$prtcpnt_injured_qs_sr_18 <- gen_part_injury_qs_sr(18)
output$prtcpnt_injured_qs_sr_19 <- gen_part_injury_qs_sr(19)
output$prtcpnt_injured_qs_sr_20 <- gen_part_injury_qs_sr(20)



# ** Participant Injuried - Fatal ----------------------------------------------

gen_part_injury_fatal_qs_sr <- function(i){
  renderUI({
    
    #part_i_injured_yn <- input[[paste0("prtcpnt_injured_yn_", i)]]
    part_i_injured_type <- input[[paste0("prtcpnt_injurytype_", i)]]
    
    if ( (!('Fatal' %in% part_i_injured_type)) ) return(NULL) else {
      div(
        fluidRow(
          column(6,
                 textInput(session$ns(paste0("prtcpnt_mortuary_taken_", i)), "Mortuary Taken To", check_selected_blanktext(data_i[[paste0("prtcpnt_mortuary_taken_", i)]]))
          ),
          column(6,
                 textInput(session$ns(paste0("prtcpnt_mortuary_admno_", i)), "Mortuary Admission No.", check_selected_blanktext(data_i[[paste0("prtcpnt_mortuary_admno_", i)]]))
          ),
        )
      )
    }
  })
}

output$prtcpnt_injured_fatal_qs_sr_1 <- gen_part_injury_fatal_qs_sr(1)
output$prtcpnt_injured_fatal_qs_sr_2 <- gen_part_injury_fatal_qs_sr(2)
output$prtcpnt_injured_fatal_qs_sr_3 <- gen_part_injury_fatal_qs_sr(3)
output$prtcpnt_injured_fatal_qs_sr_4 <- gen_part_injury_fatal_qs_sr(4)
output$prtcpnt_injured_fatal_qs_sr_5 <- gen_part_injury_fatal_qs_sr(5)
output$prtcpnt_injured_fatal_qs_sr_6 <- gen_part_injury_fatal_qs_sr(6)
output$prtcpnt_injured_fatal_qs_sr_7 <- gen_part_injury_fatal_qs_sr(7)
output$prtcpnt_injured_fatal_qs_sr_8 <- gen_part_injury_fatal_qs_sr(8)
output$prtcpnt_injured_fatal_qs_sr_9 <- gen_part_injury_fatal_qs_sr(9)
output$prtcpnt_injured_fatal_qs_sr_10 <- gen_part_injury_fatal_qs_sr(10)
output$prtcpnt_injured_fatal_qs_sr_11 <- gen_part_injury_fatal_qs_sr(11)
output$prtcpnt_injured_fatal_qs_sr_12 <- gen_part_injury_fatal_qs_sr(12)
output$prtcpnt_injured_fatal_qs_sr_13 <- gen_part_injury_fatal_qs_sr(13)
output$prtcpnt_injured_fatal_qs_sr_14 <- gen_part_injury_fatal_qs_sr(14)
output$prtcpnt_injured_fatal_qs_sr_15 <- gen_part_injury_fatal_qs_sr(15)
output$prtcpnt_injured_fatal_qs_sr_16 <- gen_part_injury_fatal_qs_sr(16)
output$prtcpnt_injured_fatal_qs_sr_17 <- gen_part_injury_fatal_qs_sr(17)
output$prtcpnt_injured_fatal_qs_sr_18 <- gen_part_injury_fatal_qs_sr(18)
output$prtcpnt_injured_fatal_qs_sr_19 <- gen_part_injury_fatal_qs_sr(19)
output$prtcpnt_injured_fatal_qs_sr_20 <- gen_part_injury_fatal_qs_sr(20)

# ** Victims -----------------------------------------------------------------
gen_victim_qs_sr <- function(i){
  renderUI({
    
    if(!is.numeric(input$N_victims)){
      return(NULL)
    } else if(!(input$N_victims >= i)){
      return(NULL)
    } else{
      
      #if (!(input$N_victims >= i)) return(NULL) else {
      
      div(
        hr(),
        h3(paste0("Victim ",i)),
        fluidRow(
          column(4,
                 textInput(session$ns(paste0("victim_name_",i)), "Name", check_selected_blanktext(data_i[[paste0("victim_name_", i)]]))
          ),
          column(4,
                 numericInput(session$ns(paste0("victim_age_years_",i)), "Age", check_selected_numeric(data_i[[paste0("victim_age_years_", i)]]))
          ),
          column(4,
                 selectInput(session$ns(paste0("victim_gender_",i)), "Gender", choices=c(".","Male","Female"), selected=check_selected_period(data_i[[paste0("victim_gender_", i)]]))
          )
        ),
        fluidRow(
          column(12,
                 radioButtons(session$ns(paste0("victim_personclass_",i)), "Class of Person", 
                              choices = c("Pedestrian",
                                          "Cyclist",
                                          "Motor-cycle",
                                          "Driver",
                                          "Passenger"),
                              selected=check_selected_character0(data_i[[paste0("victim_personclass_", i)]]),
                              inline=T)
          )
        ),
        fluidRow(
          column(12,
                 radioButtons(session$ns(paste0("victim_police_officer_",i)), "Is the victim a police officer?", 
                              choices = c("Yes",
                                          "No"),
                              selected=check_selected_character0(data_i[[paste0("victim_police_officer_", i)]]),
                              inline=T)
          )
        ),
        uiOutput(session$ns(paste0("victim_officer_sn_qs_sr_", i))),
        fluidRow(
          column(2,
                 radioButtons(session$ns(paste0("victim_injurytype_",i)), "Injury Type",
                              choices = c("Fatal","Serious", "Slight"),
                              selected=check_selected_character0(data_i[[paste0("victim_injurytype_", i)]]),
                              inline=T)
          ),
          column(5,
                 textInput(session$ns(paste0("victim_hospital_taken_",i)), "Hospital Taken To", check_selected_blanktext(data_i[[paste0("victim_hospital_taken_", i)]]))
          ),
          column(5,
                 textInput(session$ns(paste0("victim_hospital_admno_", i)), "Hospital Admission No.", check_selected_blanktext(data_i[[paste0("victim_hospital_admno_", i)]]))
          )
        ),
      )
    }
  })
}

output$victim_1_qs_sr <- gen_victim_qs_sr(1)
output$victim_2_qs_sr <- gen_victim_qs_sr(2)
output$victim_3_qs_sr <- gen_victim_qs_sr(3)
output$victim_4_qs_sr <- gen_victim_qs_sr(4)
output$victim_5_qs_sr <- gen_victim_qs_sr(5)
output$victim_6_qs_sr <- gen_victim_qs_sr(6)
output$victim_7_qs_sr <- gen_victim_qs_sr(7)
output$victim_8_qs_sr <- gen_victim_qs_sr(8)
output$victim_9_qs_sr <- gen_victim_qs_sr(9)
output$victim_10_qs_sr <- gen_victim_qs_sr(10)
output$victim_11_qs_sr <- gen_victim_qs_sr(11)
output$victim_12_qs_sr <- gen_victim_qs_sr(12)
output$victim_13_qs_sr <- gen_victim_qs_sr(13)
output$victim_14_qs_sr <- gen_victim_qs_sr(14)
output$victim_15_qs_sr <- gen_victim_qs_sr(15)
output$victim_16_qs_sr <- gen_victim_qs_sr(16)
output$victim_17_qs_sr <- gen_victim_qs_sr(17)
output$victim_18_qs_sr <- gen_victim_qs_sr(18)
output$victim_19_qs_sr <- gen_victim_qs_sr(19)
output$victim_20_qs_sr <- gen_victim_qs_sr(20)

# ** Victim Injuried - Fatal ---------------------------------------------------

gen_victim_fatal_qs_sr <- function(i){
  renderUI({
    
    victim_i_injured_type <- input[[paste0("victim_injurytype_", i)]]
    
    if(!is.numeric(input$N_victims)){
      return(NULL)
    } else if(!(input$N_victims >= i)){
      return(NULL)
    } else{
      
      if (!('Fatal' %in% victim_i_injured_type)) return(NULL) else {
        div(
          fluidRow(
            column(6,
                   textInput(session$ns(paste0("victim_mortuary_taken_", i)), "Mortuary Taken To", check_selected_blanktext(data_i[[paste0("victim_mortuary_taken_", i)]]))
            ),
            column(6,
                   textInput(session$ns(paste0("victim_mortuary_admno_", i)), "Mortuary Admission No.", check_selected_blanktext(data_i[[paste0("victim_mortuary_admno_", i)]]))
            ),
          )
        )
      }
      
    }
  })
}

output$victim_fatal_1_qs_sr <- gen_victim_fatal_qs_sr(1)
output$victim_fatal_2_qs_sr <- gen_victim_fatal_qs_sr(2)
output$victim_fatal_3_qs_sr <- gen_victim_fatal_qs_sr(3)
output$victim_fatal_4_qs_sr <- gen_victim_fatal_qs_sr(4)
output$victim_fatal_5_qs_sr <- gen_victim_fatal_qs_sr(5)
output$victim_fatal_6_qs_sr <- gen_victim_fatal_qs_sr(6)
output$victim_fatal_7_qs_sr <- gen_victim_fatal_qs_sr(7)
output$victim_fatal_8_qs_sr <- gen_victim_fatal_qs_sr(8)
output$victim_fatal_9_qs_sr <- gen_victim_fatal_qs_sr(9)
output$victim_fatal_10_qs_sr <- gen_victim_fatal_qs_sr(10)
output$victim_fatal_11_qs_sr <- gen_victim_fatal_qs_sr(11)
output$victim_fatal_12_qs_sr <- gen_victim_fatal_qs_sr(12)
output$victim_fatal_13_qs_sr <- gen_victim_fatal_qs_sr(13)
output$victim_fatal_14_qs_sr <- gen_victim_fatal_qs_sr(14)
output$victim_fatal_15_qs_sr <- gen_victim_fatal_qs_sr(15)
output$victim_fatal_16_qs_sr <- gen_victim_fatal_qs_sr(16)
output$victim_fatal_17_qs_sr <- gen_victim_fatal_qs_sr(17)
output$victim_fatal_18_qs_sr <- gen_victim_fatal_qs_sr(18)
output$victim_fatal_19_qs_sr <- gen_victim_fatal_qs_sr(19)
output$victim_fatal_20_qs_sr <- gen_victim_fatal_qs_sr(20)

# ** Participant Police Officer Service Number ---------------------------------
gen_part_officer_sn_qs_sr <- function(i){
  renderUI({
    if (!("Yes" %in% input[[paste0("prtcpnt_police_officer_", i)]]) ) return(NULL) else {
      #fluidRow(
      column(12,
             textInput(session$ns(paste0("prtcpnt_police_officer_serviceno_", i)), "Police Officer Service Number", check_selected_blanktext(data_i[[paste0("prtcpnt_police_officer_serviceno_", i)]]))
      )
      #)
    }
  })
}

output$prtcpnt_part_officer_sn_qs_sr_1 <- gen_part_officer_sn_qs_sr(1)
output$prtcpnt_part_officer_sn_qs_sr_2 <- gen_part_officer_sn_qs_sr(2)
output$prtcpnt_part_officer_sn_qs_sr_3 <- gen_part_officer_sn_qs_sr(3)
output$prtcpnt_part_officer_sn_qs_sr_4 <- gen_part_officer_sn_qs_sr(4)
output$prtcpnt_part_officer_sn_qs_sr_5 <- gen_part_officer_sn_qs_sr(5)
output$prtcpnt_part_officer_sn_qs_sr_6 <- gen_part_officer_sn_qs_sr(6)
output$prtcpnt_part_officer_sn_qs_sr_7 <- gen_part_officer_sn_qs_sr(7)
output$prtcpnt_part_officer_sn_qs_sr_8 <- gen_part_officer_sn_qs_sr(8)
output$prtcpnt_part_officer_sn_qs_sr_9 <- gen_part_officer_sn_qs_sr(9)
output$prtcpnt_part_officer_sn_qs_sr_10 <- gen_part_officer_sn_qs_sr(10)
output$prtcpnt_part_officer_sn_qs_sr_11 <- gen_part_officer_sn_qs_sr(11)
output$prtcpnt_part_officer_sn_qs_sr_12 <- gen_part_officer_sn_qs_sr(12)
output$prtcpnt_part_officer_sn_qs_sr_13 <- gen_part_officer_sn_qs_sr(13)
output$prtcpnt_part_officer_sn_qs_sr_14 <- gen_part_officer_sn_qs_sr(14)
output$prtcpnt_part_officer_sn_qs_sr_15 <- gen_part_officer_sn_qs_sr(15)
output$prtcpnt_part_officer_sn_qs_sr_16 <- gen_part_officer_sn_qs_sr(16)
output$prtcpnt_part_officer_sn_qs_sr_17 <- gen_part_officer_sn_qs_sr(17)
output$prtcpnt_part_officer_sn_qs_sr_18 <- gen_part_officer_sn_qs_sr(18)
output$prtcpnt_part_officer_sn_qs_sr_19 <- gen_part_officer_sn_qs_sr(19)
output$prtcpnt_part_officer_sn_qs_sr_20 <- gen_part_officer_sn_qs_sr(20)

# ** Victim Police Officer Service Number ---------------------------------
gen_victim_officer_sn_qs_sr <- function(i){
  renderUI({
    if (!("Yes" %in% input[[paste0("victim_police_officer_", i)]]) ) return(NULL) else {
      #fluidRow(
      column(12,
             textInput(session$ns(paste0("victim_police_officer_serviceno_", i)), "Police Officer Service Number", check_selected_blanktext(data_i[[paste0("victim_police_officer_serviceno_", i)]]))
      )
      #)
    }
  })
}

output$victim_officer_sn_qs_sr_1 <- gen_victim_officer_sn_qs_sr(1)
output$victim_officer_sn_qs_sr_2 <- gen_victim_officer_sn_qs_sr(2)
output$victim_officer_sn_qs_sr_3 <- gen_victim_officer_sn_qs_sr(3)
output$victim_officer_sn_qs_sr_4 <- gen_victim_officer_sn_qs_sr(4)
output$victim_officer_sn_qs_sr_5 <- gen_victim_officer_sn_qs_sr(5)
output$victim_officer_sn_qs_sr_6 <- gen_victim_officer_sn_qs_sr(6)
output$victim_officer_sn_qs_sr_7 <- gen_victim_officer_sn_qs_sr(7)
output$victim_officer_sn_qs_sr_8 <- gen_victim_officer_sn_qs_sr(8)
output$victim_officer_sn_qs_sr_9 <- gen_victim_officer_sn_qs_sr(9)
output$victim_officer_sn_qs_sr_10 <- gen_victim_officer_sn_qs_sr(10)
output$victim_officer_sn_qs_sr_11 <- gen_victim_officer_sn_qs_sr(11)
output$victim_officer_sn_qs_sr_12 <- gen_victim_officer_sn_qs_sr(12)
output$victim_officer_sn_qs_sr_13 <- gen_victim_officer_sn_qs_sr(13)
output$victim_officer_sn_qs_sr_14 <- gen_victim_officer_sn_qs_sr(14)
output$victim_officer_sn_qs_sr_15 <- gen_victim_officer_sn_qs_sr(15)
output$victim_officer_sn_qs_sr_16 <- gen_victim_officer_sn_qs_sr(16)
output$victim_officer_sn_qs_sr_17 <- gen_victim_officer_sn_qs_sr(17)
output$victim_officer_sn_qs_sr_18 <- gen_victim_officer_sn_qs_sr(18)
output$victim_officer_sn_qs_sr_19 <- gen_victim_officer_sn_qs_sr(19)
output$victim_officer_sn_qs_sr_20 <- gen_victim_officer_sn_qs_sr(20)

# ** Telephone Number Lenght Warning -------------------------------------------
gen_telephone_length_warning_qs_sr <- function(i){
  renderUI({
    out <- NULL
    
    # Show warning if number if greater than zero and not 10
    if(!is.null(input[[paste0("prtcpnt_telephone_", i)]])){
      if(!is.na(input[[paste0("prtcpnt_telephone_", i)]])){
        if(nchar(input[[paste0("prtcpnt_telephone_", i)]]) > 0){
          if(nchar(input[[paste0("prtcpnt_telephone_", i)]]) != 10){
            
            out <- HTML(paste0("<strong><span style='color:red'>The telephone 
                               number must be 10 digits long. You have entered ",
                               nchar(input[[paste0("prtcpnt_telephone_", i)]]),
                               " digits.</span><strong>"))
            
          }
        }
      }
    }
    
    out
    
  })
}

output$telephone_length_warning_1 <- gen_telephone_length_warning_qs_sr(1)
output$telephone_length_warning_2 <- gen_telephone_length_warning_qs_sr(2)
output$telephone_length_warning_3 <- gen_telephone_length_warning_qs_sr(3)
output$telephone_length_warning_4 <- gen_telephone_length_warning_qs_sr(4)
output$telephone_length_warning_5 <- gen_telephone_length_warning_qs_sr(5)
output$telephone_length_warning_6 <- gen_telephone_length_warning_qs_sr(6)
output$telephone_length_warning_7 <- gen_telephone_length_warning_qs_sr(7)
output$telephone_length_warning_8 <- gen_telephone_length_warning_qs_sr(8)
output$telephone_length_warning_9 <- gen_telephone_length_warning_qs_sr(9)
output$telephone_length_warning_10 <- gen_telephone_length_warning_qs_sr(10)
output$telephone_length_warning_11 <- gen_telephone_length_warning_qs_sr(11)
output$telephone_length_warning_12 <- gen_telephone_length_warning_qs_sr(12)
output$telephone_length_warning_13 <- gen_telephone_length_warning_qs_sr(13)
output$telephone_length_warning_14 <- gen_telephone_length_warning_qs_sr(14)
output$telephone_length_warning_15 <- gen_telephone_length_warning_qs_sr(15)
output$telephone_length_warning_16 <- gen_telephone_length_warning_qs_sr(16)
output$telephone_length_warning_17 <- gen_telephone_length_warning_qs_sr(17)
output$telephone_length_warning_18 <- gen_telephone_length_warning_qs_sr(18)
output$telephone_length_warning_19 <- gen_telephone_length_warning_qs_sr(19)
output$telephone_length_warning_20 <- gen_telephone_length_warning_qs_sr(20)

# ** Hand-Push Cart ---------------------------------------------------------------
gen_ped_pushcart_qs_sr <- function(i){
  renderUI({
    if (!ifelse(length(input[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, ".", 
                input[[paste0("prtcpnt_vehpart_type_", i)]]) 
        %in% c("Pedestrian")) return(NULL) else {
          
          column(12,
                 radioButtons(session$ns(paste0("prtcpnt_ped_operating_push_cart_",i)), "Was the pedestrian operating a push cart?",
                              choices = c("Yes","No", "Unknown"),
                              selected=check_selected_character0(data_i[[paste0("prtcpnt_ped_operating_push_cart_", i)]]),
                              inline=T)
                 
          )
          
        }
  })
}

output$prtcpnt_ped_pushcart_qs_sr_1 <- gen_ped_pushcart_qs_sr(1)
output$prtcpnt_ped_pushcart_qs_sr_2 <- gen_ped_pushcart_qs_sr(2)
output$prtcpnt_ped_pushcart_qs_sr_3 <- gen_ped_pushcart_qs_sr(3)
output$prtcpnt_ped_pushcart_qs_sr_4 <- gen_ped_pushcart_qs_sr(4)
output$prtcpnt_ped_pushcart_qs_sr_5 <- gen_ped_pushcart_qs_sr(5)
output$prtcpnt_ped_pushcart_qs_sr_6 <- gen_ped_pushcart_qs_sr(6)
output$prtcpnt_ped_pushcart_qs_sr_7 <- gen_ped_pushcart_qs_sr(7)
output$prtcpnt_ped_pushcart_qs_sr_8 <- gen_ped_pushcart_qs_sr(8)
output$prtcpnt_ped_pushcart_qs_sr_9 <- gen_ped_pushcart_qs_sr(9)
output$prtcpnt_ped_pushcart_qs_sr_10 <- gen_ped_pushcart_qs_sr(10)
output$prtcpnt_ped_pushcart_qs_sr_11 <- gen_ped_pushcart_qs_sr(11)
output$prtcpnt_ped_pushcart_qs_sr_12 <- gen_ped_pushcart_qs_sr(12)
output$prtcpnt_ped_pushcart_qs_sr_13 <- gen_ped_pushcart_qs_sr(13)
output$prtcpnt_ped_pushcart_qs_sr_14 <- gen_ped_pushcart_qs_sr(14)
output$prtcpnt_ped_pushcart_qs_sr_15 <- gen_ped_pushcart_qs_sr(15)
output$prtcpnt_ped_pushcart_qs_sr_16 <- gen_ped_pushcart_qs_sr(16)
output$prtcpnt_ped_pushcart_qs_sr_17 <- gen_ped_pushcart_qs_sr(17)
output$prtcpnt_ped_pushcart_qs_sr_18 <- gen_ped_pushcart_qs_sr(18)
output$prtcpnt_ped_pushcart_qs_sr_19 <- gen_ped_pushcart_qs_sr(19)
output$prtcpnt_ped_pushcart_qs_sr_20 <- gen_ped_pushcart_qs_sr(20)

# ** Nationality ---------------------------------------------------------------
gen_nationality_other_qs_sr <- function(i){
  renderUI({
    if (!("Other" %in% input[[paste0("prtcpnt_nationality_", i)]]) ) return(NULL) else {
      #fluidRow(
      column(12,
             textInput(session$ns(paste0("prtcpnt_nationality_other_", i)), "Nationality (Other)", check_selected_blanktext(data_i[[paste0("prtcpnt_nationality_other_", i)]]))
      )
      #)
    }
  })
}

output$prtcpnt_nationality_other_qs_sr_1 <- gen_nationality_other_qs_sr(1)
output$prtcpnt_nationality_other_qs_sr_2 <- gen_nationality_other_qs_sr(2)
output$prtcpnt_nationality_other_qs_sr_3 <- gen_nationality_other_qs_sr(3)
output$prtcpnt_nationality_other_qs_sr_4 <- gen_nationality_other_qs_sr(4)
output$prtcpnt_nationality_other_qs_sr_5 <- gen_nationality_other_qs_sr(5)
output$prtcpnt_nationality_other_qs_sr_6 <- gen_nationality_other_qs_sr(6)
output$prtcpnt_nationality_other_qs_sr_7 <- gen_nationality_other_qs_sr(7)
output$prtcpnt_nationality_other_qs_sr_8 <- gen_nationality_other_qs_sr(8)
output$prtcpnt_nationality_other_qs_sr_9 <- gen_nationality_other_qs_sr(9)
output$prtcpnt_nationality_other_qs_sr_10 <- gen_nationality_other_qs_sr(10)
output$prtcpnt_nationality_other_qs_sr_11 <- gen_nationality_other_qs_sr(11)
output$prtcpnt_nationality_other_qs_sr_12 <- gen_nationality_other_qs_sr(12)
output$prtcpnt_nationality_other_qs_sr_13 <- gen_nationality_other_qs_sr(13)
output$prtcpnt_nationality_other_qs_sr_14 <- gen_nationality_other_qs_sr(14)
output$prtcpnt_nationality_other_qs_sr_15 <- gen_nationality_other_qs_sr(15)
output$prtcpnt_nationality_other_qs_sr_16 <- gen_nationality_other_qs_sr(16)
output$prtcpnt_nationality_other_qs_sr_17 <- gen_nationality_other_qs_sr(17)
output$prtcpnt_nationality_other_qs_sr_18 <- gen_nationality_other_qs_sr(18)
output$prtcpnt_nationality_other_qs_sr_19 <- gen_nationality_other_qs_sr(19)
output$prtcpnt_nationality_other_qs_sr_20 <- gen_nationality_other_qs_sr(20)

# ** Registration Number -------------------------------------------------------
gen_part_reg_no_qs_sr <- function(i){
  renderUI({
    if (!ifelse(length(input[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, ".", 
                input[[paste0("prtcpnt_vehpart_type_", i)]]) 
        %in% c("Saloon Car",
               "Pickup van",
               "Lorry",
               "Lorry + trailer",
               "Bus",
               "Matatu",
               "Motor-cycle",
               "Other Vehicle")) return(NULL) else {
                 
                 
                 column(12,
                        textInput(session$ns(paste0("prtcpnt_regnumber_", i)), "Registration Number", check_selected_blanktext(data_i[[paste0("prtcpnt_regnumber_", i)]]))
                 )
                 
                 
               }
  })
}

output$prtcpnt_reg_no_qs_sr_1 <- gen_part_reg_no_qs_sr(1)
output$prtcpnt_reg_no_qs_sr_2 <- gen_part_reg_no_qs_sr(2)
output$prtcpnt_reg_no_qs_sr_3 <- gen_part_reg_no_qs_sr(3)
output$prtcpnt_reg_no_qs_sr_4 <- gen_part_reg_no_qs_sr(4)
output$prtcpnt_reg_no_qs_sr_5 <- gen_part_reg_no_qs_sr(5)
output$prtcpnt_reg_no_qs_sr_6 <- gen_part_reg_no_qs_sr(6)
output$prtcpnt_reg_no_qs_sr_7 <- gen_part_reg_no_qs_sr(7)
output$prtcpnt_reg_no_qs_sr_8 <- gen_part_reg_no_qs_sr(8)
output$prtcpnt_reg_no_qs_sr_9 <- gen_part_reg_no_qs_sr(9)
output$prtcpnt_reg_no_qs_sr_10 <- gen_part_reg_no_qs_sr(10)
output$prtcpnt_reg_no_qs_sr_11 <- gen_part_reg_no_qs_sr(11)
output$prtcpnt_reg_no_qs_sr_12 <- gen_part_reg_no_qs_sr(12)
output$prtcpnt_reg_no_qs_sr_13 <- gen_part_reg_no_qs_sr(13)
output$prtcpnt_reg_no_qs_sr_14 <- gen_part_reg_no_qs_sr(14)
output$prtcpnt_reg_no_qs_sr_15 <- gen_part_reg_no_qs_sr(15)
output$prtcpnt_reg_no_qs_sr_16 <- gen_part_reg_no_qs_sr(16)
output$prtcpnt_reg_no_qs_sr_17 <- gen_part_reg_no_qs_sr(17)
output$prtcpnt_reg_no_qs_sr_18 <- gen_part_reg_no_qs_sr(18)
output$prtcpnt_reg_no_qs_sr_19 <- gen_part_reg_no_qs_sr(19)
output$prtcpnt_reg_no_qs_sr_20 <- gen_part_reg_no_qs_sr(20)

# ** Vehicle Make and Model ---------------------------------------------------------------------
gen_veh_make_model_qs_sr <- function(i){
  renderUI({
    if (!ifelse(length(input[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, ".", 
                input[[paste0("prtcpnt_vehpart_type_", i)]]) 
        %in% c("Saloon Car",
               "Pickup van",
               "Lorry",
               "Lorry + trailer",
               "Bus",
               "Matatu",
               "Motor-cycle",
               "Other Vehicle")) return(NULL) else {
                 fluidRow(
                   column(6,
                          textInput(session$ns(paste0("prtcpnt_veh_make_", i)), "Vehicle Make", check_selected_blanktext(data_i[[paste0("prtcpnt_veh_make_", i)]]))
                   ),
                   column(6,
                          textInput(session$ns(paste0("prtcpnt_veh_model_", i)), "Vehicle Model", check_selected_blanktext(data_i[[paste0("prtcpnt_veh_model_", i)]]))
                   )
                 )
               }
  })
}

output$prtcpnt_veh_make_model_qs_sr_1 <- gen_veh_make_model_qs_sr(1)
output$prtcpnt_veh_make_model_qs_sr_2 <- gen_veh_make_model_qs_sr(2)
output$prtcpnt_veh_make_model_qs_sr_3 <- gen_veh_make_model_qs_sr(3)
output$prtcpnt_veh_make_model_qs_sr_4 <- gen_veh_make_model_qs_sr(4)
output$prtcpnt_veh_make_model_qs_sr_5 <- gen_veh_make_model_qs_sr(5)
output$prtcpnt_veh_make_model_qs_sr_6 <- gen_veh_make_model_qs_sr(6)
output$prtcpnt_veh_make_model_qs_sr_7 <- gen_veh_make_model_qs_sr(7)
output$prtcpnt_veh_make_model_qs_sr_8 <- gen_veh_make_model_qs_sr(8)
output$prtcpnt_veh_make_model_qs_sr_9 <- gen_veh_make_model_qs_sr(9)
output$prtcpnt_veh_make_model_qs_sr_10 <- gen_veh_make_model_qs_sr(10)
output$prtcpnt_veh_make_model_qs_sr_11 <- gen_veh_make_model_qs_sr(11)
output$prtcpnt_veh_make_model_qs_sr_12 <- gen_veh_make_model_qs_sr(12)
output$prtcpnt_veh_make_model_qs_sr_13 <- gen_veh_make_model_qs_sr(13)
output$prtcpnt_veh_make_model_qs_sr_14 <- gen_veh_make_model_qs_sr(14)
output$prtcpnt_veh_make_model_qs_sr_15 <- gen_veh_make_model_qs_sr(15)
output$prtcpnt_veh_make_model_qs_sr_16 <- gen_veh_make_model_qs_sr(16)
output$prtcpnt_veh_make_model_qs_sr_17 <- gen_veh_make_model_qs_sr(17)
output$prtcpnt_veh_make_model_qs_sr_18 <- gen_veh_make_model_qs_sr(18)
output$prtcpnt_veh_make_model_qs_sr_19 <- gen_veh_make_model_qs_sr(19)
output$prtcpnt_veh_make_model_qs_sr_20 <- gen_veh_make_model_qs_sr(20)

# ** SACCO ---------------------------------------------------------------------
gen_sacco_qs_sr <- function(i){
  renderUI({
    if (!ifelse(length(input[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, ".", input[[paste0("prtcpnt_vehpart_type_", i)]])
        %in% 'Matatu') return(NULL) else {
          fluidRow(
            column(8,
                   textInput(session$ns(paste0("prtcpnt_sacco_name_", i)), "SACCO", check_selected_blanktext(data_i[[paste0("prtcpnt_sacco_name_", i)]]))
            )
          )
        }
  })
}

output$prtcpnt_sacco_qs_sr_1 <- gen_sacco_qs_sr(1)
output$prtcpnt_sacco_qs_sr_2 <- gen_sacco_qs_sr(2)
output$prtcpnt_sacco_qs_sr_3 <- gen_sacco_qs_sr(3)
output$prtcpnt_sacco_qs_sr_4 <- gen_sacco_qs_sr(4)
output$prtcpnt_sacco_qs_sr_5 <- gen_sacco_qs_sr(5)
output$prtcpnt_sacco_qs_sr_6 <- gen_sacco_qs_sr(6)
output$prtcpnt_sacco_qs_sr_7 <- gen_sacco_qs_sr(7)
output$prtcpnt_sacco_qs_sr_8 <- gen_sacco_qs_sr(8)
output$prtcpnt_sacco_qs_sr_9 <- gen_sacco_qs_sr(9)
output$prtcpnt_sacco_qs_sr_10 <- gen_sacco_qs_sr(10)
output$prtcpnt_sacco_qs_sr_11 <- gen_sacco_qs_sr(11)
output$prtcpnt_sacco_qs_sr_12 <- gen_sacco_qs_sr(12)
output$prtcpnt_sacco_qs_sr_13 <- gen_sacco_qs_sr(13)
output$prtcpnt_sacco_qs_sr_14 <- gen_sacco_qs_sr(14)
output$prtcpnt_sacco_qs_sr_15 <- gen_sacco_qs_sr(15)
output$prtcpnt_sacco_qs_sr_16 <- gen_sacco_qs_sr(16)
output$prtcpnt_sacco_qs_sr_17 <- gen_sacco_qs_sr(17)
output$prtcpnt_sacco_qs_sr_18 <- gen_sacco_qs_sr(18)
output$prtcpnt_sacco_qs_sr_19 <- gen_sacco_qs_sr(19)
output$prtcpnt_sacco_qs_sr_20 <- gen_sacco_qs_sr(20)

# ** Crash Description -------------------------------------------------------
output$crash_description <- renderUI({
  
  fluidRow(
    column(6,
           textAreaInput(session$ns("crash_description"), "Crash Description", 
                         check_selected_blanktext_define_input(data_i[["crash_description"]],
                                                               "IT HAPPENED THAT"),
                         width = '200%')
    ),
  )
  
})


# Hidden Numeric Inputs
output$crash_latlon_inputs <- renderUI({
  fluidRow(
    column(2,
           ""),
    column(4,
           numericInput(session$ns("crash_lat"), "Latitude", check_selected_numeric(data_i$crash_lat))
    ),
    column(4, 
           numericInput(session$ns("crash_long"), "Longitude", check_selected_numeric(data_i$crash_long))
    )
  )
})

output$map <- renderLeaflet({
  
  #numericInput("crash_lat", "Crash Latitude", check_selected_numeric(data_i$crash_lat))
  #numericInput("crash_long", "Crash Longitude", check_selected_numeric(data_i$crash_long))
  #addResourcePath("mytiles", "/Users/robmarty/Documents/Github/Kenya-Police-Dashboard/basemap")
  #addResourcePath("mytiles", file.path(dirname(sys.frame(1)$ofile), "basemap"))
  
  
  
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
  
  
  #if((length(input$crash_long) > 0)){
  #  if(!is.na(input$crash_long)){
  l <- l %>%
    clearMarkers() %>%
    clearPopups() %>%
    addMarkers(lng = data_i$crash_long %>% as.numeric, lat = data_i$crash_lat %>% as.numeric, group = 'new_circles',
               options = markerOptions(draggable = F), layerId = "id") 
  #  }
  #}
  
  l
  
})

observeEvent(input$map_click, {
  click <- input$map_click
  click_lat <- click$lat
  click_long <- click$lng
  
  # Add the marker to the map
  leafletProxy('map') %>%
    clearMarkers() %>%
    clearPopups() %>%
    addMarkers(lng = click_long, lat = click_lat, group = 'new_circles',
               options = markerOptions(draggable = F), layerId = id) 
  
  updateNumericInput(session, "crash_lat", value = click_lat)
  updateNumericInput(session, "crash_long", value = click_long)
  
})

# ** N Participants/Victims/Witnesses ----------------------------------------
output$N_participant_qs <- renderUI({
  column(4,
         numericInput(session$ns("N_participants"), "Number of Participants (ie, vehicles and pedestrians)", check_selected(data_i$N_participants, 1), 
                      min=1, max=10)
  )
})

output$N_victims_qs <- renderUI({
  column(4,
         numericInput(session$ns("N_victims"), "How many victims are there that are not participants?", check_selected(data_i$N_victims, 0), 
                      min=1, max=10)
  )
})

# ** Cause Codes -------------------------------------------------------------
#### Cause code category
output$cause_code_category_ui <- renderUI({
  radioButtons(session$ns("cause_code_category"), HTML("Select Cause Code Category<span style='color:red'>*</span>"), 
               choices = c("Driver (1 - 30C)",
                           "Pedal Cyclist (31 - 58)",
                           "Pedestrian (59 - 68)",
                           "Passengers etc (69 - 73)",
                           "Animals (74 - 75)",
                           "Obstruction (76 - 77)",
                           "Vehicle Defect (78 - 89)",
                           "Road Defect (90 - 93)",
                           "Weather (94 - 96)",
                           "Other Cause (97 - 98)"),
               selected=check_selected_character0(data_i[["cause_code_category"]]))
})

#### Cause Codes
output$cause_code_ui <- renderUI({
  if(length(input$cause_code_category) %in% 0){
    NULL
  } else if(input$cause_code_category %in% "Driver (1 - 30C)"){
    choices = c("1 - Fatigued",
                "2 - Asleep",
                "3 - Ill",
                "4 - Under the influence of drink or a drug",
                "5 - Physically defective",
                "6 - Inexperienced with the type of motor vehicle",
                "7 - Proceeding at excessive speed having regard to conditions",
                "8 - Failing to keep to near side or to the proper lane",
                "9 - Cutting in",
                "10 - Overtaking improperly",
                "11 - Swerving",
                "12 - Skidding (give cause of skidding)",
                "13 - Forcing way through persons boarding or alighting from omnibu",
                "14 - Failing to stop to afford free passage to pedestrians crossing",
                "15 - Turning round in road negligently",
                "16 - Reversing negligently (other than from parking area)",
                "17 - Failing to comply with traffic sign or signal",
                "18 - Failing to signal or giving indistinct or incorrect signal",
                "19 - Pulling out from near side or from one traffic lane (not from parking area) to another without due care)",
                "20 -Inattentive or attention divered",
                "21 - Hampered by passenger, animal or luggage in or on vehicle",
                "22 - Turning right without due care",
                "23 - Turning left without due care",
                "24 - Driver negligently opening door of the vehicle",
                "25 - Crossing without due care at road junction",
                "26 - Losing control (particulars to be specified)",
                "27 - Dazzed by lights of another vehicle",
                "28 - Stopping suddenly",
                "29 - Misjudging clearance, distance or speed (vehicle or object)",
                "30 - Other apparent error of judgement or negligence (specify)",
                "30a - Reversing form angle parking space negligently",
                "30b - Entering parking space (angle or flush) negligently",
                "30c - Leaving flush parking space negligently")
    column(7, radioButtons(session$ns("cause_code_value"), HTML("Select Cause Code<span style='color:red'>*</span>"), 
                           choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) )) 
  } else if(input$cause_code_category %in% "Pedal Cyclist (31 - 58)"){
    choices = c("31 - Fatigued",
                "32 - Ill",
                "33 - Under the influence of drink or a drug",
                "34 - Physically defective",
                "35 - Inexperienced with the type of motor vehicle",
                "36 - Proceeding at excessive speed having regard to conditions",
                "37 - Failing to keep to near side or to the proper lane",
                "38 - Cutting in",
                "39 - Overtaking improperly",
                "40 - Swerving",
                "41 - Skidding (give cause of skidding)",
                "42 - Forcing way through persons boarding or alighting from omnibu",
                "43 - Failing to stop to afford free passage to pedestrians crossing",
                "44 - Turning round in road negligently",
                "45 - Failing to comply with traffic sign or signal",
                "46 - Failing to signal or giving indistinct or incorrect signal",
                "47 - Pulling out from near side or from one traffic lane (not from parking area) to another without due care)",
                "48 -Inattentive or attention divered",
                "49 - Hampered by passenger, animal or luggage in or on vehicle",
                "50 - Turning right without due care",
                "51 - Turning left without due care",
                "52 - Crossing without due care at road junction",
                "53 - Pedal cyclist holding on to another vehicle",
                "54 - Losing control (particulars to be specified)",
                "55 - Dazzed by lights of another vehicle",
                "56 - Stopping suddenly",
                "57 - Misjudging clearance, distance or speed (vehicle or object)",
                "58 - Other apparent error of judgement or negligence (specify)")
    column(7, radioButtons(session$ns("cause_code_value"), HTML("Select Cause Code<span style='color:red'>*</span>"), 
                           choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Pedestrian (59 - 68)"){
    choices = c("59 - Heedless of traffic - crossing the road masked by stationary vehicle",
                "60 - Heedless of traffic - crossing the road not masked by stationary vehicle",
                "61 - Heedless of traffic - walking or standing on the road",
                "62 - Heedless of traffic - playing in road",
                "63 - Heedless of traffic - stepping walking or running off footpath or verge into road",
                "64 - Slipping or falling",
                "65 - Physical defects or physical illness",
                "66 - Under the influence of drink or a drug",
                "67 - Holding on to vehicle",
                "68 - Error of judgment or negligence")
    column(7, radioButtons(session$ns("cause_code_value"), HTML("Select Cause Code<span style='color:red'>*</span>"), 
                           choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Passengers etc (69 - 73)"){
    choices = c("69 - Boarding or alighting from vehicle without due care",
                "70 - Falling when inside or falling when from vehicle",
                "71 - Other negligence on part of the passenger",
                "72 - Stealing ride",
                "73 - Negligence on part of conductor or goods vehicle attendant")
    column(7, radioButtons(session$ns("cause_code_value"), HTML("Select Cause Code<span style='color:red'>*</span>"), 
                           choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Animals (74 - 75)"){
    choices = c("74 - Dog in carriageway",
                "75 - Other animals in carriageway")
    column(7, radioButtons(session$ns("cause_code_value"), HTML("Select Cause Code<span style='color:red'>*</span>"), 
                           choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Obstruction (76 - 77)"){
    choices = c("76 - Obstruction",
                "77 - Other obstruction")
    column(7, radioButtons(session$ns("cause_code_value"), HTML("Select Cause Code<span style='color:red'>*</span>"), 
                           choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Vehicle Defect (78 - 89)"){
    choices = c("78 - Mechanical defect or failure - brakes",
                "79 - Mechanical defect or failure - tyres or wheels",
                "80 - Mechanical defect or failure - steering",
                "81 - Mechanical defect or failure - other cause",
                "82 - No front lights",
                "83 - Inadequate front light",
                "84 - No rear light",
                "85 - Inadequate rear light",
                "86 - Unattended vehicle running away",
                "87 - Drivers view obstructed e.g. by equipment, load or obsucred windscreen",
                "88 - Vehicle overloaded, shifted or defective load",
                "89 - Any other feature of vehicle or equipment which contributed to the accident (specify)")
    column(7, radioButtons(session$ns("cause_code_value"), HTML("Select Cause Code<span style='color:red'>*</span>"), 
                           choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Road Defect (90 - 93)"){
    choices = c("90 - Road surface slippery",
                "91 - Excessive dust obscuring drivers view",
                "92 - Road surface in need of repair (state defect)",
                "93 - Other road condition, view obscured, etc. specify")
    column(7, radioButtons(session$ns("cause_code_value"), HTML("Select Cause Code<span style='color:red'>*</span>"), 
                           choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Weather (94 - 96)"){
    choices = c("94 - Fog or mist",
                "95 - Torrential rain",
                "96 - Glaring sun")
    column(7, radioButtons(session$ns("cause_code_value"), HTML("Select Cause Code<span style='color:red'>*</span>"), 
                           choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Other Cause (97 - 98)"){
    choices = c("97 - Other cause specify",
                "98 - Cause not traced")
    column(7, radioButtons(session$ns("cause_code_value"), HTML("Select Cause Code<span style='color:red'>*</span>"), 
                           choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else{
    
  }
  
})

ui_data_entry_qs <- navlistPanel(
  widths=c(3,9),
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
           uiOutput(session$ns("victim_fatal_1_qs_sr")),
           
           uiOutput(session$ns("victim_2_qs_sr")),
           uiOutput(session$ns("victim_fatal_2_qs_sr")),
           
           uiOutput(session$ns("victim_3_qs_sr")),
           uiOutput(session$ns("victim_fatal_3_qs_sr")),
           
           uiOutput(session$ns("victim_4_qs_sr")),
           uiOutput(session$ns("victim_fatal_4_qs_sr")),
           
           uiOutput(session$ns("victim_5_qs_sr")),
           uiOutput(session$ns("victim_fatal_5_qs_sr")),
           
           uiOutput(session$ns("victim_6_qs_sr")),
           uiOutput(session$ns("victim_fatal_6_qs_sr")),
           
           uiOutput(session$ns("victim_7_qs_sr")),
           uiOutput(session$ns("victim_fatal_7_qs_sr")),
           
           uiOutput(session$ns("victim_8_qs_sr")),
           uiOutput(session$ns("victim_fatal_8_qs_sr")),
           
           uiOutput(session$ns("victim_9_qs_sr")),
           uiOutput(session$ns("victim_fatal_9_qs_sr")),
           
           uiOutput(session$ns("victim_10_qs_sr")),
           uiOutput(session$ns("victim_fatal_10_qs_sr")),
           
           uiOutput(session$ns("victim_11_qs_sr")),
           uiOutput(session$ns("victim_fatal_11_qs_sr")),
           
           uiOutput(session$ns("victim_12_qs_sr")),
           uiOutput(session$ns("victim_fatal_12_qs_sr")),
           
           uiOutput(session$ns("victim_13_qs_sr")),
           uiOutput(session$ns("victim_fatal_13_qs_sr")),
           
           uiOutput(session$ns("victim_14_qs_sr")),
           uiOutput(session$ns("victim_fatal_14_qs_sr")),
           
           uiOutput(session$ns("victim_15_qs_sr")),
           uiOutput(session$ns("victim_fatal_15_qs_sr")),
           
           uiOutput(session$ns("victim_16_qs_sr")),
           uiOutput(session$ns("victim_fatal_16_qs_sr")),
           
           uiOutput(session$ns("victim_17_qs_sr")),
           uiOutput(session$ns("victim_fatal_17_qs_sr")),
           
           uiOutput(session$ns("victim_18_qs_sr")),
           uiOutput(session$ns("victim_fatal_18_qs_sr")),
           
           uiOutput(session$ns("victim_19_qs_sr")),
           uiOutput(session$ns("victim_fatal_19_qs_sr")),
           
           uiOutput(session$ns("victim_20_qs_sr")),
           uiOutput(session$ns("victim_fatal_20_qs_sr"))
           
           
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
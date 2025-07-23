
## ** General ----------------------------------------------------------------
output$crash_info_sr <- renderUI({
  
  fluidRow(
    
    fluidRow(
      column(3,
             selectInput(session$ns("accident_type"), "What is the accident type?", 
                         choices = c(".",
                                     "Non-Injury",
                                     "Slight",
                                     "Serious",
                                     "Fatal"),
                         selected=check_selected_period(data_i[["accident_type"]]))
      ),
      column(4,
             selectInput(session$ns("hit_and_run"), "Was the crash a hit and run?", 
                         choices = c(".",
                                     "Yes",
                                     "No"),
                         selected=check_selected_period(data_i[["hit_and_run"]]))
      )
    ),
    
    # Row 1 - - - - -
    fluidRow(
      column(3,
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
                         selected=check_selected_period(data_i[["police_division"]]))
      ),
      column(3,
             selectInput(session$ns("police_station"), "Police Station", 
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
                         selected=check_selected_period(data_i[["police_station"]]))
      ),
      column(2,
             textInput(session$ns("iar_no"), "IAR/Acc. Reg. No.", check_selected_blanktext(data_i[["iar_no"]]))
      ),
      column(2,
             textInput(session$ns("ch_reg_no"), "Ch-Reg No.", check_selected_blanktext(data_i[["ch_reg_no"]]))
      ),
      column(2,
             textInput(session$ns("ob_no"), "O.B. Num", check_selected_blanktext(data_i[["ob_no"]]))
      )
    ),
    
    # Row 2 - - - - -
    fluidRow(
      column(3,
             dateInput(session$ns("accident_date"), "Date of Accident", value=check_selected_date(data_i[["accident_date"]]),
                       language = "en-GB",
                       format = "dd/mm/yyyy") 
      ),
      
      column(3,
             numericInput(session$ns("accident_time_hour"),"Accident Time - Hour, 0-23",min=0,max=23,step=1, value=check_selected_numeric(data_i[["accident_time_hour"]]), width="100%")
      ),
      column(3,
             numericInput(session$ns("accident_time_minute"),"Accident Time - Minute",min=0,max=59,step=1, value=check_selected_numeric(data_i[["accident_time_minute"]]), width="100%")
      )
      
    ),
    
    # Row 2 - - - - -
    fluidRow(
      column(2,
             selectInput(session$ns("road_authority"), 
                         "Road Authority", 
                         choices = c(".",
                                     "MOTC",
                                     "Municipality",
                                     "Other"),
                         selected=check_selected_period(data_i[["road_authority"]]))
      ),
      column(2,
             selectInput(session$ns("urban_rural"), 
                         "Urban/Rural", 
                         choices = c(".", "Urban", "Rural"),
                         selected=check_selected_period(data_i[["urban_rural"]]))
      ),
      column(2,
             textInput(session$ns("speed_limit"), "Speed Limit", check_selected_blanktext(data_i[["speed_limit"]]))
      )
    ),
    
    # Row 3 - - - - -
    fluidRow(
      column(9,
             textAreaInput(session$ns("accident_location"), 
                           "Location of Accident (indicate milestone or nearest known place with distance)", 
                           check_selected_blanktext(data_i[["accident_location"]]),
                           width=500)
      )
    ),
    fluidRow(
      column(3,
             textInput(session$ns("road_name"), "Road Name", check_selected_blanktext(data_i[["road_name"]]))
      ),
      column(3,
             textInput(session$ns("roadno"), "Road Number", check_selected_blanktext(data_i[["roadno"]]))
      ),
      column(4,
             textInput(session$ns("landmark"), "Nearby Landmark (e.g, store, matatu stage)", check_selected_blanktext(data_i[["landmark"]]))
      )
    ),
    fluidRow(
      column(12, strong("Officer who Responded to Crash")),
    ),
    fluidRow(
      column(4, textInput(session$ns("officer_rspnd_crash_title"), "Title", check_selected_blanktext(data_i[["officer_rspnd_crash_title"]]))),
      column(4, textInput(session$ns("officer_rspnd_crash_firstname"), "First Name", check_selected_blanktext(data_i[["officer_rspnd_crash_firstname"]]))),
      column(4, textInput(session$ns("officer_rspnd_crash_surname"), "Surname", check_selected_blanktext(data_i[["officer_rspnd_crash_surname"]])))
    ),
    
    fluidRow(
      column(12, strong("Officer who Filled Out Form")),
    ),
    fluidRow(
      column(4, textInput(session$ns("officer_filled_form_title"), "Title", check_selected_blanktext(data_i[["officer_filled_form_title"]]))),
      column(4, textInput(session$ns("officer_filled_form_firstname"), "First Name", check_selected_blanktext(data_i[["officer_filled_form_firstname"]]))),
      column(4, textInput(session$ns("officer_filled_form_surname"), "Surname", check_selected_blanktext(data_i[["officer_filled_form_surname"]])))
    )
    
    
    
    
    
  )
  
  
})

# ** Participants ------------------------------------------------------------
gen_participant_qs <- function(i){
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
                        textInput(session$ns(paste0("prtcpnt_address_", i)), "Address", check_selected_blanktext(data_i[[paste0("prtcpnt_address_", i)]]))
                 ),
                 
                 column(12,
                        textInput(session$ns(paste0("prtcpnt_telephone_", i)), "Telephone", check_selected_blanktext(data_i[[paste0("prtcpnt_telephone_", i)]]))
                 ),
                 column(12,
                        textInput(session$ns(paste0("prtcpnt_nationalid_", i)), "National ID Number", check_selected_blanktext(data_i[[paste0("prtcpnt_nationalid_", i)]]))
                 ),
                 column(12,
                        selectInput(session$ns(paste0("prtcpnt_nationality_", i)), "Nationality",
                                    choices = c(".","Kenyan","Other"),
                                    selected = check_selected_period(data_i[[paste0("prtcpnt_nationality_", i)]]))
                 ),
                 uiOutput(session$ns(paste0("prtcpnt_nationality_other_qs_", i))),
                 
                 column(12,
                        radioButtons(session$ns(paste0("prtcpnt_gender_", i)), "Gender",
                                     choices = c("Male","Female"),
                                     selected = check_selected_character0(data_i[[paste0("prtcpnt_gender_", i)]]),
                                     inline=T)
                 )
          ),
          
          
          
          uiOutput(session$ns(paste0("prtcpnt_sacco_qs_", i))),
          uiOutput(session$ns(paste0("prtcpnt_owner_driver_", i))),
          
          uiOutput(session$ns(paste0("prtcpnt_vehpart_type_other_", i)))
          
        ),
        
        uiOutput(session$ns(paste0("prtcpnt_vehicle_adn_info_", i))),
        
        fluidRow(
          column(12,
                 textAreaInput(session$ns(paste0("prtcpnt_vehicle_damage_", i)), "Brief details of damage", check_selected_blanktext(data_i[[paste0("prtcpnt_vehicle_damage_", i)]]), width="550px")
          )
        ),
        
        uiOutput(session$ns(paste0("prtcpnt_pedestrian_qs_", i))),
        uiOutput(session$ns(paste0("gen_part_vehicle_qs_", i))),
        
        hr(),
        fluidRow(
          column(4,
                 selectInput(session$ns(paste0("prtcpnt_injured_yn_", i)), "Was the participant injured?",
                             choices = c(".","Yes","No"),
                             selected = check_selected_period(data_i[[paste0("prtcpnt_injured_yn_", i)]]))
          )
        ),
        uiOutput(session$ns(paste0("prtcpnt_injured_qs_", i))),
        uiOutput(session$ns(paste0("prtcpnt_injured_fatal_qs_", i)))
      )
    }
  })
}

output$participant_main_qs <- renderUI({
  
  if(is.null(input$N_participants)){
    N_participants_vector <- 1
  } else{
    N_participants_vector <- 1:input$N_participants
  }
  
  do.call(tabsetPanel, 
          lapply(N_participants_vector, 
                 function(i) tabPanel(i, gen_participant_qs(i) )))
})

#### Participant Vehicle Questions
gen_part_ownerdriver_qs <- function(i){
  renderUI({
    if (!ifelse(length(input[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, ".", input[[paste0("prtcpnt_vehpart_type_", i)]])
        %in% c("Saloon Car",
               "Pickup van",
               "Lorry",
               "Lorry + trailer",
               "Bus",
               "Matatu",
               "Motor-cycle",
               "Other Vehicle")) return(NULL) else {
                 
                 div(
                   column(4,
                          radioButtons(session$ns(paste0("prtcpnt_owner_driver_", i)),"Owner or Driver of Vehicle",choices=c("Owner", "Driver"), 
                                       selected = check_selected_character0(data_i[[paste0("prtcpnt_owner_driver_", i)]]))
                   )
                 )
                 
               }
  })
}

output$prtcpnt_owner_driver_1 <- gen_part_ownerdriver_qs(1)
output$prtcpnt_owner_driver_2 <- gen_part_ownerdriver_qs(2)
output$prtcpnt_owner_driver_3 <- gen_part_ownerdriver_qs(3)
output$prtcpnt_owner_driver_4 <- gen_part_ownerdriver_qs(4)
output$prtcpnt_owner_driver_5 <- gen_part_ownerdriver_qs(5)
output$prtcpnt_owner_driver_6 <- gen_part_ownerdriver_qs(6)
output$prtcpnt_owner_driver_7 <- gen_part_ownerdriver_qs(7)
output$prtcpnt_owner_driver_8 <- gen_part_ownerdriver_qs(8)
output$prtcpnt_owner_driver_9 <- gen_part_ownerdriver_qs(9)
output$prtcpnt_owner_driver_10 <- gen_part_ownerdriver_qs(10)
output$prtcpnt_owner_driver_11 <- gen_part_ownerdriver_qs(11)
output$prtcpnt_owner_driver_12 <- gen_part_ownerdriver_qs(12)
output$prtcpnt_owner_driver_13 <- gen_part_ownerdriver_qs(13)
output$prtcpnt_owner_driver_14 <- gen_part_ownerdriver_qs(14)
output$prtcpnt_owner_driver_15 <- gen_part_ownerdriver_qs(15)
output$prtcpnt_owner_driver_16 <- gen_part_ownerdriver_qs(16)
output$prtcpnt_owner_driver_17 <- gen_part_ownerdriver_qs(17)
output$prtcpnt_owner_driver_18 <- gen_part_ownerdriver_qs(18)
output$prtcpnt_owner_driver_19 <- gen_part_ownerdriver_qs(19)
output$prtcpnt_owner_driver_20 <- gen_part_ownerdriver_qs(20)

#### SACCO
gen_sacco_qs <- function(i){
  renderUI({
    if (!ifelse(length(input[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, ".", input[[paste0("prtcpnt_vehpart_type_", i)]])
        %in% 'Matatu') return(NULL) else {
          fluidRow(
            column(6,
                   textInput(session$ns(paste0("prtcpnt_sacco_name_", i)), "SACCO", check_selected_blanktext(data_i[[paste0("prtcpnt_sacco_name_", i)]]))
            )
          )
        }
  })
}

output$prtcpnt_sacco_qs_1 <- gen_sacco_qs(1)
output$prtcpnt_sacco_qs_2 <- gen_sacco_qs(2)
output$prtcpnt_sacco_qs_3 <- gen_sacco_qs(3)
output$prtcpnt_sacco_qs_4 <- gen_sacco_qs(4)
output$prtcpnt_sacco_qs_5 <- gen_sacco_qs(5)
output$prtcpnt_sacco_qs_6 <- gen_sacco_qs(6)
output$prtcpnt_sacco_qs_7 <- gen_sacco_qs(7)
output$prtcpnt_sacco_qs_8 <- gen_sacco_qs(8)
output$prtcpnt_sacco_qs_9 <- gen_sacco_qs(9)
output$prtcpnt_sacco_qs_10 <- gen_sacco_qs(10)
output$prtcpnt_sacco_qs_11 <- gen_sacco_qs(11)
output$prtcpnt_sacco_qs_12 <- gen_sacco_qs(12)
output$prtcpnt_sacco_qs_13 <- gen_sacco_qs(13)
output$prtcpnt_sacco_qs_14 <- gen_sacco_qs(14)
output$prtcpnt_sacco_qs_15 <- gen_sacco_qs(15)
output$prtcpnt_sacco_qs_16 <- gen_sacco_qs(16)
output$prtcpnt_sacco_qs_17 <- gen_sacco_qs(17)
output$prtcpnt_sacco_qs_18 <- gen_sacco_qs(18)
output$prtcpnt_sacco_qs_19 <- gen_sacco_qs(19)
output$prtcpnt_sacco_qs_20 <- gen_sacco_qs(20)

#### Nationality
gen_nationality_other_qs <- function(i){
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

output$prtcpnt_nationality_other_qs_1 <- gen_nationality_other_qs(1)
output$prtcpnt_nationality_other_qs_2 <- gen_nationality_other_qs(2)
output$prtcpnt_nationality_other_qs_3 <- gen_nationality_other_qs(3)
output$prtcpnt_nationality_other_qs_4 <- gen_nationality_other_qs(4)
output$prtcpnt_nationality_other_qs_5 <- gen_nationality_other_qs(5)
output$prtcpnt_nationality_other_qs_6 <- gen_nationality_other_qs(6)
output$prtcpnt_nationality_other_qs_7 <- gen_nationality_other_qs(7)
output$prtcpnt_nationality_other_qs_8 <- gen_nationality_other_qs(8)
output$prtcpnt_nationality_other_qs_9 <- gen_nationality_other_qs(9)
output$prtcpnt_nationality_other_qs_10 <- gen_nationality_other_qs(10)
output$prtcpnt_nationality_other_qs_11 <- gen_nationality_other_qs(11)
output$prtcpnt_nationality_other_qs_12 <- gen_nationality_other_qs(12)
output$prtcpnt_nationality_other_qs_13 <- gen_nationality_other_qs(13)
output$prtcpnt_nationality_other_qs_14 <- gen_nationality_other_qs(14)
output$prtcpnt_nationality_other_qs_15 <- gen_nationality_other_qs(15)
output$prtcpnt_nationality_other_qs_16 <- gen_nationality_other_qs(16)
output$prtcpnt_nationality_other_qs_17 <- gen_nationality_other_qs(17)
output$prtcpnt_nationality_other_qs_18 <- gen_nationality_other_qs(18)
output$prtcpnt_nationality_other_qs_19 <- gen_nationality_other_qs(19)
output$prtcpnt_nationality_other_qs_20 <- gen_nationality_other_qs(20)

#### Participant Pedestrian Questions
gen_part_vehicle_qs <- function(i){
  renderUI({
    if (!ifelse(length(input[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, ".", input[[paste0("prtcpnt_vehpart_type_", i)]])
        %in% c("Saloon Car",
               "Pickup van",
               "Lorry",
               "Lorry + trailer",
               "Bus",
               "Matatu",
               "Motor-cycle",
               "Other Vehicle",
               "Unknown")) return(NULL) else {
          
          fluidRow(

            hr(),
            fluidRow(
              column(6,
                     textInput(session$ns(paste0("prtcpnt_regnumber_", i)), "Registration Number", check_selected_blanktext(data_i[[paste0("prtcpnt_regnumber_", i)]])))
            ),
            fluidRow(
              column(3,
                     textInput(session$ns(paste0("prtcpnt_drivinglic_", i)), "Driving License No", check_selected_blanktext(data_i[[paste0("prtcpnt_drivinglic_", i)]]))
              ),
              column(4,
                     selectInput(session$ns(paste0("prtcpnt_drivinglic_valid_", i)), "Driving License Valid/Not Valid",
                                 choices = c(".","Valid","Not Valid","Not Applicable"),
                                 selected = check_selected_period(data_i[[paste0("prtcpnt_drivinglic_valid_", i)]]))
              )
            ),
            fluidRow(
              column(3,
                     textInput(session$ns(paste0("prtcpnt_roadlic_", i)), "Road License No", check_selected_blanktext(data_i[[paste0("prtcpnt_roadlic_", i)]]))
              ),
              column(4,
                     selectInput(session$ns(paste0("prtcpnt_roadlic_valid_", i)), "Road License Valid/Not Valid",
                                 choices = c(".","Valid","Not Valid","Not Applicable"),
                                 selected = check_selected_period(data_i[[paste0("prtcpnt_roadlic_valid_", i)]]))
              )
            ),
            fluidRow(
              column(6,
                     textInput(session$ns(paste0("prtcpnt_insurance_", i)), "Insurance Company", check_selected_blanktext(data_i[[paste0("prtcpnt_insurance_", i)]]))
              )
            ),
            fluidRow(
              column(3,
                     textInput(session$ns(paste0("prtcpnt_inscert_", i)), "Ins Certificate No", check_selected_blanktext(data_i[[paste0("prtcpnt_inscert_", i)]]))
              ),
              column(4,
                     selectInput(session$ns(paste0("prtcpnt_inscert_valid_", i)), "Ins Certificate No Valid/Not Valid",
                                 choices = c(".","Valid","Not Valid","Not Applicable"),
                                 selected = check_selected_period(data_i[[paste0("prtcpnt_inscert_valid_", i)]]))
              )
            ),
            fluidRow(
              column(3,
                     textInput(session$ns(paste0("prtcpnt_psv_", i)), "P.S.V License No", check_selected_blanktext(data_i[[paste0("prtcpnt_psv_", i)]]))
              ),
              column(4,
                     selectInput(session$ns(paste0("prtcpnt_psv_valid_", i)), "P.S.V License No Valid/Not Valid",
                                 choices = c(".","Valid","Not Valid","Not Applicable"),
                                 selected = check_selected_period(data_i[[paste0("prtcpnt_psv_valid_", i)]]))
              )
            )
            
          )
        }
  })
}

output$gen_part_vehicle_qs_1 <- gen_part_vehicle_qs(1)
output$gen_part_vehicle_qs_2 <- gen_part_vehicle_qs(2)
output$gen_part_vehicle_qs_3 <- gen_part_vehicle_qs(3)
output$gen_part_vehicle_qs_4 <- gen_part_vehicle_qs(4)
output$gen_part_vehicle_qs_5 <- gen_part_vehicle_qs(5)
output$gen_part_vehicle_qs_6 <- gen_part_vehicle_qs(6)
output$gen_part_vehicle_qs_7 <- gen_part_vehicle_qs(7)
output$gen_part_vehicle_qs_8 <- gen_part_vehicle_qs(8)
output$gen_part_vehicle_qs_9 <- gen_part_vehicle_qs(9)
output$gen_part_vehicle_qs_10 <- gen_part_vehicle_qs(10)
output$gen_part_vehicle_qs_11 <- gen_part_vehicle_qs(11)
output$gen_part_vehicle_qs_12 <- gen_part_vehicle_qs(12)
output$gen_part_vehicle_qs_13 <- gen_part_vehicle_qs(13)
output$gen_part_vehicle_qs_14 <- gen_part_vehicle_qs(14)
output$gen_part_vehicle_qs_15 <- gen_part_vehicle_qs(15)
output$gen_part_vehicle_qs_16 <- gen_part_vehicle_qs(16)
output$gen_part_vehicle_qs_17 <- gen_part_vehicle_qs(17)
output$gen_part_vehicle_qs_18 <- gen_part_vehicle_qs(18)
output$gen_part_vehicle_qs_19 <- gen_part_vehicle_qs(19)
output$gen_part_vehicle_qs_20 <- gen_part_vehicle_qs(20)

#### Participant Pedestrian Questions
gen_part_pedestrian_qs <- function(i){
  renderUI({
    if (!ifelse(length(input[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, ".", input[[paste0("prtcpnt_vehpart_type_", i)]])
        %in% 'Pedestrian') return(NULL) else {
          fluidRow(
            hr(),
            column(6,
                   selectInput(session$ns(paste0("prtcpnt_ped_crossing_", i)), "If the pedestrian was crossing the road he/she was:",
                               choices = c(".","On pedestrian crossing","1-25m from pedestrian crossing", "Over 25m from pedestrian crossing"),
                               selected = check_selected_period(data_i[[paste0("prtcpnt_ped_crossing_", i)]]))
            ),
            column(6,
                   selectInput(session$ns(paste0("prtcpnt_ped_along_", i)), "If the pedestrian was walking along the road he/she was walking:",
                               choices = c(".","In the direction of traffic","Towards the traffic"),
                               selected = check_selected_period(data_i[[paste0("prtcpnt_ped_along_", i)]]))
            ),
            column(12,
                   textAreaInput(session$ns(paste0("prtcpnt_ped_comments_", i)), "Comments about pedestrian movement", 
                                 check_selected_blanktext(data_i[[paste0("prtcpnt_ped_comments_", i)]]), 
                                 width=600)
            )
          )
        }
  })
}

output$prtcpnt_pedestrian_qs_1 <- gen_part_pedestrian_qs(1)
output$prtcpnt_pedestrian_qs_2 <- gen_part_pedestrian_qs(2)
output$prtcpnt_pedestrian_qs_3 <- gen_part_pedestrian_qs(3)
output$prtcpnt_pedestrian_qs_4 <- gen_part_pedestrian_qs(4)
output$prtcpnt_pedestrian_qs_5 <- gen_part_pedestrian_qs(5)
output$prtcpnt_pedestrian_qs_6 <- gen_part_pedestrian_qs(6)
output$prtcpnt_pedestrian_qs_7 <- gen_part_pedestrian_qs(7)
output$prtcpnt_pedestrian_qs_8 <- gen_part_pedestrian_qs(8)
output$prtcpnt_pedestrian_qs_9 <- gen_part_pedestrian_qs(9)
output$prtcpnt_pedestrian_qs_10 <- gen_part_pedestrian_qs(10)
output$prtcpnt_pedestrian_qs_11 <- gen_part_pedestrian_qs(11)
output$prtcpnt_pedestrian_qs_12 <- gen_part_pedestrian_qs(12)
output$prtcpnt_pedestrian_qs_13 <- gen_part_pedestrian_qs(13)
output$prtcpnt_pedestrian_qs_14 <- gen_part_pedestrian_qs(14)
output$prtcpnt_pedestrian_qs_15 <- gen_part_pedestrian_qs(15)
output$prtcpnt_pedestrian_qs_16 <- gen_part_pedestrian_qs(16)
output$prtcpnt_pedestrian_qs_17 <- gen_part_pedestrian_qs(17)
output$prtcpnt_pedestrian_qs_18 <- gen_part_pedestrian_qs(18)
output$prtcpnt_pedestrian_qs_19 <- gen_part_pedestrian_qs(19)
output$prtcpnt_pedestrian_qs_120 <- gen_part_pedestrian_qs(20)

#### Participant Injury Questions
gen_part_injury_qs <- function(i){
  renderUI({
    
    part_i_injury_personclass_default <- character(0)
    
    part_i_vehpart_type_selected <- input[[paste0("prtcpnt_vehpart_type_", i)]]
    if(length(part_i_vehpart_type_selected) %in% 0) part_i_vehpart_type_selected <- "."
    
    if(part_i_vehpart_type_selected %in% "Pedestrian"){
      part_i_injury_personclass_default <- "Pedestrian"
    } 
    
    if(part_i_vehpart_type_selected %in% "Bicycle"){
      part_i_injury_personclass_default <- "Cyclist"
    } 
    
    if(part_i_vehpart_type_selected %in% c("Pedestrian","Bicycle")){
      part_i_injury_pos_in_veh_default <- "An open body"
      part_i_injury_seatbelt_default <- "Not Relevant"
    } else{
      part_i_injury_pos_in_veh_default <- "."
      part_i_injury_seatbelt_default <- "."
    }
    
    part_i_injured_yn <- input[[paste0("prtcpnt_injured_yn_", i)]]
    
    if (!part_i_injured_yn %in% 'Yes') return(NULL) else {
      div(
        fluidRow(
          column(4,
                 radioButtons(session$ns(paste0("prtcpnt_injurytype_", i)), "Injury Type",
                              choices = c("Fatal","Serious", "Slight"),
                              selected=check_selected_character0(data_i[[paste0("prtcpnt_injurytype_", i)]]),
                              inline=T)
          ),
          column(8,
                 radioButtons(session$ns(paste0("prtcpnt_personclass_", i)), "Class of Person", 
                              choices = c("Pedestrian",
                                          "Cyclist",
                                          "Motor-cycle",
                                          "Driver",
                                          "Passenger"),
                              inline=T,
                              selected = check_selected_character0(data_i[[paste0("prtcpnt_personclass_", i)]]))
          )
        ),
        fluidRow(
          column(3,
                 selectInput(session$ns(paste0("prtcpnt_position_", i)), "Position in Vehicle",
                             choices = c(".",
                                         "Front seat",
                                         "Rear seat",
                                         "Standing (inside)",
                                         "An open body",
                                         "Not Relevant"),
                             selected = check_selected_period(data_i[[paste0("prtcpnt_position_", i)]]))
          ),
          column(3,
                 selectInput(session$ns(paste0("prtcpnt_seatbelt_", i)), "Safety Belt in Use",
                             choices = c(".", "Yes","No", "Not Relevant"),
                             selected = check_selected_period(data_i[[paste0("prtcpnt_injured_yn_", i)]]))
          )
        ),
        fluidRow(
          column(12,
                 textAreaInput(session$ns(paste0("prtcpnt_injured_comments_", i)), "Comments/Notes", check_selected_blanktext(data_i[[paste0("prtcpnt_injured_comments_", i)]]))
          )
        ),
        fluidRow(
          column(4,
                 textInput(session$ns(paste0("prtcpnt_hospital_taken_", i)), "Hospital Taken To", check_selected_blanktext(data_i[[paste0("prtcpnt_hospital_taken_", i)]]))
          ),
          column(4,
                 textInput(session$ns(paste0("prtcpnt_hospital_admno_", i)), "Hospital Admission No.", check_selected_blanktext(data_i[[paste0("prtcpnt_hospital_admno_", i)]]))
          ),
        )
      )
    }
  })
}

output$prtcpnt_injured_qs_1 <- gen_part_injury_qs(1)
output$prtcpnt_injured_qs_2 <- gen_part_injury_qs(2)
output$prtcpnt_injured_qs_3 <- gen_part_injury_qs(3)
output$prtcpnt_injured_qs_4 <- gen_part_injury_qs(4)
output$prtcpnt_injured_qs_5 <- gen_part_injury_qs(5)
output$prtcpnt_injured_qs_6 <- gen_part_injury_qs(6)
output$prtcpnt_injured_qs_7 <- gen_part_injury_qs(7)
output$prtcpnt_injured_qs_8 <- gen_part_injury_qs(8)
output$prtcpnt_injured_qs_9 <- gen_part_injury_qs(9)
output$prtcpnt_injured_qs_10 <- gen_part_injury_qs(10)
output$prtcpnt_injured_qs_11 <- gen_part_injury_qs(11)
output$prtcpnt_injured_qs_12 <- gen_part_injury_qs(12)
output$prtcpnt_injured_qs_13 <- gen_part_injury_qs(13)
output$prtcpnt_injured_qs_14 <- gen_part_injury_qs(14)
output$prtcpnt_injured_qs_15 <- gen_part_injury_qs(15)
output$prtcpnt_injured_qs_16 <- gen_part_injury_qs(16)
output$prtcpnt_injured_qs_17 <- gen_part_injury_qs(17)
output$prtcpnt_injured_qs_18 <- gen_part_injury_qs(18)
output$prtcpnt_injured_qs_19 <- gen_part_injury_qs(19)
output$prtcpnt_injured_qs_20 <- gen_part_injury_qs(20)

## Participant Fatal Questions
gen_part_injury_fatal_qs <- function(i){
  renderUI({
    
    part_i_injured_type <- input[[paste0("prtcpnt_injurytype_", i)]]
    
    if (!('Fatal' %in% part_i_injured_type)) return(NULL) else {
      div(
        fluidRow(
          column(4,
                 textInput(session$ns(paste0("prtcpnt_mortuary_taken_", i)), "Mortuary Taken To", check_selected_blanktext(data_i[[paste0("prtcpnt_mortuary_taken_", i)]]))
          ),
          column(4,
                 textInput(session$ns(paste0("prtcpnt_mortuary_admno_", i)), "Mortuary Admission No.", check_selected_blanktext(data_i[[paste0("prtcpnt_mortuary_admno_", i)]]))
          ),
        )
      )
    }
  })
}

output$prtcpnt_injured_fatal_qs_1 <- gen_part_injury_fatal_qs(1)
output$prtcpnt_injured_fatal_qs_2 <- gen_part_injury_fatal_qs(2)
output$prtcpnt_injured_fatal_qs_3 <- gen_part_injury_fatal_qs(3)
output$prtcpnt_injured_fatal_qs_4 <- gen_part_injury_fatal_qs(4)
output$prtcpnt_injured_fatal_qs_5 <- gen_part_injury_fatal_qs(5)
output$prtcpnt_injured_fatal_qs_6 <- gen_part_injury_fatal_qs(6)
output$prtcpnt_injured_fatal_qs_7 <- gen_part_injury_fatal_qs(7)
output$prtcpnt_injured_fatal_qs_8 <- gen_part_injury_fatal_qs(8)
output$prtcpnt_injured_fatal_qs_9 <- gen_part_injury_fatal_qs(9)
output$prtcpnt_injured_fatal_qs_10 <- gen_part_injury_fatal_qs(10)
output$prtcpnt_injured_fatal_qs_11 <- gen_part_injury_fatal_qs(11)
output$prtcpnt_injured_fatal_qs_12 <- gen_part_injury_fatal_qs(12)
output$prtcpnt_injured_fatal_qs_13 <- gen_part_injury_fatal_qs(13)
output$prtcpnt_injured_fatal_qs_14 <- gen_part_injury_fatal_qs(14)
output$prtcpnt_injured_fatal_qs_15 <- gen_part_injury_fatal_qs(15)
output$prtcpnt_injured_fatal_qs_16 <- gen_part_injury_fatal_qs(16)
output$prtcpnt_injured_fatal_qs_17 <- gen_part_injury_fatal_qs(17)
output$prtcpnt_injured_fatal_qs_18 <- gen_part_injury_fatal_qs(18)
output$prtcpnt_injured_fatal_qs_19 <- gen_part_injury_fatal_qs(19)
output$prtcpnt_injured_fatal_qs_20 <- gen_part_injury_fatal_qs(20)


#### Other Vehicle
gen_other_vehicle <- function(i){
  renderUI({
    
    if (!ifelse(length(input[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, ".", input[[paste0("prtcpnt_vehpart_type_", i)]])
        %in% 'Other Vehicle') return(NULL) else {
          column(8,
                 textInput(session$ns(paste0("prtcpnt_vehpart_type_other_", i)), "Specify Other Vehicle Type", 
                           check_selected_blanktext(data_i[[paste0("prtcpnt_vehpart_type_other_", i)]]))
          )
        }
  })
}

output$prtcpnt_vehpart_type_other_1 <- gen_other_vehicle(1)
output$prtcpnt_vehpart_type_other_2 <- gen_other_vehicle(2)
output$prtcpnt_vehpart_type_other_3 <- gen_other_vehicle(3)
output$prtcpnt_vehpart_type_other_4 <- gen_other_vehicle(4)
output$prtcpnt_vehpart_type_other_5 <- gen_other_vehicle(5)
output$prtcpnt_vehpart_type_other_6 <- gen_other_vehicle(6)
output$prtcpnt_vehpart_type_other_7 <- gen_other_vehicle(7)
output$prtcpnt_vehpart_type_other_8 <- gen_other_vehicle(8)
output$prtcpnt_vehpart_type_other_9 <- gen_other_vehicle(9)
output$prtcpnt_vehpart_type_other_10 <- gen_other_vehicle(10)
output$prtcpnt_vehpart_type_other_11 <- gen_other_vehicle(11)
output$prtcpnt_vehpart_type_other_12 <- gen_other_vehicle(12)
output$prtcpnt_vehpart_type_other_13 <- gen_other_vehicle(13)
output$prtcpnt_vehpart_type_other_14 <- gen_other_vehicle(14)
output$prtcpnt_vehpart_type_other_15 <- gen_other_vehicle(15)
output$prtcpnt_vehpart_type_other_16 <- gen_other_vehicle(16)
output$prtcpnt_vehpart_type_other_17 <- gen_other_vehicle(17)
output$prtcpnt_vehpart_type_other_18 <- gen_other_vehicle(18)
output$prtcpnt_vehpart_type_other_19 <- gen_other_vehicle(19)
output$prtcpnt_vehpart_type_other_20 <- gen_other_vehicle(20)

#### Additional Vehicle Info
gen_additional_vehicle_info <- function(i){
  renderUI({
    
    if (!ifelse(length(input[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, ".", input[[paste0("prtcpnt_vehpart_type_", i)]])
        %in% c("Saloon Car",
               "Pickup van",
               "Lorry",
               "Lorry + trailer",
               "Bus",
               "Matatu",
               "Motor-cycle",
               "Other Vehicle")) return(NULL) else {
                 
                 fluidRow(
                   column(8,
                          radioButtons(session$ns(paste0("prtcpnt_vehicle_adn_info_", i)), "Additional information on types of vehicles", 
                                       choices = c("country bus", 
                                                   "GK bus", 
                                                   "taxcab",
                                                   "KBS",
                                                   "institutional bus",
                                                   "tanker",
                                                   "other urban bus",
                                                   "mini bus",
                                                   "None/Not Relevant"),
                                       inline=T,
                                       selected=check_selected_character0(data_i[[paste0("prtcpnt_vehicle_adn_info_", i)]]))
                   )
                 )
               }
  })
}

output$prtcpnt_vehicle_adn_info_1 <- gen_additional_vehicle_info(1)
output$prtcpnt_vehicle_adn_info_2 <- gen_additional_vehicle_info(2)
output$prtcpnt_vehicle_adn_info_3 <- gen_additional_vehicle_info(3)
output$prtcpnt_vehicle_adn_info_4 <- gen_additional_vehicle_info(4)
output$prtcpnt_vehicle_adn_info_5 <- gen_additional_vehicle_info(5)
output$prtcpnt_vehicle_adn_info_6 <- gen_additional_vehicle_info(6)
output$prtcpnt_vehicle_adn_info_7 <- gen_additional_vehicle_info(7)
output$prtcpnt_vehicle_adn_info_8 <- gen_additional_vehicle_info(8)
output$prtcpnt_vehicle_adn_info_9 <- gen_additional_vehicle_info(9)
output$prtcpnt_vehicle_adn_info_10 <- gen_additional_vehicle_info(10)
output$prtcpnt_vehicle_adn_info_11 <- gen_additional_vehicle_info(11)
output$prtcpnt_vehicle_adn_info_12 <- gen_additional_vehicle_info(12)
output$prtcpnt_vehicle_adn_info_13 <- gen_additional_vehicle_info(13)
output$prtcpnt_vehicle_adn_info_14 <- gen_additional_vehicle_info(14)
output$prtcpnt_vehicle_adn_info_15 <- gen_additional_vehicle_info(15)
output$prtcpnt_vehicle_adn_info_16 <- gen_additional_vehicle_info(16)
output$prtcpnt_vehicle_adn_info_17 <- gen_additional_vehicle_info(17)
output$prtcpnt_vehicle_adn_info_18 <- gen_additional_vehicle_info(18)
output$prtcpnt_vehicle_adn_info_19 <- gen_additional_vehicle_info(19)
output$prtcpnt_vehicle_adn_info_20 <- gen_additional_vehicle_info(20)

# ** Victims -----------------------------------------------------------------
gen_victim_qs <- function(i){
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
          column(3,
                 textInput(session$ns(paste0("victim_name_",i)), "Name", check_selected_blanktext(data_i[[paste0("victim_name_", i)]]))
          ),
          column(3,
                 textInput(session$ns(paste0("victim_address_",i)), "Address", check_selected_blanktext(data_i[[paste0("victim_address_", i)]]))
          ),
          column(3,
                 textInput(session$ns(paste0("victim_refnum_",i)), "Part/Veh Ref No", check_selected_blanktext(data_i[[paste0("victim_refnum_", i)]]))
          )
        ),
        fluidRow(
          column(2,
                 numericInput(session$ns(paste0("victim_age_years_",i)), "Age", check_selected_numeric(data_i[[paste0("victim_age_years_", i)]]))
          ),
          column(2,
                 selectInput(session$ns(paste0("victim_gender_",i)), "Gender", choices=c(".","Male","Female"), selected=check_selected_period(data_i[[paste0("victim_gender_", i)]]))
          ),
          column(4,
                 textInput(session$ns(paste0("victim_telephone_",i)), "Telephone Number", check_selected_blanktext(data_i[[paste0("victim_telephone_", i)]]))
          )
        ),
        fluidRow(
          column(7,
                 radioButtons(session$ns(paste0("victim_personclass_",i)), "Class of Person", 
                              choices = c("Pedestrian",
                                          "Cyclist",
                                          "Motor-cycle",
                                          "Driver",
                                          "Passenger"),
                              selected=check_selected_character0(data_i[[paste0("victim_personclass_", i)]]),
                              inline=T)
          ),
          column(3, 
                 selectInput(session$ns(paste0("victim_position_",i)), "Position in Vehicle",
                             choices = c(".",
                                         "Front seat",
                                         "Rear seat",
                                         "Standing (inside)",
                                         "An open body",
                                         "Not Relevant"),
                             selected = check_selected_period(data_i[[paste0("victim_position_", i)]]))
          ),
          column(2,
                 selectInput(session$ns(paste0("victim_seatbelt_",i)), "Seat Belt Use", 
                             choices = c(".", "Yes","No", "Not Relevant"),
                             selected=check_selected_period(data_i[[paste0("victim_seatbelt_", i)]]))
          )
        ),
        fluidRow(
          column(4,
                 radioButtons(session$ns(paste0("victim_injurytype_",i)), "Injury Type",
                              choices = c("Fatal","Serious", "Slight"),
                              selected=check_selected_character0(data_i[[paste0("victim_injurytype_", i)]]),
                              inline=T)
          ),
          column(8,
                 textAreaInput(session$ns(paste0("victim_comments_",i)), "Comments/Notes", check_selected_blanktext(data_i[[paste0("victim_comments_", i)]]))
          )
        ),
        fluidRow(
          column(4,
                 textInput(session$ns(paste0("victim_hospital_taken_",i)), "Hospital Taken To", check_selected_blanktext(data_i[[paste0("victim_hospital_taken_", i)]]))
          ),
          column(4,
                 textInput(session$ns(paste0("victim_hospital_admno_", i)), "Hospital Admission No.", check_selected_blanktext(data_i[[paste0("victim_hospital_admno_", i)]]))
          )
        )
      )
    }
  })
}

output$victim_1_qs <- gen_victim_qs(1)
output$victim_2_qs <- gen_victim_qs(2)
output$victim_3_qs <- gen_victim_qs(3)
output$victim_4_qs <- gen_victim_qs(4)
output$victim_5_qs <- gen_victim_qs(5)
output$victim_6_qs <- gen_victim_qs(6)
output$victim_7_qs <- gen_victim_qs(7)
output$victim_8_qs <- gen_victim_qs(8)
output$victim_9_qs <- gen_victim_qs(9)
output$victim_10_qs <- gen_victim_qs(10)
output$victim_11_qs <- gen_victim_qs(11)
output$victim_12_qs <- gen_victim_qs(12)
output$victim_13_qs <- gen_victim_qs(13)
output$victim_14_qs <- gen_victim_qs(14)
output$victim_15_qs <- gen_victim_qs(15)
output$victim_16_qs <- gen_victim_qs(16)
output$victim_17_qs <- gen_victim_qs(17)
output$victim_18_qs <- gen_victim_qs(18)
output$victim_19_qs <- gen_victim_qs(19)
output$victim_20_qs <- gen_victim_qs(20)

## Participant Fatal Questions
gen_victim_fatal_qs <- function(i){
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
            column(4,
                   textInput(session$ns(paste0("victim_mortuary_taken_", i)), "Mortuary Taken To", check_selected_blanktext(data_i[[paste0("victim_mortuary_taken_", i)]]))
            ),
            column(4,
                   textInput(session$ns(paste0("victim_mortuary_admno_", i)), "Mortuary Admission No.", check_selected_blanktext(data_i[[paste0("victim_mortuary_admno_", i)]]))
            ),
          )
        )
      }
      
    }
    
  })
}

output$victim_fatal_1_qs <- gen_victim_fatal_qs(1)
output$victim_fatal_2_qs <- gen_victim_fatal_qs(2)
output$victim_fatal_3_qs <- gen_victim_fatal_qs(3)
output$victim_fatal_4_qs <- gen_victim_fatal_qs(4)
output$victim_fatal_5_qs <- gen_victim_fatal_qs(5)
output$victim_fatal_6_qs <- gen_victim_fatal_qs(6)
output$victim_fatal_7_qs <- gen_victim_fatal_qs(7)
output$victim_fatal_8_qs <- gen_victim_fatal_qs(8)
output$victim_fatal_9_qs <- gen_victim_fatal_qs(9)
output$victim_fatal_10_qs <- gen_victim_fatal_qs(10)
output$victim_fatal_11_qs <- gen_victim_fatal_qs(11)
output$victim_fatal_12_qs <- gen_victim_fatal_qs(12)
output$victim_fatal_13_qs <- gen_victim_fatal_qs(13)
output$victim_fatal_14_qs <- gen_victim_fatal_qs(14)
output$victim_fatal_15_qs <- gen_victim_fatal_qs(15)
output$victim_fatal_16_qs <- gen_victim_fatal_qs(16)
output$victim_fatal_17_qs <- gen_victim_fatal_qs(17)
output$victim_fatal_18_qs <- gen_victim_fatal_qs(18)
output$victim_fatal_19_qs <- gen_victim_fatal_qs(19)
output$victim_fatal_20_qs <- gen_victim_fatal_qs(20)

# ** Witnesses ---------------------------------------------------------------
gen_witness_qs <- function(i){
  renderUI({
    
    if(!is.numeric(input$N_witnesses)){ # NA and NULL both TRUE here
      return(NULL)
    } else if(!(input$N_witnesses >= i)){
      return(NULL)
    } else{
      
      #if (!(input$N_witnesses >= i)) return(NULL) else {
      
      fluidRow(
        column(2,h5(paste0("Witness ",i))),
        column(3, 
               textInput(session$ns(paste0("witness_",i,"_name")), "Name", check_selected_blanktext(data_i[[paste0("witness_",i,"_name")]]))
        ),
        column(4, 
               textInput(session$ns(paste0("witness_",i,"_address")), "Address", check_selected_blanktext(data_i[[paste0("witness_",i,"_address")]]))
        )
      )
      
    }
  })
}

output$witness_1_qs <- gen_witness_qs(1)
output$witness_2_qs <- gen_witness_qs(2)
output$witness_3_qs <- gen_witness_qs(3)
output$witness_4_qs <- gen_witness_qs(4)
output$witness_5_qs <- gen_witness_qs(5)
output$witness_6_qs <- gen_witness_qs(6)
output$witness_7_qs <- gen_witness_qs(7)
output$witness_8_qs <- gen_witness_qs(8)
output$witness_9_qs <- gen_witness_qs(9)
output$witness_10_qs <- gen_witness_qs(10)

# ** Conditions --------------------------------------------------------------
#### Road damaged
output$road_damaged_type <- renderUI({
  
  if (!(input$road_damaged_yn %in% "damaged")) return(NULL) else {
    div(
      # column(3,
      #        checkboxGroupInput("road_damaged_type", "How was the road damaged (select all that apply)",
      #                      choices = c("potholes", "damaged edges",
      #                                  "corrugated", "loose stones on the surface"),
      #                      selected = character(0))
      #  ),
      column(3,
             radioButtons(session$ns("road_damaged_type_potholes"), "How was the road damaged (select all that apply)",
                          choices = c("potholes"),
                          selected = check_selected_character0(data_i[["road_damaged_type_potholes"]])),
             radioButtons(session$ns("road_damaged_type_damagededges"), NULL,
                          choices = c("damaged edges"),
                          selected = check_selected_character0(data_i[["road_damaged_type_damagededges"]])),
             radioButtons(session$ns("road_damaged_type_corrugated"), NULL,
                          choices = c("corrugated"),
                          selected = check_selected_character0(data_i[["road_damaged_type_corrugated"]])),
             radioButtons(session$ns("road_damaged_type_loosestones"), NULL,
                          choices = c("loose stones on the surface"),
                          selected = check_selected_character0(data_i[["road_damaged_type_loosestones"]]))
      )
      #column(3,
      #       checkboxGroupInput("road_damaged_type_damagededges", "",
      #                          choices = c("damaged edges"),
      #                          selected = character(0))
      #)
    )
    
  }
  
})

##### Road Condition Questions
output$junction_questions <- renderUI({
  
  if (!(input$junction_yn %in% "Yes")) return(NULL) else {
    div(
      column(3,
             radioButtons(session$ns("junction_type"), "Junction Type",
                          choices = c("T-junction", "4-leg junction",
                                      "roundabout", "other junction"),
                          selected = check_selected_character0(data_i[["junction_type"]]))
      ),
      column(3,
             radioButtons(session$ns("junction_signs_signals"), "Traffic signs and signals at junction",
                          choices = c("give way", "stop",
                                      "no signs", "no traffic light signals"),
                          selected = check_selected_character0(data_i[["junction_signs_signals"]]))
      ),
      column(3,
             radioButtons(session$ns("junction_signals_operating"), "If there were traffic light signals, were they",
                          choices = c("operating", "not operating",
                                      "not relevant"),
                          selected = check_selected_character0(data_i[["junction_signals_operating"]]))
      )
    )
  }
  
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

output$N_witnesses_qs <- renderUI({
  column(4,
         numericInput(session$ns("N_witnesses"), "How many witnesses were there?", check_selected(data_i$N_witnesses, 0), 
                      min=1, max=10)
  )
})

# ** Road Conditions ---------------------------------------------------------
output$conditions_qs <- renderUI({
  div(
    fluidRow(
      column(2,
             selectInput(session$ns("road_surface"), "Road Surface", 
                         choices = c(".", 
                                     "Tarmac", 
                                     "Murram",
                                     "Earth"),
                         selected=check_selected_period(data_i[["road_surface"]]))
      ),
      column(4,
             numericInput(session$ns("surface_width"), "Width of surface (meters)", check_selected_numeric(data_i[["surface_width"]]))
      ),
      column(2,
             radioButtons(session$ns("surface_wet_dry"), "Surface was", 
                          choices = c("wet", "dry"),
                          select=check_selected_character0(data_i[["surface_wet_dry"]]))
      ),
      column(3,
             radioButtons(session$ns("weather_conditions"), "Weather Conditions",
                          choices=c("clear", "cloudy","foggy", "rainy"),
                          selected=check_selected_character0(data_i[["weather_conditions"]]),
                          inline=T)
      )
    ),
    fluidRow(
      column(3,
             selectInput(session$ns("road_damaged_yn"), "Road Condition",
                         choices=c(".", "damaged", "not damaged"),
                         selected=check_selected_period(data_i[["road_damaged_yn"]]))
      ),
      uiOutput(session$ns("road_damaged_type"))
    ),
    fluidRow(
      column(3,
             selectInput(session$ns("junction_yn"), "Accident at junction", choices=c(".", "Yes", "No"), selected=check_selected_period(data_i[["junction_yn"]]))
      ),
      uiOutput(session$ns("junction_questions"))
    ),
    fluidRow(
      column(3,
             radioButtons(session$ns("time_of_day"),"Illumination",
                          choices = c("daylight", "night time 6.45pm - 6.15am"),
                          selected=check_selected_character0(data_i[["time_of_day"]]))
      ),
      column(3,
             radioButtons(session$ns("street_lights"), "Street Lights",
                          choices=c("street lights on",
                                    "no street lights"),
                          selected=check_selected_character0(data_i[["street_lights"]]))
      ),
      column(3,
             selectInput(session$ns("railway_crossing"), "Railway level crossing",
                         choices=c(".", "uncontrolled", "controlled", "no railway crossing"),
                         selected=check_selected_period(data_i[["railway_crossing"]]))
      ),
      column(3,
             selectInput(session$ns("road_works"), "Road works at accident site", 
                         choices=c(".", "yes", "no"),
                         selected=check_selected_period(data_i[["road_works"]]) )
             
      )
    )
  )
  
})

# ** Crash Sketch --------------------------------------------------------------
# **** UI ----------------------------------------------------------------------
output$crash_sketch <- renderUI({
  div(
    fluidRow(
      column(12, align = "center",
             h2("Crash Sketch")
      )
    ),
    fluidRow(
      column(8, align = "center", offset = 2,
             HTML(
               ifelse(ONLINE_SYSTEM,
                      '<strong><span style="color:black">Upload crash sketch as JPEG/JPG image (for example, crash_sketch.jpg).</span></strong><br><em></em>', # After uploading, a button to download the image will appear. If the upload fails, make sure the name of the image ends in .jpg (for example, crash_sketch.jpg)
                      '<strong><span style="color:black">Upload crash sketch as JPEG/JPG or PNG image (for example, crash_sketch.jpg OR crash_sketch.png).</span></strong>'
               )
             )
      )
    ),
    #fluidRow(
    #  column(8, align = "center", offset = 2,
    #         htmlOutput(session$ns("upload_warning"))
    #         )
    #),
    br(),
    br(),
    fluidRow(
      column(12, align = "center",
             uiOutput(session$ns("upload_crash_sketch_ui"))
      )
    ),
    fluidRow(
      column(12, align = "center",
             uiOutput(session$ns("download_crash_sketch_ui"))
      )
    )
    
  )
})

# **** Show Crash Sketch -------------------------------------------------------
# output$show_crash_sketch_ui <- renderImage({
#   
#   # out <- NULL
#   # if("yes" %in% data_i$crash_sketch_uploaded){
#   #   image_i <- readPNG(file.path("data", 
#   #                                "crash_sketches",
#   #                                paste0(data_i$uid, ".png")))
#   #   
#   #   
#   # }
#   
#     #image_i <- readPNG()
#     
#     png_i <- readPNG(file.path("data",
#                          "crash_sketches",
#                          paste0(data_i$uid, ".png")))
#     #png_ia <<- png_i
# 
#     ## Following three lines CREATE a NEW image. You do not need them
#     outfile <- tempfile(fileext = '.png')
#     png(outfile, width = 400, height = 300) # Generate the PNG
#     rasterImage(png_ia,0,0,1,1)
#     dev.off()
#     
#   
#     list(src = outfile,
#          contentType = 'image/png',
#          width = 400,
#          height = 300,
#          alt = "This is alternate text")
#   
# }, deleteFile = TRUE)

# **** Upload Crash Sketch: Button ---------------------------------------------
output$upload_crash_sketch_ui <- renderUI({
  
  title <- "Upload Crash Sketch"
  if("yes" %in% data_i$crash_sketch_uploaded){
    title <- "Replace Crash Sketch"
  }
  
  if(ONLINE_SYSTEM){
    fileInput(session$ns("upload_crash_sketch"), title,
              multiple = FALSE,
              accept = c("image/jpeg"))
  } else{
    fileInput(session$ns("upload_crash_sketch"), title,
              multiple = FALSE,
              accept = c("image/jpeg",
                         "image/png"))
  }
  
  
  
})

# **** Upload Crash Sketch: Observe --------------------------------------------
observe({
  req(input$upload_crash_sketch)
  
  ## Load Image 
  img_path <- input$upload_crash_sketch$datapath
  
  # output$upload_warning <- renderText({
  #   
  #   out <- NULL
  #   
  #   if(ONLINE_SYSTEM){
  #     if(grepl(".png$", img_path)){
  #       out <- '<br><strong><span style="color:red">You uploaded a .png file. The system only works with .jpg files. Make sure the file ends with .jpg and try again.</strong></span>'
  #     } else if(!grepl(".jpg$", img_path)){
  #       out <- '<br><strong><span style="color:red">You did not upload a .jpg file. The system only works with .jpg files. Make sure the file ends with .jpg and try again.</strong></span>'
  #     }
  #   }
  #   
  #   if(ONLINE_SYSTEM %in% FALSE){
  #     if((!grepl(".png$", img_path) | !grepl(".jpg$", img_path))){
  #       out <- '<br><strong><span style="color:red">You did not upload a .jpg or .png file. The system only works with .jpg or .png files. Make sure the file ends with .jpg or .png and try again.</strong></span>'
  #     }
  #   }
  # 
  #   out
  #   
  # })
  
  
  if(grepl(".png$", img_path)){
    png_i <- readPNG(img_path)
  }
  
  if(grepl(".jpg$", img_path)){
    png_i <- readJPEG(img_path)
  }
  
  ## Name Image
  image_name <- data_i$uid[1]
  
  ## Add to Data
  uploaded_crash_sketch_r$value <<- 1
  data_i$crash_sketch_uploaded <- "yes"
  data_i <<- data_i # so data uploaded for download_sketch
  # to avoid, set priotiy for upload/download observes, and 
  # in download, reupload data... as saved here.
  
  ## Save Image as (encrypted) RDS // as png
  
  if(TESTING_SYSTEM){
    writePNG(png_i, file.path("data", "crash_sketches_testing", paste0(image_name, ".png")))
    #saveRDS(png_i, file.path("data", "crash_sketches_testing", paste0(image_name, ".Rds")), version=2)
  } else{
    writePNG(png_i, file.path("data", "crash_sketches", paste0(image_name, ".png")))
    #saveRDS(aes_cbc_encrypt(serialize(png_i, NULL), key = data_key), 
    #        file.path("data", "crash_sketches", paste0(image_name, ".Rds")), version=2)
  }
  
  ## Save Dataset
  for(var in dataset_variables){
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
  
},
priority = 2)

# **** Download Crash Sketch: Button -------------------------------------------
observe({ # Observe so appears after an upload
  
  temp <- input$upload_crash_sketch # so "upload_crash_sketch" is observed
  
  output$download_crash_sketch_ui <- renderUI({
    
    if("yes" %in% data_i$crash_sketch_uploaded){
      out <- downloadButton(session$ns("download_crash_sketch"), "Download Crash Sketch")
    } else{
      out <- NULL
    }
    
    out
  })
}, priority = 1)

# **** Download Crash Sketch: Observe ------------------------------------------
observe({
  
  output$download_crash_sketch <- downloadHandler(
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
  
})

# ** Remarks and Prosecution ------------------------------------------------- 
output$remarks_prosecution_qs <- renderUI({
  div(
    fluidRow(
      column(12,
             textAreaInput(session$ns("remarks"), "Precis of accident and remarks of the investigating officer", 
                           check_selected_blanktext(data_i[["remarks"]]),width=700))
    ),
    column(12,
           radioButtons(session$ns("intended_prosecution"), "Has a notice of intended prosecution been served?", choices=c("Yes", "No"), 
                        selected=check_selected_character0(data_i[["intended_prosecution"]]), inline=T)
    )
  )
})

# ** Cause Codes -------------------------------------------------------------

#### Cause code category
output$cause_code_category_ui <- renderUI({
  radioButtons(session$ns("cause_code_category"), "Select Cause Code Category", 
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
    column(7, radioButtons(session$ns("cause_code_value"), "Select Cause Code", choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) )) 
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
    column(7, radioButtons(session$ns("cause_code_value"), "Select Cause Code", choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
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
    column(7, radioButtons(session$ns("cause_code_value"), "Select Cause Code", choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Passengers etc (69 - 73)"){
    choices = c("69 - Boarding or alighting from vehicle without due care",
                "70 - Falling when inside or falling when from vehicle",
                "71 - Other negligence on part of the passenger",
                "72 - Stealing ride",
                "73 - Negligence on part of conductor or goods vehicle attendant")
    column(7, radioButtons(session$ns("cause_code_value"), "Select Cause Code", choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Animals (74 - 75)"){
    choices = c("74 - Dog in carriageway",
                "75 - Other animals in carriageway")
    column(7, radioButtons(session$ns("cause_code_value"), "Select Cause Code", choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Obstruction (76 - 77)"){
    choices = c("76 - Obstruction",
                "77 - Other obstruction")
    column(7, radioButtons(session$ns("cause_code_value"), "Select Cause Code", choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
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
    column(7, radioButtons(session$ns("cause_code_value"), "Select Cause Code", choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Road Defect (90 - 93)"){
    choices = c("90 - Road surface slippery",
                "91 - Excessive dust obscuring drivers view",
                "92 - Road surface in need of repair (state defect)",
                "93 - Other road condition, view obscured, etc. specify")
    column(7, radioButtons(session$ns("cause_code_value"), "Select Cause Code", choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Weather (94 - 96)"){
    choices = c("94 - Fog or mist",
                "95 - Torrential rain",
                "96 - Glaring sun")
    column(7, radioButtons(session$ns("cause_code_value"), "Select Cause Code", choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else if(input$cause_code_category %in% "Other Cause (97 - 98)"){
    choices = c("97 - Other cause specify",
                "98 - Cause not traced")
    column(7, radioButtons(session$ns("cause_code_value"), "Select Cause Code", choices=choices, selected=check_selected_character0(data_i[["cause_code_value"]]) ))
  } else{
    
  }
  
})

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
           uiOutput(session$ns("participant_main_qs")),
           
  ),
  # ** Victims 
  tabPanel("Victims",
           
           fluidRow(column(4,
                           ""),
                    #column(4,
                    #       numericInput("N_victims","How many victims are there that are not participants?",0,min=0,max=10,step=1))
                    uiOutput(session$ns("N_victims_qs"))
           ),
           uiOutput(session$ns("victim_1_qs")),
           uiOutput(session$ns("victim_fatal_1_qs")),
           
           uiOutput(session$ns("victim_2_qs")),
           uiOutput(session$ns("victim_fatal_2_qs")),
           
           uiOutput(session$ns("victim_3_qs")),
           uiOutput(session$ns("victim_fatal_3_qs")),
           
           uiOutput(session$ns("victim_4_qs")),
           uiOutput(session$ns("victim_fatal_4_qs")),
           
           uiOutput(session$ns("victim_5_qs")),
           uiOutput(session$ns("victim_fatal_5_qs")),
           
           uiOutput(session$ns("victim_6_qs")),
           uiOutput(session$ns("victim_fatal_6_qs")),
           
           uiOutput(session$ns("victim_7_qs")),
           uiOutput(session$ns("victim_fatal_7_qs")),
           
           uiOutput(session$ns("victim_8_qs")),
           uiOutput(session$ns("victim_fatal_8_qs")),
           
           uiOutput(session$ns("victim_9_qs")),
           uiOutput(session$ns("victim_fatal_9_qs")),
           
           uiOutput(session$ns("victim_10_qs")),
           uiOutput(session$ns("victim_fatal_10_qs")),
           
           uiOutput(session$ns("victim_11_qs")),
           uiOutput(session$ns("victim_fatal_11_qs")),
           
           uiOutput(session$ns("victim_12_qs")),
           uiOutput(session$ns("victim_fatal_12_qs")),
           
           uiOutput(session$ns("victim_13_qs")),
           uiOutput(session$ns("victim_fatal_13_qs")),
           
           uiOutput(session$ns("victim_14_qs")),
           uiOutput(session$ns("victim_fatal_14_qs")),
           
           uiOutput(session$ns("victim_15_qs")),
           uiOutput(session$ns("victim_fatal_15_qs")),
           
           uiOutput(session$ns("victim_16_qs")),
           uiOutput(session$ns("victim_fatal_16_qs")),
           
           uiOutput(session$ns("victim_17_qs")),
           uiOutput(session$ns("victim_fatal_17_qs")),
           
           uiOutput(session$ns("victim_18_qs")),
           uiOutput(session$ns("victim_fatal_18_qs")),
           
           uiOutput(session$ns("victim_19_qs")),
           uiOutput(session$ns("victim_fatal_19_qs")),
           
           uiOutput(session$ns("victim_20_qs")),
           uiOutput(session$ns("victim_fatal_20_qs"))
           
  ),
  # ** Cause Code 
  tabPanel("Conditions",
           
           fluidRow(column(12,
                           uiOutput(session$ns("conditions_qs"))   
           ))
           
  ),
  # ** Cause Code 
  tabPanel("Cause and Responsibility",
           
           fluidRow(column(8,
                           textAreaInput(session$ns("primarily_responsible"), "State who was primarily responsible for the accident", 
                                         check_selected_blanktext(data_i[["primarily_responsible"]]), width=400)),
                    column(4,
                           selectInput(session$ns("alcohol_involved"), "Was there alcohol involved?", 
                                       choices = c(".", "Yes", "No"), selected = check_selected_period(data_i[["alcohol_involved"]]) ))
           ),
           
           fluidRow(column(4,
                           uiOutput(session$ns("cause_code_category_ui"))   
           ),
           uiOutput(session$ns("cause_code_ui"))
           )
           
  ),
  # ** Victims 
  tabPanel("Witnesses",
           
           fluidRow(column(4,
                           ""),
                    #column(4,
                    #       numericInput("N_victims","How many victims are there that are not participants?",0,min=0,max=10,step=1))
                    uiOutput(session$ns("N_witnesses_qs"))
           ),
           uiOutput(session$ns("witness_1_qs")),
           uiOutput(session$ns("witness_2_qs")),
           uiOutput(session$ns("witness_3_qs")),
           uiOutput(session$ns("witness_4_qs")),
           uiOutput(session$ns("witness_5_qs")),
           uiOutput(session$ns("witness_6_qs")),
           uiOutput(session$ns("witness_7_qs")),
           uiOutput(session$ns("witness_8_qs")),
           uiOutput(session$ns("witness_9_qs")),
           uiOutput(session$ns("witness_10_qs"))
           
  ),
  tabPanel("Remarks and Prosecution",
           
           fluidRow(column(12,
                           uiOutput(session$ns("remarks_prosecution_qs"))   
           )
           )
           
  ),
  tabPanel("Crash Sketch",
           fluidRow(column(12,
                           uiOutput(session$ns("crash_sketch"))   
           )
           )
  )
  
)
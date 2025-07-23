# Setup

# Packages ---------------------------------------------------------------------
# For some reason, need to initialize aes_cbc_encrypt before loading other packages
library(openssl)
get_openssl_working <- aes_cbc_encrypt(serialize(serialize(data.frame(NULL), NULL), NULL), 
                                       key = openssl::sha256(charToRaw("dummytext")))
library(png)
library(jpeg)
library(dplyr)
library(bcrypt)
library(shiny)
library(aws.s3)
library(purrr)
library(ggplot2)
library(DT)
library(curl)
library(data.table)
library(shinyjs)
library(lubridate)
#library(DBI)
library(shinydashboard)
library(dashboardthemes)
library(knitr)
library(leaflet)
library(shinydashboard)
library(shiny)
library(DT)
library(shinyWidgets)
library(mapview)
library(stringr)
library(tinytex) # If stops working: reinstall_tinytex(packages = TRUE, dir = tinytex_root())
library(readr)
library(plotly)
library(tidyr)
library(forcats)
library(scales)
library(tools) 
library(grid)
library(reactable)
library(gmailr) ## ADD TO INSTALLER
library(shinyalert) ## ADD TO INSTALLER
library(R.utils) ## ADD TO INSTALLER
if (!"kableExtra" %in% installed.packages()) install.packages("kableExtra")
library(kableExtra)
#library(htmltools) # new
#library(shinyalert) ## NEW
#if (!"tidyr" %in% installed.packages()) install.packages("tidyr")
#if (!"forcats" %in% installed.packages()) install.packages("forcats")
#if (!"scales" %in% installed.packages()) install.packages("scales")
#library(devtools)
#install_github("nik01010/dashboardthemes")

# Initialize Data --------------------------------------------------------------
#### Creates null dataframe of sr_to_delete
# Don't need for "testing", as can't sync data with server when testing and only
# matters when testing
if(!file.exists(file.path("data", "sr_to_delete.Rds"))) saveRDS(NULL, file.path("data", "sr_to_delete.Rds"), version=2)

#### Create version 
this_version <- "version 1"
if(!file.exists(file.path("data", "version_local_system.Rds"))) saveRDS(this_version, file.path("data", "version_local_system.Rds"), version=2)
if(!file.exists(file.path("data", "version_latest.Rds"))) saveRDS(this_version, file.path("data", "version_latest.Rds"), version=2)

#### Default Test Data
if(!file.exists(file.path("data", "police_data_test.Rds"))){
  df_null <- data.frame(matrix(nrow = 0, ncol=9))
  names(df_null) <- c("uid", "last_updated", "accident_date", "ob_no", "iar_no", "police_division", "stage_sr_clean",  "stage_p41_clean", "stage_p41")
  
  saveRDS(df_null, file.path("data", "police_data_test.Rds"), version=2)
}

#### Load Dataframes
passwords_df <- readRDS(file.path("keys_passwords", "secured", "passwords_hashed.Rds"))
#uids_delete <- readRDS(file.path("data", "reports_to_delete.Rds"))
uids_delete <- c("temp")
dataset <- data.frame(NULL) 

data_i <- data.frame(matrix(nrow=1, ncol=0)) # need ??

#### Settings
Logged = F # set to TRUE to ignore password landing page
options(shiny.port = 8000)

# Authenticate Gmail -----------------------------------------------------------
#if(curl::has_internet()){
#  gm_auth_configure(path = "./keys_passwords/gmail_credentials/credentials.json")
#  gm_auth(cache = "./keys_passwords/gmail_credentials/.secret",
#          email = "nairobi.ecrash.system@gmail.com")
#}

# Define Variables -------------------------------------------------------------
num_var_repeat <- 10

sr_variables <- c("sr_row1_c1", "sr_row1_c2", "sr_row1_c3", 
                  "sr_row2", "sr_row3", "sr_row4", "sr_row5", 
                  "sr_row6_c1", "sr_row6_c2", "sr_row6_c3",
                  "sr_main_text",
                  "sr_row7", "sr_row8", "sr_row9", "sr_row10")

ar_vars <- c("ar_row1_c1", "ar_row1_c2", "ar_row1_c3", 
             "ar_row2", "ar_row3", "ar_row4", "ar_row5", 
             "ar_row6_c1", "ar_row6_c2", "ar_row6_c3", 
             "ar_main_text", 
             "ar_row7", "ar_row8", "ar_row9", "ar_row10")

fu_vars <- c("fu_row1_c1", "fu_row1_c2", "fu_row1_c3", 
             "fu_row2", "fu_row3", "fu_row4", "fu_row5", 
             "fu_row6_c1", "fu_row6_c2", "fu_row6_c3", 
             "fu_main_text", 
             "fu_row7", "fu_row8", "fu_row9", "fu_row10")

textInput_variables <- c("iar_no",
                         "acc_reg_no",
                         "ch_reg_no",
                         "ob_no",
                         "speed_limit",
                         "roadno",
                         "road_name", 
                         "officer_rspnd_crash_title",
                         "officer_rspnd_crash_firstname",
                         "officer_rspnd_crash_surname",
                         "officer_rspnd_crash_snumber",
                         "officer_filled_form_title",
                         "officer_filled_form_firstname",
                         "officer_filled_form_surname",
                         "officer_filled_form_snumber",
                         "officer_feedback_title",
                         "officer_feedback_firstname",
                         "officer_feedback_surname",
                         "officer_feedback_snumber",
                         
                         "officer_fu_feedback_title",
                         "officer_fu_feedback_firstname",
                         "officer_fu_feedback_surname",
                         "officer_fu_feedback_snumber",
                         
                         "station_feedback_title",
                         "station_feedback_firstname",
                         "station_feedback_surname",
                         "station_feedback_snumber",
                         
                         "station_ar_feedback_title",
                         "station_ar_feedback_firstname",
                         "station_ar_feedback_surname",
                         "station_ar_feedback_snumber",
                         
                         "station_fu_feedback_title",
                         "station_fu_feedback_firstname",
                         "station_fu_feedback_surname",
                         "station_fu_feedback_snumber",
                         
                         "station_p41_feedback_title",
                         "station_p41_feedback_firstname",
                         "station_p41_feedback_surname",
                         "station_p41_feedback_snumber",
                         
                         "landmark",

                         lapply(1:num_var_repeat, function(i) paste0("victim_police_officer_serviceno_", i)), # TODO: Just change to i:num_var_repeat in paste()
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_police_officer_serviceno_", i)), # TODO: Just change to i:num_var_repeat in paste()
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_veh_make_", i)),
                         lapply(1:num_var_repeat, function(i) paste0(" prtcpnt_veh_model_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_name_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_address_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_telephone_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_nationality_other_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_nationalid_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_vehicle_damage_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_regnumber_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_drivinglic_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_roadlic_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_insurance_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_inscert_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_psv_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_age_years_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_vehpart_type_other_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_hospital_taken_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_hospital_admno_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_mortuary_taken_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_mortuary_admno_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("prtcpnt_injured_comments_", i)),
                         
                         lapply(1:num_var_repeat, function(i) paste0("witness_",i,"_name")),
                         lapply(1:num_var_repeat, function(i) paste0("witness_",i,"_address")),
                         
                         lapply(1:num_var_repeat, function(i) paste0("victim_name_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("victim_address_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("victim_telephone_", i)),
                         
                         lapply(1:num_var_repeat, function(i) paste0("victim_age_years_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("victim_refnum_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("victim_hospital_admno_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("victim_hospital_taken_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("victim_mortuary_taken_", i)),
                         lapply(1:num_var_repeat, function(i) paste0("victim_mortuary_admno_", i))) %>% unlist

textAreaInput_variables <- c("accident_location", 
                             "primarily_responsible", 
                             "remarks",
                             "crash_description",
                             "sr_hq_comments",
                             "sr_station_comments",
                             "ar_station_comments",
                             "fu_station_comments",
                             "p41_station_comments",
                             lapply(1:num_var_repeat, function(i) paste0("prtcpnt_ped_comments_", i)),
                             lapply(1:num_var_repeat, function(i) paste0("victim_comments_", i))) %>% unlist

selectInput_variables <- c("accident_type",
                           "police_division",
                           "police_station",
                           "road_authority",
                           "urban_rural",
                           "hit_and_run",
                           "road_name",
                           "landmark",
                           "road_surface",
                           "road_damaged_yn",
                           "junction_yn",
                           "railway_crossing",
                           "road_works",
                           "alcohol_involved",
                           lapply(1:num_var_repeat, function(i) paste0("prtcpnt_nationality_", i)),
                           lapply(1:num_var_repeat, function(i) paste0("prtcpnt_drivinglic_valid_", i)),
                           lapply(1:num_var_repeat, function(i) paste0("prtcpnt_roadlic_valid_", i)),
                           lapply(1:num_var_repeat, function(i) paste0("prtcpnt_inscert_valid_", i)),
                           lapply(1:num_var_repeat, function(i) paste0("prtcpnt_psv_valid_", i)),
                           lapply(1:num_var_repeat, function(i) paste0("prtcpnt_injured_yn_", i)),
                           lapply(1:num_var_repeat, function(i) paste0("prtcpnt_ped_crossing_", i)),
                           lapply(1:num_var_repeat, function(i) paste0("prtcpnt_ped_along_",i)),
                           lapply(1:num_var_repeat, function(i) paste0("prtcpnt_position_",i)),
                           lapply(1:num_var_repeat, function(i) paste0("prtcpnt_seatbelt_", i)),
                           lapply(1:num_var_repeat, function(i) paste0("victim_gender_", i)),
                           lapply(1:num_var_repeat, function(i) paste0("victim_position_", i)),
                           lapply(1:num_var_repeat, function(i) paste0("victim_seatbelt_", i))) %>% unlist

numericInput_variables <- c("accident_time_hour",
                            "accident_time_minute",
                            "accident_reported_time_hour",
                            "accident_reported_time_minute",
                            "surface_width",
                            "N_participants",
                            "N_victims",
                            "N_witnesses",
                            "crash_lat",
                            "crash_long")

radioButtons_variables <- c("surface_wet_dry",
                            "weather_conditions",
                            "time_of_day",
                            "street_lights",
                            "road_damaged_type_potholes",
                            "road_damaged_type_damagededges",
                            "road_damaged_type_corrugated",
                            "road_damaged_type_loosestones",
                            "intended_prosecution",
                            lapply(1:num_var_repeat, function(i) paste0("prtcpnt_ped_pushcart_qs_sr_",i)),
                            lapply(1:num_var_repeat, function(i) paste0("victim_police_officer_",i)),
                            lapply(1:num_var_repeat, function(i) paste0("prtcpnt_police_officer_",i)),
                            lapply(1:num_var_repeat, function(i) paste0("prtcpnt_vehpart_type_", i)),
                            lapply(1:num_var_repeat, function(i) paste0("prtcpnt_owner_driver_", i)),
                            lapply(1:num_var_repeat, function(i) paste0("prtcpnt_injurytype_", i)),
                            lapply(1:num_var_repeat, function(i) paste0("prtcpnt_personclass_", i)),
                            lapply(1:num_var_repeat, function(i) paste0("prtcpnt_gender_",i)),
                            lapply(1:num_var_repeat, function(i) paste0("prtcpnt_vehicle_adn_info_",i)),
                            "junction_type",
                            "junction_signs_signals",
                            "junction_signals_operating",
                            "cause_code_category",
                            "cause_code_value",
                            lapply(1:num_var_repeat, function(i) paste0("victim_personclass_",i)),
                            lapply(1:num_var_repeat, function(i) paste0("victim_injurytype_",i))) %>% unlist

dateInput_variables <- c("accident_date", "accident_reported_date")
timeInput_variables <- c("accident_time")
checkBoxGroupInput_variables <- c("road_damaged_type")

dataset_variables <- c(dateInput_variables, 
                       textInput_variables, 
                       textAreaInput_variables, 
                       selectInput_variables, 
                       numericInput_variables, 
                       radioButtons_variables, 
                       checkBoxGroupInput_variables,
                       sr_variables,
                       ar_vars,
                       fu_vars) %>%
  unique()



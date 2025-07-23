# Function that preps data for the P41

#data_i <- read.csv("~/Desktop/DATA.csv", stringsAsFactors = F)

prep_data_for_p41 <- function(data_i){
  
  data_i <- as.data.frame(data_i)
  
  # Prep Variables ---------------------------------------------------------------
  ## As Character
  for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
  
  # General Information ----------------------------------------------------------
  
  #### Road Authority
  data_i$road_authority_MOTC         <- ifelse_ignoreNULL(grepl("MOTC",         data_i$road_authority), "X", "")
  data_i$road_authority_Municipality <- ifelse_ignoreNULL(grepl("Municipality", data_i$road_authority), "X", "")
  data_i$road_authority_Other        <- ifelse_ignoreNULL(grepl("Other",        data_i$road_authority), "X", "")
  
  #### Urban/Rural
  data_i$urban_rural_Urban <- ifelse_ignoreNULL(grepl("Urban", data_i$urban_rural), "X", "")
  data_i$urban_rural_Rural <- ifelse_ignoreNULL(grepl("Rural", data_i$urban_rural), "X", "")
  
  #### Road Surface
  data_i$road_surface_Tarmac <- ifelse_ignoreNULL(data_i$road_surface %in% "Tarmac", "X", "")
  data_i$road_surface_Murram <- ifelse_ignoreNULL(data_i$road_surface %in% "Murram", "X", "")
  data_i$road_surface_Earth  <- ifelse_ignoreNULL(data_i$road_surface %in% "Earth", "X", "")
  
  #### Wet/Dry Surface
  data_i$surface_wet_dry_Wet <- ifelse_ignoreNULL(data_i$surface_wet_dry %in% "wet", "X", "")
  data_i$surface_wet_dry_Dry <- ifelse_ignoreNULL(data_i$surface_wet_dry %in% "dry", "X", "")
  
  #### Weather
  data_i$weather_conditions_clear  <- ifelse_ignoreNULL(data_i$weather_conditions %in% "clear", "X", "")
  data_i$weather_conditions_cloudy <- ifelse_ignoreNULL(data_i$weather_conditions %in% "cloudy", "X", "")
  data_i$weather_conditions_foggy  <- ifelse_ignoreNULL(data_i$weather_conditions %in% "foggy", "X", "")
  data_i$weather_conditions_rainy  <- ifelse_ignoreNULL(data_i$weather_conditions %in% "rainy", "X", "")
  
  #### Damaged/Not Damaged
  data_i$road_damaged_yn_damaged    <- ifelse_ignoreNULL(data_i$road_damaged_yn %in% "damaged", "X", "")
  data_i$road_damaged_yn_notdamaged <- ifelse_ignoreNULL(data_i$road_damaged_yn %in% "not damaged", "X", "")
  
  #### How Damaged
  data_i$road_damaged_type_potholes     <- ifelse_ignoreNULL(grepl("potholes", paste0(data_i$road_damaged_type, collapse=";", sep="")), "X", "")
  data_i$road_damaged_type_damagededges <- ifelse_ignoreNULL(grepl("damaged edges", paste0(data_i$road_damaged_type, collapse=";", sep="")), "X", "")
  data_i$road_damaged_type_corrugated   <- ifelse_ignoreNULL(grepl("corrugated", paste0(data_i$road_damaged_type, collapse=";", sep="")), "X", "")
  data_i$road_damaged_type_loosestones  <- ifelse_ignoreNULL(grepl("loose stones on the surface", paste0(data_i$road_damaged_type, collapse=";", sep="")), "X", "")
  
  #### Junction/No Junction
  data_i$junction_yn_Yes <- ifelse_ignoreNULL(data_i$junction_yn %in% "Yes", "X", "")
  data_i$junction_yn_No  <- ifelse_ignoreNULL(data_i$junction_yn %in% "No", "X", "")
  
  #### Junction Type
  data_i$junction_type_tjunction  <- ifelse_ignoreNULL(data_i$junction_type %in% "T-junction", "X", "")
  data_i$junction_type_4leg       <- ifelse_ignoreNULL(data_i$junction_type %in% "4-leg junction", "X", "")
  data_i$junction_type_roundabout <- ifelse_ignoreNULL(data_i$junction_type %in% "roundabout", "X", "")
  data_i$junction_type_other      <- ifelse_ignoreNULL(data_i$junction_type %in% "other junction", "X", "")
  
  #### Junction Signs and Signals
  data_i$junction_signs_signals_giveway        <- ifelse_ignoreNULL(data_i$junction_signs_signals %in% "give way", "X", "")
  data_i$junction_signs_signals_stop           <- ifelse_ignoreNULL(data_i$junction_signs_signals %in% "stop", "X", "")
  data_i$junction_signs_signals_nosigns        <- ifelse_ignoreNULL(data_i$junction_signs_signals %in% "no signs", "X", "")
  data_i$junction_signs_signals_nolightsignals <- ifelse_ignoreNULL(data_i$junction_signs_signals %in% "no traffic light signals", "X", "")
  
  #### Signals Operating
  data_i$junction_signals_operating_Operating    <- ifelse_ignoreNULL(data_i$junction_signals_operating %in% "operating", "X", "")
  data_i$junction_signals_operating_NotOperating <- ifelse_ignoreNULL(data_i$junction_signals_operating %in% "not operating", "X", "")
  
  #### Time of Day
  data_i$time_of_day_daylight  <- ifelse_ignoreNULL(data_i$time_of_day %in% "daylight", "X", "")
  data_i$time_of_day_nighttime <- ifelse_ignoreNULL(data_i$time_of_day %in% "night time 6.45pm - 6.15am", "X", "")
  
  #### Street Lights
  data_i$street_lights_lightson       <- ifelse_ignoreNULL(data_i$street_lights %in% "street lights on", "X", "")
  data_i$street_lights_nostreetlights <- ifelse_ignoreNULL(data_i$street_lights %in% "no street lights", "X", "")
  
  #### Railway Crossing
  data_i$railway_crossing_uncontrolled <- ifelse_ignoreNULL(data_i$railway_crossing %in% "uncontrolled", "X", "")
  data_i$railway_crossing_controlled   <- ifelse_ignoreNULL(data_i$railway_crossing %in% "controlled", "X", "")
  data_i$railway_crossing_none         <- ifelse_ignoreNULL(data_i$railway_crossing %in% "no railway crossing", "X", "")
  
  #### Road Works
  data_i$road_works_yes <- ifelse_ignoreNULL(data_i$road_works %in% "yes", "X", "")
  data_i$road_works_no  <- ifelse_ignoreNULL(data_i$road_works %in% "no", "X", "")
  
  #### Alcohol Involved
  data_i$alcohol_involved_Yes <- ifelse_ignoreNULL(data_i$alcohol_involved %in% "Yes", "X", "")
  data_i$alcohol_involved_No  <- ifelse_ignoreNULL(data_i$alcohol_involved %in% "No", "X", "")
  
  #### Intended Prosecution
  data_i$intended_prosecution_Yes <- ifelse_ignoreNULL(grepl("Yes", data_i$intended_prosecution), "X", "")
  data_i$intended_prosecution_No  <- ifelse_ignoreNULL(grepl("No", data_i$intended_prosecution), "X", "")
  
  #### Accident Time
  data_i$accident_time <- paste0(data_i$accident_time_hour, ":",
                                 data_i$accident_time_minute) %>%
    str_replace_all("NA", "")
  
  #### Pad Witness Variables
  # for(i in 1:10){
  #   if(is.null(data_i[[paste0("witness_",i,"_name")]]))    data_i[[paste0("witness_",i,"_name")]] <- ""
  #   if(is.null(data_i[[paste0("witness_",i,"_address")]])) data_i[[paste0("witness_",i,"_address")]] <- ""
  # }
  
  #### Cause Code
  if(length(data_i$cause_code_value) %in% 0){
    data_i$cause_code <- ""
  } else{
    data_i$cause_code <- data_i$cause_code_value %>% 
      substring(1,3) %>% 
      str_replace_all("[[:punct:]]", "") %>% 
      str_squish()
  }
  
  #### Crash Sketch
  # file.path(file.path("..", "data", "crash_sketches", paste0(data_i$uid, ".png")))
  if("yes" %in% data_i$crash_sketch_uploaded){ 
    data_i$crash_sketch_image_path <- file.path("data", "crash_sketches", paste0(data_i$uid, ".png"))
  } else{
    data_i$crash_sketch_image_path <- file.path("data", "crash_sketches", paste0("blank_image", ".png"))
  }
  
  #cause_code_categories <- c("cause_code_driver", "cause_code_pedal_cyclist", "cause_code_pedestrian", 
  #                           "cause_code_passengers", "cause_code_animals", "cause_code_obstruction", 
  #                           "cause_code_vehicle_defect", "cause_code_road_defect", "cause_code_weather", 
  #                           "cause_code_other")
  #cause_code <- lapply(cause_code_categories, function(cat) data_i[[cat]]) %>% unlist
  #if(length(cause_code) > 0){
  #}
  
  # Create Participants/Victims Dataframes -------------------------------------
  prtcpnt_victim_df_list <- create_prtcpnt_victim_dfs(data_i)
  
  prtcpnt_df <- prtcpnt_victim_df_list$prtcpnt_df
  victim_df <- prtcpnt_victim_df_list$victim_df
  
  # Add variables to data_i from participant/victim dataframe ------------------
  
  ## Update N victims with victims from pedestrians
  data_i$N_victims <- nrow(victim_df)
  
  ## Pedestrian crossing details
  # Use the first pedestrian
  if(!is.null(prtcpnt_df$vehpart_type)){
    ped_i <- prtcpnt_df %>% 
      filter(vehpart_type %in% "Pedestrian") %>% 
      head(1)
  } else{
    ped_i <- data.frame(NULL)
  }

  if(nrow(ped_i) > 0){
    
    data_i$ped_crossing_OnCrossing <- ifelse_ignoreNULL(ped_i$ped_crossing %in% "On pedestrian crossing", "X", " ")
    data_i$ped_crossing_1_25m      <- ifelse_ignoreNULL(ped_i$ped_crossing %in% "1-25m from pedestrian crossing", "X", " ")
    data_i$ped_crossing_over_25m   <- ifelse_ignoreNULL(ped_i$ped_crossing %in% "Over 25m from pedestrian crossing", "X", " ")
    
    data_i$ped_along_inDirOfTraffic <- ifelse_ignoreNULL(ped_i$ped_along %in% "In the direction of traffic", "X", " ")
    data_i$ped_along_towardTraffic  <- ifelse_ignoreNULL(ped_i$ped_along %in% "Towards the traffic", "X", " ")
    
    data_i$ped_comments <- ped_i$ped_comments
  } 
  
  # Add variables to participants dataframe --------------------------------------
  ## Type of Vehicle Involved
  
  ## If no participants, add dummy row
  if(nrow(prtcpnt_df) %in% 0){
    prtcpnt_df <- bind_rows(prtcpnt_df, data.frame(matrix(nrow = 1, ncol=0)))
  }
  
  if(is.null(prtcpnt_df$vehpart_type)) prtcpnt_df$vehpart_type <- NA
  prtcpnt_df$salooncar    <- ifelse_ignoreNULL(prtcpnt_df$vehpart_type %in% "Saloon Car",      "X", "")
  prtcpnt_df$pickupvan    <- ifelse_ignoreNULL(prtcpnt_df$vehpart_type %in% "Pickup van",      "X", "")
  prtcpnt_df$lorry        <- ifelse_ignoreNULL(prtcpnt_df$vehpart_type %in% "Lorry",           "X", "")
  prtcpnt_df$lorrytrailer <- ifelse_ignoreNULL(prtcpnt_df$vehpart_type %in% "Lorry + trailer", "X", "")
  prtcpnt_df$bus          <- ifelse_ignoreNULL(prtcpnt_df$vehpart_type %in% "Bus",             "X", "")
  prtcpnt_df$matatu       <- ifelse_ignoreNULL(prtcpnt_df$vehpart_type %in% "Matatu",          "X", "")
  prtcpnt_df$motorcycle   <- ifelse_ignoreNULL(prtcpnt_df$vehpart_type %in% "Motor-cycle",     "X", "")
  prtcpnt_df$bicycle      <- ifelse_ignoreNULL(prtcpnt_df$vehpart_type %in% "Bicycle",         "X", "")
  prtcpnt_df$othervehicle <- ifelse_ignoreNULL(prtcpnt_df$vehpart_type %in% "Other Vehicle",   "X", "")
  prtcpnt_df$pedestrian   <- ifelse_ignoreNULL(prtcpnt_df$vehpart_type %in% "Pedestrian",      "X", "")
  
  ## Additional Vehicle Information
  if(is.null(prtcpnt_df$vehicle_adn_info)) prtcpnt_df$vehicle_adn_info <- NA
  prtcpnt_df$countrybus       <- ifelse_ignoreNULL(prtcpnt_df$vehicle_adn_info %in% "country bus",       "X", "")
  prtcpnt_df$gkbus            <- ifelse_ignoreNULL(prtcpnt_df$vehicle_adn_info %in% "GK bus",            "X", "")
  prtcpnt_df$taxcab           <- ifelse_ignoreNULL(prtcpnt_df$vehicle_adn_info %in% "taxcab",            "X", "")
  prtcpnt_df$kbs              <- ifelse_ignoreNULL(prtcpnt_df$vehicle_adn_info %in% "KBS",               "X", "")
  prtcpnt_df$institutionalbus <- ifelse_ignoreNULL(prtcpnt_df$vehicle_adn_info %in% "institutional bus", "X", "")
  prtcpnt_df$tanker           <- ifelse_ignoreNULL(prtcpnt_df$vehicle_adn_info %in% "tanker",            "X", "")
  prtcpnt_df$otherurbanbus    <- ifelse_ignoreNULL(prtcpnt_df$vehicle_adn_info %in% "other urban bus",   "X", "")
  prtcpnt_df$minibus          <- ifelse_ignoreNULL(prtcpnt_df$vehicle_adn_info %in% "mini bus",          "X", "")
  
  ## Name, Address, Owner/Driver
  if(is.null(prtcpnt_df$name)) prtcpnt_df$name <- NA
  if(is.null(prtcpnt_df$address)) prtcpnt_df$address <- NA
  if(is.null(prtcpnt_df$owner_driver)) prtcpnt_df$owner_driver <- NA
  
  prtcpnt_df <- prtcpnt_df %>%
    mutate(name_address_driver = paste(name, address, owner_driver, sep=", ") %>%
             str_replace_all(", NA", "") %>%
             str_replace_all("\\bNA\\b", ""))
  
  # Add variables to vicitm dataframe --------------------------------------------
  
  ## If no victims, add dummy row
  if(nrow(victim_df) %in% 0){
    victim_df <- bind_rows(victim_df, data.frame(matrix(nrow = 1, ncol=0)))
  }
  
  ## Injury Type
  if(is.null(victim_df$injurytype)) victim_df$injurytype <- NA
  victim_df$injurytype_Fatal   <- ifelse_ignoreNULL(victim_df$injurytype %in% "Fatal", "X", "")
  victim_df$injurytype_Serious <- ifelse_ignoreNULL(victim_df$injurytype %in% "Serious", "X", "")
  victim_df$injurytype_Slight  <- ifelse_ignoreNULL(victim_df$injurytype %in% "Slight", "X", "")
  
  ## Position in Vehicle
  if(is.null(victim_df$position)) victim_df$position <- NA
  victim_df$position[victim_df$position %in% "Front seat"] <- "FS"
  victim_df$position[victim_df$position %in% "Rear seat"] <- "RS"
  victim_df$position[victim_df$position %in% "Standing (inside)"] <- "ST"
  victim_df$position[victim_df$position %in% "An open body"] <- "OB"
  
  ## Class of Person
  if(is.null(victim_df$personclass)) victim_df$personclass <- NA
  victim_df$personclass[victim_df$personclass %in% "Pedestrian"] <- "PED"
  victim_df$personclass[victim_df$personclass %in% "Cyclist"] <- "CY"
  victim_df$personclass[victim_df$personclass %in% "Motor-cycle"] <- "MC"
  victim_df$personclass[victim_df$personclass %in% "Driver"] <- "DR"
  victim_df$personclass[victim_df$personclass %in% "Passenger"] <- "PS"
  
  ## Safety Belt
  if(is.null(victim_df$seatbelt)) victim_df$seatbelt <- NA
  victim_df$seatbelt[victim_df$seatbelt %in% "Not Relevant"] <- ""
  
  ## Gender
  if(is.null(victim_df$gender)) victim_df$gender <- NA
  victim_df$gender[victim_df$gender %in% "Male"] <- "M"
  victim_df$gender[victim_df$gender %in% "Female"] <- "F"
  
  ## Name, Address
  if(is.null(victim_df$name))    victim_df$name <- NA
  if(is.null(victim_df$address)) victim_df$address <- NA
  
  victim_df <- victim_df %>%
    mutate(name_address = paste(name, address, sep=", ") %>%
             str_replace_all(", NA", "") %>%
             str_replace_all("\\bNA\\b", ""))
  
  # Cleanup Dataframes/Variables -------------------------------------------------
  
  #### Pad dataframes
  prtcpnt_df <- pad_df(prtcpnt_df, 3)
  victim_df  <- pad_df(victim_df, 10)
  
  #### Cleanup variables
  cleanup_vars_for_p41 <- function(df){
    for(var in names(df)){
      df[[var]][is.na(df[[var]])] <- ""
      df[[var]] <- str_replace_all(df[[var]], "\\\\", "/") # \ cases error with latex
    }
    
    return(df)
  }
  
  data_i     <- cleanup_vars_for_p41(data_i)
  prtcpnt_df <- cleanup_vars_for_p41(prtcpnt_df)
  victim_df  <- cleanup_vars_for_p41(victim_df)
  
  #### Return
  return(list(data_i = data_i,
              prtcpnt_df = prtcpnt_df,
              victim_df = victim_df))
}


















#### Pedestrians
## Create pedestrian dataframe
# pedestrian_df <- map_df(1:10, function(i){
#   pedestrian_df_i <- data.frame(matrix(nrow=1,ncol=0))
#   
#   pedestrian_df_i$vehpart_type <- ifelse_ignoreNULL(length(data_i[[paste0("prtcpnt_vehpart_type_", i)]]) %in% 0, "", data_i[[paste0("prtcpnt_vehpart_type_", i)]])
#   pedestrian_df_i$ped_crossing <- ifelse_ignoreNULL(length(data_i[[paste0("prtcpnt_ped_crossing_", i)]]) %in% 0, "", data_i[[paste0("prtcpnt_ped_crossing_", i)]])
#   pedestrian_df_i$ped_along    <- ifelse_ignoreNULL(length(data_i[[paste0("prtcpnt_ped_along_", i)]]) %in% 0, "", data_i[[paste0("prtcpnt_ped_along_", i)]])
#   pedestrian_df_i$ped_comments <- ifelse_ignoreNULL(length(data_i[[paste0("prtcpnt_ped_comments_", i)]]) %in% 0, "", data_i[[paste0("prtcpnt_ped_comments_", i)]])
#   
#   for(var in names(pedestrian_df_i)){
#     if(length(pedestrian_df_i[[var]][1]) %in% 0) pedestrian_df_i[[var]] <- ""
#   }
#   
#   return(pedestrian_df_i)
# })
# 
# pedestrian_df$ped_crossing_OnCrossing <- ifelse_ignoreNULL(pedestrian_df$ped_crossing %in% "On pedestrian crossing", "X", " ")
# pedestrian_df$ped_crossing_1_25m      <- ifelse_ignoreNULL(pedestrian_df$ped_crossing %in% "1-25m from pedestrian crossing", "X", " ")
# pedestrian_df$ped_crossing_over_25m   <- ifelse_ignoreNULL(pedestrian_df$ped_crossing %in% "Over 25m from pedestrian crossing", "X", " ")
# 
# pedestrian_df$ped_along_inDirOfTraffic <- ifelse_ignoreNULL(pedestrian_df$ped_along %in% "In the direction of traffic", "X", " ")
# pedestrian_df$ped_along_towardTraffic  <- ifelse_ignoreNULL(pedestrian_df$ped_along %in% "Towards the traffic", "X", " ")
# 
# pedestrian_df <- pedestrian_df[pedestrian_df$vehpart_type %in% "Pedestrian",]
# 
# if(nrow(pedestrian_df) %in% 0){
#   pedestrian_df <- data.frame(ped_crossing_OnCrossing=" ",ped_crossing_1_25m=" ", ped_crossing_over_25m=" ",
#                               ped_along_inDirOfTraffic=" ", ped_along_towardTraffic=" ",
#                               ped_comments=" ")
# } 
# pedestrian_df <- pedestrian_df[1,]

# # Victims ----------------------------------------------------------------------
# #### Create victims dataframe
# if(!is.numeric(data_i$N_participants)){
#   N_participants <- 1
# } else{
#   N_participants <- data_i$N_participants
# }
# 
# participant_victims_df <- lapply(1:N_participants, function(i){
#   
#   df_out_i <- tryCatch({
#     
#     victim_df_i <- data.frame(matrix(nrow=1,ncol=0))
#     
#     victim_df_i$name <- data_i[[paste0("prtcpnt_name_", i)]] %>% as.character
#     victim_df_i$address <- data_i[[paste0("prtcpnt_address_", i)]] %>% as.character
#     victim_df_i$injurytype <- data_i[[paste0("prtcpnt_injurytype_", i)]] %>% as.character
#     victim_df_i$refno <- i  %>% as.character
#     victim_df_i$personclass <- data_i[[paste0("prtcpnt_personclass_", i)]] %>% as.character
#     victim_df_i$age_years <- data_i[[paste0("prtcpnt_age_years_", i)]] %>% as.character
#     victim_df_i$gender <- data_i[[paste0("prtcpnt_gender_", i)]] %>% as.character
#     victim_df_i$position <- data_i[[paste0("prtcpnt_position_", i)]] %>% as.character
#     victim_df_i$seatbelt <- data_i[[paste0("prtcpnt_seatbelt_", i)]] %>% as.character
#     victim_df_i$injured_yn <- data_i[[paste0("prtcpnt_injured_yn_", i)]] %>% as.character
#     
#     return(victim_df_i)
#     
#   }, error=function(e) data.frame(NULL))
#   
#   return(df_out_i)
# }) %>% bind_rows
# 
# victim_section_df <- map_df(1:10, function(i){
#   check_error <- tryCatch({
#     
#     df_out_i <- data.frame(matrix(nrow=1,ncol=0))
#     
#     df_out_i$name <- data_i[[paste0("victim_name_",i)]] %>% as.character
#     df_out_i$address <- data_i[[paste0("victim_address_",i)]] %>% as.character
#     df_out_i$injurytype <- data_i[[paste0("victim_injurytype_",i)]] %>% as.character
#     df_out_i$refno <- data_i[[paste0("victim_refnum_",i)]]  %>% as.character
#     df_out_i$personclass <- data_i[[paste0("victim_personclass_",i)]] %>% as.character
#     df_out_i$age_years <- data_i[[paste0("victim_age_years_",i)]] %>% as.character
#     df_out_i$gender <- data_i[[paste0("victim_gender_",i)]] %>% as.character
#     df_out_i$position <- data_i[[paste0("victim_position_",i)]] %>% as.character
#     df_out_i$seatbelt <- data_i[[paste0("victim_seatbelt_",i)]] %>% as.character
#     df_out_i$injured_yn <- "Yes"
#     
#     return(df_out_i)
#     
#   }, error=function(e) data.frame(NULL))
#   
#   return(df_out_i)
# })
# 
# # victim_df not found TODO ##############################################################################################
# if(!exists("participant_victims_df")) participant_victims_df <- data.frame(NULL)
# if(!exists("victim_df")) victim_df <- data.frame(NULL)
# victim_df <- bind_rows(participant_victims_df,
#                        victim_section_df) 
# #victim_df <- victim_section_df
# 
# # TODO: Issues here
# # victim_df <- victim_df[victim_df$injured_yn %in% "Yes",]
# # victim_df <- victim_df[1:N_victims_total,]
# 
# for(var in names(victim_df)){
#   victim_df[[var]] <- victim_df[[var]] %>% as.character
#   victim_df[[var]][is.na(victim_df[[var]])] <- ""
#   victim_df[[var]][victim_df[[var]] %in% "."] <- ""
# }
# 
# #### Prep variables
# ## Name/Address
# victim_df$name_address <- paste0(victim_df$name,
#                                  ifelse(victim_df$address %in% "",
#                                         "",
#                                         paste0(", ", victim_df$address)))
# 
# 
# 
# ##### OTHER
# #data_i$N_participants
# 
# for(var in names(data_i)){
#   if(is.na(data_i[[var]])) data_i[[var]] <- ""
# }
# 
# #### Participant checks
# N_part_check <- paste0("_",1:N_participants) %>% paste(collapse = "|")
# prtcnpt_vars <- names(data_i)[grepl("prtcpnt_", names(data_i))]
# for(var in prtcnpt_vars){
#   if(!grepl(N_part_check, var)) data_i[[var]] <- ""
# }
# 
# for(var in names(part_name_address_driver)){
#   if(!grepl(N_part_check, var)) part_name_address_driver[[var]] <- ""
#   
# }
# 
# 
# 
# 
# 

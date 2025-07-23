# Commonly Used Functions

# ** General Functions ---------------------------------------------------------
# https://stackoverflow.com/questions/37239715/convert-letters-to-numbers
LETTER2num <- function(x) {utf8ToInt(x) - utf8ToInt("A") + 1L}

check_selected <- function(input_value, other_value){ 
  
  if(length(input_value) %in% 0){
    out <- other_value
  } else if(is.na(input_value)){
    out <- other_value
  } else{
    out <- input_value
  }
  
  return(out)
}

values_equal <- function(value_1, value_2){
  if(is.null(value_1)) value_1 <- ""
  if(is.null(value_2)) value_2 <- ""
  
  return(value_1 %in% value_2)
}

ifelse_ignoreNULL <- function(test, yes, no){
  
  # ifelse, but if test is NULL, then return "no" condition
  
  if(is.null(test) | length(test) %in% 0){
    out <- no
  } else{
    out <- ifelse(test, yes, no)
  }
  
  return(out)
}

is_na_null <- function(x){
  # Returns TRUE is NA or NULL; FALSE otherwise
  
  out <- FALSE
  
  out <- is.null(x)
  if(out %in% FALSE) out <- is.na(x)
  
  return(out)
}

equals_ignore_length0 <- function(data_i_value, value){
  # Check if "data_i_value" equals "value". Before checks,
  # turns NULL into ""
  
  if(length(data_i_value) %in% 0) data_i_value <- ""
  out <- data_i_value %in% value
  
  return(out)
}

pad_df <- function(df, nrows){
  
  df_blank <- data.frame(matrix(nrow=nrows,ncol=0))
  df <- bind_rows(df, df_blank)
  df <- df[1:nrows,]
  
  return(df)
}




check_selected_character0 <- function(input_value){ return(check_selected(input_value, character(0)))}
check_selected_blanktext <- function(input_value){ return(check_selected(input_value, ""))}
check_selected_period <- function(input_value){ return(check_selected(input_value, "."))}
check_selected_numeric <- function(input_value){ return(check_selected(input_value, NA))}
check_selected_date <- function(input_value){ return(check_selected(input_value, as.Date(NA)  ))} # Sys.time() %>% substring(1,10)
check_selected_blanktext_define_input <- function(input_value, text){ return(check_selected(input_value, text))}

#check_selected_character0 <- function(input_value){ return( ifelse(length(input_value) %in% 0, character(0), input_value) )}
#check_selected_blanktext <- function(input_value){ return( ifelse(length(input_value) %in% 0, character(0), input_value) )}
#check_selected_period <- function(input_value){ return( ifelse(length(input_value) %in% 0, character(0), input_value) )}

# ** Clean Data Functions ------------------------------------------------------
create_prtcpnt_victim_dfs <- function(data_i){
  # Using a single row of data as an input, creates a long datafame of 
  # victims and participants. Assumes that participants data comes in format
  # of prtcpnt_[varname]_# and victims: victim_[varname]_#. For victim dataframe,
  # appends injured participants to victim dataframe. Returns list with a
  # victim and participant dataframe
  
  data_i <- as.data.frame(data_i)
  
  # Create Participants Dataframe - - - - - - - - - - - - - - - - - - - - - - - 
  ## Wide dataset of just participant variables
  if(is.null(data_i$N_participants)) data_i$N_participants <- 1
  N_participants <- ifelse(is.na(data_i$N_participants), 1, data_i$N_participants) %>% as.numeric()
  n_partcpnt_regex <- 1:N_participants %>% paste0("$") %>% paste(collapse = "|")
  prtcpnt_varnames <- names(data_i) %>% str_subset("^prtcpnt_") %>% str_subset(n_partcpnt_regex)
  
  prtcpnt_wide <- data_i[,prtcpnt_varnames]
  
  ## Long dataset of participants
  prtcpnt_df <- data.frame(NULL)
  if(nrow(data_i) > 0){
    if(length(ncol(prtcpnt_wide)) > 0){ # NULL check
      if(ncol(prtcpnt_wide) > 1){
        
        #aaa <<- prtcpnt_wide
        #prtcpnt_wide <- aaa
        
        prtcpnt_df <- prtcpnt_wide %>%
          dplyr::mutate(id = 1) %>%
          pivot_longer(cols = -id) %>%
          dplyr::mutate(prtcpnt_N = name %>% str_replace_all(".*_", "")) %>%
          dplyr::mutate(name = name %>% 
                          str_replace_all("[[:digit:]]$", "") %>%
                          str_replace_all("[[:digit:]]$", "") %>%
                          str_replace_all("[[:digit:]]$", "") %>%
                          str_replace_all("_$", "")) %>%
          dplyr::select(-id) %>%
          pivot_wider(id_cols = prtcpnt_N,
                      names_from = name,
                      values_from = value) %>%
          arrange(prtcpnt_N)
        
        ## To character
        for(var in names(prtcpnt_df)){
          prtcpnt_df[[var]] <- prtcpnt_df[[var]] %>% as.character()
        }
        
      }
    }
  }
  
  # Create Victims Dataframe - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Wide dataset of just participant variables
  if(is.null(data_i$N_victims)) data_i$N_victims <- 0
  if(is.na(data_i$N_victims) | data_i$N_victims %in% 0){
    victim_df <- data.frame(NULL)
  } else{
    n_victim_regex <- 1:data_i$N_victims %>% paste0("$") %>% paste(collapse = "|")
    victim_varnames <- names(data_i) %>% str_subset("^victim_") %>% str_subset(n_victim_regex)
    
    victim_wide <- data_i[,victim_varnames]
    
    ## Long dataset of participants
    victim_df <- victim_wide %>%
      dplyr::mutate(id = 1) %>%
      #melt(id.var = "id") %>%
      pivot_longer(cols = -id) %>%
      dplyr::mutate(victim_N = name %>% str_replace_all(".*_", "")) %>%
      dplyr::mutate(name = name %>% 
                      str_replace_all("[[:digit:]]$", "") %>%
                      str_replace_all("[[:digit:]]$", "") %>%
                      str_replace_all("[[:digit:]]$", "") %>%
                      str_replace_all("_$", "")) %>%
      dplyr::select(-id) %>%
      pivot_wider(id_cols = victim_N,
                  names_from = name,
                  values_from = value) %>%
      arrange(victim_N)
    
    ## To character
    for(var in names(victim_df)){
      victim_df[[var]] <- victim_df[[var]] %>% as.character()
    }
  }
  
  ## Prep participants to append 
  if(!is.null(prtcpnt_df$prtcpnt_injured_yn)){
    victim_prtcpnt_df <- prtcpnt_df %>% dplyr::filter(prtcpnt_injured_yn %in% "Yes")
  } else{
    victim_prtcpnt_df <- prtcpnt_df 
  }
  
  names(victim_prtcpnt_df) <- names(victim_prtcpnt_df) %>% str_replace_all("^prtcpnt_","victim_")
  victim_prtcpnt_df$victim_refnum <- victim_prtcpnt_df$victim_N
  
  ## Append participants to victims
  victim_df <- bind_rows(victim_df, victim_prtcpnt_df)
  
  # Cleanup - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  names(prtcpnt_df) <- names(prtcpnt_df) %>% str_replace_all("^prtcpnt_", "")
  names(victim_df)  <- names(victim_df) %>% str_replace_all("^victim_", "")
  
  # Return - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  return(list(prtcpnt_df = prtcpnt_df,
              victim_df = victim_df))
}

create_prtcpnt_victim_fulldata_dfs <- function(data_i){
  # Using all data, creates a long datafame of 
  # victims and participants. Assumes that participants data comes in format
  # of prtcpnt_[varname]_# and victims: victim_[varname]_#. For victim dataframe,
  # appends injured participants to victim dataframe. Returns list with a
  # victim and participant dataframe.
  # Assumes one row per uid!
  
  data_i <- as.data.frame(data_i)
  
  for(var in names(data_i)) data_i[[var]] <- data_i[[var]] %>% as.character()
  
  # Create Participants Dataframe - - - - - - - - - - - - - - - - - - - - - - - 
  ## Wide dataset of just participant variables
  prtcpnt_varnames <- names(data_i) %>% str_subset("^prtcpnt_|uid|N_participants") #  
  
  prtcpnt_wide <- data_i[,prtcpnt_varnames]
  
  ## Long dataset of participants
  prtcpnt_df <- data.frame(NULL)
  
  if(nrow(data_i) > 0){
    if(length(ncol(prtcpnt_wide)) > 0){ # NULL check
      if(ncol(prtcpnt_wide) > 2){
        
        prtcpnt_df <- prtcpnt_wide %>%
          pivot_longer(cols = -c(uid, N_participants)) %>%
          mutate(prtcpnt_N = name %>% str_replace_all(".*_", "")) %>%
          mutate(name = name %>% 
                   str_replace_all("[[:digit:]]$", "") %>%
                   str_replace_all("[[:digit:]]$", "") %>%
                   str_replace_all("[[:digit:]]$", "") %>%
                   str_replace_all("_$", "")) %>%
          mutate(N_participants = replace_na(N_participants, 1) %>% as.numeric,
                 prtcpnt_N = prtcpnt_N %>% as.numeric) %>%
          dplyr::filter(prtcpnt_N <= N_participants) %>%
          pivot_wider(id_cols = c(uid, prtcpnt_N),
                      names_from = name,
                      values_from = value) 
        
        ## To character
        for(var in names(prtcpnt_df)) prtcpnt_df[[var]] <- prtcpnt_df[[var]] %>% as.character()
      }
    }
  }
  
  
  # Create Victims Dataframe - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Wide dataset of just victim variables
  victim_varnames <- names(data_i) %>% str_subset("^victim_|uid|N_victims")
  
  victim_wide <- data_i[,victim_varnames]
  
  ## Long dataset of participants
  victim_df <- data.frame(NULL)
  
  if(nrow(data_i) > 0){
    if(length(ncol(victim_wide)) > 0){ # NULL check
      if(ncol(victim_wide) > 2){
        
        victim_df <- victim_wide %>%
          pivot_longer(cols = -c(uid, N_victims)) %>%
          #melt(id.var = c("uid", "N_victims")) %>%
          mutate(victim_N = name %>% str_replace_all(".*_", "")) %>%
          mutate(name = name %>% 
                   str_replace_all("[[:digit:]]$", "") %>%
                   str_replace_all("[[:digit:]]$", "") %>%
                   str_replace_all("[[:digit:]]$", "") %>%
                   str_replace_all("_$", "")) %>%
          mutate(N_victims = replace_na(N_victims, 0) %>% as.numeric(),
                 victim_N = victim_N %>% as.numeric()) %>%
          dplyr::filter(victim_N <= N_victims) %>%
          pivot_wider(id_cols = c(uid, victim_N),
                      names_from = name,
                      values_from = value) 
        
        ## To character
        for(var in names(victim_df)) victim_df[[var]] <- victim_df[[var]] %>% as.character()
      }
    }
  }
  
  ## Prep participants to append 
  if(!is.null(prtcpnt_df$prtcpnt_injured_yn)){
    victim_prtcpnt_df <- prtcpnt_df %>% dplyr::filter(prtcpnt_injured_yn %in% "Yes")
  } else{
    victim_prtcpnt_df <- prtcpnt_df 
  }
  
  names(victim_prtcpnt_df) <- names(victim_prtcpnt_df) %>% str_replace_all("^prtcpnt_","victim_")
  victim_prtcpnt_df$victim_refnum <- victim_prtcpnt_df$victim_N
  
  ## Append participants to victims
  victim_df <- bind_rows(victim_df, victim_prtcpnt_df)
  
  # Cleanup - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  names(prtcpnt_df) <- names(prtcpnt_df) %>% str_replace_all("^prtcpnt_", "")
  names(victim_df)  <- names(victim_df) %>% str_replace_all("^victim_", "")
  
  # Return - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  return(list(prtcpnt_df = prtcpnt_df,
              victim_df = victim_df))
}


# ** Shiny Functions -----------------------------------------------------------

buttonInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

mk_stage_sr_clean_var <- function(dataset){
  
  if(nrow(dataset) > 0){
    
    stage_sr_df <- dataset %>%
      dplyr::select(uid, stage, report_type) %>%
      filter(report_type == "sit_report") %>%
      dplyr::select(uid, stage)
    
    dataset <- dataset %>%
      dplyr::select(-stage) %>%
      left_join(stage_sr_df, by = "uid")
    
    dataset$stage_sr_clean <- NA
    
    dataset$stage_sr_clean[is.na(dataset$stage)] <- "None"
    dataset$stage_sr_clean[dataset$stage %in% "editing"] <- "Edit"
    dataset$stage_sr_clean[dataset$stage %in% "submitted_to_hq"] <- "Pending HQ approval"
    dataset$stage_sr_clean[dataset$stage %in% "hq_provided_feedback"] <- "Pending officer revision"
    dataset$stage_sr_clean[dataset$stage %in% "finalized"] <- "Completed"
    
  }
  
  return(dataset)
}

mk_stage_p41_clean_var <- function(dataset){
  
  if(nrow(dataset) > 0){
    
    stage_p41_df <- dataset %>%
      dplyr::select(uid, stage_p41, report_type) %>%
      filter(report_type == "p41") %>%
      dplyr::select(uid, stage_p41)
    
    dataset <- dataset %>%
      dplyr::select(-stage_p41) %>%
      left_join(stage_p41_df, by = "uid")
    
    dataset$stage_p41_clean <- NA
    
    dataset$stage_p41_clean[is.na(dataset$stage_p41)] <- "None"
    dataset$stage_p41_clean[dataset$stage_p41 %in% "editing"] <- "Edit"
    dataset$stage_p41_clean[dataset$stage_p41 %in% "submitted_to_hq"] <- "Pending HQ approval"
    dataset$stage_p41_clean[dataset$stage_p41 %in% "hq_provided_feedback"] <- "Pending officer revision"
    dataset$stage_p41_clean[dataset$stage_p41 %in% "finalized"] <- "Completed"
    
  }
  
  return(dataset)
}

mk_stage_ar_clean_var <- function(dataset){
  
  if(nrow(dataset) > 0){
    
    stage_ar_df <- dataset %>%
      dplyr::select(uid, stage_ar, report_type) %>%
      filter(report_type == "amendment") %>%
      dplyr::select(uid, stage_ar)
    
    dataset <- dataset %>%
      dplyr::select(-stage_ar) %>%
      left_join(stage_ar_df, by = "uid")
    
    #dataset$stage_ar_clean <- NA
    
    dataset$stage_ar_clean[is.na(dataset$stage_ar)] <- "None"
    dataset$stage_ar_clean[dataset$stage_ar %in% "editing"] <- "Edit"
    dataset$stage_ar_clean[dataset$stage_ar %in% "submitted_to_hq"] <- "Pending HQ approval"
    dataset$stage_ar_clean[dataset$stage_ar %in% "hq_provided_feedback"] <- "Pending officer revision"
    dataset$stage_ar_clean[dataset$stage_ar %in% "finalized"] <- "Completed"
    
  }
  
  return(dataset)
}

mk_stage_fu_clean_var <- function(dataset){
  
  if(nrow(dataset) > 0){
    
    stage_fu_df <- dataset %>%
      dplyr::select(uid, stage_fu, report_type) %>%
      filter(report_type == "followup") %>%
      dplyr::select(uid, stage_fu)
    
    dataset <- dataset %>%
      dplyr::select(-stage_fu) %>%
      left_join(stage_fu_df, by = "uid")
    
    #dataset$stage_fu_clean <- NA
    
    dataset$stage_fu_clean[is.na(dataset$stage_fu)] <- "None"
    dataset$stage_fu_clean[dataset$stage_fu %in% "editing"] <- "Edit"
    dataset$stage_fu_clean[dataset$stage_fu %in% "submitted_to_hq"] <- "Pending HQ approval"
    dataset$stage_fu_clean[dataset$stage_fu %in% "hq_provided_feedback"] <- "Pending officer revision"
    dataset$stage_fu_clean[dataset$stage_fu %in% "finalized"] <- "Completed"
    
  }
  
  return(dataset)
}

add_edit_button <- function(dataset, var = "stage"){
  
  dataset$stage_VAR <- dataset[[var]]
  
  #### Add Edit Button
  dataset[["Edit"]] = buttonInput(
    FUN = actionButton,
    len = nrow(dataset),
    id = 'button_',
    label = "View",
    onclick = 'Shiny.onInputChange(\"lastClick\",  this.id)'
  )
  
  # if(length(dataset$stage_VAR) > 0){
  #   if(TRUE %in% (dataset$stage_VAR %in% "editing")){
  #     
  #     dataset[["Edit"]][dataset$stage_VAR %in% "editing"] <- dataset[["Edit"]][dataset$stage_VAR %in% "editing"] %>% 
  #       str_replace_all("BUTTON_NAME", 
  #                       "Edit")
  #     
  #   }
  # }
  # 
  # if(length(dataset$stage_VAR) > 0){
  #   if(TRUE %in% (dataset$stage_VAR %in% "submitted_to_hq")){
  #     
  #     dataset[["Edit"]][dataset$stage_VAR %in% "submitted_to_hq"] <- dataset[["Edit"]][dataset$stage_VAR %in% "submitted_to_hq"] %>% 
  #       str_replace_all("BUTTON_NAME", 
  #                       "Pending HQ approval")
  #     
  #   }
  # }
  # 
  # if(length(dataset$stage_VAR) > 0){
  #   if(TRUE %in% (dataset$stage_VAR %in% "hq_provided_feedback")){
  #     
  #     dataset[["Edit"]][dataset$stage_VAR %in% "hq_provided_feedback"] <- dataset[["Edit"]][dataset$stage_VAR %in% "hq_provided_feedback"] %>% 
  #       str_replace_all("BUTTON_NAME", 
  #                       "Pending officer revision")
  #     
  #   }
  # }
  # 
  # if(length(dataset$stage_VAR) > 0){
  #   if(TRUE %in% (dataset$stage_VAR %in% "finalized")){
  #     
  #     dataset[["Edit"]][dataset$stage_VAR %in% "finalized"] <- dataset[["Edit"]][dataset$stage_VAR %in% "finalized"] %>% 
  #       str_replace_all("BUTTON_NAME", 
  #                       "View")
  #     
  #   }
  # }
  
  return(dataset)
}

# Finalize variable is NA or TRUE
check_if_finalized <- function(x){
  out <- FALSE
  
  # First check if null
  if(length(x) > 0){
    # If not null and finalized, out is finalized
    if(x %in% TRUE){
      out <- TRUE
    }
  }
  
  return(out)
}

filter_dataset <- function(df_out, input){
  
  ## ** Date
  # If don't change filters on date, do nothing. Allows observations
  # with no date to be shown -- as when restrict date these will be removed.
  start_date_default <- "2020-01-01"
  end_date_default <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% substring(1,10)
  
  if((input$dt_filter_daterange[1] %in% start_date_default) & (input$dt_filter_daterange[2] %in% end_date_default)){
    # Do nothing
  } else{
    #df_out <- df_out[!is.na(df_out$accident_date),]
    df_out <- df_out[((df_out$accident_date >= input$dt_filter_daterange[1]) & 
                        (df_out$accident_date <= input$dt_filter_daterange[2])) |
                       is.na(df_out$accident_date),]
  }
  
  ## ** Injury Type 
  if(!is.null(input$dt_filter_injury_type)){
    if(input$dt_filter_injury_type %in% "Non-Injury")   df_out <- df_out[df_out$accident_type %in% "Non-Injury",]
    if(input$dt_filter_injury_type %in% "Slight")  df_out <- df_out[df_out$accident_type %in% "Slight",]
    if(input$dt_filter_injury_type %in% "Serious") df_out <- df_out[df_out$accident_type %in% "Serious",]
    if(input$dt_filter_injury_type %in% "Fatal")   df_out <- df_out[df_out$accident_type %in% "Fatal",]
    
  }
  
  ## ** Finalized or To Be Completed - SR
  if(!is.null(input$dt_filter_final_edit)){
    if(input$dt_filter_final_edit %in% "Edit")       df_out <- df_out[df_out$stage %in% "editing",]
    if(input$dt_filter_final_edit %in% "Pending HQ approval")       df_out <- df_out[df_out$stage %in% "submitted_to_hq",]
    if(input$dt_filter_final_edit %in% "Pending officer revision (HQ feedback)")       df_out <- df_out[df_out$stage %in% "hq_provided_feedback",]
    if(input$dt_filter_final_edit %in% "Completed")       df_out <- df_out[df_out$stage %in% "finalized",]
  }
  
  ## ** Finalized or To Be Completed - P41
  if(!is.null(input$dt_filter_p41_final_edit)){
    if(input$dt_filter_p41_final_edit %in% "Not Started")       df_out <- df_out[df_out$stage_p41_clean %in% "None",]
    if(input$dt_filter_p41_final_edit %in% "Edit")       df_out <- df_out[df_out$stage_p41_clean %in% "Edit",]
    if(input$dt_filter_p41_final_edit %in% "Pending HQ approval")       df_out <- df_out[df_out$stage_p41_clean %in% "Pending HQ approval",]
    if(input$dt_filter_p41_final_edit %in% "Pending officer revision (HQ feedback)")       df_out <- df_out[df_out$stage_p41_clean %in% "Pending officer revision",]
    if(input$dt_filter_p41_final_edit %in% "Completed")       df_out <- df_out[df_out$stage_p41_clean %in% "Completed",]
  }
  
  ## ** Finalized or To Be Completed - AR
  if(!is.null(input$dt_filter_ar_final_edit)){
    if(input$dt_filter_ar_final_edit %in% "Not Started")       df_out <- df_out[df_out$stage_ar_clean %in% "None",]
    if(input$dt_filter_ar_final_edit %in% "Edit")       df_out <- df_out[df_out$stage_ar_clean %in% "Edit",]
    if(input$dt_filter_ar_final_edit %in% "Pending HQ approval")       df_out <- df_out[df_out$stage_ar_clean %in% "Pending HQ approval",]
    if(input$dt_filter_ar_final_edit %in% "Pending officer revision (HQ feedback)")       df_out <- df_out[df_out$stage_ar_clean %in% "Pending officer revision",]
    if(input$dt_filter_ar_final_edit %in% "Completed")       df_out <- df_out[df_out$stage_ar_clean %in% "Completed",]
  }
  
  ## ** Finalized or To Be Completed - FU
  if(!is.null(input$dt_filter_fu_final_edit)){
    if(input$dt_filter_fu_final_edit %in% "Not Started")               df_out <- df_out[df_out$stage_fu_clean %in% "None",]
    if(input$dt_filter_fu_final_edit %in% "Edit")                      df_out <- df_out[df_out$stage_fu_clean %in% "Edit",]
    if(input$dt_filter_fu_final_edit %in% "Pending HQ approval")       df_out <- df_out[df_out$stage_fu_clean %in% "Pending HQ approval",]
    if(input$dt_filter_fu_final_edit %in% "Pending officer revision (HQ feedback)")       df_out <- df_out[df_out$stage_fu_clean %in% "Pending officer revision",]
    if(input$dt_filter_fu_final_edit %in% "Completed")       df_out <- df_out[df_out$stage_fu_clean %in% "Completed",]
  }
  
  ## ** Station
  if(input$dt_filter_station %in% "All"){
    # Do Nothing
  } else{
    df_out <- df_out[df_out$police_division %in% input$dt_filter_station,]
  }
  
  if(Username %in% "embakasi") df_out <- df_out[df_out$police_division %in% "Embakasi",]
  if(Username %in% "langata") df_out <- df_out[df_out$police_division %in% "Langata",]
  
  df_out
  
}

## Define Function
saveRDS_version2 <- function(x, file) {
  saveRDS(x, file, version=2)
}

# ** Functions to gen sit report -----------------------------------------------
## **** Functions to generate report
sr_row1_c1_fun <- function(data_i) "P    P"
sr_row1_c2_fun <- function(data_i) "NA9/3"
sr_row1_c3_fun <- function(data_i) "7/1300C"

sr_row2_fun <- function(data_i) paste0("FROM STATPOL ", toupper(data_i$police_division))
sr_row3_fun <- function(data_i){
  paste0("TO D/IG KPS [R] POLTRAFF NBI [R] REGPOL/REGTRAFF NBI [R] SUBCOUNTYPOL ", 
         toupper(data_i$police_division))
}
sr_row4_fun <- function(data_i) "BT"
sr_row5_fun <- function(data_i) "GR"

sr_row6_c1_fun <- function(data_i) "REF."
sr_row6_c2_fun <- function(data_i) paste0("OB. ", toupper(data_i$ob_no))
sr_row6_c3_fun <- function(data_i){
  paste0(substring(data_i$accident_date, 9,10), "/",
         substring(data_i$accident_date, 6,7), "/",
         substring(data_i$accident_date, 1,4))
} 


sr_row7_fun <- function(data_i) paste("DRAFTER", 
                                      toupper(data_i$officer_filled_form_title), 
                                      toupper(data_i$officer_filled_form_firstname), 
                                      toupper(data_i$officer_filled_form_surname)) %>% str_squish()
sr_row8_fun <- function(data_i) paste0("FOR STATPOL ", toupper(data_i$police_division))
sr_row9_fun <- function(data_i) "BT"
sr_row10_fun <- function(data_i) "NNN"

# Participant Text - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
participant_text_fun <- function(data_i){
  
  #### Accords for when "input" is inputted to function
  df_input <- data.frame(matrix(nrow = 1, ncol=0))
  for(var in dataset_variables){
    if(length(data_i[[var]]) > 0){
      df_input[[var]] <- data_i[[var]] %>% as.character()
    } else{
      df_input[[var]] <- NA
    }
  }
  data_i <- df_input
  
  if(is.na(data_i$N_participants)){
    participant_text <- ""
  } else{
    
    participant_text <- lapply(1:data_i$N_participants, function(i){
      
      if(data_i[[paste0("prtcpnt_vehpart_type_", i)]] %in% c("Pedestrian")){
        txt <- paste(
          "PEDESTRIAN NAMELY",
          data_i[[paste0("prtcpnt_name_", i)]], 
          "OF",
          data_i[[paste0("prtcpnt_address_", i)]],
          "TEL",
          data_i[[paste0("prtcpnt_telephone_", i)]]
        )
      } else if(data_i[[paste0("prtcpnt_vehpart_type_", i)]] %in% c("Bicycle")){
        txt <- paste(
          "CYCLIST NAMELY",
          data_i[[paste0("prtcpnt_name_", i)]], 
          "OF",
          data_i[[paste0("prtcpnt_address_", i)]],
          "TEL",
          data_i[[paste0("prtcpnt_telephone_", i)]]
        )
      } else{
        txt <- paste(
          "INVOLVING M/V REG. NO.",
          data_i[[paste0("prtcpnt_regnumber_", i)]],
          data_i[[paste0("prtcpnt_vehpart_type_", i)]],
          "DRIVEN BY",
          data_i[[paste0("prtcpnt_name_", i)]], 
          "OF",
          data_i[[paste0("prtcpnt_address_", i)]],
          "TEL",
          data_i[[paste0("prtcpnt_telephone_", i)]]
        )
      }
      
      return(txt)
    }) %>% 
      unlist %>% 
      as.vector %>% 
      paste(collapse=" AND ") %>%
      str_replace_all(" NA ", " ")
  }
  
  return(participant_text)
  
}

# Victim Text - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
victim_text_fun <- function(data_i){
  
  #### Accords for when "input" is inputted to function
  df_input <- data.frame(matrix(nrow = 1, ncol=0))
  for(var in dataset_variables){
    if(length(data_i[[var]]) > 0){
      df_input[[var]] <- data_i[[var]] %>% as.character()
    } else{
      df_input[[var]] <- NA
    }
  }
  data_i <- df_input
  
  #### Participant Victims
  if(is.na(data_i$N_participants)){
    victim_participant_text <- ""
  } else{
    
    victim_participant_text <- lapply(1:data_i$N_participants, function(i){
      
      if(values_equal(data_i[[paste0("prtcpnt_injured_yn_", i)]], "Yes")){
        txt <- paste(data_i[[paste0("prtcpnt_name_", i)]],
                     "SUSTAINED A",
                     data_i[[paste0("prtcpnt_injurytype_", i)]],
                     "INJURY")
        
        if(!values_equal(data_i[[paste0("prtcpnt_hospital_taken_", i)]], "")){
          txt <- paste(txt, "AND TAKEN TO", data_i[[paste0("prtcpnt_hospital_taken_", i)]])
        }
        
        
      } else{
        txt <- ""
      }
      
      return(txt)
      
    }) %>% 
      unlist %>% 
      as.vector %>% 
      paste(collapse=" AND ") %>%
      str_replace_all(" NA ", " ")
  }
  
  #### Non-Participant Victims
  if(is.na(data_i$N_victims)){
    victim_nonparticipant_text <- ""
  } else{
    
    victim_nonparticipant_text <- lapply(1:data_i$N_victims, function(i){
      
      
      txt <- paste(data_i[[paste0("victim_name_", i)]],
                   "SUSTAINED A",
                   data_i[[paste0("victim_injurytype_", i)]],
                   "INJURY")
      
      if(!values_equal(data_i[[paste0("victim_hospital_taken_", i)]], "")){
        txt <- paste(txt, "AND TAKEN TO", data_i[[paste0("victim_hospital_taken_", i)]])
      }
      
      return(txt)
      
    }) %>% 
      unlist %>% 
      as.vector %>% 
      paste(collapse=" AND ") %>%
      str_replace_all(" NA ", " ")
  }
  
  #### Victims [All]
  if((nchar(victim_participant_text) > 0) &
     (nchar(victim_nonparticipant_text) > 0)){
    victim_participant_text <- paste(victim_participant_text, "AND")
  }
  
  victim_text <- paste(victim_participant_text,
                       victim_nonparticipant_text)
  
  return(victim_text)
}

# Main Text - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
main_text_fun <- function(data_i,
                          participant_text,
                          victim_text){
  
  #### Accords for when "input" is inputted to function
  df_input <- data.frame(matrix(nrow = 1, ncol=0))
  for(var in dataset_variables){
    if(length(data_i[[var]]) > 0){
      df_input[[var]] <- data_i[[var]] %>% as.character()
    } else{
      df_input[[var]] <- NA
    }
  }
  data_i <- df_input
  
  # Prep Variables for Main Text - - - - - - - - - - - - - - - - - - - -
  
  ## Hit and Run
  hit_and_run_sr <- ""
  if(length(data_i$hit_and_run) > 0){
    if("Yes" %in% data_i$hit_and_run) hit_and_run_sr <- "HIT AND RUN"
  }
  
  ## Injury Type
  #injurytype_vec <- lapply(1:10, function(i){
  #  out <- c(data_i[[paste0("victim_injurytype_",i)]], data_i[[paste0("prtcpnt_injurytype_", i)]])
  #  return(out)
  #}) %>% unlist %>% as.vector
  
  #injurytype_sr <- "Unknown"
  #if(length(injurytype_vec) > 0){
  #  if("Slight" %in% injurytype_vec) injurytype_sr <- "Slight"
  #  if("Serious" %in% injurytype_vec) injurytype_sr <- "Serious"
  #  if("Fatal" %in% injurytype_vec) injurytype_sr <- "Fatal"
  #}
  
  ## Date
  accident_date_sr <- ""
  if(!is.null(data_i$accident_date)){
    if(!is.na(data_i$accident_date)){
      accident_date_sr <- data_i$accident_date
      accident_date_sr <- strsplit(accident_date_sr, "-")[[1]]
      accident_date_sr <- paste(rev(accident_date_sr), collapse="/")
    }
  }
  
  sr_main_text <- paste(
    "SUBJECT", 
    hit_and_run_sr, 
    data_i$accident_type, 
    ifelse(data_i$accident_type %in% c(".", "Non-Injury"), "", "INJURY"),
    "TRAFFIC ACCIDENT PD THIS OCCURRED",
    accident_date_sr,
    "AT AROUND",
    paste0(data_i$accident_time_hour,":",data_i$accident_time_minute),
    data_i$accident_time,
    "HRS ALONG", 
    data_i$road_name,
    "NEAR",
    data_i$landmark,
    "INVOLVING",
    #"[loop through participants]",
    participant_text,
    "PD",
    data_i$crash_description,
    victim_text,
    "",
    "CAUSE CODE",
    data_i$cause_code_value %>% substring(1,2) %>% str_replace_all("-","") %>% str_squish(),
    "USED PD STAPOL",
    data_i$police_division,
    "DEALING PD"
  ) %>% toupper()
  
  return(sr_main_text)
}


prep_data_for_figures <- function(dataset){
  
  # Data/Vicitm/Participant Dataframes -----------------------------------------
  prtcnpt_victim_df_list <- create_prtcpnt_victim_fulldata_dfs(dataset)
  
  victim_df <- prtcnpt_victim_df_list$victim_df
  prtcpnt_df <- prtcnpt_victim_df_list$prtcpnt_df
  
  # If variables don't exist, add as NA ----------------------------------------
  add_vars <- c("crash_lat", "crash_long", "cause_code_value", "cause_code_category")
  
  if(nrow(dataset) > 0){
    for(var in add_vars) if(is.null(dataset[[var]])) dataset[[var]] <- NA
  }
  
  # Prep Variables -------------------------------------------------------------
  #### Lat/Lon
  dataset$crash_lat <- dataset$crash_lat %>% as.numeric()
  dataset$crash_long <- dataset$crash_long %>% as.numeric()
  
  #### Crash Month
  dataset$accident_month <- substring(dataset$accident_date, 1, 7)
  
  #### Injuries and Worst Injury Type
  if((TRUE %in% grepl("injurytype", names(dataset))) & nrow(dataset) > 0){
    
    dataset$injuries <- dataset %>%
      dplyr::select(contains("injurytype")) %>%
      apply(1, paste, collapse=";") %>%
      str_replace_all("NA;|;NA", "") %>%
      str_replace_all("NA", "")
    
    # dataset$injuries <- apply(dataset[,grepl("injurytype", names(dataset))], 1, paste, collapse=";") %>%
    #   str_replace_all("NA;|;NA", "") %>%
    #   str_replace_all("NA", "")
  } else{
    dataset <- dataset %>% dplyr::mutate(injuries = "")
  }
  
  #### Worst Injury
  # dataset$worst_injury <- dataset$accident_type %>% 
  #   na_if(".") %>%
  #   factor(levels = c("Fatal", 
  #                     "Serious",
  #                     "Slight",
  #                     "Non-Injury"))
  
  if(nrow(victim_df) > 0){
    wrst_inj_df <- victim_df %>%
      dplyr::filter(!is.na(injurytype)) %>%
      dplyr::mutate(injurytype_rank = 
                      case_when(injurytype == "Fatal" ~ 3,
                                injurytype == "Serious" ~ 2,
                                injurytype == "Slight" ~ 1)) %>%
      arrange(desc(injurytype_rank)) %>%
      dplyr::distinct(uid, .keep_all = T) %>%
      dplyr::select(uid, injurytype) %>%
      dplyr::rename(worst_injury = injurytype)
    
    dataset <- dataset %>%
      left_join(wrst_inj_df, by = "uid") %>%
      dplyr::mutate(worst_injury = worst_injury %>% replace_na("Non-Injury"))
    
  } else{
    dataset <- dataset %>%
      dplyr::mutate(worst_injury = NA)
  }
  
  # https://www.color-hex.com/color-palette/21974
  dataset <- dataset %>%
    dplyr::mutate(worst_injury_color = "")
  if(nrow(dataset) > 0){
    dataset$worst_injury_color[dataset$worst_injury %in% "Slight"] <- "#27b376" # green
    dataset$worst_injury_color[dataset$worst_injury %in% "Serious"] <- "#264b96" # blue
    dataset$worst_injury_color[dataset$worst_injury %in% "Fatal"] <- "#f24224" # red
    dataset$worst_injury_color[dataset$worst_injury %in% "Non-Injury"] <- "black"
  }
  # dataset$cause_code_cat_color <- ""
  # dataset$cause_code_cat_color[dataset$cause_code_category %in% "Driver"] <- "" # green
  # dataset$cause_code_cat_color[dataset$cause_code_category %in% "Pedal Cyclist"] <- "" # blue
  # dataset$cause_code_cat_color[dataset$cause_code_category %in% "Pedestrian"] <- "" # red
  # dataset$cause_code_cat_color[dataset$cause_code_category %in% "Passengers etc"] <- ""
  # dataset$cause_code_cat_color[dataset$cause_code_category %in% "Animals"] <- ""
  # dataset$cause_code_cat_color[dataset$cause_code_category %in% "Obstruction"] <- ""
  # dataset$cause_code_cat_color[dataset$cause_code_category %in% "Vehicle Defect"] <- ""
  # dataset$cause_code_cat_color[dataset$cause_code_category %in% "Road Defect"] <- ""
  # dataset$cause_code_cat_color[dataset$cause_code_category %in% "Weather"] <- ""
  # dataset$cause_code_cat_color[dataset$cause_code_category %in% "Other Cause"] <- ""
  
  
  #### Number Injuries
  # dataset$N_fatal_injuries <- lapply(dataset$injuries, function(x) sum((x %>% str_split(";") %>% unlist()) %in% "Fatal")) %>% unlist()
  # dataset$N_serious_injuries <- lapply(dataset$injuries, function(x) sum((x %>% str_split(";") %>% unlist()) %in% "Serious")) %>% unlist()
  # dataset$N_slight_injuries <- lapply(dataset$injuries, function(x) sum((x %>% str_split(";") %>% unlist()) %in% "Slight")) %>% unlist()
  
  #### Cause Codes
  dataset$cause_code_value <- dataset$cause_code_value %>%
    str_replace_all(".* - ", "") %>%
    str_squish() %>%
    str_to_sentence()
  
  dataset$cause_code_category <- dataset$cause_code_category %>%
    str_replace_all("\\(.*", "") %>%
    str_squish() %>%
    factor(levels = c("Driver",
                      "Pedal Cyclist",
                      "Pedestrian",
                      "Passengers etc",
                      "Animals",
                      "Obstruction",
                      "Vehicle Defect",
                      "Road Defect",
                      "Weather",
                      "Other Cause"))
  
  # Order cause code factor
  dataset$cause_code_value <- dataset$cause_code_value %>% 
    fct_reorder(as.numeric(dataset$cause_code_category)) %>% 
    fct_rev()
  
  return(list(dataset = dataset,
              victim_df = victim_df,
              prtcpnt_df = prtcpnt_df))
}


cause_code_text_to_value <- function(data_table){
  
  data_table <- data_table %>%
    dplyr::mutate(cause_code_number =
                    case_when(cause_code_category %in% "Driver" & cause_code_value %in% "Fatigued" ~ "1",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Asleep" ~ "2",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Ill" ~ "3",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Under the influence of drink or a drug" ~ "4",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Physically defective" ~ "5",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Inexperienced with the type of motor vehicle" ~ "6",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Proceeding at excessive speed having regard to conditions" ~ "7",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Failing to keep to near side or to the proper lane" ~ "8",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Cutting in" ~ "9",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Overtaking improperly" ~ "10",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Swerving" ~ "11",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Skidding (give cause of skidding)" ~ "12",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Forcing way through persons boarding or alighting from omnibu" ~ "13",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Failing to stop to afford free passage to pedestrians crossing" ~ "14",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Turning round in road negligently" ~ "15",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Reversing negligently (other than from parking area)" ~ "16",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Failing to comply with traffic sign or signal" ~ "17",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Failing to signal or giving indistinct or incorrect signal" ~ "18",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Pulling out from near side or from one traffic lane (not from parking area) to another without due care)" ~ "19",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Inattentive or attention divered" ~ "20",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Hampered by passenger, animal or luggage in or on vehicle" ~ "21",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Turning right without due care" ~ "22",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Turning left without due care" ~ "23",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Driver negligently opening door of the vehicle" ~ "24",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Crossing without due care at road junction" ~ "25",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Losing control (particulars to be specified)" ~ "26",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Dazzed by lights of another vehicle" ~ "27",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Stopping suddenly" ~ "28",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Misjudging clearance, distance or speed (vehicle or object)" ~ "29",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Other apparent error of judgement or negligence (specify)" ~ "30",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Reversing form angle parking space negligently" ~ "30a",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Entering parking space (angle or flush) negligently" ~ "30b",
                              cause_code_category %in% "Driver" & cause_code_value %in% "Leaving flush parking space negligently" ~ "30c",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Fatigued" ~ "31",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Ill" ~ "32",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Under the influence of drink or a drug" ~ "33",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Physically defective" ~ "34",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Inexperienced with the type of motor vehicle" ~ "35",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Proceeding at excessive speed having regard to conditions" ~ "36",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Failing to keep to near side or to the proper lane" ~ "37",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Cutting in" ~ "38",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Overtaking improperly" ~ "39",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Swerving" ~ "40",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Skidding (give cause of skidding)" ~ "41",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Forcing way through persons boarding or alighting from omnibu" ~ "42",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Failing to stop to afford free passage to pedestrians crossing" ~ "43",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Turning round in road negligently" ~ "44",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Failing to comply with traffic sign or signal" ~ "45",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Failing to signal or giving indistinct or incorrect signal" ~ "46",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Pulling out from near side or from one traffic lane (not from parking area) to another without due care)" ~ "47",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Inattentive or attention divered" ~ "48",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Hampered by passenger, animal or luggage in or on vehicle" ~ "49",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Turning right without due care" ~ "50",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Turning left without due care" ~ "51",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Crossing without due care at road junction" ~ "52",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Pedal cyclist holding on to another vehicle" ~ "53",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Losing control (particulars to be specified)" ~ "54",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Dazzed by lights of another vehicle" ~ "55",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Stopping suddenly" ~ "56",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Misjudging clearance, distance or speed (vehicle or object)" ~ "57",
                              cause_code_category %in% "Pedal Cyclist" & cause_code_value %in% "Other apparent error of judgement or negligence (specify)" ~ "58",
                              cause_code_category %in% "Pedestrian" & cause_code_value %in% "Heedless of traffic - crossing the road masked by stationary vehicle" ~ "59",
                              cause_code_category %in% "Pedestrian" & cause_code_value %in% "Heedless of traffic - crossing the road not masked by stationary vehicle" ~ "60",
                              cause_code_category %in% "Pedestrian" & cause_code_value %in% "Heedless of traffic - walking or standing on the road" ~ "61",
                              cause_code_category %in% "Pedestrian" & cause_code_value %in% "Heedless of traffic - playing in road" ~ "62",
                              cause_code_category %in% "Pedestrian" & cause_code_value %in% "Heedless of traffic - stepping walking or running off footpath or verge into road" ~ "63",
                              cause_code_category %in% "Pedestrian" & cause_code_value %in% "Slipping or falling" ~ "64",
                              cause_code_category %in% "Pedestrian" & cause_code_value %in% "Physical defects or physical illness" ~ "65",
                              cause_code_category %in% "Pedestrian" & cause_code_value %in% "Under the influence of drink or a drug" ~ "66",
                              cause_code_category %in% "Pedestrian" & cause_code_value %in% "Holding on to vehicle" ~ "67",
                              cause_code_category %in% "Pedestrian" & cause_code_value %in% "Error of judgment or negligence" ~ "68",
                              cause_code_category %in% "Passengers etc" & cause_code_value %in% "Boarding or alighting from vehicle without due care" ~ "69",
                              cause_code_category %in% "Passengers etc" & cause_code_value %in% "Falling when inside or falling when from vehicle" ~ "70",
                              cause_code_category %in% "Passengers etc" & cause_code_value %in% "Other negligence on part of the passenger" ~ "71",
                              cause_code_category %in% "Passengers etc" & cause_code_value %in% "Stealing ride" ~ "72",
                              cause_code_category %in% "Passengers etc" & cause_code_value %in% "Negligence on part of conductor or goods vehicle attendant" ~ "73",
                              cause_code_category %in% "Animals" & cause_code_value %in% "Dog in carriageway" ~ "74",
                              cause_code_category %in% "Animals" & cause_code_value %in% "Other animals in carriageway" ~ "75",
                              cause_code_category %in% "Obstruction" & cause_code_value %in% "Obstruction" ~ "76",
                              cause_code_category %in% "Obstruction" & cause_code_value %in% "Other obstruction" ~ "77",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "Mechanical defect or failure - brakes" ~ "78",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "Mechanical defect or failure - tyres or wheels" ~ "79",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "Mechanical defect or failure - steering" ~ "80",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "Mechanical defect or failure - other cause" ~ "81",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "No front lights" ~ "82",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "Inadequate front light" ~ "83",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "No rear light" ~ "84",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "Inadequate rear light" ~ "85",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "Unattended vehicle running away" ~ "86",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "Drivers view obstructed e.g. by equipment, load or obsucred windscreen" ~ "87",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "Vehicle overloaded, shifted or defective load" ~ "88",
                              cause_code_category %in% "Vehicle Defect" & cause_code_value %in% "Any other feature of vehicle or equipment which contributed to the accident (specify)" ~ "89",
                              cause_code_category %in% "Road Defect" & cause_code_value %in% "Road surface slippery" ~ "90",
                              cause_code_category %in% "Road Defect" & cause_code_value %in% "Excessive dust obscuring drivers view" ~ "91",
                              cause_code_category %in% "Road Defect" & cause_code_value %in% "Road surface in need of repair (state defect)" ~ "92",
                              cause_code_category %in% "Road Defect" & cause_code_value %in% "Other road condition, view obscured, etc. specify" ~ "93",
                              cause_code_category %in% "Weather" & cause_code_value %in% "Fog or mist" ~ "94",
                              cause_code_category %in% "Weather" & cause_code_value %in% "Torrential rain" ~ "95",
                              cause_code_category %in% "Weather" & cause_code_value %in% "Glaring sun" ~ "96",
                              cause_code_category %in% "Other Cause" & cause_code_value %in% "Other cause specify" ~ "97",
                              cause_code_category %in% "Other Cause" & cause_code_value %in% "Cause not traced" ~ "98"))
  
  return(data_table)
}

# ** Summary Table Functions ---------------------------------------------------
# Clean data for summary tables

prep_data_weekly_report_table <- function(data_table,
                                          victim_df){
  
  ## Add UID to victim_df
  uid_station_df <- data_table %>%
    dplyr::filter(police_division != ".") %>%
    dplyr::select(uid, police_division) %>%
    distinct()
  
  victim_df <- victim_df %>%
    left_join(uid_station_df, by = "uid")
  
  ## Add worst injury to data_table
  wi_df <- victim_df
  wi_df$worst_injury <- NA
  wi_df$worst_injury[wi_df$injurytype %in% "Slight"]  <- 1
  wi_df$worst_injury[wi_df$injurytype %in% "Serious"]  <- 2
  wi_df$worst_injury[wi_df$injurytype %in% "Fatal"]  <- 3
  
  wi_df <- wi_df %>%
    group_by(uid) %>%
    dplyr::mutate(worst_injury = max(worst_injury)) %>%
    ungroup() %>%
    distinct(worst_injury, uid) %>% 
    dplyr::mutate(worst_injury =
                    case_when(worst_injury %in% 1 ~ "Slight",
                              worst_injury %in% 2 ~ "Serious",
                              worst_injury %in% 3 ~ "Fatal")) 
  
  data_table$worst_injury <- NULL
  
  data_table <- data_table %>%
    left_join(wi_df, by = "uid")
  
  #### NO. OF ACCIDENTS
  no_accidents <- data_table %>%
    dplyr::filter(worst_injury %in% c("Slight", "Serious", "Fatal"),
                  police_division != ".") %>%
    dplyr::group_by(police_division, worst_injury) %>%
    dplyr::summarise(N = n()) %>%
    dplyr::rename('NO. OF ACCIDENTS' = N) %>%
    dplyr::rename(injurytype = worst_injury)
  
  #### NO. OF VICTIMS
  no_victims <- victim_df %>%
    dplyr::filter(injurytype %in% c("Slight", "Serious", "Fatal"),
                  police_division != ".") %>%
    dplyr::group_by(police_division, injurytype) %>%
    dplyr::summarise(N = n()) %>%
    dplyr::rename('NO. OF VICTIMS' = N)
  
  #### VICTIM CLASS
  ## Prep victim class variable
  # From "victim" tab
  victim_df$victim_class <- victim_df$personclass 
  
  # From "participant" tab
  victim_df$victim_class[is.na(victim_df$victim_class)] <- victim_df$vehpart_type[is.na(victim_df$victim_class)]
  
  # Cleanup
  victim_df$victim_class[victim_df$victim_class %in% c("Saloon Car",
                                                       "Pickup van",
                                                       "Lorry",
                                                       "Lorry + trailer",
                                                       "Bus",
                                                       "Matatu",
                                                       "Motor-cycle", ## TODO: in this category??
                                                       "Other Vehicle")] <- "Driver"
  
  victim_df$victim_class[victim_df$victim_class %in% c("Bicycle", "Cyclist")] <- "Pedal Cyclist"
  
  ## Make Data
  victim_class <- victim_df %>%
    filter(injurytype %in% c("Slight", "Serious", "Fatal"),
           police_division != ".") %>%
    filter(!is.na(victim_class)) %>%
    dplyr::group_by(police_division, injurytype, victim_class) %>%
    dplyr::summarise(N = n()) %>%
    dplyr::mutate(victim_class = 
                    case_when((victim_class == "Pedestrian" & N >= 2) ~ "Pedestrians",
                              (victim_class == "Driver" & N >= 2) ~ "Drivers",
                              (victim_class == "Passenger" & N >= 2) ~ "Passengers",
                              (victim_class == "Pedal Cyclist" & N >= 2) ~ "Pedal Cyclists",
                              TRUE ~ victim_class)) %>%
    dplyr::mutate(victim_class = paste(N, victim_class)) %>%
    dplyr::select(-N) %>%
    dplyr::rename('CLASS OF VICTIM' = victim_class) %>%
    mutate(`CLASS OF VICTIM` = `CLASS OF VICTIM` %>% toupper) %>%
    group_by(police_division, injurytype) %>%
    dplyr::summarise_all( ~ paste(., collapse="<br>"))
  
  #### Crash Details
  # Date, time, code, road
  
  data_table$accident_time_hour[nchar(data_table$accident_time_hour) %in% 1] <-
    paste0("0", data_table$accident_time_hour[nchar(data_table$accident_time_hour) %in% 1])
  
  data_table$accident_time_minute[nchar(data_table$accident_time_minute) %in% 1] <-
    paste0("0", data_table$accident_time_minute[nchar(data_table$accident_time_minute) %in% 1])
  
  data_table$accident_time <- paste0(data_table$accident_time_hour, data_table$accident_time_minute, " HRS")
  data_table$accident_time[is.na(data_table$accident_time_hour)] <- "UNKNOWN"
  
  crash_details <- data_table %>%
    dplyr::filter(worst_injury %in% c("Slight", "Serious", "Fatal"),
                  police_division != ".") %>%
    dplyr::select(police_division, worst_injury, accident_date, accident_time, cause_code_number, road_name) %>%
    dplyr::mutate(accident_date = accident_date %>% as.Date() %>% format("%d/%m/%Y")) %>%
    dplyr::rename(injurytype = worst_injury,
                  DATE = accident_date,
                  TIME = accident_time,
                  CODE = cause_code_number,
                  ROAD = road_name) %>%
    mutate(DATE = DATE %>% toupper,
           TIME = TIME %>% toupper,
           CODE = CODE %>% toupper,
           ROAD = ROAD %>% toupper) %>%
    group_by(police_division, injurytype) %>%
    dplyr::summarise_all( ~ paste(., collapse="<br>"))
  
  #### Merge
  df_table <- victim_class %>%
    merge(no_victims, by = c("police_division", "injurytype"), all = T) %>%
    merge(no_accidents, by = c("police_division", "injurytype"), all = T) %>%
    merge(crash_details, by = c("police_division", "injurytype"), all = T) %>%
    dplyr::rename('STATION' = police_division,
                  'TYPE OF ACCIDENT' = injurytype) %>%
    dplyr::mutate(`TYPE OF ACCIDENT` = `TYPE OF ACCIDENT` %>% factor(levels = c("Fatal", "Serious", "Slight"))) %>%
    complete(STATION, `TYPE OF ACCIDENT`) %>% 
    arrange(STATION, `TYPE OF ACCIDENT`) %>%
    dplyr::mutate(CODE = CODE %>% str_replace_all("NA", ""),
                  STATION = STATION %>% toupper,
                  `TYPE OF ACCIDENT` = `TYPE OF ACCIDENT` %>% toupper)
  
  return(df_table)
}

prep_data_victim_summary_table <- function(data_table,
                                           victim_df){
  
  #### Number of victims per type per crash
  data_victims <- victim_df %>%
    dplyr::select(uid, injurytype) %>%
    group_by(uid, injurytype) %>%
    dplyr::summarise(N = n()) %>%
    pivot_wider(id_cols = uid,
                values_from = N,
                names_from = injurytype)
  
  if(is.null(data_victims$Fatal))   data_victims$Fatal   <- 0
  if(is.null(data_victims$Serious)) data_victims$Serious <- 0
  if(is.null(data_victims$Slight))  data_victims$Slight  <- 0
  
  data_victims <- data_victims %>%
    dplyr::mutate(Victims = Slight + Serious + Fatal)
  
  ## Worst Type
  data_victims$worst_injury <- NA
  data_victims$worst_injury[data_victims$Slight > 0]  <- "Slight"
  data_victims$worst_injury[data_victims$Serious > 0] <- "Serious"
  data_victims$worst_injury[data_victims$Fatal > 0]   <- "Fatal"
  
  #### Merge with Crashes, Prep, Collapse
  data_station_sum <- data_table %>%
    
    ## Merge
    dplyr::select(uid, police_division) %>%
    left_join(data_victims, by = "uid") %>%
    
    ## Prep
    dplyr::mutate(police_division = police_division %>%
                    na_if(".")) %>%
    filter(!is.na(police_division)) %>%
    
    ## Collapse
    group_by(police_division) %>%
    dplyr::summarise(
      #"Total Victims" = sum(Victims, na.rm=T),
      "Slight Victims" = sum(Slight, na.rm=T),
      "Serious Victims" = sum(Serious, na.rm=T),
      "Fatal Victims" = sum(Fatal, na.rm=T),
      
      "Total Crashes" = n(),
      "Slight Crashes" = sum(worst_injury %in% "Slight", na.rm=T),
      "Serious Crashes" = sum(worst_injury %in% "Serious", na.rm=T),
      "Fatal Crashes" = sum(worst_injury %in% "Fatal", na.rm=T)
    ) %>%
    
    dplyr::mutate(`Total Victims` = `Slight Victims` + `Serious Victims` + `Fatal Victims`) %>%
    
    ## Final Prep
    dplyr::rename(Station = police_division) 
  
  ## Nrow 0 check and fill variables
  if(nrow(data_station_sum) %in% 0){
    data_station_sum <- data.frame(matrix(nrow = 1, ncol=0)) %>%
      dplyr::mutate(Station = "Buruburu",
                    "Total Victims" = 0,
                    "Slight Victims" = 0,
                    "Serious Victims" = 0,
                    "Fatal Victims" = 0,
                    "Total Crashes" = 0,
                    "Slight Crashes" = 0,
                    "Serious Crashes" = 0,
                    "Fatal Crashes" = 0)
  }
  
  data_station_sum <- data_station_sum %>%
    dplyr::mutate(Station = Station %>%
                    factor(levels = c("Buruburu",
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
                                      "Starehe"))) %>%
    complete(Station, 
             fill = list(`Total Victims` = 0,
                         `Slight Victims` = 0,
                         `Serious Victims` = 0,
                         `Fatal Victims` = 0,
                         `Total Crashes` = 0,
                         `Slight Crashes` = 0,
                         `Serious Crashes` = 0,
                         `Fatal Crashes` = 0)) %>%
    
    # Order
    dplyr::select("Station", 
                  "Total Crashes",
                  "Slight Crashes",
                  "Serious Crashes",
                  "Fatal Crashes",
                  "Total Victims",
                  "Slight Victims",
                  "Serious Victims",
                  "Fatal Victims") %>%
    
    # As Integer
    mutate_if(is.numeric, . %>% as.integer) 
  
  return(data_station_sum)
}



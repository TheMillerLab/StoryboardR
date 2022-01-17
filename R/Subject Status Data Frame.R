#' Create a Data Frame of Subject Status
#' @description
#' `subject_status_df()` creates a table of subject status information
#' @param data  The data frame from the MCC Patient registry. Required.
#'
#' @return a data frame
#' @export
#'
subject_status_df <- function(data){
  ##########################################################################################################################
  # load data and platform
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Replace numbers with Strings
  ##########################################################################################################################
  dt.1 <- dt
  dt.1$ss <- replace(dt.1$ss, dt.1$ss == 0, "Alive")
  dt.1$ss <- replace(dt.1$ss, dt.1$ss == 1, "Dead")

  dt.1$ss_ned_awd <- replace(dt.1$ss_ned_awd, dt.1$ss_ned_awd == 0, "No Evidence of Disease")
  dt.1$ss_ned_awd <- replace(dt.1$ss_ned_awd, dt.1$ss_ned_awd == 1, "Alive with Disease")
  dt.1$ss_ned_awd <- replace(dt.1$ss_ned_awd, dt.1$ss_ned_awd == 2, "Indeterminate")

  ##########################################################################################################################
  # prepare data frame
  ##########################################################################################################################
  # Create a table of subject status, keeping only those observations with a cell with a value for ss
  ss <- dt.1 %>% dplyr::select(record_id, ss_dtc, ss, ss_os, ss_ned_awd) %>% drop_na(ss)

  ## Arrange by Ascending date of SS
  ss <- ss %>% arrange(record_id, ss_dtc)
  ## select the latest SS within each record (i.e. the longest OS)
  ss_last <- ss %>% group_by(record_id) %>% slice(which.max(ss_os)) # slice(which.max(ss_os) will choose the highest value of each of the record_id

  ##########################################################################################################################
  # Replace NA with deceased if alive/dead status is dead
  ##########################################################################################################################
  ss_last$ss_ned_awd[ss_last$ss == "Dead"] <- "Deceased"

  ##########################################################################################################################
  # Rename variables
  ##########################################################################################################################
  ss_last <- ss_last %>% select(record_id, ss,  ss_ned_awd, ss_dtc, ss_os)

  ss_last <- ss_last %>%
    rename(`Subject Status Date` = ss_dtc,
           `Current Subject Status` = ss,
           `Overall Survival (Days)` = ss_os,
           `Disease Status` = ss_ned_awd) %>%
    ungroup()



  return(ss_last)

}

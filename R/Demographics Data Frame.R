#' Create a Data Frame of Patient Demographics
#' @description
#' `demographics_df()` creates a table of patient demographics
#' @param data  The data frame from the MCC Patient registry. Required.
#' @param record_id  The record id of the subject of interest. Required.
#'
#' @return a data frame
#' @export
demographics_df <- function(data, record_id){

  ##########################################################################################################################
  # load data and platform
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Replace numbers with Strings
  ##########################################################################################################################
  dt$sex <- replace(dt$sex, dt$sex==0, "Female")
  dt$sex <- replace(dt$sex, dt$sex==1, "Male")

  dt$pres_ecog <- replace(dt$pres_ecog, dt$pres_ecog==0, "ECOG 0 (Asymptomatic)")
  dt$pres_ecog <- replace(dt$pres_ecog, dt$pres_ecog==1, "ECOG 1 (Symptomatic but completely ambulatory)")
  dt$pres_ecog <- replace(dt$pres_ecog, dt$pres_ecog==2, "ECOG 2 (Symptomatic, < 50% in bed during the day)")
  dt$pres_ecog <- replace(dt$pres_ecog, dt$pres_ecog==3, "ECOG 3 (Symptomatic, >50% in bed, but not bedbound)")
  dt$pres_ecog <- replace(dt$pres_ecog, dt$pres_ecog==4, "ECOG 4 (Bedbound)")
  dt$pres_ecog <- replace(dt$pres_ecog, dt$pres_ecog==98, "Unknown")



  ##########################################################################################################################
  # Select Relevant Patient Demographics
  ##########################################################################################################################
  demo.1 <- dt %>% dplyr::select(record_id, age_at_dx, sex, pres_ecog) %>% tidyr::drop_na(age_at_dx)

  ##########################################################################################################################
  # Rename Variables
  ##########################################################################################################################
  demo.2 <- demo.1 %>%
    rename(`Age at Diagnosis` = age_at_dx,
           `Sex` = sex,
           `Performance Status at Presentation` = pres_ecog)


  return(demo.2)

  }


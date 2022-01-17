#' Create a Data Frame of Initial Staging for MCC patients
#' @description
#' `staging_df()` creates a table of initial staging at presentation
#' @param data  The data frame from the MCC Patient registry. Required.
#'
#' @return a data frame with four columns: clinical and pathological staging at presentation and the dates staged
#' @export
#'
staging_df <- function(data){

  ##########################################################################################################################
  # load data and platform
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # prepare data frame
  ##########################################################################################################################
  data_with_NA_repeat_instance <- dt[is.na(dt$redcap_repeat_instance), ]

  df <- data_with_NA_repeat_instance

  ## Make x an indicator variable indicating which clinical stages that have been automatically generated (since there is a default mechanism in the calculation, and we when to eliminate these records)
  a <- (df$t_stg == 8 &
          df$n_clinstg == 6 &
          df$m_clinstg == 6 &
          df$calc_clinstg == 7 &
          df$calc_clinstg_uk___1 == 0)

  ## Make y an indicator variable indicating records whose  clinical stage have been checked off as confirmed
  b <- (df$calc_clinstg_0___1 == 1 |
          df$calc_clinstg_i___1 == 1 |
          df$calc_clinstg_i___1 == 1 |
          df$calc_clinstg_iia___1 == 1 |
          df$calc_clinstg_iib___1 == 1 |
          df$calc_clinstg_iii___1 == 1 |
          df$calc_clinstg_iv___1 == 1 |
          df$calc_clinstg_uk___1 == 1)
  ##########################################################################################################################
  # Create df of cStage
  ##########################################################################################################################

  cStage <-df %>%
    select(record_id,
           clnstg_date,
           t_stg,
           n_clinstg,
           m_clinstg,
           calc_clinstg,
           starts_with("calc_clinstg_"),
           calc_clinstg_uk___1)  %>%
    filter(!a, b)

  # rename calc_clinstg
  cStage$calc_clinstg <- as.numeric(cStage$calc_clinstg)
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 2, "I")
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 3, "IIA")
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 4, "IIB")
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 5, "III")
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 6, "IV")
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 7, "Not Clinically Staged")


  # clean up pStage
  cStage <-cStage %>%
    select(record_id,
           calc_clinstg,
           clnstg_date) %>%
    rename(`Clinical Stage` = calc_clinstg,
           `Date of Clinical Staging` = clnstg_date)

  ##########################################################################################################################
  # Pathological staging
  ##########################################################################################################################
  ### Make x an indicator variable signifying those path stages that have been automatically generated
  x <- (df$t_stg == 8 &
          df$n_pathstg == 9 &
          df$calc_pathstg == 8 &
          df$calc_pathstg_uk___1 == 0)

  ## Make y an inidcator variable indicating records whose stage have been checked off as confirmed
  y <- (df$calc_pathstg_0___1 == 1 |
          df$calc_pathstg_i___1 == 1 |
          df$calc_pathstg_iia___1 == 1 |
          df$calc_pathstg_iib___1 == 1 |
          df$calc_pathstg_iiia___1 == 1 |
          df$calc_pathstg_iiib___1 == 1 |
          df$calc_pathstg_iv___1 == 1 |
          df$calc_pathstg_uk___1 ==1)

  ## Let's make an indicator variable that identifies those records with an unconfirmed undetermined path stage, as these are not likely to be genuine
  z <- (df$calc_pathstg == 8 &
          df$calc_pathstg_uk___1 == 0)

  ### Create pStage df that contains only records that have been verified
  pStage <-df %>%
    select(record_id,
           t_stg,
           n_pathstg,
           m_pathstg,
           calc_pathstg,
           calc_pathstg_uk___1,
           starts_with("calc_pathstg_"),
           pathstg_date) %>%
    filter(!x,
           y,
           !z)

  # rename calc_clinstg
  pStage$calc_pathstg <- as.numeric(pStage$calc_pathstg)
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 2, "I")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 3, "IIA")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 4, "IIB")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 5, "IIIA")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 6, "IIIB")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 7, "IV")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 8, "Not Pathologically Staged")


  # clean up pStage
  pStage <- pStage %>%
    select(record_id,
           calc_pathstg,
           pathstg_date) %>%
    rename(`Pathological Stage` = calc_pathstg,
           `Date of Pathological Staging` = pathstg_date)

  ##########################################################################################################################
  # Combine Clinical and Pathological Staging
  ##########################################################################################################################

  staging <- dplyr::left_join(cStage,
                              pStage,
                              by = "record_id")


  return(staging)
}

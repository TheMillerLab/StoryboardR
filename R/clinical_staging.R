#' Create a dataframe of clinical staging from a tumor registry that can be combined into a storyboard
#' @description
#' `clinical_staging()`wrangles data from the Presentation and Initial Staging form of tumor registries to produce a dataframe of details about the initial clinical staging, which can then be incorporated into a Patient Storyboard
#' @param data is a data frame which contains the data for which you want to create a storyboard
#' @return A data frame with five variables ("record_id", "description", "value", "date", and "hover" that can be combined with others dfs with the same five variables to form a storyboard
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   clinical_staging()
#'
clinical_staging <- function(data){
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  dt <- data


  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a df that removes those rows with repeating instruments, as the clinical staging info is found in a non-repeating
  ## form
  ##########################################################################################################################
  df <- dt[is.na(dt$redcap_repeat_instance), ]
  ##########################################################################################################################
  # Make an indicator variable indicating which clinical stages that have been automatically generated (since there is a
  ## default mechanism in the calculation, and we when to eliminate these records)
  ##########################################################################################################################
  a <- (df$t_stg == 8 &
        df$n_clinstg == 6 &
        df$m_clinstg == 6 &
        df$calc_clinstg == 7 &
        df$calc_clinstg_uk___1 == 0)
  ##########################################################################################################################
  # Make b an inidicator variable indicating records whose clinical stage have been checked off as confirmed
  ##########################################################################################################################
  b <- (df$calc_clinstg_0___1 == 1 |
        df$calc_clinstg_i___1 == 1 |
        df$calc_clinstg_i___1 == 1 |
        df$calc_clinstg_iia___1 == 1 |
        df$calc_clinstg_iib___1 == 1 |
        df$calc_clinstg_iii___1 == 1 |
        df$calc_clinstg_iv___1 == 1 |
        df$calc_clinstg_uk___1 == 1)

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create df of cStage by selecting the relevant variables and filter with the indicator variables made above
  ## with are filtering for those that are not "a" and are "b"
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
  ##########################################################################################################################
  # rename calc_clinstg numerics with the appropriate clinical staging strings
  ##########################################################################################################################
  cStage$calc_clinstg <- as.numeric(cStage$calc_clinstg)
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 2, "I")
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 3, "IIA")
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 4, "IIB")
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 5, "III")
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 6, "IV")
  cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 7, "Not Clinically Staged")
  ##########################################################################################################################
  # Select the relevant variables for cStage and  rename "calc_clinstg"
  ##########################################################################################################################
  cStage <-cStage %>%
    select(record_id,
           calc_clinstg,
           clnstg_date) %>%
    rename(cStage = calc_clinstg)
  ##########################################################################################################################
  # Create the relevant variables that will allow us to combine with other Storyboard Data Frames
  ## "date", "description", "value" and "hover"
  ##########################################################################################################################
  cStage$date <- as.Date(cStage$clnstg_date)
  cStage$description <- "cStage"
  cStage$value <- "Initial\n Clinical\n Stage"
  cStage$hover.a <- paste("<b>Initial Clinical Stage:</b>", cStage$cStage)
  cStage$hover.b <- paste("<b>Date:</b>", cStage$date)
  cStage$hover <- paste(cStage$hover.a,
                        cStage$hover.b,
                        sep = "<br>")

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a final df that will be used to combine with other Storyboard DFs
  ##########################################################################################################################
  cStage.final <- cStage %>%
    select(record_id,
           description,
           value,
           date,
           hover)
  ##########################################################################################################################
  # Return cStage.final
  ##########################################################################################################################
  return(cStage.final)
  }

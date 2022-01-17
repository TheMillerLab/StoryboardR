#' Create a dataframe of pathological staging from a tumor registry that can be combined into a storyboard
#' @description
#' `pathological_staging()`wrangles data from the Presentation and Initial Staging form of tumor registries to produce a dataframe of details about the initial pathological staging, which can then be incorporated into a Patient Storyboard
#' @param data is a data frame which contains the data for which you want to create a storyboard
#' @return A data frame with five variables ("record_id", "description", "value", "date", and "hover" that can be combined with others dfs with the same five variables to form a storyboard
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   pathological_staging()
#'
pathological_staging <- function(data){
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
  x <- (df$t_stg == 8 &
        df$n_pathstg == 9 &
        df$calc_pathstg == 8 &
        df$calc_pathstg_uk___1 == 0)
  ##########################################################################################################################
  # Make y an inidicator variable indicating records whose clinical stage have been checked off as confirmed
  ##########################################################################################################################
  y <- (df$calc_pathstg_0___1 == 1 |
        df$calc_pathstg_i___1 == 1 |
        df$calc_pathstg_iia___1 == 1 |
        df$calc_pathstg_iib___1 == 1 |
        df$calc_pathstg_iiia___1 == 1 |
        df$calc_pathstg_iiib___1 == 1 |
        df$calc_pathstg_iv___1 == 1 |
        df$calc_pathstg_uk___1 ==1)

  ##########################################################################################################################
  # Let's make an indicator variable that identifies those records with an unconfirmed undetermined path stage, as these are
  ## not likely to be genuine
  ##########################################################################################################################
  z <- (df$calc_pathstg == 8 &
        df$calc_pathstg_uk___1 == 0)

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create df of pStage by selecting the relevant variables and filter with the indicator variables made above
  ## with are filtering for those that are not in "x" or "y" and are "y"
  ##########################################################################################################################
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

  ##########################################################################################################################
  # rename calc_pathstg numerics with the appropriate clinical staging strings
  ##########################################################################################################################
  pStage$calc_pathstg <- as.numeric(pStage$calc_pathstg)
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 2, "I")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 3, "IIA")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 4, "IIB")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 5, "IIIA")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 6, "IIIB")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 7, "IV")
  pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 8, "Not Pathologically Staged")
  ##########################################################################################################################
  # Select the relevant variables for pStage and  rename "calc_pathstg"
  ##########################################################################################################################
  pStage <- pStage %>%
    select(record_id,
           calc_pathstg,
           pathstg_date) %>%
    rename(pStage = calc_pathstg)
  ##########################################################################################################################
  # Create the relevant variables that will allow us to combine with other Storyboard Data Frames
  ## "date", "description", "value" and "hover"
  ##########################################################################################################################
  pStage$date <- as.Date(pStage$pathstg_date)
  pStage$description <- "pStage"
  pStage$value <- "Initial\n Pathological\n Stage"
  pStage$hover.a <- paste("<b>Initial Pathological Stage:</b>", pStage$pStage)
  pStage$hover.b <- paste("<b>Date:</b>", pStage$date)
  pStage$hover <- paste(pStage$hover.a,
                        pStage$hover.b,
                        sep = "<br>")
  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a final df that will be used to combine with other Storyboard DFs
  ##########################################################################################################################
  pStage.final <- pStage %>%
    select(record_id,
           description,
           value,
           date,
           hover)
  ##########################################################################################################################
  # Return pStage.final
  ##########################################################################################################################
  return(pStage.final)
  }


#' Create a dataframe of Systemic Therapy from a tumor registry that can be combined into a storyboard
#' @description
#' `systemic_therapy()`wrangles data from the Systemic Antineoplastic Therapy form of tumor registries to produce a dataframe of details about systemic therapy, which can then be incorporated into a Patient Storyboard
#' @param data is a data frame which contains the data for which you want to create a storyboard
#' @return A data frame with five variables ("record_id", "description", "value", "date", and "hover" that can be combined with others dfs with the same five variables to form a storyboard
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   systemic_therapy()
#'
systemic_therapy <- function(data){
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  dt <- data
  dt <- dt %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  ##########################################################################################################################
  # Replace values with strings
  ##########################################################################################################################
  dt$sat_setting[dt$sat_setting == 1] <- "Primary Therapy"
  dt$sat_setting[dt$sat_setting == 2] <- "NeoAdjuvant Therapy"
  dt$sat_setting[dt$sat_setting == 3] <- "Adjuvant Therapy"
  dt$sat_setting[dt$sat_setting == 4] <- "Concurrent, non-MCC Neoplasm"
  dt$sat_setting[dt$sat_setting == 99] <- "Other"

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a new dataframe of relevant variables
  ##########################################################################################################################
  systemic_tx <- dt %>%
    select(record_id,
           rx_name,
           rx_name_other,
           sat_setting,
           redcap_repeat_instance,
           contains("sat_date_dose_")) %>%
    drop_na(sat_date_dose_1)
  ##########################################################################################################################
  # If the rx_name is "Other" replace with the the entered name "rx_name_other", if there isn't one labeled, just use "other"
  ##########################################################################################################################
  systemic_tx$rx_name <- ifelse(test = systemic_tx$rx_name == "other",
                                yes = systemic_tx$rx_name_other,
                                no = systemic_tx$rx_name)
  ##########################################################################################################################
  # Change drugs to title case
  ##########################################################################################################################
  systemic_tx$rx_name <- stringr::str_to_title(systemic_tx$rx_name)


  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a new df in long format to select the first and last dose
  ##########################################################################################################################
  systemic_tx_longer <- tidyr::pivot_longer(
    data = systemic_tx,
    cols = sat_date_dose_1:length(systemic_tx),
    names_to = "cycles",
    values_to = "date") %>%
    drop_na(date)
  ##########################################################################################################################
  # Make "date" as.Date
  ##########################################################################################################################
  systemic_tx_longer$date <- as.Date(systemic_tx_longer$date)
  ##########################################################################################################################
  # create a new variable "cycle" that numbers each dose
  ##########################################################################################################################
  systemic_tx_longer <- systemic_tx_longer %>%
    group_by(record_id,
             rx_name,
             redcap_repeat_instance) %>%
    mutate(cycle = seq_along(rx_name)) %>%
    ungroup()
  ##########################################################################################################################
  # Create the relevant variables that will allow us to combine with other Storyboard Data Frames
  ## "date", "description", "value" and "hover"
  ##########################################################################################################################
  systemic_tx_longer$value <- systemic_tx_longer$rx_name
  systemic_tx_longer$description <- "Systemic Therapy"

  systemic_tx_longer$hover.a<- paste("<b>Therapeutic:</b>", systemic_tx_longer$rx_name)
  systemic_tx_longer$hover.b <- paste("<b>Treatment Setting:</b>", systemic_tx_longer$sat_setting)
  systemic_tx_longer$hover.c <- paste("<b>Dose Number:</b>", systemic_tx_longer$cycle)
  systemic_tx_longer$hover.d <- paste("<b>Date Administered:</b>", systemic_tx_longer$date)

  systemic_tx_longer$hover <- paste(systemic_tx_longer$hover.a,
                                    systemic_tx_longer$hover.b,
                                    systemic_tx_longer$hover.c,
                                    systemic_tx_longer$hover.d,
                                    sep = "<br>")

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Select the relevant variables for the final dataframe
  ##########################################################################################################################
  systemic_tx.final <- systemic_tx_longer %>%
    select(record_id,
           description,
           value,
           date,
           hover)
  ##########################################################################################################################
  # Return the final df which is intended to be combined with other Storyboard DFs
  ##########################################################################################################################
  return(systemic_tx.final)
  }

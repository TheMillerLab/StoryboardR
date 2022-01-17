#' Wrangles data from a tumor registry regarding date of initial histological confirmation
#' @description
#' `diagnosis()` wrangles data from a tumor registry regarding date of initial histological confirmation of the diagnosis, which can then be incorporated into a Patient Storyboard
#' @return A data frame with five variables ("record_id", "description", "value", "date", and "hover" that can be combined with others dfs with the same five variables to form a storyboard
#' @param data is a data frame which contains the data for which you want to create a storyboard
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   diagnosis()

diagnosis <-  function(data){
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Create a data frame of record_id and diagnosis date (dx_dtc)
  ##########################################################################################################################
  diagnosis <- dt %>%
    select(record_id,
           dx_dtc) %>%
    drop_na(dx_dtc)
  ##########################################################################################################################
  # Transform relevant variables
  ##########################################################################################################################
  diagnosis$dx_dtc <- as.Date(diagnosis$dx_dtc)
  ##########################################################################################################################
  # Create a variable called "value" that has a string "Intial Histological Diagnosis"
  ##########################################################################################################################
  diagnosis$value <- "Initial\nHistological\nDiagnosis"
  ##########################################################################################################################
  # Create a variable called "value.hover" that has a string "Initial Histological Diagnosis" & will serve as the hover text
  ##########################################################################################################################
  diagnosis$value.hover <- "<b>Initial Histological Diagnosis</b>"
  ##########################################################################################################################
  # Create a variable called "date" that is the date of diagnosis; "date" will be a universal column for all these dfs
  ##########################################################################################################################
  diagnosis$date <- diagnosis$dx_dtc
  ##########################################################################################################################
  # Create a variable called "hover.a" that is a separate string for hover, that we will combine later
  ##########################################################################################################################
  diagnosis$hover.a <- paste("<b>Date:</b>", diagnosis$date)
  ##########################################################################################################################
  # Combine the various hover texts
  ##########################################################################################################################
  diagnosis$hover <- paste(diagnosis$value.hover,
                           diagnosis$hover.a,
                           sep = "<br>")
  ##########################################################################################################################
  # Add a varialbe called "desription" and fill it with "dx" which will serve as unique value for the diagnosis data
  ##########################################################################################################################
  diagnosis$description <- "dx"
  ##########################################################################################################################
  # Create a final diagnosis df, that will return with this function
  ##########################################################################################################################
  diagnosis.final <- diagnosis %>%
    select(record_id,
           description,
           value,
           date,
           hover)

  return(diagnosis.final)
}

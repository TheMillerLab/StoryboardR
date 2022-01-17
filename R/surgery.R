#' Create a data frame of surgical data from a tumor registry that can be combined into a storyboard
#' @description
#' `surgery()`wrangles data from the Surgery form of tumor registries to produce a data frame of details about surgical therapy, which can then be incorporated into a Patient Storyboard
#' @param data is a data frame which contains the data for which you want to create a storyboard
#' @return A data frame with five variables ("record_id", "description", "value", "date", and "hover" that can be combined with others dfs with the same five variables to form a storyboard
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   surgery()
#'
surgery <- function(data) {
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
  # Select relevant variables
  ##########################################################################################################################
   surgery <- dt %>%
    select(record_id,
           surg_dtc_1,
           surg_type_1,
           lesion_tag_surg,
           surg_marg_1,
           surg_marg_1_unit,
           surg_outcome) %>%
    drop_na(surg_dtc_1)
  ##########################################################################################################################
  # Create the relevant variables that will allow us to combine with other Storyboard Data Frames
  ## "date", "description", "value" and "hover"
  ##########################################################################################################################
  surgery$description <- "surgery"
  surgery$value <- "Surgery"
  surgery$date <- as.Date(surgery$surg_dtc_1)
  ##########################################################################################################################
  # Replace numerics of the data with the appropriate strings from the data dictionary
  ##########################################################################################################################
  surgery$surg_marg_1_unit <- replace(surgery$surg_marg_1_unit, surgery$surg_marg_1_unit == 0, "mm")
  surgery$surg_marg_1_unit <- replace(surgery$surg_marg_1_unit, surgery$surg_marg_1_unit == 1, "cm")

  surgery$surg_type_1 <- replace(surgery$surg_type_1, surgery$surg_type_1 == 1, "Mohs Micrographic Surgery")
  surgery$surg_type_1 <- replace(surgery$surg_type_1, surgery$surg_type_1 == 2, "Excision")
  surgery$surg_type_1 <- replace(surgery$surg_type_1, surgery$surg_type_1 == 3, "Nodal Dissection")
  surgery$surg_type_1 <- replace(surgery$surg_type_1, surgery$surg_type_1 == 98, "Not Reported")
  surgery$surg_type_1 <- replace(surgery$surg_type_1, surgery$surg_type_1 == 99, "Other")

  surgery$surg_outcome <- replace(surgery$surg_outcome, surgery$surg_outcome == "r0", "RO")
  surgery$surg_outcome <- replace(surgery$surg_outcome, surgery$surg_outcome == "r1", "R1")
  surgery$surg_outcome <- replace(surgery$surg_outcome, surgery$surg_outcome == "r2", "R2")
  surgery$surg_outcome <- replace(surgery$surg_outcome, surgery$surg_outcome == 3, "Tumor not present on resection")
  surgery$surg_outcome <- replace(surgery$surg_outcome, surgery$surg_outcome == 4, "Lesion was histologically confirmed to NOT be MCC")
  ##########################################################################################################################
  # unite the variables relating to surgical margins and create a new variable "margin"
  ##########################################################################################################################
  surgery <- tidyr::unite(data = surgery,
                          col = "margin",
                          surg_marg_1,
                          surg_marg_1_unit,
                          sep = " ",
                          na.rm = TRUE,
                          remove = FALSE)
  ##########################################################################################################################
  # Create hover text variable
  ##########################################################################################################################
  surgery$hover.a <- paste("<b>Lesion Surgerized:</b>", surgery$lesion_tag_surg)
  surgery$hover.b <- paste("<b>Type of Sugery:</b>", surgery$surg_type_1)
  surgery$hover.c <- paste("<b>Surgical Margins:</b>", surgery$margin)
  surgery$hover.d <- paste("<b>Surgical Outcome:</b>", surgery$surg_outcome)
  surgery$hover.e <- paste("<b>Date:</b>", surgery$surg_dtc_1)

  surgery$hover <- paste(surgery$hover.a,
                         surgery$hover.b,
                         surgery$hover.c,
                         surgery$hover.d,
                         surgery$hover.e,
                         sep = "<br>")
  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Select the relevant variables for the final data frame
  ##########################################################################################################################
  surgery.final <- surgery %>%
    select(record_id,
           description,
           value,
           date,
           hover)
  ##########################################################################################################################
  # Return the final df which is intended to be combined with other Storyboard DFs
  ##########################################################################################################################
  return(surgery.final)
}

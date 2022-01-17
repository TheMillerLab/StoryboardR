#' Create a data frame of lesions from a tumor registry that can be combined into a storyboard
#' @description
#' `lesion()`wrangles data from the Lesion form of tumor registries to produce a data frame of details about the individual tumors, which can then be incorporated into a Patient Storyboard
#' @param data is a data frame which contains the data for which you want to create a storyboard
#' @return A data frame with five variables ("record_id", "description", "value", "date", and "hover" that can be combined with others dfs with the same five variables to form a storyboard
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   lesion()
#'
lesion <- function(data){
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Replace values in the registry with the appropriate strings from the data dictionary
  ##########################################################################################################################
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 1, "PCT")
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 2, "Metastasis")
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 3, "MCCUP")
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 4, "Local Recurrence")
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 5, "Histology Performed and Not Consistent with MCC")
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 6, "Unclear Etiology/Not Confirmed as MCC")

  dt$tum_histo_conf_yn <- replace(dt$tum_histo_conf_yn, dt$tum_histo_conf_yn == 0, "No")
  dt$tum_histo_conf_yn <- replace(dt$tum_histo_conf_yn, dt$tum_histo_conf_yn == 1, "Yes")
  dt$tum_histo_conf_yn <- replace(dt$tum_histo_conf_yn, dt$tum_histo_conf_yn == 2, "Histology performed and lesion was not consistent with MCC")


  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a df called "lesion" of the relevant variables
  ##########################################################################################################################
  lesion <- dt %>%
    select(record_id,
           lesion_tag,
           tum_dtctn_dt,
           tum_type,
           tum_histo_conf_yn,
           tum_histo_date) %>%
    drop_na(lesion_tag)
  ##########################################################################################################################
  # Create a variable "value" of the tumor type
  ##########################################################################################################################
  lesion$value <- lesion$tum_type
  ##########################################################################################################################
  # create a variable "date
  ##########################################################################################################################
  lesion$date <- as.Date(lesion$tum_dtctn_dt)
  ##########################################################################################################################
  # arrange by date
  ##########################################################################################################################
  lesion <- lesion %>%
    dplyr::arrange(lesion$date)
  ##########################################################################################################################
  # create a variable for the hover texts
  ##########################################################################################################################
  lesion.hover.a <- paste("<b>Lesion:</b>", lesion$lesion_tag)
  lesion.hover.b <- paste("<b>Date of First Detection:</b>", lesion$date)
  lesion.hover.c <- paste("<b>Histologically Confirmed:</b>", lesion$tum_histo_conf_yn)

  lesion$hover <- paste(lesion.hover.a,
                        lesion.hover.b,
                        lesion.hover.c,
                        sep = "<br>")
  ##########################################################################################################################
  # create a variable "description" and label it "lesion"; this will distinguish these values as "lesions" in the larger
  ## Storyboard
  ##########################################################################################################################
  lesion$description <- "lesion"

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a final df that will be used to combine with other Storyboard DFs
  ##########################################################################################################################
  lesion.final <- lesion %>%
    select(record_id,
           description,
           value,
           date,
           hover)
  ##########################################################################################################################
  # Return lesion.final
  ##########################################################################################################################
  return(lesion.final)
}

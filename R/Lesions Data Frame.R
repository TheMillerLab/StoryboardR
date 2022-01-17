#' Create a Data Frame of MCC Lesions in Patients
#' @description
#' `lesions_df()` creates a table of MCC lesions experienced by the patient
#' @param data  The data frame from the MCC Patient registry. Required.
#'
#' @return a data frame
#' @export
lesions_df <- function(data){

  ##########################################################################################################################
  # load data and platform
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Replace Numbers with Strings
  ##########################################################################################################################
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 1, "Primary Cutaneous Tumor")
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 2, "Metastasis")
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 3, "MCCUP")
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 4, "Local Recurrence")
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 5, "Histology Performed and Not Consistent with MCC")
  dt$tum_type <- replace(dt$tum_type, dt$tum_type == 6, "Unclear Etiology/Not Confirmed as MCC")

  dt$tum_histo_conf_yn <- replace(dt$tum_histo_conf_yn, dt$tum_histo_conf_yn == 0, "No")
  dt$tum_histo_conf_yn <- replace(dt$tum_histo_conf_yn, dt$tum_histo_conf_yn == 1, "Yes")
  dt$tum_histo_conf_yn <- replace(dt$tum_histo_conf_yn, dt$tum_histo_conf_yn == 2, "Histology performed and lesion was not consistent with MCC")

  ##########################################################################################################################
  # Select Relevant Variables
  ##########################################################################################################################
  lesion <- dt %>%
    select(record_id,
           lesion_tag,
           tum_type,
           tum_dtctn_dt,
           tum_histo_date) %>%
    drop_na(lesion_tag)
  ##########################################################################################################################
  # Arrange on Date
  ##########################################################################################################################
  lesion <- lesion %>%
    dplyr::arrange(lesion$tum_dtctn_dt)
  ##########################################################################################################################
  # Rename Variables
  ##########################################################################################################################
  lesion_renamed <- lesion %>%
    rename(`Lesion Tag` = lesion_tag,
           `Date Lesion Detected` = tum_dtctn_dt,
           `Lesion Type` = tum_type,
           `Date of Histological Confirmation` = tum_histo_date)


  return(lesion_renamed)

}

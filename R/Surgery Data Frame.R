#' Create a Data Frame of Surgical Therapy
#' @description
#' `surgery_df()` creates a table of Surgical Therapy information
#' @param data  The data frame from the MCC Patient registry. Required.
#'
#' @return a data frame
#' @export
#'
surgery_df <- function(data){
  ##########################################################################################################################
  # load data and platform
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Prepare the Data
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

  surgery$date <- as.Date(surgery$surg_dtc_1)

  surgery$surg_marg_1_unit <- replace(surgery$surg_marg_1_unit, surgery$surg_marg_1_unit == 0, "mm")
  surgery$surg_marg_1_unit <- replace(surgery$surg_marg_1_unit, surgery$surg_marg_1_unit == 1, "cm")

  surgery$surg_type_1 <- replace(surgery$surg_type_1, surgery$surg_type_1 == 1, "Mohs Micrographic Surgery")
  surgery$surg_type_1 <- replace(surgery$surg_type_1, surgery$surg_type_1 == 2, "Excision")
  surgery$surg_type_1 <- replace(surgery$surg_type_1, surgery$surg_type_1 == 3, "Nodal Dissection")
  surgery$surg_type_1 <- replace(surgery$surg_type_1, surgery$surg_type_1 == 98, "Not Reported")
  surgery$surg_type_1 <- replace(surgery$surg_type_1, surgery$surg_type_1 == 99, "Other")

  surgery$surg_outcome <- replace(surgery$surg_outcome, surgery$surg_outcome == "r0", "RO (Complete Resection)")
  surgery$surg_outcome <- replace(surgery$surg_outcome, surgery$surg_outcome == "r1", "R1 (Histological Margins Positive)")
  surgery$surg_outcome <- replace(surgery$surg_outcome, surgery$surg_outcome == "r2", "R2 (Macroscopic Margins Positive)")
  surgery$surg_outcome <- replace(surgery$surg_outcome, surgery$surg_outcome == 3, "Tumor not present on resection")
  surgery$surg_outcome <- replace(surgery$surg_outcome, surgery$surg_outcome == 4, "Lesion was histologically confirmed to NOT be MCC")

  surgery <- surgery %>% dplyr::arrange(date)

  surgery.1 <- surgery %>% mutate(`Surgical Margin` = paste(surg_marg_1,surg_marg_1_unit, sep = " "))
  surgery.1$`Surgical Margin`[surgery.1$`Surgical Margin` == "NA NA"] <- "Not Reported"

  surgery.2 <- surgery.1 %>% select(record_id, lesion_tag_surg, surg_type_1, surg_dtc_1, `Surgical Margin`, surg_outcome)
  surgery.2 <- surgery.2 %>%
    rename(`Lesion Surgerized` = lesion_tag_surg,
           `Procedure` = surg_type_1,
           `Date of Surgery` = surg_dtc_1,
           `Surgical Outcome` = surg_outcome)

  return(surgery.2)

}

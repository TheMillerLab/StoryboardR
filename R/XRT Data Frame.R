#' Create a Data Frame of Radiation Therapy
#' @description
#' `xrt_df()` creates a table of Radiation Therapy information
#' @param data  The data frame from the MCC Patient registry. Required.
#'
#' @return a data frame
#' @export
#'
xrt_df <- function(data){
  ##########################################################################################################################
  # load data and platform
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Prepare the Data
  ##########################################################################################################################
  dt$xrt_set[dt$xrt_set == 1] <- "Definitive Radiotherapy"
  dt$xrt_set[dt$xrt_set == 2] <- "NeoAdjuvant Radiotherapy"
  dt$xrt_set[dt$xrt_set == 3] <- "Adjuvant Radiotherapy"
  dt$xrt_set[dt$xrt_set == 4] <- "Palliative Radiotherapy"
  dt$xrt_set[dt$xrt_set == 5] <- "Prophylactic Treatment"

  dt$xrt_dose_1_unit[dt$xrt_dose_1_unit == 0] <- "cGy"
  dt$xrt_dose_1_unit[dt$xrt_dose_1_unit == 1] <- "Gy"


  ## XRT
  xrt <- dt %>%
    select(record_id,
           xrt_st_dtc_1,
           xrt_end_dtc_1,
           xrt_set,
           lesion_tag_xrt,
           lesion_tag_xrt_2,
           lesion_tag_xrt_3,
           lesion_tag_xrt_4,
           xrt_dose_1,
           xrt_fraction,
           xrt_dose_1_unit) %>%
    drop_na(xrt_end_dtc_1)


  xrt.1 <- xrt %>%
    group_by(record_id,
             xrt_end_dtc_1) %>%
    mutate(lesion_tag_xrt_grouped = paste0(lesion_tag_xrt,
                                           collapse = ", ")) %>%
    ungroup()

  xrt.2 <- xrt.1 %>%
    group_by(record_id,
             xrt_end_dtc_1) %>%
    slice_head() %>%
    select(record_id, xrt_st_dtc_1, xrt_end_dtc_1, xrt_set, lesion_tag_xrt_grouped, xrt_dose_1, xrt_dose_1_unit, xrt_fraction)

  xrt.2 <- xrt.2 %>% mutate(`XRT Dose` = paste(xrt_dose_1, xrt_dose_1_unit, sep = " "))
  xrt.2$`XRT Dose`[xrt.2$`XRT Dose` == "NA NA"] <- "Not Reported"

  xrt.3 <- xrt.2 %>%
    rename(`Start Date` = xrt_st_dtc_1,
           `Completion Date` = xrt_end_dtc_1,
           `Intent` = xrt_set,
           `Lesion(s) Targeted` = lesion_tag_xrt_grouped,
           `# of Fractions` = xrt_fraction)

  xrt.4 <- xrt.3 %>%
    select(record_id, `Lesion(s) Targeted`, `Intent`, `Start Date`, `Completion Date`, `XRT Dose`, `# of Fractions`) %>%
    ungroup()

  return(xrt.4)
}

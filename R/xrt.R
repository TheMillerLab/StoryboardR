#' Create a dataframe of radiotherapy from a tumor registry that can be combined into a storyboard
#' @description
#' `xrt()`wrangles data from the Radiotherapy form of tumor registries to produce a dataframe of details about radiation therapy, which can then be incorporated into a Patient Storyboard
#' @param data is a data frame which contains the data for which you want to create a storyboard
#' @return A data frame with five variables ("record_id", "description", "value", "date", and "hover" that can be combined with others dfs with the same five variables to form a storyboard
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   xrt()
#'
xrt <- function(data){
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Replace values with strings
  ##########################################################################################################################
  dt$xrt_set[dt$xrt_set == 1] <- "Definitive Radiotherapy"
  dt$xrt_set[dt$xrt_set == 2] <- "NeoAdjuvant Radiotherapy"
  dt$xrt_set[dt$xrt_set == 3] <- "Adjuvant Radiotherapy"
  dt$xrt_set[dt$xrt_set == 4] <- "Palliative Radiotherapy"
  ##########################################################################################################################
  # create a new variable of xrt_set called xrt_set.value
  ##########################################################################################################################
  dt$xrt_set.value <- dt$xrt_set
  dt$xrt_set.value[dt$xrt_set.value == "Definitive Radiotherapy"] <- "Definitive\nRadiotherapy"
  dt$xrt_set.value[dt$xrt_set.value == "NeoAdjuvant Radiotherapy"] <- "NeoAdjuvant\nRadiotherapy"
  dt$xrt_set.value[dt$xrt_set.value == "Adjuvant Radiotherapy"] <- "Adjuvant\nRadiotherapy"
  dt$xrt_set.value[dt$xrt_set.value == "Palliative Radiotherapy"] <- "Palliative\nRadiotherapy"

  dt$xrt_dose_1_unit[dt$xrt_dose_1_unit == 0] <- "cGy"
  dt$xrt_dose_1_unit[dt$xrt_dose_1_unit == 1] <- "Gy"

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a new dataframe of data involving the initiation of XRT
  ## This will later be combined with details about the completion of XRT
  ##########################################################################################################################
  xrt_start <- dt %>%
    select(record_id,
           redcap_repeat_instrument,
           redcap_repeat_instance,
           xrt_st_dtc_1,
           xrt_set,
           xrt_set.value,
           lesion_tag_xrt,
           lesion_tag_xrt_2,
           lesion_tag_xrt_3,
           lesion_tag_xrt_4) %>%
    drop_na(xrt_st_dtc_1)
  ##########################################################################################################################
  # Because the data is collected in a way that multiple lesions/fields may be targeted by a single radiation plan, we will
  ## create a new variable "lesions_targeted" that unites lesions within rows
  ##########################################################################################################################
  xrt_start <- tidyr::unite(
    data = xrt_start,
    col = "lesions_targeted",
    lesion_tag_xrt, lesion_tag_xrt_2, lesion_tag_xrt_3, lesion_tag_xrt_4,
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  )
  ##########################################################################################################################
  # Because the data is collected in a way that multiple lesions/fields may be targeted by a single radiation plan, we will
  ## create a new variable "lesion_tag_xrt_grouped" that group lesions by record_id & xrt_st_dtc_1
  ##########################################################################################################################
  xrt_start <- xrt_start %>%
    group_by(record_id,
             xrt_st_dtc_1) %>%
    mutate(lesion_tag_xrt_grouped = paste0(lesions_targeted,
                                           collapse = ", ")) %>%
    ungroup()
  ##########################################################################################################################
  # Create "value" variables that will be grouped to a single "value"
  ## Here we want one that states that therapy was initiated and the details of that setting (e.g. definitive vs. adjuvant)
  ##########################################################################################################################
  xrt_start$value.a <- "Initiating"
  xrt_start$value.b <- xrt_start$xrt_set.value

  xrt_start$value <- paste(xrt_start$value.a,
                           xrt_start$value.b,
                           sep = "<br>")
  ##########################################################################################################################
  # create a "date" variable
  ##########################################################################################################################
  xrt_start$date <- as.Date(xrt_start$xrt_st_dtc_1)
  ##########################################################################################################################
  # Make a hover text variable
  ##########################################################################################################################
  xrt_start.hover.a <- paste("<b>Setting of Radiotherapy</b>", xrt_start$xrt_set)
  xrt_start.hover.b <- paste("<b>Target of Radiotherapy:</b>", xrt_start$lesion_tag_xrt_grouped)
  xrt_start.hover.c <- paste("<b>Start Date:</b>", xrt_start$xrt_st_dtc_1)
  xrt_start$hover <- paste(xrt_start.hover.a,
                           xrt_start.hover.b,
                           xrt_start.hover.c,
                           sep = "<br>")
  ##########################################################################################################################
  # Create a description variable and load it with the numeric of the radiation therapy instance,
  ##########################################################################################################################
  xrt_start$description <- xrt_start$redcap_repeat_instrument
  ##########################################################################################################################
  # Since there will be duplicate rows grouped by the same record_id & start date, eliminate duplicates
  ##########################################################################################################################
  xrt_start <- xrt_start %>%
    group_by(record_id,
             xrt_st_dtc_1) %>%
    slice_head() %>%
    ungroup()

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a final dataframe of data involving the initiation of XRT
  ##########################################################################################################################
  xrt_start.final <- xrt_start %>%
    select(record_id,
           description,
           value,
           date,
           hover)


  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a new dataframe of data involving the COMPLETION of XRT
  ##########################################################################################################################
  xrt_end <- dt %>%
    select(record_id,
           redcap_repeat_instrument,
           redcap_repeat_instance,
           xrt_end_dtc_1,
           xrt_set,
           xrt_set.value,
           lesion_tag_xrt,
           lesion_tag_xrt_2,
           lesion_tag_xrt_3,
           lesion_tag_xrt_4,
           xrt_dose_1,
           xrt_fraction,
           xrt_dose_1_unit) %>%
    drop_na(xrt_end_dtc_1)
  ##########################################################################################################################
  # Because the data is collected in a way that multiple lesions/fields may be targeted by a single radiation plan, we will
  ## create a new variable "lesions_targeted" that unites lesions within rows
  ##########################################################################################################################
  xrt_end <- tidyr::unite(
    data = xrt_end,
    col = "lesions_targeted",
    lesion_tag_xrt, lesion_tag_xrt_2, lesion_tag_xrt_3, lesion_tag_xrt_4,
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  )
  ##########################################################################################################################
  # Because the data is collected in a way that multiple lesions/fields may be targeted by a single radiation plan, we will
  ## create a new variable "lesion_tag_xrt_grouped" that group lesions by record_id & xrt_end_dtc_1 & xrt_dose
  ##########################################################################################################################
  xrt_end <- xrt_end %>%
    group_by(record_id,
             xrt_end_dtc_1,
             xrt_dose_1) %>%
    mutate(lesion_tag_xrt_grouped = paste0(lesions_targeted,
                                           collapse = ", ")) %>%
    ungroup()
  ##########################################################################################################################
  # Create "value" variables that will be grouped to a single "value"
  ## Here we want one that states that therapy was completed and the details of that setting (e.g. definitive vs. adjuvant)
  ##########################################################################################################################
  xrt_end$value.a <- "Completed"
  xrt_end$value.b <- xrt_end$xrt_set.value

  xrt_end$value <- paste(xrt_end$value.a,
                         xrt_end$value.b,
                         sep = "<br>")
  ##########################################################################################################################
  # create a "date" variable
  ##########################################################################################################################
  xrt_end$date <- as.Date(xrt_end$xrt_end_dtc_1)
  ##########################################################################################################################
  # Make a hover text variable
  ##########################################################################################################################
  xrt_end.hover.a <- paste("<b>Setting of Radiotherapy</b>", xrt_end$xrt_set)
  xrt_end.hover.b <- paste("<b>Target of Radiotherapy:</b>", xrt_end$lesion_tag_xrt_grouped)
  xrt_end.hover.c <- paste("<b>Number of Fractions:</b>", xrt_end$xrt_fraction)
  xrt_end.hover.d <- paste("<b>Total Dose Delivered:</b>", xrt_end$xrt_dose_1, xrt_end$xrt_dose_1_unit)
  xrt_end.hover.e <- paste("<b>End Date:</b>", xrt_end$xrt_end_dtc_1)
  xrt_end$hover <- paste(xrt_end.hover.a,
                         xrt_end.hover.b,
                         xrt_end.hover.c,
                         xrt_end.hover.d,
                         xrt_end.hover.e,
                         sep = "<br>")
  ##########################################################################################################################
  # Create a description variable and load it with the numeric of the radiation therapy instance,
  ##########################################################################################################################
  xrt_end$description <- xrt_end$redcap_repeat_instrument
  ##########################################################################################################################
  # Since there will be duplicate rows grouped by the same record_id & start date, eliminate duplicates
  ##########################################################################################################################
  xrt_end <- xrt_end %>%
    group_by(record_id,
             xrt_end_dtc_1) %>%
    slice_head() %>%
    ungroup()

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a final df for the end of XRT
  ##########################################################################################################################
  xrt_end.final <- xrt_end %>%
    select(record_id,
           description,
           value,
           date,
           hover)
  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Create a final df that will be used to combine with other Storyboard DFs
  ##########################################################################################################################
  xrt.final <- rbind(xrt_start.final,
                     xrt_end.final)

  xrt.final <- xrt.final %>%
    group_by(record_id) %>%
    dplyr::arrange(record_id, xrt.final$date) %>%
    ungroup()
  ##########################################################################################################################
  # Return xrt.final
  ##########################################################################################################################
  return(xrt.final)
}

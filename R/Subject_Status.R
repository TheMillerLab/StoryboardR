#' Create a dataframe of Subject Status from a tumor registry
#' @description
#' `ss()` wrangles data from the Subject Status form of tumor registries to produce a dataframe of details about the Subject Status of subjects, which can then be incorporated into a Patient Storyboard
#' @param data is a data frame which contains the data for which you want to create a storyboard
#' @return A data frame with five variables ("record_id", "description", "value", "date", and "hover" that can be combined with others dfs with the same five variables to form a storyboard
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   ss()
ss <- function(data){
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Replace numeric values from the data set into interpretable strings according to the data dictionary
  ##########################################################################################################################
  dt$ss <- replace(dt$ss, dt$ss == 0, "Alive")
  dt$ss <- replace(dt$ss, dt$ss == 1, "Dead")

  dt$ss_ned_awd <- replace(dt$ss_ned_awd , dt$ss_ned_awd  == 0, "NED")
  dt$ss_ned_awd <- replace(dt$ss_ned_awd , dt$ss_ned_awd  == 1, "AWD")
  dt$ss_ned_awd <- replace(dt$ss_ned_awd , dt$ss_ned_awd  == 2, "IND")

  dt$trend_ned <- replace(dt$trend_ned, dt$trend_ned == "385633008", "Improving")
  dt$trend_ned <- replace(dt$trend_ned, dt$trend_ned == "58158008", "Stable")
  dt$trend_ned <- replace(dt$trend_ned, dt$trend_ned == "230993007", "Worsening")
  dt$trend_ned <- replace(dt$trend_ned, dt$trend_ned == "82334004", "Indeterminate")

  dt$trend_ned_improve <- replace(dt$trend_ned_improve, dt$trend_ned_improve == 1, "Treatment Response")
  dt$trend_ned_improve <- replace(dt$trend_ned_improve, dt$trend_ned_improve == 2, "Recovering for a Treatment-Related Adverse Event")

  dt$trend_awd <- replace(dt$trend_awd, dt$trend_awd == "385633008", "Improving")
  dt$trend_awd <- replace(dt$trend_awd, dt$trend_awd == "58158008", "Stable")
  dt$trend_awd <- replace(dt$trend_awd, dt$trend_awd == "230993007", "Worsening")
  dt$trend_awd <- replace(dt$trend_awd, dt$trend_awd == "82334004", "Indeterminate")
  dt$trend_awd <- replace(dt$trend_awd, dt$trend_awd == "42425007", "Equivocal")

  dt$trend_awd_improve <- replace(dt$trend_awd_improve, dt$trend_awd_improve == 1, "Treatment Response")
  dt$trend_awd_improve <- replace(dt$trend_awd_improve, dt$trend_awd_improve == 2, "Recovering for a Treatment-Related Adverse Event")
  dt$trend_awd_improve <- replace(dt$trend_awd_improve, dt$trend_awd_improve == 3, "Improvement of a Non-MCC related comorbidity")

  dt$trend_indeterminate <- replace(dt$trend_indeterminate, dt$trend_indeterminate == "385633008", "Improving")
  dt$trend_indeterminate <- replace(dt$trend_indeterminate, dt$trend_indeterminate == "58158008", "Stable")
  dt$trend_indeterminate <- replace(dt$trend_indeterminate, dt$trend_indeterminate == "230993007", "Worsening")
  dt$trend_indeterminate <- replace(dt$trend_indeterminate, dt$trend_indeterminate == "82334004", "Indeterminate")

  dt$trend_indeterminate_improve <- replace(dt$trend_indeterminate_improve, dt$trend_indeterminate_improve == 1, "Treatment Response")
  dt$trend_indeterminate_improve <- replace(dt$trend_indeterminate_improve, dt$trend_indeterminate_improve == 2, "Recovering for a Treatment-Related Adverse   Event")


  dt$indeterminate___1 <- replace(dt$indeterminate___1, dt$indeterminate___1 == 1, "No clinical assessment of disease status was performed")
  dt$indeterminate___2 <- replace(dt$indeterminate___2, dt$indeterminate___2 == 1, "Awaiting results of work up")
  dt$indeterminate___3 <- replace(dt$indeterminate___3, dt$indeterminate___3 == 1, "Subject had a positive margin at last treatment")
  dt$indeterminate___4 <- replace(dt$indeterminate___4, dt$indeterminate___4 == 1, "Current work up, including physical exam, are equivocal")
  dt$indeterminate___5 <- replace(dt$indeterminate___5, dt$indeterminate___5 == 1, "Data abstractor cannot elucidate")
  dt$indeterminate___6 <- replace(dt$indeterminate___6, dt$indeterminate___6 == 1, "Telemedicine Visit")

  dt$indeterminate_ned___1 <- replace(dt$indeterminate_ned___1, dt$indeterminate_ned___1 == 1, "First visit with care team")
  dt$indeterminate_ned___2 <- replace(dt$indeterminate_ned___2, dt$indeterminate_ned___2 == 1, "Awaiting results of work up")
  dt$indeterminate_ned___3 <- replace(dt$indeterminate_ned___3, dt$indeterminate_ned___3 == 1, "Subject had a positive margin at last treatment")
  dt$indeterminate_ned___4 <- replace(dt$indeterminate_ned___4, dt$indeterminate_ned___4 == 1, "Current work up, including physical exam, are equivocal")
  dt$indeterminate_ned___5 <- replace(dt$indeterminate_ned___5, dt$indeterminate_ned___5 == 1, "Data abstractor cannot elucidate")
  dt$indeterminate_ned___6 <- replace(dt$indeterminate_ned___6, dt$indeterminate_ned___6 == 1, "Telemedicine Visit")

  dt$indeterminate_awd___0 <- replace(dt$indeterminate_awd___0, dt$indeterminate_awd___0 == 1, "Day of Diagnostic Biopsy/Date of Diagnosis ")
  dt$indeterminate_awd___1 <- replace(dt$indeterminate_awd___1, dt$indeterminate_awd___1 == 1, "First visit with care team")
  dt$indeterminate_awd___2 <- replace(dt$indeterminate_awd___2, dt$indeterminate_awd___2 == 1, "Awaiting results of work up")
  dt$indeterminate_awd___3 <- replace(dt$indeterminate_awd___3, dt$indeterminate_awd___3 == 1, "Subject had a positive margin at last treatment")
  dt$indeterminate_awd___4 <- replace(dt$indeterminate_awd___4, dt$indeterminate_awd___4 == 1, "Current work up, including physical exam, are equivocal")
  dt$indeterminate_awd___5 <- replace(dt$indeterminate_awd___5, dt$indeterminate_awd___5 == 1, "Data abstractor cannot elucidate")
  dt$indeterminate_awd___6 <- replace(dt$indeterminate_awd___6, dt$indeterminate_awd___6 == 1, "Telemedicine Visit")

  dt$indeterminate_indeter___1 <- replace(dt$indeterminate_indeter___1, dt$indeterminate_indeter___1 == 1, "First visit with care team")
  dt$indeterminate_indeter___2 <- replace(dt$indeterminate_indeter___2, dt$indeterminate_indeter___2 == 1, "No clinical assessment was performed")
  dt$indeterminate_indeter___3 <- replace(dt$indeterminate_indeter___3, dt$indeterminate_indeter___3 == 1, "Awaiting results of work up")
  dt$indeterminate_indeter___4 <- replace(dt$indeterminate_indeter___4, dt$indeterminate_indeter___4 == 1, "Current work up, including physical exam, are   equivocal")
  dt$indeterminate_indeter___5 <- replace(dt$indeterminate_indeter___5, dt$indeterminate_indeter___5 == 1, "Data abstractor cannot elucidate")
  dt$indeterminate_indeter___6 <- replace(dt$indeterminate_indeter___6, dt$indeterminate_indeter___6 == 1, "Telemedicine Visit")

  dt$worsening_ned___1 <- replace(dt$worsening_ned___1, dt$worsening_ned___1 == 1, "Clinical deterioration from a MCC-directed therapy/intervention")
  dt$worsening_ned___2 <- replace(dt$worsening_ned___2, dt$worsening_ned___1 == 2, "Clinical deterioration from a non-MCC related event/comorbidity")

  dt$worsening_indeterminate___1 <- replace(dt$worsening_indeterminate___1, dt$worsening_indeterminate___1 == 1, "Clinical deterioration from a MCC-directed   therapy/intervention")
  dt$worsening_indeterminate___2 <- replace(dt$worsening_indeterminate___2, dt$worsening_indeterminate___2 == 1, "Clinical deterioration from a non-MCC related   event/comorbidity")

  dt$worsening_awd___1 <- replace(dt$worsening_awd___1, dt$worsening_awd___1 == 1, "Development of a New Lesion(s)")
  dt$worsening_awd___2 <- replace(dt$worsening_awd___2, dt$worsening_awd___2 == 1, "Growth of Existing Lesion(s)")
  dt$worsening_awd___3 <- replace(dt$worsening_awd___3, dt$worsening_awd___3 == 1, "Clinical deterioration from an MCC- specific Complication")
  dt$worsening_awd___4 <- replace(dt$worsening_awd___4, dt$worsening_awd___4 == 1, "Clinical deterioration from an Adverse Event MCC-directed therapy")
  dt$worsening_awd___5 <- replace(dt$worsening_awd___5, dt$worsening_awd___5 == 1, "Clinical deterioration from a non-MCC related event/comorbidity")
  dt$worsening_awd___6 <- replace(dt$worsening_awd___6, dt$worsening_awd___6 == 1, "Clinical deterioration of unclear etiology (e.g. w/u ongoing)")

  dt$equivocal_awd___1 <- replace(dt$equivocal_awd___1, dt$equivocal_awd___1 == 1, "Mixed response to treatment (e.g. development of new lesions, but shrinkage of   others)")
  dt$equivocal_awd___2 <- replace(dt$equivocal_awd___2, dt$equivocal_awd___2 == 1, "Treatment response but concomitant Treatment-Related Adverse Event (TRAE)")
  dt$equivocal_awd___3 <- replace(dt$equivocal_awd___3, dt$equivocal_awd___3 == 1, "Treatment response but concomitant non-MCC related comorbidity is worsening")


  dt$ecog <- replace(dt$ecog, dt$ecog == 423409002, "ECOG Grade 0: Asymptomatic")
  dt$ecog <- replace(dt$ecog, dt$ecog == 422512005, "ECOG Grade 1: Symptomatic but completely ambulatory")
  dt$ecog <- replace(dt$ecog, dt$ecog == 422894000, "ECOG Grade 2: Symptomatic, < 50% in bed during the day")
  dt$ecog <- replace(dt$ecog, dt$ecog == 423053003, "ECOG Grade 3: Symptomatic, >50% in bed, but not bedbound")
  dt$ecog <- replace(dt$ecog, dt$ecog == 423409001, "ECOG Grade 5: Death")
  dt$ecog <- replace(dt$ecog, dt$ecog ==  423237006, "ECOG Grade 4: Bedbound")

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Filter for only those rows from the "Subject Status" form and select relevant variables
  ##########################################################################################################################
  ss <- dt %>%
    filter(redcap_repeat_instrument == "subject_status") %>%
    select(record_id,
           ss_dtc,
           redcap_repeat_instance,
           ss,
           ss_ned_awd,
           trend_ned,
           trend_awd) %>%
    drop_na(ss)
  ##########################################################################################################################
  # create the "date" variable and enter the subject status date into these cells
  ##########################################################################################################################
  ss$date <- ss$ss_dtc
  ##########################################################################################################################
  # create the "value" variable and replace those with an alive subject status as "Alive" otherwise they are "Deceased
  ##########################################################################################################################
  ss$value <- ifelse(test = ss$ss == "Alive",
                     yes = ss$ss_ned_awd,
                     no = "Deceased")
  ##########################################################################################################################
  # create the "ss_ned_awd.hover" variable and replace those with an alive subject status as "Alive" otherwise they are
  ## "Deceased"
  ##########################################################################################################################
  ss$ss_ned_awd.hover <- ifelse(ss$ss == "Alive",
                                ss$ss_ned_awd,
                                "Deceased")
  ##########################################################################################################################
  # Add more detail to "ss_ned_awd.hover" by replacing with the following details
  ##########################################################################################################################
  ss$ss_ned_awd.hover <- replace(ss$ss_ned_awd.hover, ss$ss_ned_awd.hover == "AWD","Alive With Disease")
  ss$ss_ned_awd.hover <- replace(ss$ss_ned_awd.hover, ss$ss_ned_awd.hover == "NED","No Evidence of Disease")
  ss$ss_ned_awd.hover <- replace(ss$ss_ned_awd.hover, ss$ss_ned_awd.hover == "IND","Indeterminate")
  ##########################################################################################################################
  # Create a "description" variable make the value "ss" to give those rows specific to subject status identifiable when
  ## it is added to the other data frames
  ##########################################################################################################################
  ss$description <- "ss"
  ##########################################################################################################################
  # Create a new column "trend" that unites both "trend_ned" and "trend_awd"
  ## remove those NA values with na.rm = TRUE, but don't remove these columns (thus remove = FALSE)
  ##########################################################################################################################
  ss <- tidyr::unite(
    data = ss,
    col = "trend",trend_ned,trend_awd,
    na.rm = TRUE,
    remove = FALSE)
  ##########################################################################################################################
  # If a patient ss is equal "Dead", then their trend is "Worsening", so let's create an ifelse() statement to reflect this
  ##########################################################################################################################
  ss$trend <- ifelse(test = ss$ss == "Dead",
                     yes = "Worsening",
                     no = ss$trend)
  ##########################################################################################################################
  # If a subject's ss is blank, than let's replace it with "Indeterminate"
  ##########################################################################################################################
  ss$trend[ss$trend ==""] <- "Indeterminate"
  ##########################################################################################################################
  # make ss_dtc a date
  ##########################################################################################################################
  ss$ss_dtc <- as.Date(x = ss$ss_dtc,
                       format = "%Y-%m-%d")


  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Let's make a df of trend details to link with ss
  ##########################################################################################################################


  ss.trend.details <- dt %>%
    select(record_id,
           ss_dtc,
           redcap_repeat_instance,
           contains(c("indeterminate",
                      "worsening",
                      "improve",
                      "equivocal"))) %>%
    select(-trend_indeterminate)
  ##########################################################################################################################
  # we must first convert the doubles into characters so we can pivot
  ##########################################################################################################################
  ss.trend.details.chr <- purrr::map(ss.trend.details, as.character)
  ss.trend.details.chr <- as_data_frame(ss.trend.details.chr)
  ##########################################################################################################################
  # pivot longer to a variable "character" and "details"
  ##########################################################################################################################
  ss.trend.longer <- pivot_longer(data = ss.trend.details.chr,
                                  cols = 4:68,
                                  names_to = "character",
                                  values_to = "details")
  ##########################################################################################################################
  # Drop NAs
  ##########################################################################################################################
  ss.trend.longer.drop_na <- ss.trend.longer %>%
    drop_na(details)
  ##########################################################################################################################
  # Drop those rows that have have a "0" in them, and thus no important data
  ##########################################################################################################################
  ss.trend.longer.drop.zero <- ss.trend.longer.drop_na %>%
    filter(details != "0")
  ##########################################################################################################################
  # Let's find those WITH duplicates of details
  ##########################################################################################################################
  ss.trend.details.dupes <- ss.trend.longer.drop.zero %>%
    janitor::get_dupes(record_id,
                       ss_dtc,
                       details)
  ##########################################################################################################################
  # Let's find those WITHOUT duplicates of details
  ##########################################################################################################################
  ss.trend.no.dupes.details <- anti_join(ss.trend.longer.drop.zero,
                                         ss.trend.details.dupes,
                                         by = c("record_id",
                                                "ss_dtc",
                                                "details"))
  ##########################################################################################################################
  # Now let's slice the duplicated df to isolate the single instance that we want
  ##########################################################################################################################
  ss.trend.details.dupes.slice <- ss.trend.details.dupes %>%
    group_by(record_id,
             ss_dtc,
             redcap_repeat_instance) %>%
    slice_head() %>%
    ungroup()
  ##########################################################################################################################
  # remove "-dupe_count" variable
  ##########################################################################################################################
  ss.trend.details.dupes.slice <- ss.trend.details.dupes.slice %>%
    select(-dupe_count)
  ##########################################################################################################################
  # Now let's combine these df
  ##########################################################################################################################
  ss.trend.single <- rbind(ss.trend.no.dupes.details,
                           ss.trend.details.dupes.slice)
  ##########################################################################################################################
  # b/c we might have multiple reasons to explain a trend, let's paste reasons grouped by record id and ss_dtc
  ##########################################################################################################################
  ss.trend.single <- ss.trend.single %>%
    group_by(record_id,
             ss_dtc,
             redcap_repeat_instance) %>%
    mutate(trend.paste = paste0(details,
                                collapse = ", ")) %>%
    ungroup()
  ##########################################################################################################################
  ## Select only one row per date
  ##########################################################################################################################
  ss.trend.single.slice <- ss.trend.single %>%
    group_by(record_id,
             ss_dtc) %>%
    slice_head()
  # Make Date
  ss.trend.single.slice$ss_dtc <- as.Date(ss.trend.single.slice$ss_dtc)
  ##########################################################################################################################
  # Make "redcap_repeat_instance" a numeric double so we can group it with ss in a later step
  ##########################################################################################################################
  ss.trend.single.slice$redcap_repeat_instance <- as.double(ss.trend.single.slice$redcap_repeat_instance)
  # ungroup this df
  ss.trend.single.slice <- ss.trend.single.slice %>%
    ungroup()

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# Old Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################



  ##########################################################################################################################
  # Left join the dfs: ss & ss.trend.single.slice
  ##########################################################################################################################
  ss <- dplyr::left_join(ss,
                         ss.trend.single.slice,
                         by = c("record_id",
                                "ss_dtc",
                                "redcap_repeat_instance"))
  ##########################################################################################################################
  # Replace those missing cells in "details" with "Not Reported"
  ##########################################################################################################################
  ss$details[is.na(ss$details)] <- "Not Reported"
  ss$trend.paste[is.na(ss$trend.paste)] <- "Not Reported"

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################


  ##########################################################################################################################
  # Create an new data called "ss.ecog" to detail performance status on the storyboard
  ##########################################################################################################################
  ss.ecog <- dt %>%
    filter(redcap_repeat_instrument == "subject_status") %>%
    select(record_id,
           ss_dtc,
           redcap_repeat_instance,
           ecog) %>%
    drop_na(ecog)
  # make ss_dtc a date
  ss.ecog$ss_dtc <- as.Date(ss.ecog$ss_dtc)

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# Old Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Combine the dfs ss and ss.ecog
  ##########################################################################################################################
  ss <- dplyr::left_join(ss,
                         ss.ecog,
                         by = c("record_id",
                                "ss_dtc",
                                "redcap_repeat_instance"))
  ##########################################################################################################################
  # Replace those missing ECOG values with Not Reported
  ##########################################################################################################################
  ss$ecog[is.na(ss$ecog)] <- "Not Reported"
  ##########################################################################################################################
  # Let's create a hover column that will allow us to select a unique SS/Trend status so we can eliminate redundant entries
  ##########################################################################################################################
  ss$hover.a <- paste("<b>Subject Status:</b>",
                      ss$ss_ned_awd.hover)

  ss$hover.b <- paste("<b>Clinical Trend:</b>",
                      ss$trend)

  ss$hover.c <- paste("<b>Trend Details:</b>",
                      ss$trend.paste)

  ss$hover.d <- paste("<b>Performance Status:</b>",
                      ss$ecog)

  ss$hover.e <- paste("<b>Date:</b>",
                      ss$date)

  ss$hover.pre <- paste(ss$hover.a,
                        ss$hover.b,
                        sep = "<br>")

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Select the first unique row of "hover.pre" as there may be redundant ones into a new DF
  ##########################################################################################################################
  ss.pre <- ss %>%
    group_by(record_id,
             hover.pre) %>%
    slice_head() %>%
    ungroup()
  ##########################################################################################################################
  # Make a varialbe "hover" that combines the various hover columns previously created
  ##########################################################################################################################
  ss.pre$hover <- paste(ss.pre$hover.a,
                        ss.pre$hover.b,
                        ss.pre$hover.c,
                        ss.pre$hover.d,
                        ss.pre$hover.e,
                        sep = "<br>")

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # Select the desired variables that will make up the final df
  ##########################################################################################################################
  ss.final <- ss.pre %>%
    dplyr::select(record_id,
                  description,
                  value,
                  date,
                  hover)
  ##########################################################################################################################
  # Replace any missing values with "Not Reported"
  ##########################################################################################################################
  ss.final$value[is.na(ss.final$value)] <-  "Not Reported"
  ##########################################################################################################################
  # Return the final df
  ##########################################################################################################################
  return(ss.final)

  }

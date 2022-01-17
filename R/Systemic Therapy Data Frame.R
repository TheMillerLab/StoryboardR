#' Create a Data Frame of Systemic Therapy
#' @description
#' `systemic_therapy_df()` creates a table of Systemic Therapy information
#' @param data  The data frame from the MCC Patient registry. Required.
#'
#' @return a data frame
#' @export
#'
systemic_therapy_df <- function(data){
  ##########################################################################################################################
  # load data and platform
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Replace numerics with strings
  ##########################################################################################################################
  dt$bor <- replace(dt$bor, dt$bor == "cr", "Complete Response")
  dt$bor <- replace(dt$bor, dt$bor == "pr", "Partial Response")
  dt$bor <- replace(dt$bor, dt$bor == "sd", "Stable Disease")
  dt$bor <- replace(dt$bor, dt$bor == "pd", "Progressive Disease")
  dt$bor <- replace(dt$bor, dt$bor == "ne", "Not Evaluable")
  dt$bor <- replace(dt$bor, dt$bor == 98, "Not Reported")

  ##########################################################################################################################
  # select relevant variables
  ##########################################################################################################################
  syst_tx <- dt %>% select(record_id, rx_name, contains("sat_date_dose")) %>% drop_na(rx_name)

  ##########################################################################################################################
  # Create DF of Duration of Therapy
  ##########################################################################################################################
  date.l.dose.m <- reshape2::melt(syst_tx %>%
                                    select(record_id,
                                           rx_name,
                                           contains("sat_date_dose")),
                                  id.var=c("record_id","rx_name"), na.rm = TRUE)

  ## Arrange by group and order dose
  date.l.dose.m <- date.l.dose.m %>%
    dplyr::arrange(record_id,  value, rx_name)

  date.l.dose.m$value <- as.Date.character(date.l.dose.m$value, format = "%m/%d/%y")

  ## select the latest SS within each record (i.e. the longest OS)
  date.l.dose.m1 <- date.l.dose.m %>%
    group_by(record_id, rx_name) %>%
    slice(which.max(value))

  ### rename "value" with "date_last_dose"
  date.l.dose.m1 <- date.l.dose.m1 %>%
    rename(date_last_dose = value)
  ### drop column 3
  date.l.dose.m1 <- date.l.dose.m1[,-3]
  #### left join syst_xt and date.l.dose.m1 to add the date of last dose to syst_tx
  syst_tx.1 <- left_join(syst_tx,
                       date.l.dose.m1,
                       by = c("record_id" = "record_id", "rx_name" = "rx_name"))

  ### convert date_last_dose and sat_date_dose_1 to dates
  syst_tx.1$date_last_dose <- as.Date(syst_tx.1$date_last_dose, format = "%m/%d/%y")
  syst_tx.1$sat_date_dose_1 <- as.Date(syst_tx.1$sat_date_dose_1, format = "%m/%d/%y")

  ## Create a duration of therapy column
  syst_tx.1$dot <- syst_tx.1$date_last_dose - syst_tx.1$sat_date_dose_1
  ### Add one day to each dot, b/c the last dose is not really accounted for with the current method (e.g. if we don't, then 2 doses of avelumab (q14 days), will get listed as 14 days, which is not exactly accurate)
  syst_tx.1$dot.1 <- syst_tx.1$dot + 1

  # relocate dot.1
  syst_tx.1 <- syst_tx.1 %>% dplyr::relocate(dot.1, .after = rx_name)

  # Select relevant variables
  syst_tx.2 <- syst_tx.1 %>% select(record_id, rx_name, sat_date_dose_1, dot.1)

  ##########################################################################################################################
  # Create DF of Best Overall Response
  ##########################################################################################################################
  bor.1 <- dt %>% select(record_id, rx_name, sat_date_dose_1, bor) %>% drop_na(bor)
  bor.1$sat_date_dose_1 <- as.Date(bor.1$sat_date_dose_1, format = "%m/%d/%y")
  syst_tx.3 <- left_join(syst_tx.2,
                         bor.1,
                         by = c("record_id","rx_name","sat_date_dose_1"))
  syst_tx.3$dot.1 <- stringr::str_replace(string = syst_tx.3$dot.1,
                                          pattern = "days",
                                          replacement = "day(s)")

  syst_tx.3$rx_name <- stringr::str_to_title(string = syst_tx.3$rx_name)

  ##########################################################################################################################
  # Rename Variables
  ##########################################################################################################################
  syst_tx.4 <- syst_tx.3 %>%
    rename(`Therapy` = rx_name,
           `Date of 1st Dose` = sat_date_dose_1,
           `Duration of Therapy (Day(s))` = dot.1,
           `Best Overall Response` = bor)

  return(syst_tx.4)

}

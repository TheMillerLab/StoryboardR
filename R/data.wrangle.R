
#' Wrangle Data to Prepare for Storyboards
#'
#' @return
#' @export
#'
data.wrangle <- function(){
# Make Dates, as.Date

dt$dx_dtc <- as.Date(dt$dx_dtc)



# Date of Diagnosis

diagnosis <- dt %>%
  select(record_id,
         dx_dtc) %>%
  drop_na(dx_dtc)

diagnosis$value <- "Initial\nHistological\nDiagnosis"

diagnosis$value.hover <- "<b>Initial Histological Diagnosis</b>"

diagnosis$date <- diagnosis$dx_dtc

diagnosis$hover.a <- paste("<b>Date:</b>", diagnosis$date)

diagnosis$hover <- paste(diagnosis$value.hover,
                         diagnosis$hover.a,
                         sep = "<br>")

diagnosis$description <- "dx"

diagnosis.final <- diagnosis %>%
  select(record_id,
         description,
         value,
         date,
         hover)



# Clinical Staging

## Replace codes with strings





## Create df of Staging


# Create df of Staging
data_with_NA_repeat_instance <- dt[is.na(dt$redcap_repeat_instance), ]

df <- data_with_NA_repeat_instance

## Make x an indicator variable indicating which clinical stages that have been automatically generated (since there is a default mechanism in the calculation, and we when to eliminate these records)
a <- (df$t_stg == 8 &
        df$n_clinstg == 6 &
        df$m_clinstg == 6 &
        df$calc_clinstg == 7 &
        df$calc_clinstg_uk___1 == 0)

## Make y an inidcator variable indicating records whose  clinical stage have been checked off as confirmed
b <- (df$calc_clinstg_0___1 == 1 |
        df$calc_clinstg_i___1 == 1 |
        df$calc_clinstg_i___1 == 1 |
        df$calc_clinstg_iia___1 == 1 |
        df$calc_clinstg_iib___1 == 1 |
        df$calc_clinstg_iii___1 == 1 |
        df$calc_clinstg_iv___1 == 1 |
        df$calc_clinstg_uk___1 == 1)

# Create df of cStage
cStage <-df %>%
  select(record_id,
         clnstg_date,
         t_stg,
         n_clinstg,
         m_clinstg,
         calc_clinstg,
         starts_with("calc_clinstg_"),
         calc_clinstg_uk___1)  %>%
  filter(!a, b)

# rename calc_clinstg
cStage$calc_clinstg <- as.numeric(cStage$calc_clinstg)
cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 2, "I")
cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 3, "IIA")
cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 4, "IIB")
cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 5, "III")
cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 6, "IV")
cStage$calc_clinstg <- replace(cStage$calc_clinstg, cStage$calc_clinstg == 7, "Not Clinically Staged")


# clean up pStage
cStage <-cStage %>%
  select(record_id,
         calc_clinstg,
         clnstg_date) %>%
  rename(cStage = calc_clinstg)

cStage$date <- as.Date(cStage$clnstg_date)
cStage$description <- "cStage"
cStage$value <- "Initial\n Clinical\n Stage"
cStage$hover.a <- paste("<b>Initial Clinical Stage:</b>", cStage$cStage)
cStage$hover.b <- paste("<b>Date:</b>", cStage$date)
cStage$hover <- paste(cStage$hover.a,
                      cStage$hover.b,
                      sep = "<br>")

cStage.final <- cStage %>%
  select(record_id,
         description,
         value,
         date,
         hover)


## pStage Df
### Make x an indicator variable signifying those path stages that have been automatically generated


x <- (df$t_stg == 8 &
        df$n_pathstg == 9 &
        df$calc_pathstg == 8 &
        df$calc_pathstg_uk___1 == 0)

## Make y an inidcator variable indicating records whose stage have been checked off as confirmed
y <- (df$calc_pathstg_0___1 == 1 |
        df$calc_pathstg_i___1 == 1 |
        df$calc_pathstg_iia___1 == 1 |
        df$calc_pathstg_iib___1 == 1 |
        df$calc_pathstg_iiia___1 == 1 |
        df$calc_pathstg_iiib___1 == 1 |
        df$calc_pathstg_iv___1 == 1 |
        df$calc_pathstg_uk___1 ==1)

## Let's make an indicator variable that identifies those records with an unconfirmed undetermined path stage, as these are not likely to be genuine
z <- (df$calc_pathstg == 8 &
        df$calc_pathstg_uk___1 == 0)

### Create pStage df that contains only records that have been verified
pStage <-df %>%
  select(record_id,
         t_stg,
         n_pathstg,
         m_pathstg,
         calc_pathstg,
         calc_pathstg_uk___1,
         starts_with("calc_pathstg_"),
         pathstg_date) %>%
  filter(!x,
         y,
         !z)

# rename calc_clinstg
pStage$calc_pathstg <- as.numeric(pStage$calc_pathstg)
pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 2, "I")
pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 3, "IIA")
pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 4, "IIB")
pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 5, "IIIA")
pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 6, "IIIB")
pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 7, "IV")
pStage$calc_pathstg <- replace(pStage$calc_pathstg, pStage$calc_pathstg == 8, "Not Pathologically Staged")


# clean up pStage
pStage <- pStage %>%
  select(record_id,
         calc_pathstg,
         pathstg_date) %>%
  rename(pStage = calc_pathstg)

pStage$date <- as.Date(pStage$pathstg_date)
pStage$description <- "pStage"
pStage$value <- "Initial\n Pathological\n Stage"
pStage$hover.a <- paste("<b>Initial Pathological Stage:</b>", pStage$pStage)
pStage$hover.b <- paste("<b>Date:</b>", pStage$date)
pStage$hover <- paste(pStage$hover.a,
                      pStage$hover.b,
                      sep = "<br>")

pStage.final <- pStage %>%
  select(record_id,
         description,
         value,
         date,
         hover)




# Let's make a df for subject status

## Replace values with names

dt$ss <- replace(dt$ss, dt$ss == 0, "Alive")
dt$ss <- replace(dt$ss, dt$ss == 1, "Dead")

dt$ss_ned_awd <- replace(dt$ss_ned_awd , dt$ss_ned_awd  == 0, "NED")
dt$ss_ned_awd <- replace(dt$ss_ned_awd , dt$ss_ned_awd  == 1, "AWD")
dt$ss_ned_awd <- replace(dt$ss_ned_awd , dt$ss_ned_awd  == 2, "IND")

dt$trend_ned <- replace(dt$trend_ned, dt$trend_ned == "385633008", "Improving")
dt$trend_ned <- replace(dt$trend_ned, dt$trend_ned == "58158008", "Stable")
dt$trend_ned <- replace(dt$trend_ned, dt$trend_ned == "230993007", "Worsening")
dt$trend_ned <- replace(dt$trend_ned, dt$trend_ned == "82334004", "Indeterminate")

dt$trend_awd <- replace(dt$trend_awd, dt$trend_awd == "385633008", "Improving")
dt$trend_awd <- replace(dt$trend_awd, dt$trend_awd == "58158008", "Stable")
dt$trend_awd <- replace(dt$trend_awd, dt$trend_awd == "230993007", "Worsening")
dt$trend_awd <- replace(dt$trend_awd, dt$trend_awd == "82334004", "Indeterminate")


ss <- dt %>%
  filter(redcap_repeat_instrument == "subject_status") %>%
  select(record_id,
         ss_dtc,
         ss,
         ss_ned_awd,
         trend_ned,
         trend_awd) %>%
  drop_na(ss)

ss$date <- ss$ss_dtc

ss$value <- ifelse(ss$ss == "Alive",
                   ss$ss_ned_awd,
                   "Deceased")

ss$ss_ned_awd.hover <- ifelse(ss$ss == "Alive",
                              ss$ss_ned_awd,
                              "Deceased")

ss$ss_ned_awd.hover <- replace(ss$ss_ned_awd.hover, ss$ss_ned_awd.hover == "AWD","Alive With Disease")
ss$ss_ned_awd.hover <- replace(ss$ss_ned_awd.hover, ss$ss_ned_awd.hover == "NED","No Evidence of Disease")
ss$ss_ned_awd.hover <- replace(ss$ss_ned_awd.hover, ss$ss_ned_awd.hover == "IND","Indeterminate")

ss$description <- "ss"

ss <- ss %>% unite("trend", trend_ned,
                   trend_awd,
                   na.rm = TRUE,
                   remove = FALSE)

ss$trend <- ifelse(ss$ss == "Dead",
                   "Worsening",
                   ss$trend)

ss$trend[ss$trend ==""] <- "Indeterminate"


# Let's create a hove that will allow us to select a unique SS/Trend status so we can eliminate redundant entries

ss$hover.a <- paste("<b>Subject Status:</b>",
                    ss$ss_ned_awd.hover)

ss$hover.b <- paste("<b>Clinical Trend:</b>",
                    ss$trend)

ss$hover.c <- paste("<b>Date:</b>",
                    ss$date)


ss$hover.pre <- paste(ss$hover.a,
                      ss$hover.b,
                      sep = "<br>")
## this will select the first unique
ss.pre <- ss %>%
  group_by(record_id,
           hover.pre) %>%
  slice_head()

ss.pre <- ss.pre %>%
  ungroup()

ss.pre$hover <- paste(ss.pre$hover.a,
                      ss.pre$hover.b,
                      ss.pre$hover.c,
                      sep = "<br>")


ss.final <- ss.pre %>%
  select(record_id,
         description,
         value,
         date,
         hover)




# Lesion
## Replace numbers with values


dt$tum_type <- replace(dt$tum_type, dt$tum_type == 1, "PCT")
dt$tum_type <- replace(dt$tum_type, dt$tum_type == 2, "Metastasis")
dt$tum_type <- replace(dt$tum_type, dt$tum_type == 3, "MCCUP")
dt$tum_type <- replace(dt$tum_type, dt$tum_type == 4, "Local Recurrence")

dt$tum_histo_conf_yn <- replace(dt$tum_histo_conf_yn, dt$tum_histo_conf_yn == 0, "No")
dt$tum_histo_conf_yn <- replace(dt$tum_histo_conf_yn, dt$tum_histo_conf_yn == 1, "Yes")




lesion <- dt %>%
  select(record_id,
         lesion_tag,
         tum_dtctn_dt,
         tum_type,
         tum_histo_conf_yn,
         tum_histo_date) %>%
  drop_na(lesion_tag)

lesion$value <- lesion$tum_type

lesion$date <- as.Date(lesion$tum_dtctn_dt)

lesion <- lesion %>%
  dplyr::arrange(lesion$date)

lesion.hover.a <- paste("<b>Lesion:</b>", lesion$lesion_tag)
lesion.hover.b <- paste("<b>Date of First Detection:</b>", lesion$date)
lesion.hover.c <- paste("<b>Histologically Confirmed:</b>", lesion$tum_histo_conf_yn)

lesion$hover <- paste(lesion.hover.a,
                      lesion.hover.b,
                      lesion.hover.c,
                      sep = "<br>")

lesion$description <- "lesion"

lesion.final <- lesion %>%
  select(record_id,
         description,
         value,
         date,
         hover)



# Surgery


surgery <- dt %>%
  select(record_id,
         surg_dtc_1,
         surg_type_1,
         lesion_tag_surg,
         surg_marg_1,
         surg_marg_1_unit,
         surg_outcome) %>%
  drop_na(surg_dtc_1)


surgery$description <- "surgery"
surgery$value <- "Surgery"
surgery$date <- as.Date(surgery$surg_dtc_1)

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

surgery <- unite(data = surgery,
                 col = "margin",
                 surg_marg_1,
                 surg_marg_1_unit,
                 sep = " ",
                 na.rm = TRUE,
                 remove = FALSE)

surgery$hover.a <- paste("<b>Lesion Surgerized:</b>", surgery$lesion_tag_surg)
surgery$hover.b <- paste("<b>Type of Sugery:</b>", surgery$surg_type_1)
surgery$hover.c <- paste("<b>Surgical Margins:</b>", surgery$margin)
surgery$hover.d <- paste("<b>Surgical Outcome:</b>", surgery$surg_outcome)
surgery$hover.e <- paste("<b>Date:</b>", surgery$lesion_tag_surg)

surgery$hover <- paste(surgery$hover.a,
                       surgery$hover.b,
                       surgery$hover.c,
                       surgery$hover.d,
                       surgery$hover.e,
                       sep = "<br>")

surgery.final <- surgery %>%
  select(record_id,
         description,
         value,
         date,
         hover)



# Radiotherapy
## Replace values with strings

dt$xrt_set[dt$xrt_set == 1] <- "Definitive Radiotherapy"
dt$xrt_set[dt$xrt_set == 2] <- "NeoAdjuvant Radiotherapy"
dt$xrt_set[dt$xrt_set == 3] <- "Adjuvant Radiotherapy"
dt$xrt_set[dt$xrt_set == 4] <- "Palliative Radiotherapy"

dt$xrt_set.value <- dt$xrt_set

dt$xrt_set.value[dt$xrt_set.value == "Definitive Radiotherapy"] <- "Definitive\nRadiotherapy"
dt$xrt_set.value[dt$xrt_set.value == "NeoAdjuvant Radiotherapy"] <- "NeoAdjuvant\nRadiotherapy"
dt$xrt_set.value[dt$xrt_set.value == "Adjuvant Radiotherapy"] <- "Adjuvant\nRadiotherapy"
dt$xrt_set.value[dt$xrt_set.value == "Palliative Radiotherapy"] <- "Palliative\nRadiotherapy"

dt$xrt_dose_1_unit[dt$xrt_dose_1_unit == 0] <- "cGy"
dt$xrt_dose_1_unit[dt$xrt_dose_1_unit == 1] <- "Gy"



## XRT Start

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



xrt_start <- xrt_start %>%
  group_by(record_id,
           xrt_st_dtc_1) %>%
  mutate(lesion_tag_xrt_grouped = paste0(lesion_tag_xrt,
                                         collapse = ", ")) %>%
  ungroup()

xrt_start$value.a <- "Initiating"
xrt_start$value.b <- xrt_start$xrt_set.value


xrt_start$value <- paste(xrt_start$value.a,
                         xrt_start$value.b,
                         sep = "<br>")

xrt_start$date <- as.Date(xrt_start$xrt_st_dtc_1)





xrt_start.hover.a <- paste("<b>Setting of Radiotherapy</b>", xrt_start$xrt_set)
xrt_start.hover.b <- paste("<b>Target of Radiotherapy:</b>", xrt_start$lesion_tag_xrt_grouped)
xrt_start.hover.c <- paste("<b>Start Date:</b>", xrt_start$xrt_st_dtc_1)
xrt_start$hover <- paste(xrt_start.hover.a,
                         xrt_start.hover.b,
                         xrt_start.hover.c,
                         sep = "<br>")

xrt_start$description <- xrt_start$redcap_repeat_instrument

xrt_start <- xrt_start %>%
  group_by(record_id,
           xrt_st_dtc_1) %>%
  slice_head()

xrt_start <-  xrt_start %>%
  ungroup()

xrt_start.final <- xrt_start %>%
  select(record_id,
         description,
         value,
         date,
         hover)




## XRT Completion

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


xrt_end <- xrt_end %>%
  group_by(record_id,
           xrt_end_dtc_1) %>%
  mutate(lesion_tag_xrt_grouped = paste0(lesion_tag_xrt,
                                         collapse = ", ")) %>%
  ungroup()

xrt_end$value.a <- "Completed"
xrt_end$value.b <- xrt_end$xrt_set.value


xrt_end$value <- paste(xrt_end$value.a,
                       xrt_end$value.b,
                       sep = "<br>")

xrt_end$date <- as.Date(xrt_end$xrt_end_dtc_1)

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

xrt_end$description <- xrt_end$redcap_repeat_instrument

xrt_end <- xrt_end %>%
  group_by(record_id,
           xrt_end_dtc_1) %>%
  slice_head()

xrt_end <-  xrt_end %>%
  ungroup()

xrt_end.final <- xrt_end %>%
  select(record_id,
         description,
         value,
         date,
         hover)



## Joint XRT start with end


xrt.final <- rbind(xrt_start.final,
                   xrt_end.final)

xrt.final <- xrt.final %>%
  group_by(record_id) %>%
  dplyr::arrange(record_id, xrt.final$date) %>%
  ungroup()




# Systemic Therapy

## replace strings with names

dt$sat_setting[dt$sat_setting == 1] <- "Primary Therapy"
dt$sat_setting[dt$sat_setting == 2] <- "NeoAdjuvant Therapy"
dt$sat_setting[dt$sat_setting == 3] <- "Adjuvant Therapy"
dt$sat_setting[dt$sat_setting == 4] <- "Concurrent, non-MCC Neoplasm"
dt$sat_setting[dt$sat_setting == 99] <- "Other"




systemic_tx <- dt %>%
  select(record_id,
         rx_name,
         rx_name_other,
         sat_setting,
         contains("sat_date_dose_")) %>%
  drop_na(sat_date_dose_1)

systemic_tx$rx_name <- ifelse(systemic_tx$rx_name == "other",
                              systemic_tx$rx_name_other,
                              systemic_tx$rx_name)

systemic_tx$rx_name <- str_to_title(systemic_tx$rx_name)

systemic_tx_longer <- systemic_tx %>%
  pivot_longer(cols = 5:84,
               names_to = "cycles",
               values_to = "date") %>%
  drop_na(date)

systemic_tx_longer$date <- as.Date(systemic_tx_longer$date)

systemic_tx_longer <- systemic_tx_longer %>%
  group_by(record_id,
           rx_name) %>%
  mutate(cycle = seq_along(rx_name)) %>%
  ungroup()

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


systemic_tx.final <- systemic_tx_longer %>%
  select(record_id,
         description,
         value,
         date,
         hover)



storyboard <- rbind(diagnosis.final,
                    ss.final,
                    cStage.final,
                    pStage.final,
                    lesion.final,
                    surgery.final,
                    xrt.final,
                    systemic_tx.final)

return(storyboard)
}


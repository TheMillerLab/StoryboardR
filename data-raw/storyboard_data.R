## code to prepare `storyboard` simulated dataset goes here
storyboard_dataset <- readr::read_csv("data-raw/storyboard data set.csv")
storyboard_dataset$tum_dtctn_dt <- as.Date.character(storyboard_dataset$tum_dtctn_dt, format = "%m/%d/%y")
storyboard_dataset$ss_dtc <- as.Date.character(storyboard_dataset$ss_dtc, format = "%m/%d/%y")
storyboard_dataset$clnstg_date <- as.Date.character(storyboard_dataset$clnstg_date, format = "%m/%d/%y")
storyboard_dataset$pathstg_date <- as.Date.character(storyboard_dataset$pathstg_date, format = "%m/%d/%y")
storyboard_dataset$tum_histo_date <- as.Date.character(storyboard_dataset$tum_histo_date, format = "%m/%d/%y")
storyboard_dataset$surg_dtc_1 <- as.Date.character(storyboard_dataset$surg_dtc_1, format = "%m/%d/%y")
storyboard_dataset$xrt_st_dtc_1 <- as.Date.character(storyboard_dataset$xrt_st_dtc_1, format = "%m/%d/%y")
storyboard_dataset$xrt_end_dtc_1 <- as.Date.character(storyboard_dataset$xrt_end_dtc_1, format = "%m/%d/%y")



usethis::use_data(storyboard_dataset, overwrite = TRUE)

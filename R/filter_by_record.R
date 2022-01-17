#' Filter the storyboards dataframe by the subject of interest
#' @description
#' `filter_by_record()` filters the storyboards dataframe by the subject of interest
#' @param data is a data frame of the combined storyboard (e.g. after combine_storyboard_dfs() is called)
#' @param id is a character vector corresponding to a subject id: e.g. "7612-1"
#' @return The storyboard data frame filtered by the record of interest
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   filter.by.record()
#'
filter_by_record <- function(data,
                             id){
  ##########################################################################################################################
  # Run combine_storyboard_dfs
  ##########################################################################################################################
  dt.1 <- data

  ##########################################################################################################################
  # Filter the data set for the subject of interest
  ##########################################################################################################################
  dt.2 <- dt.1 %>% filter(record_id == id)

  ##########################################################################################################################
  # Adjust for lack of pStage Date
  ##########################################################################################################################
  ## B/c not all patients are pathologically staged and a date is needed for the timeline, replace "NA" with the cStage date
  ### This is not misleading b/c the hover text will make it clear that the subject was not pathologically staged
  cstage.date <- dt.2$date[dt.2$description == "cStage"]
  pstage.date <-  dt.2$date[dt.2$description == "pStage"]
  dt.2$date[dt.2$description == "pStage"] <- if(base::is.na(dt.2$date[dt.2$description == "pStage"])) cstage.date else pstage.date


  ##########################################################################################################################
  # Arrange by Date
  ##########################################################################################################################
  timeline <- dt.2 %>% arrange(date)

  return(timeline)
}

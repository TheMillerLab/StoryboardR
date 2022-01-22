#' Combine Storyboard data frames
#' @description
#' `combine_storyboard_dfs()` combine the various Storybards DFs
#' @param data is a data frame which contains the data for which you want to create a storyboard
#' @return A data frame that has combined the various data frames of the storyboard package
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   combine_storyboard_dfs()
combine_storyboard_dfs <- function(data) {
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  dt <- data
  ##########################################################################################################################
  # Run individual functions to generate data frames of desired elements of the storyboard
  ##########################################################################################################################
  diagnosis.final <- StoryboardR::diagnosis(data = dt)
  ss.final <- StoryboardR::ss(data = dt)
  cStage.final <- StoryboardR::clinical_staging(data = dt)
  pStage.final <- StoryboardR::pathological_staging(data = dt)
  lesion.final <- StoryboardR::lesion(data = dt)
  surgery.final <- StoryboardR::surgery(data = dt)
  xrt.final <- StoryboardR::xrt(data = dt)
  systemic_tx.final <- StoryboardR::systemic_therapy(data = dt)
  genomics.final <- StoryboardR::genomics(data = dt)
  ae.final <- StoryboardR::adverse_events(data = dt)

  ##########################################################################################################################
  ##########################################################################################################################
  ################################################# New Data Frame  ########################################################
  ##########################################################################################################################
  ##########################################################################################################################

  ##########################################################################################################################
  # rbind the various data frames to create a final storyboard DF
  ##########################################################################################################################
  storyboard <- rbind(diagnosis.final,
                    ss.final,
                    cStage.final,
                    pStage.final,
                    lesion.final,
                    surgery.final,
                    xrt.final,
                    systemic_tx.final,
                    genomics.final)

  ##########################################################################################################################
  # Convert date to Lubridate and Arrange by date
  ##########################################################################################################################
  storyboard$date <- as.Date(storyboard$date, "%Y-%m-%d")
  storyboard <- storyboard %>% dplyr::arrange(date)

  ##########################################################################################################################
  # return the storyboard DF
  ##########################################################################################################################
  return(storyboard)
}

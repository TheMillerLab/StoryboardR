#' Randomly date shift the combined storyboard data frame
#' @description
#' `date.shift.df()` shifts the dates a unified random number of weeks either forward or back between 1 and 52
#' @param data a data frame that is downstream of combine_storyboard_df()
#' @param date the name of the column with the date intended to be shifted. Defaults to "date".
#' @return A data frame of shifted dates
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#' storyboard_dataset %>%
#'   date.shift.df()
#'
date.shift.df <- function(data){
  ##########################################################################################################################
  # Load Data Frame
  ##########################################################################################################################
  dt <- data

  ##########################################################################################################################
  # Generate a random integer between 1 and 52 that will be the number used to shift the data set
  ##########################################################################################################################
  random.integer.week <- floor(runif(1, min=1, max=52))

  ##########################################################################################################################
  # Generate a random integer between -1 and 1, which will be used to shift the dates eithe back or forward
  ##########################################################################################################################
  random.integer.direction <- sample(c(-1,1),
                                      size = 1)

  ##########################################################################################################################
  # Use dateShift function from TimeWarp app to shift the date by the random.integer
  ##########################################################################################################################
  dt.date.shifted <- dt
  dt.date.shifted$date <- TimeWarp::dateShift(
    x = dt$date,
    by = "weeks",
    k.by = random.integer.week,
    direction = random.integer.direction
  )

  ##########################################################################################################################
  # Replace the date in the hover text with the new date that has been date shifted in the "date" column
  ##########################################################################################################################
  # First convert date to a character since the stringr package requires the replacement value to be a character
  dt.date.shifted$date <- as.character(dt.date.shifted$date)

  dt.date.shifted$hover <- stringr::str_replace(
    string = dt.date.shifted$hover,
    pattern = regex("[0-9]{4}-[0-9]{2}-[0-9]{2}"),
    replacement = dt.date.shifted$date
  )
  # Make the "date" column a date class once again
  dt.date.shifted$date <- as.Date(dt.date.shifted$date, "%Y-%m-%d")

  return(dt.date.shifted)

}

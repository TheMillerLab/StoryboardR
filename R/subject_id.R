#' Title
#'
#' @param x A character string of the subject you want to create the storboard for
#'
#' @return
#' @export
#'
subject_id <- function(x) {
  subject_id <- x

  timeline <- storyboard %>%
    filter(record_id == subject_id)

  return(timeline)
}

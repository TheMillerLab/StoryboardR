#' Lauch StoryboardR App
#' @description
#' launch_StoryboardR()` launches the StoryboardR shiny application.
#' @param Data a data frame contains raw data from an exported REDCap project.
#' @param DateShift If TRUE, dates will be shifted a unified random number of weeks either forward or back between 1 and 52. Defaults to FALSE.
#' @export
launch_StoryboardR <- function(Data, DateShift = FALSE, ...) {
  shiny_env <- new.env()
  if(!missing(Data)) {
    print('Setting parameters')
    assign('Data', Data, shiny_env)
  }
  environment(shiny_ui) <- shiny_env

  environment(shiny_server) <- shiny_env

  app <- shiny::shinyApp(
    ui = shiny_ui,
    server = shiny_server
  )
  runApp(app, ...)
}

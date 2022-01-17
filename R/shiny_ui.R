#' The Shiny App UI.
#' @export
shiny_ui <- function() {


  ##########################################################################################################################
  # Load Packages
  ##########################################################################################################################
  library(shiny)
  library(StoryboardR)
  library(tidyverse)
  library(shinydashboard)
  library(plotly)
  library(TimeWarp)

  dt <- Data

  # Create a character vector of record_id to be used for the selectInput function
  record_id_chr <- dt %>% select(record_id) %>% unique() %>% arrange(record_id)


  ##########################################################################################################################
  # Shiny User Interface
  ##########################################################################################################################
  # Define UI for application that draws a histogram
  ui <- shinydashboard::dashboardPage(
    ################################################ Title ###################################################
    shinydashboard::dashboardHeader(title = "StoryboardR"),
    ########################################### Sidebar Content ##############################################
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        menuItem("Subject Dashboard", tabName = "overview", icon = icon("copy")),
        menuItem("Storyboard", tabName = "graph", icon = icon("scroll"))
      )
    ),
    ########################################### Page(s) Content ##############################################
    shinydashboard::dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "overview",
                fluidRow(
                  h1("Select Subject"),
                  column(width = 4, offset = 0, style='padding-left:30px; padding-right:30px; padding-top:30px; padding-bottom:30px;',
                         selectInput(inputId = "record_id",
                                     label = tagList(icon("id-card"), "Record ID"),
                                     choices = c("Select Subject's Record ID To Filter" = "",
                                                 record_id_chr$record_id))
                  )
                ),
                fluidRow(
                  h1("Patient Characteristics and Initial Staging"),
                  column(width = 6,
                         h3("Patient Demographics"),
                         tableOutput("demo")),
                  column(width = 6,
                         h3("Subject Status"),
                         tableOutput("ss"))
                ),
                fluidRow(
                  column(width = 6,
                         h3("Initial Staging"),
                         tableOutput("staging"))
                ),
                fluidRow(
                  column(width = 12,
                         h1("Burden of Disease"),
                         tableOutput("lesions"))
                ),
                fluidRow(
                  h1("Genomic Analysis"),
                  column(width = 12,
                         tableOutput("genomics"))
                ),
                fluidRow(
                  column(width = 12,
                         h1("Therapeutic Interventions"),
                         h3("Surgery"),
                         tableOutput("surgery"))
                ),
                fluidRow(
                  column(width = 12,
                         h3("Radiotherapy"),
                         tableOutput("xrt"))
                ),
                fluidRow(
                  column(width = 12,
                         h3("Systemic Therapy"),
                         tableOutput("systTx"))
                )
        ),
        # Second tab content
        tabItem(tabName = "graph",
                plotlyOutput("storyboard", height = "1000px")
        )
      )
    )
  )

}

##' The Shiny App Server.
#' @param input input set by Shiny.
#' @param output output set by Shiny.
#' @param session session set by Shiny.
#' @export
shiny_server <- function(input, output, session) {


  ##########################################################################################################################
  # Prepare data frames for Patient Overview Tables
  ##########################################################################################################################
  dt <- Data

  # Create a character vector of record_id to be used for the selectInput function
  record_id_chr <- dt %>% select(record_id) %>% unique() %>% arrange(record_id)

  # Load Demographics Table
  demo_table <- StoryboardR::demographics_df(data = dt)
  demo_table$`Age at Diagnosis` <- as.integer(demo_table$`Age at Diagnosis`)

  # Load Genomics Table
  genomics_table <- StoryboardR::genomics_df(data = dt)
  genomics_table$Date <- format(genomics_table$Date,'%Y-%m-%d')
  genomics_table$TMB <- as.integer(genomics_table$TMB)

  # Load Subject Status Table
  ss_table <- StoryboardR::subject_status_df(data = dt)
  ss_table$`Subject Status Date` <- format(ss_table$`Subject Status Date`,'%Y-%m-%d')
  ss_table$`Overall Survival (Days)` <- as.integer(ss_table$`Overall Survival (Days)`)

  # Load Staging Table
  staging_table <- StoryboardR::staging_df(data = dt)
  staging_table$`Date of Clinical Staging` <- format(staging_table$`Date of Clinical Staging`, '%Y-%m-%d')
  staging_table$`Date of Pathological Staging` <- format(staging_table$`Date of Pathological Staging`, '%Y-%m-%d')

  # Load Lesions Table
  lesions_table <- StoryboardR::lesions_df(data = dt)
  lesions_table$`Date Lesion Detected` <- format(lesions_table$`Date Lesion Detected`, '%Y-%m-%d')
  lesions_table$`Date of Histological Confirmation`<- format(lesions_table$`Date of Histological Confirmation`, '%Y-%m-%d')

  # Load Systemic Therapy Table
  systTx_table <- StoryboardR::systemic_therapy_df(data = dt)
  systTx_table$`Date of 1st Dose` <- format(systTx_table$`Date of 1st Dose`,'%Y-%m-%d')


  # Load Subject Status Table
  surgery_table <- StoryboardR::surgery_df(data = dt)
  surgery_table$`Date of Surgery` <- format(surgery_table$`Date of Surgery`, '%Y-%m-%d')

  # Load Subject Status Table
  xrt_table <- StoryboardR::xrt_df(data = dt)
  xrt_table$`Start Date` <- format(xrt_table$`Start Date`,'%Y-%m-%d')
  xrt_table$`Completion Date` <- format(xrt_table$`Completion Date`,'%Y-%m-%d')
  xrt_table$`# of Fractions` <- as.integer(xrt_table$`# of Fractions`)


  ##########################################################################################################################
  # Create Reactives for Patient Overview Tables
  ##########################################################################################################################

   ############################ demographics ################################
    demo_table.reactive <- reactive({
      demo_table %>% dplyr::filter(record_id == input$record_id) %>% select(-record_id)
    })

    output$demo <- renderTable(demo_table.reactive(),
                               striped = TRUE,
                               bordered = TRUE,
                               hover = TRUE,
                               align = "c")


    ############################ Staging ################################
    staging_table.reactive <- reactive({
      staging_table %>% dplyr::filter(record_id == input$record_id) %>% select(-record_id)
    })

    output$staging <- renderTable(staging_table.reactive(),
                                  striped = TRUE,
                                  bordered = TRUE,
                                  hover = TRUE,
                                  align = "c")

    ############################ Subject Status ################################
    ss_table.reactive <- reactive({
      ss_table %>% dplyr::filter(record_id == input$record_id) %>% select(-record_id)
    })

    output$ss <- renderTable(ss_table.reactive(),
                             striped = TRUE,
                             bordered = TRUE,
                             hover = TRUE,
                             align = "c",
                             na = "Not Performed")

    ############################ Genomics ################################
    genomics_table.reactive <- reactive({
      genomics_table %>% dplyr::filter(record_id == input$record_id) %>% select(-record_id)
    })

    output$genomics <- renderTable(genomics_table.reactive(),
                                   striped = TRUE,
                                   bordered = TRUE,
                                   hover = TRUE,
                                   align = "lccclc",
                                   na = "Not Performed")

    ############################ lesions ################################
    lesions_table.reactive <- reactive({
      lesions_table %>% dplyr::filter(record_id == input$record_id) %>% select(-record_id)
    })

    output$lesions <- renderTable(lesions_table.reactive(),
                                  striped = TRUE,
                                  bordered = TRUE,
                                  hover = TRUE,
                                  align = "cllcc",
                                  rownames = TRUE,
                                  na = "Not Confirmed")

    ############################ Surgery ################################
    surgery_table.reactive <- reactive({
      surgery_table %>% dplyr::filter(record_id == input$record_id) %>% select(-record_id)
    })

    output$surgery <- renderTable(surgery_table.reactive(),
                                  striped = TRUE,
                                  bordered = TRUE,
                                  hover = TRUE,
                                  align = "lcccc",
                                  na = "NA")

    ############################ Radiotherapy ################################
    xrt_table.reactive <- reactive({
      xrt_table %>% dplyr::filter(record_id == input$record_id) %>% select(-record_id)
    })

    output$xrt <- renderTable(xrt_table.reactive(),
                              striped = TRUE,
                              bordered = TRUE,
                              hover = TRUE,
                              align = "lccccc",
                              na = "NA")

    ############################ Systemic Therapy ################################
    systTx_table.reactive <- reactive({
      systTx_table %>% dplyr::filter(record_id == input$record_id) %>% select(-record_id)
    })

    output$systTx <- renderTable(systTx_table.reactive(),
                                 striped = TRUE,
                                 bordered = TRUE,
                                 hover = TRUE,
                                 align = "lccc",
                                 na = "NA")


    ##########################################################################################################################
    # Create Reactive for Storyboard Plot
    ##########################################################################################################################
    ############################ Storyboard Plot ################################
    story <- reactive({

      combined.storyboards <- StoryboardR::combine_storyboard_dfs(data = dt)

      filtered.storyboards <- StoryboardR::filter_by_record(data = combined.storyboards,
                                                            id = input$record_id)

      date.shifted.storyboards <- StoryboardR::date.shift.df(data = filtered.storyboards)

      storyboards.df <- if (DateShift == TRUE) date.shifted.storyboards else filtered.storyboards

      storyboards.plot <- StoryboardR::storyboard_plot(data = storyboards.df)

      return(storyboards.plot)

    }
    )

    output$storyboard <- renderPlotly(story()) %>%
      bindCache(input$record_id)




}

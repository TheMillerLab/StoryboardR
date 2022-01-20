#' Create a plotly interactive timeline of a patient journey from a tumor registry
#' @description
#' `storyboard_plot()` takes the aggregated data frames from combine_storyboard_dfs to produce a plotly data visualization of a patient journey
#' @param data is a data frame down stream of combine_storyboard_df and further filtered for a specific record ID of interest
#' @return A ggplotly Storyboard for a given subject
#' @export
#' @examples
#' # Test with embedded data set "storyboard_dataset"
#'StoryboardR::storyboard_dataset %>%
#`  StoryboardR::combine_storyboard_dfs() %>%
#`  dplyr::filter(record_id == "Simulated Patient 1") %>%
#`  StoryboardR::storyboard_plot()
#'
storyboard_plot <- function(data){
  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  timeline <- data

  ##########################################################################################################################
  # Create the Line Segments that will populate the Storyboard
  ##########################################################################################################################
  # Each row in the data frame created by `combine_storyboard_dfs` is an event that will  populate the Storyboard. Thus, we
  ## need to generate a line segment that will attach that event to the center line of the Storyboard. We need a method that
  ### allows for a different number of line segments that is completely dependent on the number of observations that are found
  #### in each individual patient story

  # Therefore let's create a R object that is the length of the dataframe (since this will vary from patient to patient)
  timeline.x <- 1:nrow(timeline)
  # Create an empty integer object that we will fill with a for loop
  out <- vector(mode = "integer")
  # Use a for loop to populate the above integer object that will serve as the number of repeating line segments
  for(i in 1:4) {  # thus there will be four line segments
    out[i] <- i*11  ## that are spaced out by a factor of 11 (that actual length of the line segments will be determined later)
  }
  # Now that we have a object "out" we want to repeat it twice as this will serve as segments above and below the central line of the storyboard
  # Create a R object that repeats the above df "out" twice out a length of the patient's timeline "timeline.x"
  timeline.rep <- base::rep(x = out,
                            each = 2,
                            len = length(timeline.x))
  # Because we want lines of the same length on either side of the central storyboard line, we create a numeric R object of
  ## -1 and 1 that is the length of the patient's timeline and that we will use to multiply to the length of the segment later
  timeline.neg_rep <- rep_len(c(-1,1),
                              length.out = length(timeline.x))
  # Create a new R object that is a df combining "timeline.x", "timeline.rep", "timeline.neg_rep"
  vec <- data.frame(timeline.c = timeline.x,
                    timeline.d = timeline.rep,
                    timeline.e = timeline.neg_rep)
  # Add a vector to "vec" that is the product of timeline.rep and timeline.neg_rep which be used as a base for the length
  ## of the segments above and below the central storyboard line
  vec$timeline.f <- timeline.rep*timeline.neg_rep
  # Add another vector to "vec" that now takes the vector we just made "timeline.f" and add "50" to it, as 50 is the center
  ## of a 100 pixel plot. This should only have positive numbers at this point in a range of 1-100.
  vec$timeline.line.coord <- vec$timeline.f+50
  # Add that above vector "timeline.text.coord", which has the coordinates of where the lines will go to the "timeline"
  ## dataframe
  timeline$y <- vec$timeline.line.coord

  ##########################################################################################################################
  # Create the Coordinates for the text labels for the Storyboard
  ##########################################################################################################################
  # We want the text of each  storyboard to be offset a bit from the line segment, so let's create a new vector that is a
  ## multiple of "timeline.e" by some amount (here 3.6)
  vec$timeline.text.coord.pre <- 3.6*vec$timeline.e
  # now add vec$timeline.text.coord.pre  with vec$timeline.line.coord to get the position on the storyboard where the text
  ## will go
  vec$timeline.text.coord <- vec$timeline.text.coord.pre + vec$timeline.line.coord
  # Add this vector "timeline.text.coord" to the "timeline" data frame and call it "label"
  timeline$label <- vec$timeline.text.coord

  ##########################################################################################################################
  # Develop the "x-axis' of the time
  ##########################################################################################################################
  # Let's begin by defining the start of the timeline
  start_date.a <- timeline %>% slice_head()
  # Let's build in a 20 day buffer before the start date
  start_date <- start_date.a$date - 20
  # Let's define the end of the timeline
  end_date.a <- timeline %>%
    slice_tail()
  # Let's build in a 20 day buffer after the start date
  end_date <- end_date.a$date + 20
  # Create an R object that is a Date vector that spans the start_date to the end_date by "days"
  dateVec <- seq(from = start_date,
                 to = end_date,
                 by = "days")
  # Create an R object that is a Date vector that spans the start_date to the end_date by "4 months" which will serve as
  ## the labels for the graphy
  x_axis_label <- seq(from = start_date,
                      to = end_date,
                      by = "4 months")


  ##########################################################################################################################
  # Create the Text for the Subtitle of the Graph
  ##########################################################################################################################
  timeline$subject_id.label <- paste("<b>Subject:</b>",
                                     timeline$record_id)

  ##########################################################################################################################
  # Create the Storyboard Plot
  ##########################################################################################################################
  # Create a blank data frame that will serve as the substrate for the graph
  plot.storyboard.blank <- data.frame()
  # Build the Storyboard plot using ggplot2
  plot.storyboard <- ggplot(plot.storyboard.blank) +
    ##### Define the x and y boundaries as 0 - 100
    xlim(0, 100) +
    ylim(0, 100) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank())+
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_blank(),
          axis.ticks.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank()) +
    ##### Set a horizontal line at 50, midway through the plot which serves as the timeline
    geom_hline(yintercept = 50) +
    ##### Graph the vertical segments that span the midline to the text label
    geom_segment(data = timeline,
                 aes(x = date,
                     xend = date,
                     y = y,
                     yend = 50
                 )) +
    ##### Graph the text labels
    geom_text(data = timeline,
              aes(x = date,
                  y = label,
                  label = value,
                  text = hover,
                  color = description),
              size = 4.0,
              position = position_nudge()) +
    ##### Format the X Axis
    scale_x_date(breaks = x_axis_label, # this is the name of a vector i created to set the x-axis, otherwise R was deciding for me
                 labels = scales::date_format("%m/%Y"), # This will control the format the above vector will be displayed "%Y" will be a 4 digit date
                 limits = c(min(dateVec), max=max(dateVec)))    # here I had to create a separate vector called "dateVec" to set the limits on the x

  ##########################################################################################################################
  # Convert the ggplot object to a plotly graph
  ##########################################################################################################################
  plot.storyboard.plotly <- plotly::ggplotly(p = plot.storyboard,
           tooltip = "text") %>%
    layout(title = list(text = paste0("<b>Patient Storyboard<b>",
                                      "<br>",
                                      "<sup>",
                                      timeline$subject_id.label,
                                      "</sup>")),
           titlefont = list(size = 28,
                            color = "black",
                            family = "Arial")) %>%
    layout(subtitle = "testing") %>%
    layout(showlegend = FALSE) %>%
    layout(hoverlabel = list(font=list(size=18))) %>%
    layout(xaxis = list(tickfont = list(size = 16))) %>%
    layout(margin = list(
      l = 10,
      r = 10,
      b = 10,
      t = 50))

  ##########################################################################################################################
  # Return the plotly graph of the Storyboard
  ##########################################################################################################################
  return(plot.storyboard.plotly)
}

<img src='man/figures/StoryboardR_Logo.png' align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

## Overview  
<font color = 'orange' ><b>StoryboardR</b></font> (pronounced "Story Boarder") an R package and Shiny application designed to visualize Real-World Data (RWD) from clinical tumor registries. Cancer registries are a rich source of RWD which can be used to test important hypotheses that inform clinical care. Exploratory data analysis (EDA) at the level of individual subjects, when enhanced by interactive data visualizations, has the potential to provide novel insights and generate new hypothesis. <font color = 'orange' ><b>StoryboardR</b></font> facilitates the data visualization of real-world data from tumor registries captured in REDCap<sup>®</sup>. StoryboardR is freely available under the Massachusetts Institute of Technology license and can be obtained from GitHub. StoryboardR is executed in R and deployed as a Shiny application for non-R users. It produces data visualizations of patient journeys from tumor registries.

A video demonstration of <font color = 'orange' ><b>StoryboardR</b></font> can be seen [here](https://github.com/TheMillerLab/StoryboardR/blob/main/Video_Demo.md).

<font color = 'orange' ><b>StoryboardR</b></font> provides a set of verbs that wrangle, process and graph clinical tumor characteristics from structured data:  

  | Verbs | Function |
  | :---        |    :---   |
  |diagnosis() |wrangles data from a tumor registry regarding date of initial histological confirmation of the diagnosis, which can then be incorporated into a Patient Storyboard|
  |ss() |wrangles data from the Subject Status form of tumor registries to produce a dataframe of details about the Subject Status of subjects|
  |clinical_staging() |wrangles data from the Presentation and Initial Staging form of tumor registries to produce a dataframe of details about the initial clinical staging, which can then be incorporated into a Patient Storyboard|
  |pathological_staging() |wrangles data from the Presentation and Initial Staging form of tumor registries to produce a dataframe of details about the initial pathological staging, which can then be incorporated into a Patient Storyboard|
  |lesion() |wrangles data from the Lesion form of tumor registries to produce a dataframe of details about the individual tumors, which can then be incorporated into a Patient Storyboard|
  |surgery() |wrangles data from the Surgery form of tumor registries to produce a dataframe of details about surgical therapy, which can then be incorporated into a Patient Storyboard|
  |xrt() |wrangles data from the Radiotherapy form of tumor registries to produce a dataframe of details about radiation therapy, which can then be incorporated into a Patient Storyboard|
  |systemic_therapy() |wrangles data from the Systemic Antineoplastic Therapy form of tumor registries to produce a dataframe of details about systemic therapy, which can then be incorporated into a Patient Storyboard|
  |genomics() |wrangles data from the Genomics form of tumor registries to produce a dataframe of details about genomic data from tumors or blood, which can then be incorporated into a Patient Storyboard|
  |adverse_events() |wrangles data from the Adverse Events form of tumor registries to produce a dataframe of details about adverse events of systemic therapy, which can then be incorporated into a Patient Storyboard|
  |combine_storyboard_dfs() |integrates the various storyboards across the patient journey into one final data frame|
  |storyboard_plot() |takes the aggregated data frames from `combine_storyboards_dfs` to produce a plotly data visualization of a patient journey|
  |date.shift.df()	|shifts the dates a unified random number of weeks either forward or back between 1 and 52|
  |launch_StoryboardR()	|launches the StoryboardR shiny application|)

## Dependencies
### Software Dependencies
<font color = 'orange' ><b>StoryboardR</b></font> is written in R (version 4.0.0), organized using roxygen2, and utilizes the following packages dplyr, tidyr, readr, stringr, TimeWarp, magrittr, plotly,  splitstackshape, Shinydashboard, and Shiny. For full details, instructions and examples refer to the [video demonstration](https://github.com/TheMillerLab/StoryboardR/blob/main/Video_Demo.md).

### Clinical Informatics Dependencies  
StoryboardR facilitates data visualizations of patient data from the Merkel Cell Carcinoma Tumor Registry electronic data capture (EDC) system, a REDCap<sup>®</sup>-based EDC. The data dictionary for this platform is available [here](https://github.com/TheMillerLab/StoryboardR/data-raw/registry_data_dictionary.csv). While this platform is currently being used by the Merkel Cell Carcinoma Tumor Registry, the fields are generalizable to most solid tumors. Potential customizations of the platform are described below.

## Installation

### Development version

To get a bug fix or to use a feature from the development version, you can install 
the development version of <font color = 'orange' ><b>StoryboardR</b></font> from GitHub.

`devtools::install_github("TheMillerLab/StoryboardR")`


## Usage
`library(StoryboardR)`

### StoryboardR Input  
<font color = 'orange' ><b>StoryboardR</b></font> takes data from a REDCap<sup>®</sup> project that has incorporated the instruments found in the data dictionary. The StoryboardR Shiny application is launched via the function `launch_StoryboardR()`. This function takes two arguments: "Data" and "DateShift". The "Data" argument is a data frame that contains the raw data from the desired REDCap<sup>®</sup> project. "DateShift", which defaults to `FALSE`, will generate a random and uniform shift of all the dates in the data frame if `TRUE` is used. `launch_StoryboardR()` is the only function required to execute and utilize StorybaordR. Once `launch_StoryboardR()` is called, end users interface with StoryboardR in a web browser.

### StoryboardR Output 
The <font color = 'orange' ><b>StoryboardR</b></font> shiny application returns two outputs: a subject Dashboard and Storyboard. The subject Dashboard centralizes high-yield data from the tumor registry in tabular form. This provides an important overview of patient-level information and is fully customizable by the end user. To visualize the temporal relationship between patient-level data elements, StoryboardR generates an interactive timeline. This creates a method of EDA to allow for a visual interpretation of the relationship between certain potential prognostic and/or predictive biomarkers (e.g., tumor genetics) and outcomes (e.g., overall survival, response to therapy).

## Getting help
If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/TheMillerLab/StoryboardR/issues).

## Disclaimer and Acknowledgements
<font color = 'orange' ><b>StoryboardR</b></font> is for research purposes only. No clinical decisions should be made with the information obtained from its output.

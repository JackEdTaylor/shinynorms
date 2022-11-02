# shinynorms

A Shiny app for norming items and/or participants on Likert ratings using cumulative link mixed effects models (CLMMs).

This app accompanies our paper ([https://doi.org/10.3758/s13428-022-01814-7](https://doi.org/10.3758/s13428-022-01814-7)) on using CLMMs to analyse the results of norming studies. The app is designed for researchers who are unfamiliar with R but wish to compare traditional norming results to those derived from CLMMs.

## How to use

### Running Online

The app is currently hosted online at the following urls:

* [https://rstudio-connect.psy.gla.ac.uk/shinynorms/] (University of Glasgow RStudio Connect server)
* [shiny.psy.gla.ac.uk/jackt/shinynorms](https://shiny.psy.gla.ac.uk/jackt/shinynorms) (University of Glasgow Psychology Shiny server)
* [jackt.shinyapps.io/shinynorms](https://jackt.shinyapps.io/shinynorms/) (total monthly usage is limited; timeout after 30 minutes)

### Running Locally

It is usually more reliable to run the app locally in R with:

```
shiny::runGitHub(repo="shinynorms", username="JackEdTaylor")
```

Alternatively, you can download the repository and run `app.R`.

## Dependencies

The app uses the following R packages:

* shiny
* shinyjs
* shinycssloaders
* ordinal
* readr
* dplyr
* openxlsx
* ggplot2

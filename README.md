# shinynorms

A Shiny app for norming items and/or participants on Likert ratings using cumulative link mixed effects models (CLMMs).

This app accompanies our preprint ([https://psyarxiv.com/3vgwk/](https://psyarxiv.com/3vgwk/)) on using CLMMs to analyse the results of norming studies. The app is designed for researchers who are unfamiliar with R but wish to compare traditional norming results to those derived from CLMMs.

## How to use

The app is currently hosted online at the following urls:

* [shiny.psy.gla.ac.uk/jackt/shinynorms](https://shiny.psy.gla.ac.uk/jackt/shinynorms) (University of Glasgow Psychology Shiny server)
* [jackt.shinyapps.io/shinynorms](https://jackt.shinyapps.io/shinynorms/) (total monthly usage is limited; timeout after 30 minutes)

You can also run the app locally in R with:

```
shiny::runGitHub(repo="shinynorms", username="JackEdTaylor")
```

Finally, you can download the repository and run `app.R`.

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

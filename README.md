
<!-- README.md is generated from README.Rmd. Please edit that file -->

# macrodashboard

<!-- badges: start -->

[![R build
status](https://github.com/grattan/macrodashboard/workflows/R-CMD-check/badge.svg)](https://github.com/grattan/macrodashboard/actions)
<!-- badges: end -->

This repo contains builds a Shiny dashboard of graphs relevant to
Grattan Institute’s work on macroeconomic policy.

This is an *unfinished work in progress*.

The live dashboard can be found at:
<https://grattan.shinyapps.io/macrodashboard/>

# Data

All data for the dashboard comes via the `macro_dashboard_data` repo,
[available here](https://github.com/grattan/macro_dashboard_data/). That
repo contains functions to download the latest version of each dataset,
do some minimal tidying, and then write files to its ‘data’ directory.
The functions in `macro_dashboard_data` execute on a schedule using
GitHub Actions, so that the data stays up-to-date.

# Structure

The ‘R’ folder of this repo contains:

-   files beginning with `viz_`. These take a dataframe as input and
    return a ggplot2 plot. There is one function per file.
-   `dash_app.R`. This contains the server-side code for the Shiny app.
-   `dash_ui.R`. This contains the UI code for the Shiny app.
-   `globals.R`. This file defines global variables, to prevent warnings
    regarding functions that use non-standard evaluation.
-   `load_data.R` This file contains a function (`load_data()`) that
    gets data from the `macro_data_dashboard` repo.

library(tidyverse)
library(conflicted)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(here)
library(bslib)
library(crosstalk)
library(shiny)
library(gt)
library(gtExtras)
library(revealjs)
library(quarto)
library(reactable)
library(tippy)
library(vvcanvas)

conflicts_prefer(dplyr::filter)
conflicts_prefer(DT::dataTableOutput)
conflicts_prefer(DT::renderDataTable)
conflicts_prefer(readr::col_factor)
conflicts_prefer(purrr::discard)
conflicts_prefer(dplyr::lag)
conflicts_prefer(gt::google_font)

source(here("helpers/common_helpers.R"))
source(here("helpers/reactablefmtr_themes.R"))
source(here("helpers/obsidian_schedule.R"))
source(here("helpers/javascript.R"))
source(here("helpers/semesters.R"))
source(here("helpers/course_list.R"))
source(here("helpers/agenda_helpers.R"))
source(here("helpers/slides.R"))
source(here("helpers/card_templates.R"))
source(here("helpers/syllabus_helpers.R"))
source(here("helpers/syllabus_defaults.R"))
source(here("helpers/syllabus_table_functions.R"))
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      include = TRUE,
                      echo    = FALSE,
                      eval    = TRUE,
                      comment = "")

my_canvas <- canvas_authenticate(
  "29076~mKFA7LkeHBTvk7RQGhAEAM8MVtXZRRHaFxx626Kuaf4TzLnLHDZaaJwyF8nWQn4F",
  "https://mynu.instructure.com"
  )

library(tidyverse)
library(conflicted)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(here)
library(fs)
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

my_canvas <- vvcanvas::canvas_authenticate(
  "29076~mKFA7LkeHBTvk7RQGhAEAM8MVtXZRRHaFxx626Kuaf4TzLnLHDZaaJwyF8nWQn4F",
  "https://mynu.instructure.com"
  )

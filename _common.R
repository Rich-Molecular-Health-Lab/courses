library(tidyverse)
library(conflicted)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(here)
library(bslib)
library(crosstalk)
library(shiny)
source(here("helpers/card_templates.R"))
source(here("helpers/course_list.R"))
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      include = FALSE,
                      eval    = TRUE,
                      comment = "")
conflicts_prefer(dplyr::filter)
conflicts_prefer(DT::dataTableOutput)
conflicts_prefer(DT::renderDataTable)
conflicts_prefer(readr::col_factor)
conflicts_prefer(purrr::discard)

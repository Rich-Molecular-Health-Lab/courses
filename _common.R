library(tidyverse)
library(conflicted)
library(countdown)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(here)
library(bslib)
library(shiny)
source(here("helpers.R"))
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      include = TRUE,
                      eval    = TRUE,
                      comment = "")
conflicts_prefer(dplyr::filter)
conflicts_prefer(DT::dataTableOutput)
conflicts_prefer(DT::renderDataTable)
conflicts_prefer(readr::col_factor)
conflicts_prefer(purrr::discard)

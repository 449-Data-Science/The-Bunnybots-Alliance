library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(bslib)

library(tidyverse)
library(DT)
library(ggplot2)

source("R/api-interface.R")
source("R/helper-functions.R")
source("R/table-helpers.R")

POLL_INTERVAL <- 5000

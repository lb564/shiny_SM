library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(stringr)
library(dplyr)
library(DT)
library(ggplot2)
library(ggforce)

setwd("G:/Il mio Drive/CALCIO/calcio_shiny")
source('G:/Il mio Drive/CALCIO/calcio_shiny/funzioni_parte1.R')
source('G:/Il mio Drive/CALCIO/calcio_shiny/funzioni_parte2.R')
source('G:/Il mio Drive/CALCIO/calcio_shiny/funzioni_html.R')

my_calendar <- update_my_calendar()
my_matches <- get_my_matches()


folder_address = 'G://Il mio Drive//CALCIO//calcio_shiny'
runApp(folder_address, launch.browser=TRUE)
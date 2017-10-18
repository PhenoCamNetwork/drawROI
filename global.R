list.of.packages <- c(
  'rgdal',
  'shiny',
  'shinyjs',
  'shinyBS',
  'shinyAce',
  'shinyTime',
  'shinydashboard',
  'colourpicker',
  'rjson',
  'stringr',
  'sendmailR',
  'sp',
  'raster',
  'jpeg',
  'tiff',
  'data.table',
  'lubridate',
  'plotly',
  'RCurl'
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



library(rgdal)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyAce)
library(shinyTime)
library(shinydashboard)
library(colourpicker)
library(rjson)
library(stringr)
library(sendmailR)
library(sp)
library(raster)
library(jpeg)
library(tiff)
library(data.table)
library(lubridate)
library(plotly)
library(RCurl)


# TEST_MODE <- FALSE
# if(system('hostname', intern=T)%in%c('phenocam')) TEST_MODE <- TRUE

HTTP_LOAD <- T

sitesInfoURL <- 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/'

if(HTTP_LOAD){
  midddayListPath <- 'https://canopy.sr.unh.edu/webcam/network/middayimglist/'
  mainDataPath <- 'https://phenocam.sr.unh.edu'
  
}else{
  midddayListPath <- '/mnt/klima/home/shiny/middayList/'
  mainDataPath <- '/mnt/klima'
  
}



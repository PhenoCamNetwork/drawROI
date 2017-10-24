source('funcs.R')

list.of.packages <- c(
  'rgdal',
  'shiny',
  'shinyjs',
  'shinyBS',
  'shinyAce',
  'shinyTime',
  'shinyFiles',
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

for(p in list.of.packages) library(p, character.only = T)


HTTP_LOAD <- T
PRINT_LOGS <- T

if(system('hostname', intern=T)%in%c('phenocam')) PRINT_LOGS <- T
   
# if(system('hostname', intern=T)%in%c('phenocam')&
#    system('whoami', intern=T)%in%c('bijan')) HTTP_LOAD <- F


sitesInfoURL <- 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/'

if(HTTP_LOAD){
  middayListPath <- 'https://phenocam.sr.unh.edu/webcam/network/middayimglist/'
  mainDataPath <- 'https://phenocam.sr.unh.edu'
  
}else{
  middayListPath <- '/mnt/klima/home/shiny/middayList/'
  # middayListPath <- 'https://phenocam.sr.unh.edu/webcam/network/middayimglist/'
  mainDataPath <- '/mnt/klima'
}



#######################################################################
# The global setup for the drawROI shiny app. 
# 
# The drawROI app is developed and maintained by Bijan Seyednasrollah.
# The main initial development was done in May to November, 2017.
#
# Most recent release: https://github.com/bnasr/drawROI
#######################################################################


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
  'shinythemes',
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
if(length(new.packages)) install.packages(new.packages, repos='http://cran.rstudio.com/')

for(p in list.of.packages) library(p, character.only = T)


CACHE_LIMIT <- 500

PRINT_LOGS <- TRUE
if(system('hostname', intern=T)%in%c('phenocam')) PRINT_LOGS <- T
   
HTTP_LOAD <- FALSE  
# if(system('hostname', intern=T)%in%c('phenocam')&
#    system('whoami', intern=T)%in%c('bijan')) HTTP_LOAD <- F

SHINY_SERVER <- FALSE
if(system('hostname', intern=T)%in%c('phenocam')&
   system('whoami', intern=T)%in%c('shiny')) SHINY_SERVER <- T


sitesInfoURL <- 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/'

if(HTTP_LOAD){
  middayListPath <- 'https://phenocam.sr.unh.edu/webcam/network/middayimglist/'
  mainDataPath <- 'https://phenocam.sr.unh.edu'
  
}else{
  middayListPath <- 'https://phenocam.sr.unh.edu/webcam/network/middayimglist/'
  mainDataPath <- ''
}



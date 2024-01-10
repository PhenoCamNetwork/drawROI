#######################################################################
# The global setup for the drawROI shiny app. 
# 
# The drawROI app is developed and maintained by Bijan Seyednasrollah.
# The main initial development was done in May to November, 2017.
#
# Most recent release: https://github.com/bnasr/drawROI
#######################################################################

# library(shiny, lib="/home/mkf58/Documents/Development/Phoenocam/shiny_1_4_0_2")

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


CACHE_LIMIT <- 5000

PRINT_LOGS <- TRUE
if(system('hostname', intern=T)%in%c('phenocam')) PRINT_LOGS <- T
   
HTTP_LOAD <- TRUE
SHINY_SERVER <- FALSE

SHINY_SERVER_FQDN = Sys.getenv("HOSTNAME")
# For Testing on local system
SHINY_SERVER_FQDN = 'phenocam.nau.edu'

maskStartDate = '2001-01-01'
maskEndDate = '9999-12-31'

sitesInfoURL <- paste0('https://', SHINY_SERVER_FQDN, '/webcam/network/siteinfo/')
print(paste0('sitesInfoURL: ', sitesInfoURL))

middayListPath <- 'https://phenocam.nau.edu/api/middayimages/'
if(HTTP_LOAD){
  mainDataPath <- 'https://phenocam.nau.edu'
}else{
  middayListPath <- '/data/archive/'
  mainDataPath <- ''
}

print('Global Page Bottom')

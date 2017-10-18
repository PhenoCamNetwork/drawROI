# TEST_MODE <- FALSE
# if(system('hostname', intern=T)%in%c('phenocam')) TEST_MODE <- TRUE

HTTP_LOAD <- TRUE

sitesInfoURL <- 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/'

if(HTTP_LOAD){
  midddayListPath <- 'https://canopy.sr.unh.edu/webcam/network/middayimglist/'
  mainDataPath <- 'https://phenocam.sr.unh.edu'
  
}else{
  midddayListPath <- '/mnt/klima/home/shiny/middayList/'
  mainDataPath <- '/mnt/klima'
  
}



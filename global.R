TEST_MODE <- TRUE

if(getwd()=='/home/bijan/Projects/drawROI') TEST_MODE <- TRUE
if(getwd()=='/home/shiny/apps/drawROI') TEST_MODE <- TRUE

middayimglistURL <- 'https://canopy.sr.unh.edu/webcam/network/middayimglist/'
sitesInfoURL <- 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/'

mountPath <- ''

if(TEST_MODE) mountPath <- '/mnt/klima'

midddayListPath <- paste0(mountPath, '/home/shiny/middayList/')
# midddayListPath <- NULL
clImagePath <- paste0(mountPath, '/home/shiny/climages/')


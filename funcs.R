

#' Check If String is a URL
is.url <-function(x) {
  grepl("www.|http:|https:", x)
}

# plot jpeg image using as raster given image path.
plotJPEG <- function(path, add=FALSE, xlim = NULL, ylim = NULL, downloadDataTable, downloadDir, showLoad = F)
{
  # jpgNonNative <-  readJPEG(path, native=F) # read the file
  jpgNonNative <- NULL
  
  if(is.url(path)){
    tmppath <- paste0(tempdir(), '/tmp.jpg')
    # showModal(strong(
    #   modalDialog(HTML(paste0('Loading ', basename(path) , '...')),
    #               easyClose = F,
    #               size = 's',
    #               style='background-color:#3b3a35; color:#fce319; ',
    #               footer = NULL
    #   )))
    # download.file(path, destfile = tmppath, method = 'curl', quiet = !PRINT_LOGS)
    tmpdl <- tryDownload(path, downloadDataTable = downloadDataTable, downloadDir = downloadDir, showLoad = showLoad)
    downloadDataTable <- tmpdl$downloadDataTable
    tmppath <- tmpdl$destfile
    # removeModal()
  }else{
    tmppath = path
  }
  jpgNative <-  readJPEG(tmppath, native=T) # read the file
  res <-  dim(jpgNative)[2:1] # get the resolution
  if(is.null(xlim)) xlim <- c(1,res[1])
  if(is.null(ylim)) ylim <- c(1,res[2])
  if (!add) # initialize an empty plot area if add==FALSE
    plot(NA, xlim = xlim, ylim = ylim, type='n',
         xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
  rasterImage(jpgNative,1,1,res[1],res[2])
  invisible(list(res=res, 
                 jpgNonNative=jpgNonNative, 
                 jpgNative=jpgNative, 
                 downloadDataTable=downloadDataTable))
}


# interactive drawing of polygons by user clicks on the plot
draw.polygon <-
  function (col = "#80303080", lty = 1, ...) 
  {
    xy <- locator(2)
    lines(xy$x, xy$y, lty = lty)
    
    while(is.list(c1 <- locator(1))) {
      xy$x <- c(xy$x, c1$x)
      xy$y <- c(xy$y, c1$y)
      lines(xy$x, xy$y, lty = lty)
    }
    xy <- data.frame(xy)
    xy <- rbind(xy, xy[1, ])
    polygon(xy$x, xy$y, lty = lty, col = col, ...)
    
    invisible(xy)
  }


# extract chromatic colors of RGB channels for given jpeg file and mask matrix
extractCCC <- function(path, m, downloadDataTable, downloadDir){
  if(is.url(path)){
    tmpdl <- tryDownload(path, downloadDataTable = downloadDataTable, downloadDir = downloadDir)
    path <- tmpdl$destfile
    downloadDataTable <- tmpdl$downloadDataTable
  }
  
  jp <- readJPEG(path)
  dm <- dim(jp)
  rgb <- jp
  dim(rgb) <- c(dm[1]*dm[2],3)
  
  if(!identical(dim(rgb), dim(m))) return(NULL)
  
  mrgb <- rgb*m
  RGB <- colMeans(mrgb, na.rm = T)
  
  RGBTOT <- sum(RGB)
  
  if(RGBTOT==0) {
    rcc <- 0
    gcc <- 0
    bcc <- 0
  }else{
    rcc <- RGB[1]/RGBTOT
    gcc <- RGB[2]/RGBTOT
    bcc <- RGB[3]/RGBTOT
  }
  
  list(rcc = rcc,
       gcc = gcc,
       bcc = bcc, 
       downloadDataTable = downloadDataTable)
}


# shapefile to mask raster given coordinates of the polygon 
createRasteredROI <- function(pnts, imgSize){
  
  pnts <- t(apply(pnts, 1, '*', imgSize))
  ext <- extent(1, imgSize[1], 1, imgSize[2])
  poly <- as(ext  ,"SpatialPolygons")
  # poly@polygons[[1]]@Polygons[[1]]@coords <- as.matrix(pnts)
  
  tbl <- as.data.table(na.omit(cbind(pnts,cumsum(is.na(pnts[,1]))+1 )))
  colnames(tbl) <- c('x', 'y', 'g')
  ng <- table(tbl$g)
  
  polyList <- list()
  np <- length(ng[which(ng>=3)])
  
  for(gi in 1:np)
    polyList[[gi]] <- as.matrix(tbl[g==gi, .(x,y)])
  
  polys <- SpatialPolygons(
    lapply(1:np,
           function(x){
             p <- slot(poly@polygons[[1]], "Polygons")[[1]]
             slot(p, "coords") <- polyList[[x]]  
             pp <- Polygons(list(p), ID = x)
             return(pp)
           })
  )
  
  r <- rasterize(polys, raster(ext, nrow = imgSize[1], ncol = imgSize[2]))
  r[!is.na(r)] <- 1
  
  m1 <- as.matrix(r)
  m <- m1
  m[m1==0|is.na(m1)] <- 1
  m[m1!=0] <- 0
  m
}


#extract time series of chromatic colors for a vector of jpeg files.
extractCCCTimeSeries <- function(rmsk, paths, PLUS=F, session=shiny::getDefaultReactiveDomain(), downloadDataTable, downloadDir){
  
  continue = TRUE
  
  mmsk <- 1-as.matrix(rmsk)
  msk <- mmsk
  m <- as.vector(mmsk)
  m[m==0] <- NA
  mmm <- cbind(m, m, m)
  
  n <- length(paths)
  CCCT <- matrix(NA, nrow=n, ncol=3)
  
  
  # if(exists('session'))
  withProgress(value = 0, message = 'Extracting CCs',
               for(i in 1:n){
                 if(isTRUE(session$input$stopThis))break
                 ccc <- extractCCC(paths[i], mmm, downloadDataTable = downloadDataTable, downloadDir = downloadDir)
                 downloadDataTable <- ccc$downloadDataTable
                 if(!is.null(ccc))
                   CCCT[i,] <- as.numeric((ccc[c("rcc", "gcc", "bcc")]))
                 incProgress(1/n)
                 # Sys.sleep(1)
                 httpuv:::service()
               }
  )
  CCCT <- as.data.table(CCCT)
  colnames(CCCT) <- c('rcc','gcc','bcc')
  list(CCCT = CCCT, downloadDataTable = downloadDataTable)
}


# writing ROIList file on the given list, path and filename
writeROIListFile <- function(ROIList, path, roifilename){
  
  updateTime <- Sys.time()
  hdrText <- paste0('#\n# ROI List for ', ROIList$siteName,
                    '\n#',
                    '\n# Site: ', ROIList$siteName,
                    '\n# Veg Type: ', ROIList$vegType, 
                    '\n# ROI ID Number: ', sprintf('%04d', ROIList$ID),
                    '\n# Owner: ', ROIList$Owner,
                    '\n# Creation Date: ', ROIList$createDate,
                    '\n# Creation Time: ', ROIList$createTime,
                    '\n# Update Date: ', strftime(updateTime, format = '%Y-%m-%d'),
                    '\n# Update Time: ', strftime(updateTime, format = '%H:%M:%S'),
                    '\n# Description: ', ROIList$Description,
                    '\n#\n')
  
  
  bdyText <- 'start_date,start_time,end_date,end_time,maskfile,sample_image\n'
  
  for(i in 1:length(ROIList$masks)){
    m <- ROIList$masks[[i]]$rasteredMask
    
    rName <- names(ROIList$masks)[i]
    
    writeTIFF(m*1 , where = paste0(path, rName,'.tif'))
    
    maskpoints <- ROIList$masks[[i]]$maskpoints
    maskpoints <- rbind(dim(m), maskpoints)
    if(nrow(maskpoints)>3)
      write.table(maskpoints, file = paste0(path, rName,'_vector.csv'), col.names = F, row.names = F, sep = ',')
    
    bdyLine <- paste( ROIList$masks[[i]]$startdate,
                      ROIList$masks[[i]]$starttime,
                      ROIList$masks[[i]]$enddate, 
                      ROIList$masks[[i]]$endtime,
                      paste0(rName,'.tif'),
                      ROIList$masks[[i]]$sampleImage, sep = ',')
    
    
    bdyText <- paste0(bdyText, bdyLine, '\n')
  }
  allText <- paste0(hdrText, bdyText)
  writeLines(allText, paste0(path, roifilename))
}



# parsing klima filenames to data tabels of site, date and time given vector of filenames
filePathParse <- function(filenames)
{
  imgDT <- data.table(filenames = filenames)
  imgDT$tmp <- unlist(lapply(filenames, function(x){strsplit(x,split = '/', fixed = T)[[1]][3]}))
  imgDT[,c('Site', 'Year', 'Month','Day','HHMMSS'):=as.data.table(matrix(unlist(strsplit(gsub(tmp,pattern = '.jpg', replacement = ''), split = '_')), ncol=5, byrow = T))]
  imgDT[,Year:=as.numeric(Year)]
  imgDT[,Month:=as.numeric(Month)]
  imgDT[,Day:=as.numeric(Day)]
  imgDT[,HHMMSS:=as.numeric(HHMMSS)]
  imgDT[,Hour:=floor(HHMMSS/10000)]
  imgDT[,Minute:=floor((HHMMSS%%10000)/100)]
  imgDT[,Second:=HHMMSS%%100]
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  imgDT[,Date:=date(ISOdate(Year, Month, Day))]
  imgDT[,DateTime:=ISOdatetime(Year, Month, Day, Hour, Minute, Second)]
  imgDT[,conT:=Year+DOY/(365+(2001%%4==0))]
  imgDT[,YearDOY:=Year+DOY/1000]
  imgDT
}


# constraining string input to fixed time format
fixFormatTime <- function(asText){
  asSplit <- unlist(strsplit(asText, ':'))
  for(i in 1:length(asSplit))
    if(any(!unlist(strsplit(asSplit[i], ''))%in%as.character(0:9))) asSplit[i] <- 0
    
    asNum <- as.numeric(asSplit)
    if(length(asNum)>3) asNum <- asNum[1:3]
    if(length(asNum)==0) asNum <- c(0, 0, 0)
    if(length(asNum)==1) asNum <- c(asNum[1], 0,0)
    if(length(asNum)==2) asNum <- c(asNum[1:2], 0)
    
    asNum[1] <- min(23, max(asNum[1], 0))
    asNum[2] <- min(59, max(asNum[2], 0))
    asNum[3] <- min(59, max(asNum[3], 0))
    
    asTextNew <- sapply(asNum, sprintf, fmt='%02d')
    asTextNew <- paste(asTextNew, collapse = ':')
    asTextNew
}




#parsing ROIList file into a list in R
parseROI <- function(roifilename, roipath,  downloadDataTable, downloadDir){
  fname <- paste0(roipath, roifilename)
  #if(!file.exists(fname)) return(NULL)
  
  roilines <- readLines(fname)
  
  wEmptyLine <- roilines%in%c('', ' ',  '  ')
  wCommented <- as.vector(sapply(roilines, grepl,  pattern = '^#'))
  wNotSkip <- !(wEmptyLine|wCommented)
  
  
  parseroiline <- function(roilines, property){
    wProp <- grepl(roilines, pattern = property)
    gsub(roilines[wProp], pattern = paste0('# ', property, ': '), replacement = '')
  }
  
  ROIList <- list(siteName = parseroiline(roilines[wCommented], 'Site'), 
                  vegType = parseroiline(roilines[wCommented], 'Veg Type'), 
                  ID = as.numeric(parseroiline(roilines[wCommented], 'ROI ID Number')), 
                  Owner = parseroiline(roilines[wCommented], 'Owner'), 
                  createDate = parseroiline(roilines[wCommented], 'Creation Date'), 
                  createTime = parseroiline(roilines[wCommented], 'Creation Time'), 
                  updateDate = parseroiline(roilines[wCommented], 'Update Date'), 
                  updateTime = parseroiline(roilines[wCommented], 'Update Time'), 
                  Description = parseroiline(roilines[wCommented], 'Description'), 
                  masks = NULL)
  
  
  parsedMasks <- read.table(textConnection(roilines[which(wNotSkip)]), sep = ',', header = T)
  
  masksList <- list()
  for(i in 1:nrow(parsedMasks)){
    maskpath <- paste0(roipath, parsedMasks$maskfile[i])
    maskpointspath <- gsub(maskpath, pattern = '.tif', replacement = '_vector.csv')
    if(file.exists(maskpointspath)|url.exists(maskpointspath)) {
      dummy=0
      maskpoints <- as.matrix(read.csv(maskpointspath, header = F, skip = 1))
    }else{
      maskpoints <- NULL
    }
    
    dummy=0
    
    if(is.url(maskpath)){
      # tmpath <- tempfile()
      # showModal(strong(
      #   modalDialog(HTML(paste0('Loading ', basename(maskpath) , '...')),
      #               easyClose = F,
      #               size = 's',
      #               style='background-color:#3b3a35; color:#fce319; ',
      #               footer = NULL
      #   )))
      # download.file(maskpath, destfile = tmpath, quiet = !PRINT_LOGS)
      
      tmpdl <- tryDownload(maskpath, downloadDataTable = downloadDataTable, downloadDir = downloadDir, showLoad = T)
      downloadDataTable <- tmpdl$downloadDataTable
      maskpath <- tmpdl$destfile
      # removeModal()
      
    }
    
    tmpMask <- list(maskpoints = maskpoints, 
                    startdate = as.character(parsedMasks$start_date[i]), 
                    enddate = as.character(parsedMasks$end_date[i]), 
                    starttime = as.character(parsedMasks$start_time[i]), 
                    endtime = as.character(parsedMasks$end_time[i]), 
                    sampleyear = NULL, 
                    sampleday = NULL,
                    sampleImage = as.character(parsedMasks$sample_image[i]),
                    rasteredMask = as.matrix(raster(maskpath)))
    
    tmpMask$rasteredMask[(!is.na(tmpMask$rasteredMask))&tmpMask$rasteredMask!=0] <- 1
    
    
    sampleYMD <- strsplit(tmpMask$sampleImage, split = '_')[[1]][2:4]
    tmpMask$sampleyear <- as.numeric(sampleYMD)[1]
    tmpMask$sampleday <- yday(paste(sampleYMD, collapse = '-'))
    
    masksList[[length(masksList)+1]] <- tmpMask
    
  }
  names(masksList) <- gsub(parsedMasks$maskfile, pattern = '.tif', replacement = '')
  ROIList$masks <- masksList
  
  list(ROIList = ROIList,
       downloadDataTable = downloadDataTable)
}




# an effort to convert rasterized mask to vectorized shape
maskRaster2Vector <- function(r){
  r[r==0] <- 1
  r[r==255] <- NA
  
  p <- rasterToPolygons(r, dissolve = T)
  c <- p@polygons[[1]]@Polygons[[1]]@coords
  c
}


#parse image data table to site, date and time, probably redundant function
parseIMG.DT <- function(imgDT){
  
  imgDT[,c('Site', 'Year', 'Month','Day','HHMMSS'):=as.data.table(matrix(unlist(strsplit(gsub(filenames, pattern = '.jpg', replacement = ''), split = '_')), ncol=5, byrow = T))]
  imgDT[,Year:=as.numeric(Year)]
  imgDT[,Month:=as.numeric(Month)]
  imgDT[,Day:=as.numeric(Day)]
  imgDT[,HHMMSS:=as.numeric(HHMMSS)]
  imgDT[,Hour:=floor(HHMMSS/10000)]
  imgDT[,Minute:=floor((HHMMSS%%10000)/100)]
  imgDT[,Second:=HHMMSS%%100]
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  imgDT[,Date:=date(ISOdate(Year, Month, Day))]
  imgDT[,DateTime:=ISOdatetime(Year, Month, Day, Hour, Minute, Second)]
  imgDT[,conT:=Year+DOY/(365+(2001%%4==0))]
  imgDT[,YearDOY:=Year+DOY/1000]
  imgDT
}


# gettng image data table given site and midday list path 
getIMG.DT <- function(sites){
  imgDT <- data.table()
  
  for(site in sites){
    if(is.url(middayListPath)){
      mdiJSON = fromJSON(file = paste0(middayListPath, site,'/'))
      tbl <- data.table( DateJSON = as.Date(sapply(mdiJSON$images, function(x){x$date })),
                         path = as.character(sapply(mdiJSON$images, function(x){x$midimg})))
    }else{
      tbl <- read.table(paste0(middayListPath, site), header = F, colClasses = 'character', col.names = 'path')
    }
    
    imgDT.tmp <- as.data.table(tbl)
    imgDT.tmp$path <- paste0(mainDataPath, imgDT.tmp$path)
    imgDT <- rbind(imgDT, imgDT.tmp)
  }
  pathsub <- imgDT[, .(gsub(path, pattern = mainDataPath, replacement = ''))]
  splt <- pathsub[, tstrsplit(V1, split = '/')]
  colnames(splt) <- c('empty','data','archive','site','year','month','filenames') 
  splt[, newpath:=paste(empty, data, archive, site, 'originals', year, month, filenames, sep='/')]
  
  imgDT$filenames <- splt$filenames
  imgDT$newpath <- splt$newpath
  
  imgDT$newpath <- NULL
  
  imgDT <- imgDT[str_count(filenames, pattern = '_')==4, ]
  imgDT <- parseIMG.DT(imgDT)
  imgDT
}


# print a message into konsole given the message string for logging purposes
printLog <- function(msg=NULL, init=F, finit=F){
  if(!PRINT_LOGS) return()
  
  if(init){
    message(paste('\n--------------------------------------------------------------------\n', 
                  as.character(Sys.time()),'New session just started!',
                  '\n--------------------------------------------------------------------\n'))
    return()
  }
  
  if(finit){
    message(paste('\n--------------------------------------------------------------------\n', 
                  as.character(Sys.time()),'Initial setup was completed!',
                  '\n--------------------------------------------------------------------\n'))
    return()
  }
  
  message(paste(as.character(Sys.time()), msg, '\t'))
}


dirHTML <- function(url, sitename, pattern = 'roi.csv$'){
  showModal(strong(
    modalDialog(HTML('Loading roi.csv files ...'),
                easyClose = F,
                size = 's',
                style='background-color:#3b3a35; color:#fce319; ',
                footer = NULL
    )))
  
  tmpath <- tempfile()
  download.file(url, destfile = tmpath, quiet = !PRINT_LOGS, method = 'curl')
  
  txt <- readLines(tmpath)
  n <- grep(paste0('<a href=\"', sitename), x = txt)
  DT <- data.table(file = as.character(sapply(txt[n], function(x){s=strsplit(x, split='<|>'); s[[1]][3]})))
  roi <- grep(DT$file, pattern = pattern)
  removeModal()
  DT$file[roi]
}


tryDownload <- function(path, downloadDataTable, downloadDir, showLoad = T){
  fname <- basename(path)
  printLog(paste('tryDownload was called with ', path, downloadDir ))
  if(showLoad)showModal(strong(
    modalDialog(HTML(paste0('Loading ', fname , '...')),
                easyClose = F,
                size = 's',
                style='background-color:#3b3a35; color:#fce319; ',
                footer = NULL
    )), session=shiny::getDefaultReactiveDomain())
  
  w <- which(downloadDataTable$path == path)
  
  destfile <- paste0(downloadDir, '/', fname)
  
  if(length(w)==0) {
    download.file(path, destfile = destfile, quiet = !PRINT_LOGS, method = 'curl')
    downloadDataTable <- rbind(downloadDataTable,
                               data.table(path = path, Date = Sys.Date()))
  }else if(length(w)==1){
    if(Sys.Date()!=downloadDataTable$Date[w]){
      download.file(path, destfile = destfile, quiet = !PRINT_LOGS, method = 'curl')
      downloadDataTable$Date[w] <- Sys.Date()
    }
  } else
    stop('More than one file in download dir!!')
  
  downloadDataTable$Date <- as.Date(downloadDataTable$Date)
  downloadDataTable$path <- as.character(downloadDataTable$path)
  
  if(showLoad)removeModal()
  return(list(destfile = destfile, 
              downloadDataTable = downloadDataTable))
}


getNAfromLast <- function(x){
  xrev <- rev(x)
  xnew <- xrev
  repeat{
    w <- which(is.na(xrev))
    xnew[w] <- xrev[w+1]
    if(identical(xnew, xrev)) break
    xrev <- xnew
  }
  rev(xrev)
}

putImageFooter <- function(id, mrgDT, footer='', grid = T){
  Date <- mrgDT[ID==id, Date] 
  if(length(Date)==0) return()
  Haze <- mrgDT[ID==id, Haze]
  Black <- signif(mrgDT[ID==id, blackness], 2)
  
  rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4]*.05, col = 'white')
  mtext(side = 1, Date, line = -1, adj = .05, col = 'black', font = 2, cex = 1.5)
  mtext(side = 1, footer, line = -1, adj = .5, col = 'black', font = 2, cex = 1.5)
  mtext(side = 1, Black, line = -1, adj = .85, col = 'black', font = 2, cex = 1.5)
  mtext(side = 1, Haze, line = -1, adj = .95, col = 'black', font = 2, cex = 1.5)
  
  if(grid){
    usr <- par()$usr
    abline(v=seq(usr[1], usr[2], length.out = 10), lty=2, col='yellow', lwd = 2)
    abline(h=seq(usr[3], usr[4], length.out = 10), lty=2, col='yellow', lwd = 2)
  }
}
read_spc_files <- function(directory
                           , baseline = NA
                           , pngplot = F
                           , plotlyplot = T
                           , exportplot = F
                           , recursive = F
                           , colp = NA
                           , shinyoutput  = T){
  
  # Create empty lists
  spcfiles <- list() #all data
  drk <- list()  #dark spectra
  ref <- list() # background
  spc <- list() # spc
  trans <- list() # transmission
  
  if( all(tools::file_ext( directory) == ""))
    # list spc files in directories
    for(i in 1:length(directory)){
      setwd(directory[ i ])
      
      spcfiles$files[[ i ]] <- lapply(directory[ i ],function(x) paste(x,list.files(recursive=recursive,pattern = ".spc",include.dirs = T),sep = "//"))
      spcfiles$files[[ i ]] <- lapply(spcfiles$files[[ i ]],function(x) x[grep("spc",tools::file_ext(x))])
      
      spcfiles$fileslow[[ i ]] <- lapply(directory[ i ],function(x) enc2native(list.files(recursive=recursive,pattern = ".spc",include.dirs = T)))
      spcfiles$fileslow[[ i ]] <- lapply(spcfiles$fileslow[[ i ]],function(x) x[grep("spc",tools::file_ext(x))])
    }
  
  if( all(tools::file_ext( directory) == "spc")){
    spcfiles$files <- directory
    spcfiles$fileslow <- enc2native(directory)
  }
  
  if( all(tools::file_ext( directory) == "csv")){
    spcfiles$files <- directory
    spcfiles$fileslow <- enc2native(directory)
  }
  
  spcfiles$files <- unlist(spcfiles$files)
  spcfiles$fileslow <- unlist(spcfiles$fileslow)
  
  # text options ####
  spcfiles$filestext <- spcfiles$fileslow
  spcfiles$filestext <- gsub(".spc","",spcfiles$filestext)
  spcfiles$filestext <- gsub("__","_",spcfiles$filestext);spcfiles$filestext <- gsub("__","_",spcfiles$filestext);spcfiles$filestext <- gsub("__","_",spcfiles$filestext)
  
  # read spc files ####
  spcfiles$files <- as.list(spcfiles$files)
  
  if( any( tools::file_ext( directory ) != "csv"))
    try(
      suppressWarnings( spcfiles$raw <- lapply(spcfiles$files,function(x) read.spc(x,keys.hdr2data = T, keys.log2data = T, log.txt = TRUE,log.bin = FALSE)))
    )
  
  if( any( tools::file_ext( directory ) != "csv"))
    if(length(grep("ChannelType", names(spcfiles$raw[[1]]@data)))!=0){
      spcfiles$type <- lapply(spcfiles$raw,function(x) x@data[grep("ChannelType", names(x@data))][1,1])
      
      if(length(which(spcfiles$type == "RawData"))>0)  spcfiles$type[which(spcfiles$type == "RawData")] <- lapply(spcfiles$raw,function(x) x@data[grep("ChannelName", names(x@data))][1,1])[which(spcfiles$type == "RawData")]
      
      for(i in which(spcfiles$type == "Normalized")){if(length(grep("Absorbance", names(spcfiles$raw[[ i ]]@data)))) spcfiles$type[ i ] <- "Absorbance" else spcfiles$type[ i ] <- "Transmission"}
      
      for(i in grep("Dark",spcfiles$type)){
        drk$files[[ i ]] <- spcfiles$files[ i ]
        drk$filestext[[ i ]] <- basename(spcfiles$filestext[ i ])
        drk$wl[[ i ]] <- spcfiles$raw[[ i ]]@wavelength
        drk$data[[ i ]] <- spcfiles$raw[[ i ]]@data
        drk$label[[ i ]] <- spcfiles$raw[[ i ]]@label
        drk$log[[ i ]] <- spcfiles$raw[[ i ]]@log
        drk$spc[[ i ]] <- t(spcfiles$raw[[ i ]]@data$spc)
      }
      
      for(i in grep("Refer",spcfiles$type)){
        ref$files[[ i ]] <- spcfiles$files[ i ]
        ref$filestext[[ i ]] <- basename(spcfiles$filestext[ i ])
        ref$wl[[ i ]] <- spcfiles$raw[[ i ]]@wavelength
        ref$data[[ i ]] <- spcfiles$raw[[ i ]]@data
        ref$label[[ i ]] <- spcfiles$raw[[ i ]]@label
        ref$log[[ i ]] <- spcfiles$raw[[ i ]]@log
        ref$spc[[ i ]] <- t(spcfiles$raw[[ i ]]@data$spc)
      }
      
      for(i in which(spcfiles$type == "Absorbance")){
        spc$files[[ i ]] <- spcfiles$files[ i ]
        spc$filestext[[ i ]] <- basename(spcfiles$filestext[ i ])
        spc$wl[[ i ]] <- spcfiles$raw[[ i ]]@wavelength
        spc$data[[ i ]] <- spcfiles$raw[[ i ]]@data
        spc$label[[ i ]] <- spcfiles$raw[[ i ]]@label
        spc$log[[ i ]] <- spcfiles$raw[[ i ]]@log
        spc$spc[[ i ]] <- t(spcfiles$raw[[ i ]]@data$spc)
        spc$spc_baseline[[ i ]] <- spc$spc[[ i ]]
      }
      
      for(i in which(spcfiles$type == "Transmission")){
        trans$files[[ i ]] <- spcfiles$files[ i ]
        trans$filestext[[ i ]] <- basename(spcfiles$filestext[ i ])
        trans$wl[[ i ]] <- spcfiles$raw[[ i ]]@wavelength
        trans$data[[ i ]] <- spcfiles$raw[[ i ]]@data
        trans$label[[ i ]] <- spcfiles$raw[[ i ]]@label
        trans$log[[ i ]] <- spcfiles$raw[[ i ]]@log
        trans$spc[[ i ]] <- t(spcfiles$raw[[ i ]]@data$spc)
      }
    } else {
      
      for(i in grep("-D",spcfiles$filestext)){
        drk$files[[ i ]] <- spcfiles$files[ i ]
        drk$filestext[[ i ]] <- basename(spcfiles$filestext[ i ])
        drk$wl[[ i ]] <- spcfiles$raw[[ i ]]@wavelength
        drk$data[[ i ]] <- spcfiles$raw[[ i ]]@data
        drk$label[[ i ]] <- spcfiles$raw[[ i ]]@label
        drk$spc[[ i ]] <- t(spcfiles$raw[[ i ]]@data$spc)
      }
      
      for(i in grep("-R",spcfiles$filestext)){
        ref$files[[ i ]] <- spcfiles$files[ i ]
        ref$filestext[[ i ]] <- basename(spcfiles$filestext[ i ])
        ref$wl[[ i ]] <- spcfiles$raw[[ i ]]@wavelength
        ref$data[[ i ]] <- spcfiles$raw[[ i ]]@data
        ref$label[[ i ]] <- spcfiles$raw[[ i ]]@label
        ref$spc[[ i ]] <- t(spcfiles$raw[[ i ]]@data$spc)
      }
      
      for(i in grep("-M",spcfiles$filestext)){
        spc$files[[ i ]] <- spcfiles$files[ i ]
        spc$filestext[[ i ]] <- basename(spcfiles$filestext[ i ])
        spc$wl[[ i ]] <- spcfiles$raw[[ i ]]@wavelength
        spc$data[[ i ]] <- spcfiles$raw[[ i ]]@data
        spc$label[[ i ]] <- spcfiles$raw[[ i ]]@label
        spc$spc[[ i ]] <- t(spcfiles$raw[[ i ]]@data$spc)
        spc$spc_baseline[[ i ]] <- spc$spc[[ i ]]
      }
      
      if(is.null(spc$spc)){
        for(i in grep("_c",substr(spcfiles$filestext, nchar(spcfiles$filestext) - 5, nchar(spcfiles$filestext)))){
          spc$files[[ i ]] <- spcfiles$files[ i ]
          spc$filestext[[ i ]] <- basename(spcfiles$filestext[ i ])
          spc$wl[[ i ]] <- spcfiles$raw[[ i ]]@wavelength
          spc$data[[ i ]] <- spcfiles$raw[[ i ]]@data
          spc$label[[ i ]] <- spcfiles$raw[[ i ]]@label
          spc$spc[[ i ]] <- t(spcfiles$raw[[ i ]]@data$spc)
          spc$spc_baseline[[ i ]] <- spc$spc[[ i ]]
        }
      }
      
    }
  
  if( any( tools::file_ext( directory ) != "csv")){
    # wl ####
    for(i in 1:length(spc$spc)) if(length(ncol(spc$spc[[ i ]]))>0) spc$wl[[ i ]] <-  replicate(ncol(spc$spc[[ i ]]),spc$wl[[ i ]])
    for(i in 1:length(trans$spc)) if(length(ncol(trans$spc[[ i ]]))>0) trans$wl[[ i ]] <-  replicate(ncol(trans$spc[[ i ]]),trans$wl[[ i ]])
    
    # nullvec ####
    nullvecref <- which(lapply(ref$wl,is.null)==F)
    nullvecdrk <- which(lapply(drk$wl,is.null)==F)
    nullvectrans <- which(lapply(trans$wl,is.null)==F)
    nullvecspc <- which(lapply(spc$wl,is.null)==F)
    
    # baseline ####
    if(!is.na(baseline)){
      for(i in nullvecspc){
        if(ncol(spc$spc_baseline[[ i ]])==1) spc$spc_baseline[[ i ]] <- spc$spc_baseline[[ i ]]-spc$spc_baseline[[ i ]][which.min(abs(spc$wl[[ i ]]-baseline)),]
        if(ncol(spc$spc_baseline[[ i ]])>1){
          for(j in 1:ncol(spc$spc_baseline[[ i ]])){
            spc$spc_baseline[[ i ]][  , j ] <- spc$spc_baseline[[ i ]][  , j ]-as.numeric(spc$spc_baseline[[ i ]][  , j ])[which.min(abs(spc$wl[[ i ]][  , j ]-baseline))]
          }
        }
      }
    }
  }
  
  # colp ####
  if(is.na(colp)[1]){
    colp2 <- rainbow(length(unique(unlist(spcfiles$files))))
    colp <- colp2[factor(unlist(spcfiles$files))]
  }
  
  # !is.na & !is.null ####
  drk$wl <- drk$wl[which(!is.na(drk$files))]
  drk$data <- drk$data[which(!is.na(drk$files))]
  drk$label <- drk$label[which(!is.na(drk$files))]
  drk$log <- drk$log[which(!is.na(drk$files))]
  drk$spc <- drk$spc[which(!is.na(drk$files))]
  drk$files <- drk$files[which(!is.na(drk$files))]
  drk$filestext <- drk$filestext[which(!is.na(drk$filestext))]
  
  ref$wl <- ref$wl[which(!is.na(ref$files))]
  ref$data <- ref$data[which(!is.na(ref$files))]
  ref$label <- ref$label[which(!is.na(ref$files))]
  ref$log <- ref$log[which(!is.na(ref$files))]
  ref$spc <- ref$spc[which(!is.na(ref$files))]
  ref$files <- ref$files[which(!is.na(ref$files))]
  ref$filestext <- ref$filestext[which(!is.na(ref$filestext))]
  
  spc$wl <- spc$wl[which(!is.na(spc$files))]
  spc$data <- spc$data[which(!is.na(spc$files))]
  spc$label <- spc$label[which(!is.na(spc$files))]
  spc$log <- spc$log[which(!is.na(spc$files))]
  spc$spc <- spc$spc[which(!is.na(spc$files))]
  spc$spc_baseline <- spc$spc_baseline[which(!is.na(spc$files))] #!#
  spc$files <- spc$files[which(!is.na(spc$files))]
  spc$filestext <- spc$filestext[which(!is.na(spc$filestext))]
  
  trans$wl <- trans$wl[which(!is.na(trans$files))]
  trans$data <- trans$data[which(!is.na(trans$files))]
  trans$label <- trans$label[which(!is.na(trans$files))]
  trans$log <- trans$log[which(!is.na(trans$files))]
  trans$spc <- trans$spc[which(!is.na(trans$files))]
  trans$files <- trans$files[which(!is.na(trans$files))]
  trans$filestext <- trans$filestext[which(!is.na(trans$filestext))]
  
  drk$wl <- drk$wl[which(unlist(lapply(drk$wl, function(x) !is.null(x)))==T)]
  drk$data <- drk$data[which(unlist(lapply(drk$data, function(x) !is.null(x)))==T)]
  drk$label <- drk$label[which(unlist(lapply(drk$label, function(x) !is.null(x)))==T)]
  drk$log <- drk$log[which(unlist(lapply(drk$log, function(x) !is.null(x)))==T)]
  drk$spc <- drk$spc[which(unlist(lapply(drk$spc, function(x) !is.null(x)))==T)]
  drk$files <- drk$files[which(unlist(lapply(drk$files, function(x) !is.null(x)))==T)]
  drk$filestext <- drk$filestext[which(unlist(lapply(drk$filestext, function(x) !is.null(x)))==T)]
  
  if( length( drk$spc ) > 0) for(i in 1 : length( drk$spc)){
    drk$wldf[[ i ]] <- matrix(nrow = length( drk$wl[[ i ]] ), ncol = ncol( drk$spc[[ i ]] ))
    for(j in 1 : ncol( drk$wldf[[ i ]] )) drk$wldf[[ i ]][ , j] <- drk$wl[[ j ]]
    drk$wl[[ i ]] <- drk$wldf[[ i ]]
  }
  
  ref$wl <- ref$wl[which(unlist(lapply(ref$wl, function(x) !is.null(x)))==T)]
  ref$data <- ref$data[which(unlist(lapply(ref$data, function(x) !is.null(x)))==T)]
  ref$label <- ref$label[which(unlist(lapply(ref$label, function(x) !is.null(x)))==T)]
  ref$log <- ref$log[which(unlist(lapply(ref$log, function(x) !is.null(x)))==T)]
  ref$spc <- ref$spc[which(unlist(lapply(ref$spc, function(x) !is.null(x)))==T)]
  ref$files <- ref$files[which(unlist(lapply(ref$files, function(x) !is.null(x)))==T)]
  ref$filestext <- ref$filestext[which(unlist(lapply(ref$filestext, function(x) !is.null(x)))==T)]
  
  if( length( ref$spc ) > 0) for(i in 1 : length( ref$spc)){
    ref$wldf[[ i ]] <- matrix(nrow = length( ref$wl[[ i ]] ), ncol = ncol( ref$spc[[ i ]] ))
    for(j in 1 : ncol( ref$wldf[[ i ]] )) ref$wldf[[ i ]][ , j] <- ref$wl[[ j ]]
    ref$wl[[ i ]] <- ref$wldf[[ i ]]
  }
  
  spc$wl <- spc$wl[which(unlist(lapply(spc$wl, function(x) !is.null(x)))==T)]
  spc$data <- spc$data[which(unlist(lapply(spc$data, function(x) !is.null(x)))==T)]
  spc$label <- spc$label[which(unlist(lapply(spc$label, function(x) !is.null(x)))==T)]
  spc$log <- spc$log[which(unlist(lapply(spc$log, function(x) !is.null(x)))==T)]
  spc$spc <- spc$spc[which(unlist(lapply(spc$spc, function(x) !is.null(x)))==T)]
  spc$spc_baseline <- spc$spc_baseline[which(unlist(lapply(spc$spc_baseline, function(x) !is.null(x)))==T)]
  spc$files <- spc$files[which(unlist(lapply(spc$files, function(x) !is.null(x)))==T)]
  spc$filestext <- spc$filestext[which(unlist(lapply(spc$filestext, function(x) !is.null(x)))==T)]
  
  if( length( spc$spc ) > 0) for(i in 1 : length( spc$spc)){
    spc$wldf[[ i ]] <- matrix(nrow = length( unique( unlist( data.table(spc$wl[[ i ]]))) ), ncol = ncol( spc$spc[[ i ]] ))
    for(j in 1 : ncol( spc$wldf[[ i ]] )) spc$wldf[[ i ]][ , j] <- unique( spc$wl[[ i ]][ , j] )
    spc$wl[[ i ]] <- spc$wldf[[ i ]]
  }
  
  trans$wl <- trans$wl[which(unlist(lapply(trans$wl, function(x) !is.null(x)))==T)]
  trans$data <- trans$data[which(unlist(lapply(trans$data, function(x) !is.null(x)))==T)]
  trans$label <- trans$label[which(unlist(lapply(trans$label, function(x) !is.null(x)))==T)]
  trans$log <- trans$log[which(unlist(lapply(trans$log, function(x) !is.null(x)))==T)]
  trans$spc <- trans$spc[which(unlist(lapply(trans$spc, function(x) !is.null(x)))==T)]
  trans$files <- trans$files[which(unlist(lapply(trans$files, function(x) !is.null(x)))==T)]
  trans$filestext <- trans$filestext[which(unlist(lapply(trans$filestext, function(x) !is.null(x)))==T)]
  
  if( length( trans$spc ) > 0) for(i in 1 : length( trans$spc)){
    trans$wldf[[ i ]] <- matrix(nrow = length( trans$wl[[ i ]] ), ncol = ncol( trans$spc[[ i ]] ))
    for(j in 1 : ncol( trans$wldf[[ i ]] )) trans$wldf[[ i ]][ , j] <- trans$wl[[ j ]]
    trans$wl[[ i ]] <- trans$wldf[[ i ]]
  }
  
  # read csv ####
  if( any( tools::file_ext( directory ) == "csv")){
    readcsv <- list()
    readcsv$raw <- lapply( directory, function( x ) fread( x, sep = ";", dec = ","))
    readcsv$ppp <- lapply( readcsv$raw, transfer_csv.num.col)
    
    readcsv$spc <- mapply( function( csv, ppp ) csv[ , ppp$numcol, with = F]
                           , csv = readcsv$raw
                           , ppp = readcsv$ppp
                           , SIMPLIFY = F)
    
    options(scipen=999)
    
    for(i in 1 : length( readcsv$raw)){
      readcsv$type[[ i ]] <- "spc"
      
      # spc files detection (count zeros at the first digit)
      if( which.max( table( substr( abs(readcsv$spc[[ i ]][1,]), 1, 1) )) == 1) readcsv$type[[ i ]] <- "spc"
      
      # drk files detection (All Counts between 100 and 5000)
      if( all( readcsv$spc[[ i ]][1,] > 100 & readcsv$spc[[ i ]][1,] < 5000)) readcsv$type[[ i ]] <- "drk"
      
      # ref files detection (Most Counts at 200 : 290nm > 5000)
      if( length( table( readcsv$spc[[ i ]][1, 10 : 100] > 5000 ) ) > 1) 
        if( table( readcsv$spc[[ i ]][1, 10 : 100] > 5000 )[ 2 ]  > table( readcsv$spc[[ i ]][1, 10 : 100] > 5000 )[ 1 ] ) 
          readcsv$type[[ i ]] <- "ref"
      
      # ref files detection (All Counts at 200 : 290nm > 5000)
      if( all( readcsv$spc[[ i ]][1, 10 : 100] > 5000 )) readcsv$type[[ i ]] <- "ref"
      
      # trans files detection (Most Transmissions have the "." at position 3)
      if( length( table( as.numeric( gregexpr("\\.", abs( readcsv$spc[[ i ]][1,] ))) == 3)) > 1) 
        if(table( as.numeric( gregexpr("\\.", abs( readcsv$spc[[ i ]][1,] ))) == 3)[ 2 ] > 
           table( as.numeric( gregexpr("\\.", abs( readcsv$spc[[ i ]][1,] ))) == 3)[ 1 ])  readcsv$type[[ i ]] <- "trans"
      
    }
    
    spc$spc <- list()
    spc$files <- list()
    spc$wl <- list()
    spc$data <- list()
    spc$wldf <- list()
    
    ref$spc <- list()
    ref$files <- list()
    ref$wl <- list()
    ref$data <- list()
    ref$wldf <- list()
    
    drk$spc <- list()
    drk$files <- list()
    drk$wl <- list()
    drk$data <- list()
    drk$wldf <- list()
    
    trans$spc <- list()
    trans$files <- list()
    trans$wl <- list()
    trans$data <- list()
    trans$wldf <- list()
    
    colp <- list()
  }
  
  if( any( tools::file_ext( directory ) == "csv"))
    for(i in 1 : length( readcsv$type)){
      
      if( readcsv$type[[ i ]] == "spc"){
        
        spc$spc[[ i ]] <- readcsv$spc[[ i ]]
        spc$wl[[ i ]] <- readcsv$ppp[[ i ]]$wl
        if( min( readcsv$ppp[[ i ]]$numcol) > 1) spc$data[[ i ]] <- readcsv$raw[[ i ]][ , 1 : (min( readcsv$ppp[[ i ]]$numcol) - 1), with = F]
        
        if( min( readcsv$ppp[[ i ]]$numcol) > 1) 
          if( length( grep("filen", names( spc$data[[ i ]] ), ignore.case = T, value = T)) > 0) 
            readcsv$filestext[[ i ]] <- spc$data[[ i ]][ , grep("filen", names( spc$data[[ i ]] ), ignore.case = T, value = T), with = F]
        
        if( min( readcsv$ppp[[ i ]]$numcol) > 1)
          if(length( grep("datetime", names( spc$data[[ i ]] ), ignore.case = T, value = T)) != 0){
            readcsv$filestext[[ i ]] <- spc$data[[ i ]][ , grep("datetime", names( spc$data[[ i ]] ), ignore.case = T, value = T), with = F]} else{
              readcsv$filestext[[ i ]] <- spc$data[[ i ]][ , 1, with = F]}
        
        if( min( readcsv$ppp[[ i ]]$numcol) == 1)
          readcsv$filestext[[ i ]] <- 1 : nrow( spc$data[[ i ]] )
        
        spc$files[[ i ]] <- i
        
        spc$wldf[[ i ]] <- matrix(nrow = length( spc$wl[[ i ]] ), ncol = nrow( spc$spc[[ i ]]))
        for(j in 1 : ncol( spc$wldf[[ i ]] )) spc$wldf[[ i ]][ , j] <- spc$wl[[ i ]]
        
        spc$wl[[ i ]] <- spc$wldf[[ i ]]
        spc$spc[[ i ]] <- t( spc$spc[[ i ]] )
        
        spc$data[[ i ]] <- data.frame(spc$data[[ i ]])
        
        nullvecspc <- 1
      }
      
      if( readcsv$type[[ i ]] == "ref"){
        
        ref$spc[[ i ]] <- readcsv$spc[[ i ]]
        ref$wl[[ i ]] <- readcsv$ppp[[ i ]]$wl
        if( min( readcsv$ppp[[ i ]]$numcol) > 1) ref$data[[ i ]] <- readcsv$raw[[ i ]][ , 1 : (min( readcsv$ppp[[ i ]]$numcol) - 1), with = F]
        
        if( min( readcsv$ppp[[ i ]]$numcol) > 1) 
          if( length( grep("filen", names( ref$data[[ i ]] ), ignore.case = T, value = T)) > 0) 
            readcsv$filestext[[ i ]] <- ref$data[[ i ]][ , grep("filen", names( ref$data[[ i ]] ), ignore.case = T, value = T), with = F]
        
        if( min( readcsv$ppp[[ i ]]$numcol) > 1)
          if(length( grep("datetime", names( ref$data[[ i ]] ), ignore.case = T, value = T)) != 0){
            readcsv$filestext[[ i ]] <- ref$data[[ i ]][ , grep("datetime", names( ref$data[[ i ]] ), ignore.case = T, value = T), with = F]} else{
              readcsv$filestext[[ i ]] <- ref$data[[ i ]][ , 1, with = F]}
        
        if( min( readcsv$ppp[[ i ]]$numcol) == 1)
          readcsv$filestext[[ i ]] <- 1 : nrow( ref$data[[ i ]] )
        
        ref$files[[ i ]] <- i
        
        ref$wldf[[ i ]] <- matrix(nrow = length( ref$wl[[ i ]] ), ncol = nrow( ref$spc[[ i ]]))
        for(j in 1 : ncol( ref$wldf[[ i ]] )) ref$wldf[[ i ]][ , j] <- ref$wl[[ i ]]
        
        ref$wl[[ i ]] <- ref$wldf[[ i ]]
        ref$spc[[ i ]] <- t( ref$spc[[ i ]] )
        
        ref$data[[ i ]] <- data.frame(ref$data[[ i ]])
        
        nullvecref <- 1
      }
      
      if( readcsv$type[[ i ]] == "drk"){
        
        drk$spc[[ i ]] <- readcsv$spc[[ i ]]
        drk$wl[[ i ]] <- readcsv$ppp[[ i ]]$wl
        if( min( readcsv$ppp[[ i ]]$numcol) > 1) drk$data[[ i ]] <- readcsv$raw[[ i ]][ , 1 : (min( readcsv$ppp[[ i ]]$numcol) - 1), with = F]
        
        if( min( readcsv$ppp[[ i ]]$numcol) > 1) 
          if( length( grep("filen", names( drk$data[[ i ]] ), ignore.case = T, value = T)) > 0) 
            readcsv$filestext[[ i ]] <- drk$data[[ i ]][ , grep("filen", names( drk$data[[ i ]] ), ignore.case = T, value = T), with = F]
        
        if( min( readcsv$ppp[[ i ]]$numcol) > 1)
          if(length( grep("datetime", names( drk$data[[ i ]] ), ignore.case = T, value = T)) != 0){
            readcsv$filestext[[ i ]] <- drk$data[[ i ]][ , grep("datetime", names( drk$data[[ i ]] ), ignore.case = T, value = T), with = F]} else{
              readcsv$filestext[[ i ]] <- drk$data[[ i ]][ , 1, with = F]}
        
        if( min( readcsv$ppp[[ i ]]$numcol) == 1)
          readcsv$filestext[[ i ]] <- 1 : nrow( drk$data[[ i ]] )
        
        drk$files[[ i ]] <- i
        
        drk$wldf[[ i ]] <- matrix(nrow = length( drk$wl[[ i ]] ), ncol = nrow( drk$spc[[ i ]]))
        for(j in 1 : ncol( drk$wldf[[ i ]] )) drk$wldf[[ i ]][ , j] <- drk$wl[[ i ]]
        
        drk$wl[[ i ]] <- drk$wldf[[ i ]]
        drk$spc[[ i ]] <- t( drk$spc[[ i ]] )
        
        drk$data[[ i ]] <- data.frame(drk$data[[ i ]])
        
        nullvecdrk <- 1
      }
      
      if( readcsv$type[[ i ]] == "trans"){
        
        trans$spc[[ i ]] <- readcsv$spc[[ i ]]
        trans$wl[[ i ]] <- readcsv$ppp[[ i ]]$wl
        if( min( readcsv$ppp[[ i ]]$numcol) > 1) trans$data[[ i ]] <- readcsv$raw[[ i ]][ , 1 : (min( readcsv$ppp[[ i ]]$numcol) - 1), with = F]
        
        if( min( readcsv$ppp[[ i ]]$numcol) > 1) 
          if( length( grep("filen", names( trans$data[[ i ]] ), ignore.case = T, value = T)) > 0) 
            readcsv$filestext[[ i ]] <- trans$data[[ i ]][ , grep("filen", names( trans$data[[ i ]] ), ignore.case = T, value = T), with = F]
        
        if( min( readcsv$ppp[[ i ]]$numcol) > 1)
          if(length( grep("datetime", names( trans$data[[ i ]] ), ignore.case = T, value = T)) != 0){
            readcsv$filestext[[ i ]] <- trans$data[[ i ]][ , grep("datetime", names( trans$data[[ i ]] ), ignore.case = T, value = T), with = F]} else{
              readcsv$filestext[[ i ]] <- trans$data[[ i ]][ , 1, with = F]}
        
        if( min( readcsv$ppp[[ i ]]$numcol) == 1)
          readcsv$filestext[[ i ]] <- 1 : nrow( trans$data[[ i ]] )
        
        trans$files[[ i ]] <- i
        
        trans$wldf[[ i ]] <- matrix(nrow = length( trans$wl[[ i ]] ), ncol = nrow( trans$spc[[ i ]]))
        for(j in 1 : ncol( trans$wldf[[ i ]] )) trans$wldf[[ i ]][ , j] <- trans$wl[[ i ]]
        
        trans$wl[[ i ]] <- trans$wldf[[ i ]]
        trans$spc[[ i ]] <- t( trans$spc[[ i ]] )
        
        trans$data[[ i ]] <- data.frame(trans$data[[ i ]])
        
        nullvectrans <- 1
      }
      
      
      # colp ####
      colp2 <- rainbow(length(unique( dotspc_names( unlist( readcsv$filestext) ))))
      colp[[ i ]] <- colp2[factor(unlist( dotspc_names( unlist( readcsv$filestext) )))]
      
    }
  
  # plotly plot ####
  if(plotlyplot==T){
    
    # setwd(file.path(Sys.getenv("USERPROFILE"),"Desktop"))
    
    plotlyp <- list()
    plotlyp$sizep <- 10
    plotlyp$widthp <- 1
    
    if( !is.null( drk$filestext )) drk$filestextlegend <- substr(drk$filestext, 1, gregexpr("_c01_", drk$filestext)[[ 1 ]] - 1)
    if( !is.null( ref$filestext )) ref$filestextlegend <- substr(ref$filestext, 1, gregexpr("_r01_", ref$filestext)[[ 1 ]] - 1)
    if( !is.null( spc$filestext )) spc$filestextlegend <- substr(spc$filestext, 1, gregexpr("_c01_", spc$filestext)[[ 1 ]] - 1)
    if( !is.null( trans$filestext )) trans$filestextlegend <- substr(trans$filestext, 1, gregexpr("_c01_", trans$filestext)[[ 1 ]] - 1)
    
    # ref
    if( exists("nullvecref")) if(length(nullvecref)>0){
      plotlyp$ref <- list(yp = list(title = "Counts",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(ref$spc))[1]*0.975,-1),round(range(unlist(ref$spc))[2]*1.025,-1)),ticks = "outside"),
                          xp = list(title = "lambda in nm",showline=T,showgrid=F,mirror=T,ticks = "outside",range=c(round(range(unlist(ref$wl))[1]*0.975,-1),round(range(unlist(ref$wl))[2]*1.025,-1))))
      
      plotly_ref<-plot_ly(type = "scatter", mode = "lines")%>%layout(yaxis=plotlyp$ref$yp, xaxis=plotlyp$ref$xp,font=list(size=plotlyp$sizep), title = "Referenzspektren")
      
      for(i in 1:length(ref$files)){
        if(length(ref$spc[[ i ]])>0)  for(j in 1 : ncol(ref$spc[[ i ]])){
          plotly_ref <- plotly_ref %>% plotly::add_trace(
            x = as.numeric(ref$wl[[ i ]][  , j ])
            , y = as.numeric(ref$spc[[ i ]][  , j ])
            
            , type = "scatter", mode = "lines"
            
            , line = list(width = plotlyp$widthp
                          , color = if( any( tools::file_ext( directory ) != "csv")){ colp[ i ]} else { colp[[ i ]][ j ]})
            
            , name = if( any( tools::file_ext( directory ) != "csv")){
              dotspc_names( substr(ref$data[[ i ]][1, grep("^NAME$|filename", names(ref$data[[ i ]]))][ 1 ]
                                   , 1 + gregexpr("\\\\", ref$data[[ i ]][1,grep("^NAME$|filename", names(ref$data[[ i ]]))][ 1 ])[[ 1 ]][ length( gregexpr("\\\\", ref$data[[ i ]][1,grep("^NAME$|filename", names(ref$data[[ i ]]))][ 1 ])[[ 1 ]] )]
                                   , nchar( ref$data[[ i ]][1,grep("^NAME$|filename", names(ref$data[[ i ]]))][ 1 ])))} else{ name = dotspc_names( as.character( unlist( do.call(c, readcsv$filestext[ i ])[[ 1 ]][ j ]))) }
            
            , text = paste("Datum_Uhrzeit = ", ref$data[[ i ]][1,grep("fdate|datetime", names(ref$data[[ i ]]), ignore.case = T)[ 1 ]],
                           "<br>Dateiname = ", ifelse( length( grep("^NAME$|filename", names(ref$data[[ i ]])) ) != 0
                                                       , substr(ref$data[[ i ]][1,grep("^NAME$|filename", names(ref$data[[ i ]]))][ 1 ]
                                                                , 1 + gregexpr("\\\\", ref$data[[ i ]][j,grep("^NAME$|filename", names(ref$data[[ i ]]))][ 1 ])[[ 1 ]][ length( gregexpr("\\\\", ref$data[[ i ]][1,grep("^NAME$|filename", names(ref$data[[ i ]]))][ 1 ])[[ 1 ]] )]
                                                                , nchar( ref$data[[ i ]][j,grep("^NAME$|filename", names(ref$data[[ i ]]))][ 1 ]))
                                                       , as.character(ref$data[[ i ]][ , 1]))
                           ,
                           "<br>Integrationszeit = ",ref$data[[ i ]][j,grep("It|integrationTime", names(ref$data[[ i ]]))[1]],
                           "<br>Mittelungen = ",ref$data[[ i ]][j,grep("Aver|accumulations", names(ref$data[[ i ]]))])
          )
        }
      }
      
      if( exportplot ) tryCatch(htmlwidgets::saveWidget(as_widget(plotly_ref),paste0(date.dt(), "_ref.html")),error=function(e){})
    }
    
    # drk
    if( exists("nullvecdrk")) if(length(nullvecdrk)>0){
      plotlyp$spc <- list(yp = list(title = "AU",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(drk$spc))[1]*0.975,-1),round(range(unlist(drk$spc))[2]*1.025,-1)),ticks = "outside"),
                          xp = list(title = "lambda in nm",showline=T,showgrid=F,mirror=T,ticks = "outside",range=c(round(range(unlist(drk$wl))[1]*0.975,-1),round(range(unlist(drk$wl))[2]*1.025,-1))))
      
      plotly_drk <- plot_ly(type = "scatter", mode = "lines") %>% layout(yaxis=plotlyp$drk$yp, xaxis=plotlyp$drk$xp,font=list(size=plotlyp$sizep), title = "Absorptionsspektren")
      
      for(i in 1:length(drk$files)){
        if(length(drk$spc[[ i ]])>0)  for(j in 1 : ncol(drk$spc[[ i ]])){
          plotly_drk <- plotly_drk %>% plotly::add_trace(
            x = as.numeric(drk$wl[[ i ]][  , j ])
            , y = as.numeric(drk$spc[[ i ]][  , j ])
            
            , type = "scatter", mode = "lines"
            
            , line = list(width = plotlyp$widthp
                          , color = if( any( tools::file_ext( directory ) != "csv")){ colp[ i ]} else { colp[[ i ]][ j ]})
            
            , name = if( any( tools::file_ext( directory ) != "csv")){
              dotspc_names( substr(drk$data[[ i ]][1, grep("^NAME$|filename", names(drk$data[[ i ]]))][ 1 ]
                                   , 1 + gregexpr("\\\\", drk$data[[ i ]][1,grep("^NAME$|filename", names(drk$data[[ i ]]))][ 1 ])[[ 1 ]][ length( gregexpr("\\\\", drk$data[[ i ]][1,grep("^NAME$|filename", names(drk$data[[ i ]]))][ 1 ])[[ 1 ]] )]
                                   , nchar( drk$data[[ i ]][1,grep("^NAME$|filename", names(drk$data[[ i ]]))][ 1 ])))} else{ name = dotspc_names( as.character( unlist( do.call(c, readcsv$filestext[ i ])[[ 1 ]][ j ]))) }
            
            , text = paste("Datum_Uhrzeit = ", drk$data[[ i ]][1,grep("fdate|datetime", names(drk$data[[ i ]]), ignore.case = T)[ 1 ]],
                           "<br>Dateiname = ", ifelse( length( grep("^NAME$|filename", names(drk$data[[ i ]])) ) != 0
                                                       , substr(drk$data[[ i ]][1,grep("^NAME$|filename", names(drk$data[[ i ]]))][ 1 ]
                                                                , 1 + gregexpr("\\\\", drk$data[[ i ]][j,grep("^NAME$|filename", names(drk$data[[ i ]]))][ 1 ])[[ 1 ]][ length( gregexpr("\\\\", drk$data[[ i ]][1,grep("^NAME$|filename", names(drk$data[[ i ]]))][ 1 ])[[ 1 ]] )]
                                                                , nchar( drk$data[[ i ]][j,grep("^NAME$|filename", names(drk$data[[ i ]]))][ 1 ]))
                                                       , as.character(drk$data[[ i ]][ , 1]))
                           ,
                           "<br>Integrationszeit = ",drk$data[[ i ]][j,grep("It|integrationTime", names(drk$data[[ i ]]))[1]],
                           "<br>Mittelungen = ",drk$data[[ i ]][j,grep("Aver|accumulations", names(drk$data[[ i ]]))])
          )
        }
      }
      
      if( exportplot ) tryCatch(htmlwidgets::saveWidget(as_widget(plotly_drk),paste0(date.dt(), "_drk.html")),error=function(e){})
    }
    # spc
    if( exists("nullvecspc")) if(length(nullvecspc)>0){
      plotlyp$spc <- list(yp = list(title = "AU",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(spc$spc))[1]*0.975,-1),round(range(unlist(spc$spc))[2]*1.025,-1)),ticks = "outside"),
                          xp = list(title = "lambda in nm",showline=T,showgrid=F,mirror=T,ticks = "outside",range=c(round(range(unlist(spc$wl))[1]*0.975,-1),round(range(unlist(spc$wl))[2]*1.025,-1))))
      
      plotly_spc <- plot_ly(type = "scatter", mode = "lines") %>% layout(yaxis=plotlyp$spc$yp, xaxis=plotlyp$spc$xp,font=list(size=plotlyp$sizep), title = "Absorptionsspektren")
      
      for(i in 1:length(spc$files)){
        if(length(spc$spc[[ i ]])>0)  for(j in 1 : ncol(spc$spc[[ i ]])){
          plotly_spc <- plotly_spc %>% plotly::add_trace(
            x = as.numeric(spc$wl[[ i ]][  , j ])
            , y = as.numeric(spc$spc[[ i ]][  , j ])
            
            , type = "scatter", mode = "lines"
            
            , line = list(width = plotlyp$widthp
                          , color = if( any( tools::file_ext( directory ) != "csv")){ colp[ i ]} else { colp[[ i ]][ j ]})
            
            , name = if( any( tools::file_ext( directory ) != "csv")){
              dotspc_names( substr(spc$data[[ i ]][1, grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ]
                                   , 1 + gregexpr("\\\\", spc$data[[ i ]][1,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ])[[ 1 ]][ length( gregexpr("\\\\", spc$data[[ i ]][1,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ])[[ 1 ]] )]
                                   , nchar( spc$data[[ i ]][1,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ])))} else{ name = dotspc_names( as.character( unlist( do.call(c, readcsv$filestext[ i ])[[ 1 ]][ j ]))) }
            
            , text = paste("Datum_Uhrzeit = ", spc$data[[ i ]][1,grep("fdate|datetime", names(spc$data[[ i ]]), ignore.case = T)[ 1 ]],
                           "<br>Dateiname = ", ifelse( length( grep("^NAME$|filename", names(spc$data[[ i ]])) ) != 0
                                                       , substr(spc$data[[ i ]][1,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ]
                                                                , 1 + gregexpr("\\\\", spc$data[[ i ]][j,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ])[[ 1 ]][ length( gregexpr("\\\\", spc$data[[ i ]][1,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ])[[ 1 ]] )]
                                                                , nchar( spc$data[[ i ]][j,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ]))
                                                       , as.character(spc$data[[ i ]][ , 1]))
                           ,
                           "<br>Integrationszeit = ",spc$data[[ i ]][j,grep("It|integrationTime", names(spc$data[[ i ]]))[1]],
                           "<br>Mittelungen = ",spc$data[[ i ]][j,grep("Aver|accumulations", names(spc$data[[ i ]]))])
          )
        }
      }
      
      if( exportplot ) tryCatch(htmlwidgets::saveWidget(as_widget(plotly_spc),paste0(date.dt(), "_spc.html")),error=function(e){})
      
      # spc baseline
      if(!is.na(baseline)){
        plotlyp$spc_baseline <- list(yp = list(title = "AU",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(spc$spc_baseline))[1]*0.975,-1),round(range(unlist(spc$spc_baseline))[2]*1.025,-1)),ticks = "outside"),
                                     xp = list(title = "lambda in nm",showline=T,showgrid=F,mirror=T,ticks = "outside",range=c(round(range(unlist(spc$wl))[1]*0.975,-1),round(range(unlist(spc$wl))[2]*1.025,-1))))
        
        plotly_spc_baseline <- plot_ly(type = "scatter", mode = "lines") %>% layout(yaxis=plotlyp$spc_baseline$yp, xaxis=plotlyp$spc_baseline$xp,font=list(size=plotlyp$sizep), title = "Absorptionsspektren")
        
        for(i in 1:length(spc$files)){
          if(length(spc$spc_baseline[[ i ]])>0)  for(j in 1 : ncol(spc$spc_baseline[[ i ]])){
            plotly_spc_baseline <- plotly_spc_baseline %>% plotly::add_trace(
              x = as.numeric(spc$wl[[ i ]][  , j ])
              , y = as.numeric(spc$spc_baseline[[ i ]][  , j ])
              
              , type = "scatter", mode = "lines"
              
              , line = list(width = plotlyp$widthp
                            , color = if( any( tools::file_ext( directory ) != "csv")){ colp[ i ]} else { colp[[ i ]][ j ]})
              
              , name = if( any( tools::file_ext( directory ) != "csv")){
                dotspc_names( substr(spc$data[[ i ]][1, grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ]
                                     , 1 + gregexpr("\\\\", spc$data[[ i ]][1,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ])[[ 1 ]][ length( gregexpr("\\\\", spc$data[[ i ]][1,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ])[[ 1 ]] )]
                                     , nchar( spc$data[[ i ]][1,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ])))} else{ name = dotspc_names( as.character( unlist( do.call(c, readcsv$filestext[ i ])[[ 1 ]][ j ]))) }
              
              , text = paste("Datum_Uhrzeit = ", spc$data[[ i ]][1,grep("fdate|datetime", names(spc$data[[ i ]]), ignore.case = T)[ 1 ]],
                             "<br>Dateiname = ", ifelse( length( grep("^NAME$|filename", names(spc$data[[ i ]])) ) != 0
                                                         , substr(spc$data[[ i ]][1,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ]
                                                                  , 1 + gregexpr("\\\\", spc$data[[ i ]][j,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ])[[ 1 ]][ length( gregexpr("\\\\", spc$data[[ i ]][1,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ])[[ 1 ]] )]
                                                                  , nchar( spc$data[[ i ]][j,grep("^NAME$|filename", names(spc$data[[ i ]]))][ 1 ]))
                                                         , as.character(spc$data[[ i ]][ , 1]))
                             ,
                             "<br>Integrationszeit = ",spc$data[[ i ]][j,grep("It|integrationTime", names(spc$data[[ i ]]))[1]],
                             "<br>Mittelungen = ",spc$data[[ i ]][j,grep("Aver|accumulations", names(spc$data[[ i ]]))])
            )
          }
        }
        
        if( exportplot ) tryCatch(htmlwidgets::saveWidget(as_widget(plotly_spc_baseline),paste0(date.dt(), "_spc_baseline.html")),error=function(e){})
        
      }
    }
    
    # trans
    if( exists("nullvectrans")) if(length(nullvectrans)>0){
      plotlyp$spc <- list(yp = list(title = "AU",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(trans$spc))[1]*0.975,-1),round(range(unlist(trans$spc))[2]*1.025,-1)),ticks = "outside"),
                          xp = list(title = "lambda in nm",showline=T,showgrid=F,mirror=T,ticks = "outside",range=c(round(range(unlist(trans$wl))[1]*0.975,-1),round(range(unlist(trans$wl))[2]*1.025,-1))))
      
      plotly_trans <- plot_ly(type = "scatter", mode = "lines") %>% layout(yaxis=plotlyp$trans$yp, xaxis=plotlyp$trans$xp,font=list(size=plotlyp$sizep), title = "Absorptionsspektren")
      
      for(i in 1:length(trans$files)){
        if(length(trans$spc[[ i ]])>0)  for(j in 1 : ncol(trans$spc[[ i ]])){
          plotly_trans <- plotly_trans %>% plotly::add_trace(
            x = as.numeric(trans$wl[[ i ]][  , j ])
            , y = as.numeric(trans$spc[[ i ]][  , j ])
            
            , type = "scatter", mode = "lines"
            
            , line = list(width = plotlyp$widthp
                          , color = if( any( tools::file_ext( directory ) != "csv")){ colp[ i ]} else { colp[[ i ]][ j ]})
            
            , name = if( any( tools::file_ext( directory ) != "csv")){
              dotspc_names( substr(trans$data[[ i ]][1, grep("^NAME$|filename", names(trans$data[[ i ]]))][ 1 ]
                                   , 1 + gregexpr("\\\\", trans$data[[ i ]][1,grep("^NAME$|filename", names(trans$data[[ i ]]))][ 1 ])[[ 1 ]][ length( gregexpr("\\\\", trans$data[[ i ]][1,grep("^NAME$|filename", names(trans$data[[ i ]]))][ 1 ])[[ 1 ]] )]
                                   , nchar( trans$data[[ i ]][1,grep("^NAME$|filename", names(trans$data[[ i ]]))][ 1 ])))} else{ name = dotspc_names( as.character( unlist( do.call(c, readcsv$filestext[ which( unlist( readcsv$type ) == "trans")[ i ] ])[[ 1 ]][ j ]))) }
            
            , text = paste("Datum_Uhrzeit = ", trans$data[[ i ]][1,grep("fdate|datetime", names(trans$data[[ i ]]), ignore.case = T)[ 1 ]],
                           "<br>Dateiname = ", ifelse( length( grep("^NAME$|filename", names(trans$data[[ i ]])) ) != 0
                                                       , substr(trans$data[[ i ]][1,grep("^NAME$|filename", names(trans$data[[ i ]]))][ 1 ]
                                                                , 1 + gregexpr("\\\\", trans$data[[ i ]][j,grep("^NAME$|filename", names(trans$data[[ i ]]))][ 1 ])[[ 1 ]][ length( gregexpr("\\\\", trans$data[[ i ]][1,grep("^NAME$|filename", names(trans$data[[ i ]]))][ 1 ])[[ 1 ]] )]
                                                                , nchar( trans$data[[ i ]][j,grep("^NAME$|filename", names(trans$data[[ i ]]))][ 1 ]))
                                                       , as.character(trans$data[[ i ]][ , 1]))
                           ,
                           "<br>Integrationszeit = ",trans$data[[ i ]][j,grep("It|integrationTime", names(trans$data[[ i ]]))[1]],
                           "<br>Mittelungen = ",trans$data[[ i ]][j,grep("Aver|accumulations", names(trans$data[[ i ]]))])
          )
        }
      }
      
      if( exportplot ) tryCatch(htmlwidgets::saveWidget(as_widget(plotly_trans),paste0(date.dt(), "_trans.html")),error=function(e){})
      
    }
  }
  if(shinyoutput ){ returnlist <- list( if(exists("plotly_drk")) plotly_drk
                                        , if(exists("plotly_ref")) plotly_ref
                                        , if(exists("plotly_spc")) plotly_spc
                                        , if(exists("plotly_trans")) plotly_trans)
  
  returnlist <- returnlist[ unlist(lapply(returnlist, function( x ) !is.null( x ))) ]
  
  names(returnlist) <- c( if(exists("plotly_drk")) "drk"
                          ,if(exists("plotly_ref")) "ref"
                          ,if(exists("plotly_spc")) "spc"
                          ,if(exists("plotly_trans")) "trans")
  }
  if(!shinyoutput ){
    returnlist <- list(drk,ref,spc,trans)
    names(returnlist) <- c("drk","ref","spc","trans")
  }
  
  return(returnlist)
}
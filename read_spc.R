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
      setwd(directory[i])
      
      spcfiles$files[[i]] <- lapply(directory[i],function(x) paste(x,list.files(recursive=recursive,pattern=".spc",include.dirs = T),sep="//"))
      spcfiles$files[[i]] <- lapply(spcfiles$files[[i]],function(x) x[grep("spc",tools::file_ext(x))])
      
      spcfiles$fileslow[[i]] <- lapply(directory[i],function(x) enc2native(list.files(recursive=recursive,pattern=".spc",include.dirs = T)))
      spcfiles$fileslow[[i]] <- lapply(spcfiles$fileslow[[i]],function(x) x[grep("spc",tools::file_ext(x))])
    }
  
  if( all(tools::file_ext( directory) == "spc")){
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
  
  try(
    suppressWarnings( spcfiles$raw <- lapply(spcfiles$files,function(x) read.spc(x,keys.hdr2data = T, keys.log2data = T, log.txt = TRUE,log.bin = FALSE)))
  )
  
  if(length(grep("ChannelType",names(spcfiles$raw[[1]]@data)))!=0){
    spcfiles$type <- lapply(spcfiles$raw,function(x) x@data[grep("ChannelType",names(x@data))][1,1])
    
    if(length(which(spcfiles$type=="RawData"))>0)  spcfiles$type[which(spcfiles$type=="RawData")] <- lapply(spcfiles$raw,function(x) x@data[grep("ChannelName",names(x@data))][1,1])[which(spcfiles$type=="RawData")]
    
    for(i in which(spcfiles$type=="Normalized")){if(length(grep("Absorbance",names(spcfiles$raw[[i]]@data)))) spcfiles$type[i] <- "Absorbance" else spcfiles$type[i] <- "Transmission"}
    
    for(i in grep("Dark",spcfiles$type)){
      drk$files[[i]] <- spcfiles$files[i]
      drk$filestext[[i]] <- basename(spcfiles$filestext[i])
      drk$wl[[i]] <- spcfiles$raw[[i]]@wavelength
      drk$data[[i]] <- spcfiles$raw[[i]]@data
      drk$label[[i]] <- spcfiles$raw[[i]]@label
      drk$log[[i]] <- spcfiles$raw[[i]]@log
      drk$spc[[i]] <- t(spcfiles$raw[[i]]@data$spc)
    }
    
    for(i in grep("Refer",spcfiles$type)){
      ref$files[[i]] <- spcfiles$files[i]
      ref$filestext[[i]] <- basename(spcfiles$filestext[i])
      ref$wl[[i]] <- spcfiles$raw[[i]]@wavelength
      ref$data[[i]] <- spcfiles$raw[[i]]@data
      ref$label[[i]] <- spcfiles$raw[[i]]@label
      ref$log[[i]] <- spcfiles$raw[[i]]@log
      ref$spc[[i]] <- t(spcfiles$raw[[i]]@data$spc)
    }
    
    for(i in which(spcfiles$type=="Absorbance")){
      spc$files[[i]] <- spcfiles$files[i]
      spc$filestext[[i]] <- basename(spcfiles$filestext[i])
      spc$wl[[i]] <- spcfiles$raw[[i]]@wavelength
      spc$data[[i]] <- spcfiles$raw[[i]]@data
      spc$label[[i]] <- spcfiles$raw[[i]]@label
      spc$log[[i]] <- spcfiles$raw[[i]]@log
      spc$spc[[i]] <- t(spcfiles$raw[[i]]@data$spc)
      spc$spc_baseline[[i]] <- spc$spc[[i]]
    }
    
    for(i in which(spcfiles$type=="Transmission")){
      trans$files[[i]] <- spcfiles$files[i]
      trans$filestext[[i]] <- basename(spcfiles$filestext[i])
      trans$wl[[i]] <- spcfiles$raw[[i]]@wavelength
      trans$data[[i]] <- spcfiles$raw[[i]]@data
      trans$label[[i]] <- spcfiles$raw[[i]]@label
      trans$log[[i]] <- spcfiles$raw[[i]]@log
      trans$spc[[i]] <- t(spcfiles$raw[[i]]@data$spc)
    }
  } else {
    
    for(i in grep("-D",spcfiles$filestext)){
      drk$files[[i]] <- spcfiles$files[i]
      drk$filestext[[i]] <- basename(spcfiles$filestext[i])
      drk$wl[[i]] <- spcfiles$raw[[i]]@wavelength
      drk$data[[i]] <- spcfiles$raw[[i]]@data
      drk$label[[i]] <- spcfiles$raw[[i]]@label
      drk$spc[[i]] <- t(spcfiles$raw[[i]]@data$spc)
    }
    
    for(i in grep("-R",spcfiles$filestext)){
      ref$files[[i]] <- spcfiles$files[i]
      ref$filestext[[i]] <- basename(spcfiles$filestext[i])
      ref$wl[[i]] <- spcfiles$raw[[i]]@wavelength
      ref$data[[i]] <- spcfiles$raw[[i]]@data
      ref$label[[i]] <- spcfiles$raw[[i]]@label
      ref$spc[[i]] <- t(spcfiles$raw[[i]]@data$spc)
    }
    
    for(i in grep("-M",spcfiles$filestext)){
      spc$files[[i]] <- spcfiles$files[i]
      spc$filestext[[i]] <- basename(spcfiles$filestext[i])
      spc$wl[[i]] <- spcfiles$raw[[i]]@wavelength
      spc$data[[i]] <- spcfiles$raw[[i]]@data
      spc$label[[i]] <- spcfiles$raw[[i]]@label
      spc$spc[[i]] <- t(spcfiles$raw[[i]]@data$spc)
      spc$spc_baseline[[i]] <- spc$spc[[i]]
    }
    
    if(is.null(spc$spc)){
      for(i in grep("_c",substr(spcfiles$filestext, nchar(spcfiles$filestext) - 5, nchar(spcfiles$filestext)))){
        spc$files[[i]] <- spcfiles$files[i]
        spc$filestext[[i]] <- basename(spcfiles$filestext[i])
        spc$wl[[i]] <- spcfiles$raw[[i]]@wavelength
        spc$data[[i]] <- spcfiles$raw[[i]]@data
        spc$label[[i]] <- spcfiles$raw[[i]]@label
        spc$spc[[i]] <- t(spcfiles$raw[[i]]@data$spc)
        spc$spc_baseline[[i]] <- spc$spc[[i]]
      }
    }
    
  }
  
  # wl ####
  for(i in 1:length(spc$spc)) if(length(ncol(spc$spc[[i]]))>0) spc$wl[[i]] <-  replicate(ncol(spc$spc[[i]]),spc$wl[[i]])
  for(i in 1:length(trans$spc)) if(length(ncol(trans$spc[[i]]))>0) trans$wl[[i]] <-  replicate(ncol(trans$spc[[i]]),trans$wl[[i]])
  
  # nullvec ####
  nullvecref <- which(lapply(ref$wl,is.null)==F)
  nullvecdrk <- which(lapply(drk$wl,is.null)==F)
  nullvectrans <- which(lapply(trans$wl,is.null)==F)
  nullvecspc <- which(lapply(spc$wl,is.null)==F)
  
  # baseline ####
  if(!is.na(baseline)){
    for(i in nullvecspc){
      if(ncol(spc$spc_baseline[[i]])==1) spc$spc_baseline[[i]] <- spc$spc_baseline[[i]]-spc$spc_baseline[[i]][which.min(abs(spc$wl[[i]]-baseline)),]
      if(ncol(spc$spc_baseline[[i]])>1){
        for(j in 1:ncol(spc$spc_baseline[[i]])){
          spc$spc_baseline[[i]][,j] <- spc$spc_baseline[[i]][,j]-as.numeric(spc$spc_baseline[[i]][,j])[which.min(abs(spc$wl[[i]][,j]-baseline))]
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
  
  ref$wl <- ref$wl[which(unlist(lapply(ref$wl, function(x) !is.null(x)))==T)]
  ref$data <- ref$data[which(unlist(lapply(ref$data, function(x) !is.null(x)))==T)]
  ref$label <- ref$label[which(unlist(lapply(ref$label, function(x) !is.null(x)))==T)]
  ref$log <- ref$log[which(unlist(lapply(ref$log, function(x) !is.null(x)))==T)]
  ref$spc <- ref$spc[which(unlist(lapply(ref$spc, function(x) !is.null(x)))==T)]
  ref$files <- ref$files[which(unlist(lapply(ref$files, function(x) !is.null(x)))==T)]
  ref$filestext <- ref$filestext[which(unlist(lapply(ref$filestext, function(x) !is.null(x)))==T)]
  
  spc$wl <- spc$wl[which(unlist(lapply(spc$wl, function(x) !is.null(x)))==T)]
  spc$data <- spc$data[which(unlist(lapply(spc$data, function(x) !is.null(x)))==T)]
  spc$label <- spc$label[which(unlist(lapply(spc$label, function(x) !is.null(x)))==T)]
  spc$log <- spc$log[which(unlist(lapply(spc$log, function(x) !is.null(x)))==T)]
  spc$spc <- spc$spc[which(unlist(lapply(spc$spc, function(x) !is.null(x)))==T)]
  spc$spc_baseline <- spc$spc_baseline[which(unlist(lapply(spc$spc_baseline, function(x) !is.null(x)))==T)]
  spc$files <- spc$files[which(unlist(lapply(spc$files, function(x) !is.null(x)))==T)]
  spc$filestext <- spc$filestext[which(unlist(lapply(spc$filestext, function(x) !is.null(x)))==T)]
  
  trans$wl <- trans$wl[which(unlist(lapply(trans$wl, function(x) !is.null(x)))==T)]
  trans$data <- trans$data[which(unlist(lapply(trans$data, function(x) !is.null(x)))==T)]
  trans$label <- trans$label[which(unlist(lapply(trans$label, function(x) !is.null(x)))==T)]
  trans$log <- trans$log[which(unlist(lapply(trans$log, function(x) !is.null(x)))==T)]
  trans$spc <- trans$spc[which(unlist(lapply(trans$spc, function(x) !is.null(x)))==T)]
  trans$files <- trans$files[which(unlist(lapply(trans$files, function(x) !is.null(x)))==T)]
  trans$filestext <- trans$filestext[which(unlist(lapply(trans$filestext, function(x) !is.null(x)))==T)]
  
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
    if(length(nullvecref)>0){
      plotlyp$ref <- list(yp = list(title="Counts",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(ref$spc))[1]*0.975,-1),round(range(unlist(ref$spc))[2]*1.025,-1)),ticks="outside"),
                          xp = list(title = "lambda in nm",showline=T,showgrid=F,mirror=T,ticks="outside",range=c(round(range(unlist(ref$wl))[1]*0.975,-1),round(range(unlist(ref$wl))[2]*1.025,-1))))
      
      plotly_ref<-plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlyp$ref$yp, xaxis=plotlyp$ref$xp,font=list(size=plotlyp$sizep), title = "Referenzspektren")
      
      for(i in 1:length(ref$files)){
        plotly_ref <- plotly_ref%>% plotly::add_trace(
          x=as.numeric(ref$wl[[i]]),y=as.numeric(ref$spc[[i]]),type="scatter", mode="lines",line=list(width=plotlyp$widthp,color=colp[i]),
          name= ref$filestextlegend[ i ],
          text=paste("Time",ref$data[[i]][1,grep("fdate",names(ref$data[[i]]))],
                     "<br>File",basename(unlist(ref$filestext[[i]])),
                     "<br>Iteration",ref$data[[i]][1,grep("It",names(ref$data[[i]]))[1]],
                     "<br>Average",ref$data[[i]][1,grep("Aver",names(ref$data[[i]]))])
        )}
      if( exportplot ) tryCatch(htmlwidgets::saveWidget(as_widget(plotly_ref), paste0(date.dt(), "_ref.html")),error=function(e){})
    }
    
    # drk
    if(length(nullvecdrk)>0){
      plotlyp$drk <- list(yp = list(title="Counts",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(drk$spc))[1]*0.975,-1),round(range(unlist(drk$spc))[2]*1.025,-1)),ticks="outside"),
                          xp = list(title = "lambda in nm",showline=T,showgrid=F,mirror=T,ticks="outside",range=c(round(range(unlist(drk$wl))[1]*0.975,-1),round(range(unlist(drk$wl))[2]*1.025,-1))))
      
      plotly_drk<-plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlyp$drk$yp, xaxis=plotlyp$drk$xp,font=list(size=plotlyp$sizep), title = "Dunkelwertspektren")
      for(i in 1:length(drk$files)){
        plotly_drk <- plotly_drk%>% plotly::add_trace(
          x=as.numeric(drk$wl[[i]]),y=as.numeric(drk$spc[[i]]),type="scatter", mode="lines",line=list(width=plotlyp$widthp,color=colp[i]),
          name= drk$filestextlegend[ i ],
          text=paste("Time",drk$data[[i]][1,grep("fdate",names(drk$data[[i]]))],
                     "<br>File",basename(unlist(drk$files[[i]])),
                     "<br>Iteration",drk$data[[i]][1,grep("It",names(drk$data[[i]]))[1]],
                     "<br>Average",drk$data[[i]][1,grep("Aver",names(drk$data[[i]]))])
        )}
      if( exportplot ) tryCatch(htmlwidgets::saveWidget(as_widget(plotly_drk),paste0(date.dt(), "_drk.html")),error=function(e){})
    }
    
    # spc
    if(length(nullvecspc)>0){
      plotlyp$spc <- list(yp = list(title="AU",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(spc$spc))[1]*0.975,-1),round(range(unlist(spc$spc))[2]*1.025,-1)),ticks="outside"),
                          xp = list(title = "lambda in nm",showline=T,showgrid=F,mirror=T,ticks="outside",range=c(round(range(unlist(spc$wl))[1]*0.975,-1),round(range(unlist(spc$wl))[2]*1.025,-1))))
      
      plotly_spc <- plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlyp$spc$yp, xaxis=plotlyp$spc$xp,font=list(size=plotlyp$sizep), title = "Absorptionsspektren")
      
      for(i in 1:length(spc$files)){
        if(length(spc$spc[[i]])>0)  for(j in 1:ncol(spc$spc[[i]])){
          plotly_spc <- plotly_spc %>% plotly::add_trace(
            x=as.numeric(spc$wl[[i]][,j]),y=as.numeric(spc$spc[[i]][,j]),type="scatter", mode="lines",line=list(width=plotlyp$widthp,color=colp[i]),
            name= spc$filextextlegend[ i ],
            text=paste("Time",spc$data[[i]][1,grep("fdate",names(spc$data[[i]]))],
                       "<br>File",basename(unlist(spc$filestext[ i ] )),
                       "<br>Iteration",spc$data[[i]][1,grep("It",names(spc$data[[i]]))[1]],
                       "<br>Average",spc$data[[i]][1,grep("Aver",names(spc$data[[i]]))])
          )
        }
      }
      if( exportplot ) tryCatch(htmlwidgets::saveWidget(as_widget(plotly_spc),paste0(date.dt(), "_spc.html")),error=function(e){})
      
      # spc baseline
      if(!is.na(baseline)){
        plotlyp$spc_baseline <- list(yp = list(title="AU",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(spc$spc_baseline))[1]*0.975,-1),round(range(unlist(spc_baseline))[2]*1.025,-1)),ticks="outside"),
                                     xp = list(title = "lambda in nm",showline=T,showgrid=F,mirror=T,ticks="outside",range=c(round(range(unlist(spc$wl))[1]*0.975,-1),round(range(unlist(spc$wl))[2]*1.025,-1))))
        
        plotly_spc<-plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlyp$spc_baseline$yp, xaxis=plotlyp$spc_baseline$xp,font=list(size=plotlyp$sizep), title = "Absorptionsspektren")
        
        for(i in 1:length(spc$files)){
          if(length(spc$spc_baseline[[i]])>0)  for(j in 1:ncol(spc$spc_baseline[[i]])){
            plotly_spc <- plotly_spc%>% plotly::add_trace(
              x=as.numeric(spc$wl[[i]][,j]),y=as.numeric(spc$spc_baseline[[i]][,j]),type="scatter", mode="lines",line=list(width=plotlyp$widthp,color=colp[i]),
              name= spc$filextextlegend[ i ],
              text=paste("Time",spc$data[[i]][1,grep("fdate",names(spc$data[[i]]))],
                         "<br>File",basename(unlist(spc$filestext[ i ] )),
                         "<br>Iteration",spc$data[[i]][1,grep("It",names(spc$data[[i]]))[1]],
                         "<br>Average",spc$data[[i]][1,grep("Aver",names(spc$data[[i]]))])
            )
          }
        }
        if( exportplot ) tryCatch(htmlwidgets::saveWidget(as_widget(plotly_spc),paste0(date.dt(), "_spc_bl.html")),error=function(e){})
      }
    }
    
    # trans
    if(length(nullvectrans)>0){
      plotlyp$trans <- list(yp = list(title="% Transmission",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(trans$spc))[1]*0.975,-1),round(range(unlist(trans$spc))[2]*1.025,-1)),ticks="outside"),
                            xp = list(title = "lambda in nm",showline=T,showgrid=F,mirror=T,ticks="outside",range=c(round(range(unlist(trans$wl))[1]*0.975,-1),round(range(unlist(trans$wl))[2]*1.025,-1))))
      plotly_trans<-plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlyp$trans$yp, xaxis=plotlyp$trans$xp,font=list(size=plotlyp$sizep), title = "Transmission")
      for(i in 1:length(trans$files)){
        if(length(trans$spc[[i]])>0)  for(j in 1:ncol(trans$spc[[i]])){
          plotly_trans <- plotly_trans%>% plotly::add_trace(
            x=as.numeric(trans$wl[[i]][,j]),y=as.numeric(trans$spc[[i]][,j]),type="scatter", mode="lines",line=list(width=plotlyp$widthp,color=colp[i]),
            name= trans$filestextlegend[ i ],
            text=paste("Time",trans$data[[i]][1,grep("fdate",names(trans$data[[i]]))],
                       "<br>File",basename(unlist(trans$filestext[ i ] )),
                       "<br>Iteration",trans$data[[i]][1,grep("It",names(trans$data[[i]]))],
                       "<br>Average",trans$data[[i]][1,grep("Aver",names(trans$data[[i]]))])
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
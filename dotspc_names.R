dotspc_names <- function( spcnames ){
  
  # spcnames <- readClipboard()
  dateID <- rep(NA, length( spcnames ))
  fiberID <- rep(NA, length( spcnames ))
  mcID <- rep(NA, length( spcnames ))
  spcnames_legend <- rep(NA, length( spcnames ))
  
  # Date pattern ####
  date_pattern <- "\\d{2}\\d{2}\\d{2}"
  find_date_pattern <- unlist(lapply( regexec(date_pattern, spcnames), function( x ) x[[ 1 ]]))
  dateID[ which( find_date_pattern > 0) ] <- substr( spcnames[ which( find_date_pattern > 0) ]
                                                     , find_date_pattern[ which( find_date_pattern > 0) ]
                                                     , find_date_pattern[ which( find_date_pattern > 0) ] + sum(as.numeric(strsplit(as.character(as.numeric(gsub("\\D", "", date_pattern))), "")[[1]]))  - 1)
  
  # Date pattern
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  find_date_pattern <- unlist(lapply( regexec(date_pattern, spcnames), function( x ) x[[ 1 ]]))
  dateID[ which( find_date_pattern > 0) ] <- substr( spcnames[ which( find_date_pattern > 0) ]
                                                     , find_date_pattern[ which( find_date_pattern > 0) ]
                                                     , find_date_pattern[ which( find_date_pattern > 0) ] + sum(as.numeric(strsplit(as.character(as.numeric(gsub("\\D", "", date_pattern))), "")[[1]]))  - 1 + 4)
  
  dateID[ which( find_date_pattern > 0) ] <- substr(gsub("-", "", as.Date(dateID[ which( find_date_pattern > 0) ])), 3, 8)
  
  # Date pattern
  date_pattern <- "\\d{4}\\d{2}\\d{2}"
  find_date_pattern <- unlist(lapply( regexec(date_pattern, spcnames), function( x ) x[[ 1 ]]))
  dateID[ which( find_date_pattern > 0) ] <- substr( spcnames[ which( find_date_pattern > 0) ]
                                                     , find_date_pattern[ which( find_date_pattern > 0) ]
                                                     , find_date_pattern[ which( find_date_pattern > 0) ] + sum(as.numeric(strsplit(as.character(as.numeric(gsub("\\D", "", date_pattern))), "")[[1]]))  - 1)
  
  dateID[ which( find_date_pattern > 0) ] <- substr(dateID[ which( find_date_pattern > 0) ], 3, 8)
  
  
  # Date pattern 
  date_pattern <- "\\d{2}-\\d{2}-\\d{2}"
  find_date_pattern <- unlist(lapply( regexec(date_pattern, spcnames), function( x ) x[[ 1 ]]))
  dateID[ which( find_date_pattern > 0) ] <- substr( spcnames[ which( find_date_pattern > 0) ]
                                                     , find_date_pattern[ which( find_date_pattern > 0) ]
                                                     , find_date_pattern[ which( find_date_pattern > 0) ] + sum(as.numeric(strsplit(as.character(as.numeric(gsub("\\D", "", date_pattern))), "")[[1]]))  - 1 + 2)
  
  dateID[ which( find_date_pattern > 0) ] <- substr(gsub("-", "", dateID[ which( find_date_pattern > 0) ]), 1, 6)
  
  # Date pattern 
  date_pattern <- "\\d{2}\\d{2}\\d{2}"
  find_date_pattern <- unlist(lapply( regexec(date_pattern, substr(spcnames, 1, 6)), function( x ) x[[ 1 ]]))
  dateID[ which( find_date_pattern > 0) ] <- substr( spcnames[ which( find_date_pattern > 0) ]
                                                     , 1
                                                     , sum(as.numeric(strsplit(as.character(as.numeric(gsub("\\D", "", date_pattern))), "")[[1]])))
  
  dateID[ which( find_date_pattern > 0) ] <- substr(gsub("-", "", dateID[ which( find_date_pattern > 0) ]), 1, 6)
  
  # Fiber ####
  fiber_pattern <- c("Ly\\d+_Ly\\d+"
                     , "TZ0\\d+_TZ0\\d+"
                     , "TZO\\d+_TZO\\d+"
                     , "Ly\\d+_TZO\\d+"
                     , "TZO\\d+_Ly\\d+"
                     , "Ly\\d+_TZ0\\d+"
                     , "TZ0\\d+_Ly\\d+")
  
  for(i in 1 : length( fiber_pattern)){
    find_fiber_pattern <- unlist( lapply(spcnames, function( x ) regexpr(fiber_pattern[ i ], x)))
    fiberID[ which( find_fiber_pattern > 0) ] <- unlist( lapply(spcnames, function( x ) regmatches(x, regexpr(fiber_pattern[ i ], x))))
  }
  
  # Measurment Cell ####
  mc_pattern <- "\\DT\\d{4}"
  find_mc_pattern <- unlist( lapply(spcnames, function( x ) regexpr(mc_pattern, x)))
  mcID[ which( find_mc_pattern > 0) ] <- unlist( lapply(spcnames, function( x ) regmatches(x, regexpr(mc_pattern, x))))
  
  # spcnames_legend for standard sample ID
  spcnames_legend[ which( !is.na( mcID ) & !is.na( fiberID )) ] <- spcnames[ which( !is.na( mcID ) & !is.na( fiberID )) ]
  
  for(i in 1 : length( fiber_pattern)){
    spcnames_legend[ which( !is.na( mcID ) & !is.na( fiberID ))] <- gsub(fiber_pattern[ i ], "", spcnames_legend[ which( !is.na( mcID ) & !is.na( fiberID )) ])
    spcnames_legend[ which( !is.na( mcID ) & !is.na( fiberID ))] <- gsub("__", "_", spcnames_legend[ which( !is.na( mcID ) & !is.na( fiberID ))])
  }
  
  # LG2 Prozess ####
  LG2_pattern <- "\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}-"
  find_LG2_pattern <- unlist( lapply(spcnames, function( x ) regexpr(LG2_pattern, x)))
  spcnames_legend[ which( find_LG2_pattern > 0) ] <- spcnames[ which( find_LG2_pattern > 0) ]
  
  spcnames_legend[ which( find_LG2_pattern > 0) ] <- substr(spcnames_legend[ which( find_LG2_pattern > 0) ]
                                                            , 1
                                                            , 19)
  
  # Time Measurement
  timemeasurement_pattern <- c("spc_", "dark", "ref_", "_drk", "_int", "_ref")
  for(i in 1 : length( timemeasurement_pattern)){
    find_timemeasurement_pattern <- which( substr(spcnames, 1, 4) == timemeasurement_pattern[ i ])
    spcnames_legend[ find_timemeasurement_pattern ] <- spcnames[ find_timemeasurement_pattern ]
  }
  
  # strange labeling
  if( length( which( !is.na(mcID) & is.na(spcnames_legend)) ) > 0 )spcnames_legend[ which( !is.na(mcID) & is.na(spcnames_legend)) ] <- spcnames[ which( !is.na(mcID) & is.na(spcnames_legend)) ]
  if( length( which( !is.na(fiberID) & is.na(spcnames_legend)) ) > 0 )spcnames_legend[ which( is.na(mcID) & !is.na(spcnames_legend)) ] <- spcnames[ which( !is.na(fiberID) & is.na(spcnames_legend)) ]
  spcnames_legend[ which( is.na( spcnames_legend )) ] <- spcnames[ which( is.na( spcnames_legend )) ]
  
  # remove Measurent Type
  type_pattern <- c("r01_00", "c01_00", "d01_00", "\\.spc")
  for(i in 1 : length( type_pattern)){
    find_type_pattern <- unlist( lapply(spcnames_legend, function( x ) regexpr(type_pattern[ i ], x)))
    spcnames_legend[ which( find_type_pattern > 0) ] <- substr(spcnames_legend[ which( find_type_pattern > 0)]
                                                               , 1
                                                               , find_type_pattern[ which( find_type_pattern > 0) ] - 2)
  }
  
  return( spcnames_legend)
}
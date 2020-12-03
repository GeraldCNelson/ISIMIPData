library("terra")
terraOptions(memfrac = 1,  progress = 10, tempdir =  "data/ISIMIP", verbose = TRUE) # need to use a relative path
locOfFiles <- "data/bigFiles/"
woptList <- list(gdal=c("COMPRESS=LZW"))

yearSpan <- "2041_2060"
k = "ssp585"
i <- "IPSL-CM6A-LR"
yearRange <- 19
l = 2041
modelName.lower <- tolower(i)

fileName_tasmax <- paste0(locOfFiles,  modelName.lower, "_", k, "_tasmax_global_daily_", yearSpan, ".tif")
tmax <- rast(fileName_tasmax)
fileName_rh <- paste0(locOfFiles, modelName.lower, "_", k, "_hurs_global_daily_", yearSpan, ".tif")
rh <- rast(fileName_rh)

#comb <- sds(rh, tmax)

combined <- c(rh, tmax)
nl <- nlyr(combined)
vlend <- nl/2
v2start <- 1 + vlend

funTHI <- function(cellVector) {
  rh <- cellVector[1:vlend]
  tmax <- cellVector[v2start:nl]
  if (is.nan(cellVector[1])) { return(rep(NA, vlend)) }
  tmax[tmax < 20] <- 0
  #  print(paste("length tmax: ", (length(tmax))))
  thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) 
  thi <- round(thi, 2)
}

system.time(test <- app(combined, funTHI))

# time to do 1 20 year file
# user   system  elapsed 
# 567.024  507.328 1348.937 

system.time(writeRaster(test, "testout.tif",  overwrite =TRUE, wopt = woptList))
# user  system elapsed 
# 75.265  20.963 102.106 
# testout.tif file size is 2 gb

# now with rounding
funTHI <- function(cellVector) {
  rh <- cellVector[1:vlend]
  tmax <- cellVector[v2start:nl]
  if (is.nan(cellVector[1])) { return(rep(NA, vlend)) }
  tmax[tmax < 20] <- 0
  #  print(paste("length tmax: ", (length(tmax))))
  thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) 
  thi <- round(thi, 2)
}

system.time(test <- app(combined, funTHI))
# user   system  elapsed 
# 622.572  507.822 1377.201 

system.time(writeRaster(test, "testout2.tif",  overwrite =TRUE, wopt = woptList))
# user  system elapsed 
# 68.764  12.482  82.379 

# now see how long it takes to do one year at a time
  gc()
  startDate <- paste0(l, "-01-01"); endDate <- paste0(l, "-12-31")
  indicesYear <- seq(as.Date(startDate), as.Date(endDate), 1)
  indicesCharYear <- paste0("X", as.character(indicesYear))
  
  print(system.time(tmax_yr <- subset(tmax, indicesCharYear)))
  print(system.time(rh_yr <- subset(rh, indicesCharYear)))
  
  combined_yr <- c(rh_yr, tmax_yr)
  nl <- nlyr(combined_yr)
  vlend <- nl/2
  v2start <- 1 + vlend
  
  system.time(test <- app(combined_yr, funTHI))
  # user  system elapsed 
  # 67.149   7.394  92.803 
  system.time(writeRaster(test, "testout3.tif",  overwrite =TRUE, wopt = woptList))
  # user  system elapsed 
  # 2.823   0.389   3.275 
  
  
  # now with rounding
  funTHI <- function(cellVector) {
    rh <- cellVector[1:vlend]
    tmax <- cellVector[v2start:nl]
    if (is.nan(cellVector[1])) { return(rep(NA, vlend)) }
    #  print(paste("length tmax: ", (length(tmax))))
    thi <- (1.8 * tmax + 32.0) - ((0.55 - 0.0055 * rh) * (1.8 * tmax - 26.8)) 
    thi[tmax < 20] <- 0
    thi <- round(thi, 2)
  }
  
  system.time(test <- app(combined, funTHI))
  comb <- sds(rh, tmax)
  system.time(test <- lapp(combined, funTHI, filename = "testout4.tif", overwrite =TRUE, wopt = woptList))
  
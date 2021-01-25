library(terra)

fileList <- list.files("/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year", full.names = TRUE)
fileList <- fileList[grepl("mnthMn_10Yr", fileList, fixed = TRUE)]

climateVars <- c("tasmax", "tasmin",  "tas_", "pr", "hurs", "huss", "ps", "rsds", "rlds", "sfcWind") #"tas",

varsToKeep <- c("tasmin", "tasmax", "sfcWind") 
varsToDelete <- climateVars[!climateVars %in% varsToKeep]
for (i in varsToDelete) {
  fileList <- fileList[!grepl(i, fileList, fixed = TRUE)]
}

# yearsToRemove <- c("1991", "2001", "2041", "2051", "2081", "2091")
# 
# for (i in yearsToRemove) {
#   fileList <- fileList[!grepl(i, fileList, fixed = TRUE)]
# }

for (i in fileList) {
  toRemove <- "/Volumes/PassportMac/ISIMIP/cmip6/climate3b/monthlyMn_10year/"
  newSuffix <- "/Users/gcn/Box/ISIMIP/"
  j <- gsub(toRemove, newSuffix, i)
file.copy(from = i, to = j)
}

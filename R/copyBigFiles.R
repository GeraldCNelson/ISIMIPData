# collect 20 year files in one place
source <- "/Volumes/ExtremeSSD2/climate_land_only/unitsCorrected/"
destination <- "/Volumes/PassportMac/bigFiles/"
bigfilesIn <- list.files(source, full.names = TRUE, recursive = TRUE)

setdiff(list.files(source, recursive = TRUE), list.dirs(source, recursive = TRUE, full.names = FALSE))

yearsList <- c("1991_2010", "2041_2060", "2081_2100")
bigFilesDest <- list.files(destination)

# get list of files in source that include the yearsList string in their names
keepList <- character()
for (i in yearsList) {
  temp <- bigfilesIn[grepl(i, bigfilesIn, fixed = TRUE)]
  keepList <- c(keepList, temp)
}

for (i in keepList) {
  print(paste0("copying ", i))
file.copy(i, destination, overwrite = FALSE)
}

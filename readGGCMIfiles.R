# read in the crop calendars from the ggcmi data
ggcmiCropCalLoc <- "/Volumes/PassportMac/ggcmiCropCalendars/"
filesInDir <- list.files(ggcmiCropCalLoc, full.names = TRUE, recursive = TRUE)

filestoKeep <- filesInDir[grepl("v1.01", filesInDir, fixed = TRUE)]

for (i in 1:length(filestoKeep)) {
  rin <- rast(filestoKeep[i])
bar_ir_ggcmi_crop_calendar_phase3_v1.01.nc4
}
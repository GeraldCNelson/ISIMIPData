#  creates the data tables majorCropValues_main, majorCropValues_lo and majorCropValues_hi
library(readxl)
library(data.table)
supp_materials_chill_portions <- as.data.table(read_excel("data-raw/crops/perennials/supp_materials_2020_04_07.xlsx")) #, col_types = c("text", "text", "numeric", "text", "text")))
setnames(supp_materials_chill_portions, new = c("cropName", "cultivar", "chill_requirement", "comment", "chill_threshold", "low_temp_threshold", "chill_hours", "summer_heat_threshold", "reference_chill_portions", "reference_other_information", "other_comments"))
supp_materials_chill_portions[, cropName := tolower(cropName)]

# get rid of blueberries row for now
supp_materials_chill_portions <- supp_materials_chill_portions[!cropName == "blueberries"]
test <- data.table::copy(supp_materials_chill_portions)
test <- test[, CR_cultivar_mean := round(mean(chill_requirement), 1), by = "cultivar"][, CR_cultivar_min := min(chill_requirement, na.rm = TRUE), by = "cultivar"][, CR_cultivar_max := max(chill_requirement, na.rm = TRUE), by = "cultivar"]
test <- test[, CR_crop_mean := round(mean(chill_requirement, na.rm = TRUE), 1), by = "cropName"][, CR_crop_min := min(chill_requirement, na.rm = TRUE), by = "cropName"][, CR_crop_max := max(chill_requirement, na.rm = TRUE), by = "cropName"]
test[CR_crop_max =="Inf" | CR_crop_max =="-Inf", CR_crop_max := NA]
test[CR_crop_min =="Inf" | CR_crop_min =="-Inf", CR_crop_min := NA]

# remove extraneous columns
test [, c("chill_requirement", "comment", "reference_chill_portions", "reference_other_information", "other_comments") := NULL]
test <- unique(test)
dominantVarieties <- c("Golden Delicious", "Nonpareil", "Lapins", "Picual", "Northern highbush", "Chardonnay")
majorCropValues_main <- data.table::copy(test)
majorCropValues_main <- majorCropValues_main[cultivar %in% dominantVarieties,]
majorCropValues_lo <- data.table::copy(majorCropValues_main)
majorCropValues_lo[, chill_hours := CR_cultivar_min]
majorCropValues_hi <- data.table::copy(majorCropValues_main)
majorCropValues_hi[, chill_hours := CR_cultivar_max]
majorCropValues_main[, chill_hours := CR_cultivar_mean]

majorCropValues_hi[, cropName := lapply(.SD, paste0, "_hi"), .SDcols = "cropName"]
majorCropValues_lo[, cropName := lapply(.SD, paste0, "_lo"), .SDcols = "cropName"]
majorCropValues_main[, cropName := lapply(.SD, paste0, "_main"), .SDcols = "cropName"]

cropList_hi <- majorCropValues_hi$cropName
cropList_lo <- majorCropValues_lo$cropName

for (i in cropList_hi[!cropList_hi %in% "winegrape_hi"]) { # remove winegrape because we only have data for one variety
  varValue_max <- majorCropValues_hi[cropName == i, CR_crop_max]
  varCultivar_max <- supp_materials_chill_portions[chill_requirement == varValue_max & cropName == gsub("_hi", "", i), cultivar]
  print(paste("i: ", i, ", varCultivar_max: ", varCultivar_max, ", varValue_max: ", varValue_max))
  majorCropValues_hi[cropName == i, cultivar:=  varCultivar_max]
  majorCropValues_hi[cropName == i, CR_cultivar_mean:=  varValue_max]
  print(majorCropValues_hi)
}

for (i in cropList_lo[!cropList_lo %in% "cropList_lo "]) {
  varValue_min <- majorCropValues_lo[cropName == i, CR_crop_min]
  varCultivar_min <- supp_materials_chill_portions[chill_requirement == varValue_min & cropName == gsub("_lo", "", i), cultivar]
  print(paste("i: ", i, ", varCultivar_min: ", varCultivar_min, ", varValue_min: ", varValue_min))
  majorCropValues_lo[cropName == i, cultivar:=  varCultivar_min]
  majorCropValues_lo[cropName == i, CR_cultivar_mean:=  varValue_min]
 }

deleteListCol <- c("CR_cultivar_min", "CR_cultivar_max", "CR_crop_mean", "CR_crop_min", "CR_crop_max")
majorCropValues_main[, (deleteListCol) := NULL]
majorCropValues_lo[, (deleteListCol) := NULL]
majorCropValues_hi[, (deleteListCol) := NULL]


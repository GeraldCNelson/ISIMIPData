# extracting year which temperature is reached from various ESMs.
library(yaml)
library(data.table)

test1995 <- read_yaml("references/mathause-cmip_warming_levels-c8c34ac/warming_levels/cmip6/cmip6_warming_levels_one_ens_1995_2014_grid.yml")
test1850 <- read_yaml("references/mathause-cmip_warming_levels-c8c34ac/warming_levels/cmip6/cmip6_warming_levels_one_ens_1850_1900_grid.yml")
test1850_dt <- as.data.table((test1850))
newColNames <- c("esm", "ensemble", "ssp", "grid", "startYear", "endYear")
# for (i in c(10, 15, 20, 30, 40, 61)) {
#   colIn <- paste0("warming_level_", i)
#   wl <- as.data.table(get(colIn))
#   wl <- test1850_dt[, list(warming_level_10)]
#   wl[, col0 := newColNames]
# }
warming_level_10 <- as.data.table(test1850_dt$warming_level_10)
warming_level_15 <- as.data.table(test1850_dt$warming_level_15)
warming_level_20 <- as.data.table(test1850_dt$warming_level_20)
warming_level_30 <- as.data.table(test1850_dt$warming_level_30)
warming_level_40 <- as.data.table(test1850_dt$warming_level_40)
warming_level_061 <- as.data.table(test1850_dt$warming_level_061)

wladjust <- function(wl, wlevel) {
  wl[, col0 := c("esm", "ensemble", "ssp", "grid", "startYear", "endYear")]
  transOut <- wl[, data.table(t(.SD), keep.rownames=FALSE), .SDcols=-"col0"]
  setnames(transOut, old = names(transOut), new = newColNames)
  transOut <- transOut[, lapply(.SD, as.character), .SDcols = (newColNames)]
  transOut[, warmingLevel := wlevel]
  return(transOut)
}
  
warming_levels <- wladjust(warming_level_10, 1.0)
warming_levels <- rbind(warming_levels,  wladjust(warming_level_15, 1.5))
warming_levels <- rbind(warming_levels,  wladjust(warming_level_20, 2.0))
warming_levels <- rbind(warming_levels,  wladjust(warming_level_30, 3.0))
warming_levels <- rbind(warming_levels,  wladjust(warming_level_40, 4.0))
warming_levels <- rbind(warming_levels,  wladjust(warming_level_061, 0.6))
warming_levels <- unique(warming_levels)

write.csv(warming_levels, "data-raw/warmingResultsCMIP6.csv", row.names = FALSE)


# get cmip5 data
# extracting year which temperature is reached from various ESMs.

test1995 <- read_yaml("references/mathause-cmip_warming_levels-c8c34ac/warming_levels/cmip5/cmip5_warming_levels_one_ens_1995_2014.yml")
test1850 <- read_yaml("references/mathause-cmip_warming_levels-c8c34ac/warming_levels/cmip5/cmip5_warming_levels_one_ens_1850_1900.yml")
test1850_dt <- as.data.table(test1850)
newColNames <- c("esm", "ensemble", "ssp", "startYear", "endYear")
# for (i in c(10, 15, 20, 30, 40, 61)) {
#   colIn <- paste0("warming_level_", i)
#   wl <- as.data.table(get(colIn))
#   wl <- test1850_dt[, list(warming_level_10)]
#   wl[, col0 := newColNames]
# }
warming_level_10 <- as.data.table(test1850_dt$warming_level_10)
warming_level_15 <- as.data.table(test1850_dt$warming_level_15)
warming_level_20 <- as.data.table(test1850_dt$warming_level_20)
warming_level_30 <- as.data.table(test1850_dt$warming_level_30)
warming_level_40 <- as.data.table(test1850_dt$warming_level_40)
warming_level_061 <- as.data.table(test1850_dt$warming_level_061)

wladjust <- function(wl, wlevel) {
  wl[, col0 := newColNames]
  transOut <- wl[, data.table(t(.SD), keep.rownames=FALSE), .SDcols=-"col0"]
  setnames(transOut, old = names(transOut), new = newColNames)
  transOut <- transOut[, lapply(.SD, as.character), .SDcols = (newColNames)]
  transOut[, warmingLevel := wlevel]
  return(transOut)
}

warming_levels <- wladjust(warming_level_10, 1.0)
warming_levels <- rbind(warming_levels,  wladjust(warming_level_15, 1.5))
warming_levels <- rbind(warming_levels,  wladjust(warming_level_20, 2.0))
warming_levels <- rbind(warming_levels,  wladjust(warming_level_30, 3.0))
warming_levels <- rbind(warming_levels,  wladjust(warming_level_40, 4.0))
warming_levels <- rbind(warming_levels,  wladjust(warming_level_061, 0.6))
warming_levels <- unique(warming_levels)

write.csv(warming_levels, "data-raw/warmingResultsCMIP5.csv", row.names = FALSE)

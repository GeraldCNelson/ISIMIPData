#' Check whether a year is a leap year.
#' 
#' Check whether a year is a leap year.
#' 
#' @param year to be checked.
#' 
#' @return logical, TRUE/FALSE.
#' 
#' @author Sven Kotlarski (20.12.2016).


is_leapyearR <- function(dates){
  year <- as.numeric(format(as.Date(dates, origin = "1970-01-01"), format = "%Y"))
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}
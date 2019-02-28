#' A Date/Month Time Difference Function
#'
#' This function helps you determine the number of months between two columns containing date values.
#' @param late_date enter data frame column containing the later dates (e.g., second observation in a time-series data set).
#' @param first_date enter the earlier date column.
#' @param format format the entered date columns should be read as.
#' @param to enter the format in which you would like the time difference output (e.g., days, weeks, months, years)
#' @keywords lcmm
#' @export
#' @examples
#' m.since()

m.since <- function(late_date, first_date, format='%d/%m/%Y', to='months') {
  # first get format of output (using 'to')
  if(to == 'days') {
    divisor <- 1
  } else if(to == 'weeks') {
    divisor <- 7
  } else if(to == 'months') {
    divisor <- 30
  } else if(to == 'years') {
    divisor <- 365
  }
  # convert date_columns to as.Date(), just to be sure
  late_date <- as.Date(late_date, format = format)
  first_date <- as.Date(first_date, format = format)
  # late_late - early_date
  month_diff <- round((as.numeric(dput(as.Date(late_date, format='%d/%m/%Y'))) - 
                         as.numeric(dput(as.Date(first_date, format='%d/%m/%Y')))) / divisor)
  # end
  return(month_diff)
}

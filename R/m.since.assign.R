#' A Column Assigning Date to Month Function
#'
#' This function will convert date columns to month-since columns (months since first date in time series) and assigns them to a new data frame.
#' @param data enter data set with date columns.
#' @param date_list enter date column names (str; using e.g., c("date1", "date2" ...) notation)
#' @param format format the entered date columns should be read as.
#' @param to enter the format in which you would like the time difference output (e.g., days, weeks, months, years)
#' @keywords lcmm
#' @export
#' @examples
#' m.since.assign()

m.since.assign <- function(data, date_list, to='months') {
  # date_list should be a list of date columns names in data
  my_bind <- 0
  for(i in 2:length(date_list)) {
    month_values <- m.since(data[,which(colnames(data) == date_list[i])], data[,which(colnames(data) == date_list[1])],
                            to=to)
    my_bind <- cbind(my_bind, month_values)
  }
  # naming columns
  names_list=paste0('m',1:100)
  colnames(my_bind) <- names_list[1:dim(my_bind)[2]]
  # attach to new df
  new_df <- cbind(data, my_bind)
  # end
  return(new_df)
}

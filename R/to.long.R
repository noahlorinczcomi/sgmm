#' A Wide to Long Data Set Function
#'
#' This function helps you convert a wide time-series data set (with one row for each patient and multiple columns for each different time points), to a long time-series data set (with t rows for each patient where t=number of time poitns).
#' @param data enter data set containing month values.
#' @param id_column enter the column containing patient IDs (str)
#' @param time_columns enter the time-varying outcome variable columns
#' @param date_columns enter the time-indicating month-formatted columns (str)
#' @keywords lcmm
#' @export
#' @examples
#' to.long()

to.long <- function(data, id_column, time_columns, date_columns) {
  # reformat id col to factor
  factored_ids <- as.factor(as.character(data[, which(colnames(data) == toString(id_column))]))
  # these are the IDs in order
  ordered_id_levels <- as.numeric(as.character(factored_ids))
  # we don't want to use the actual date columns, but instead the month columns
  # corresponding to the date columns. that is what is happening here:
  names_list <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10",
                  "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20",
                  "m21", "m22", "m23", "m24", "m25", "m26", "m27", "m28", "m29", "m30",
                  "m31", "m32", "m33", "m34", "m35", "m36", "m37", "m38", "m39", "m40",
                  "m41", "m42", "m43", "m44", "m45", "m46", "m47", "m48", "m49", "m50")
  month_columns <- names_list[1:length(date_columns)]
  # new_empty df
  new_df <- data.frame()
  # looping
  # for each id...
  for(i in 1:length(ordered_id_levels)) {
    # for each time point...
    for(col in 1:length(time_columns)) {
      # stack times
      times <- t(cbind(data[i, time_columns[col]]))
      # stack dates
      months <- t(cbind(data[i, month_columns[col]]))
      # it is important to use this object, and not to recall the user-inserted data again
      ids <- ordered_id_levels[i]
      mybind <- as.data.frame(cbind(ids, times, months))
      # append a df
      new_df <- bind_rows(new_df, mybind)
    }
  }
  # renaming cols
  colnames(new_df) <- c('id', 'outcomes', 'months')
  print("colnames are (i) 'id', (ii) 'outcomes', and (iii) 'months'")
  # end
  return(new_df)
}

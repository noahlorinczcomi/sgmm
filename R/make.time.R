#' A Time-Making Function
#'
#' This function helps you create intermediary time-outcome values for known but unobserved values (e.g., dosages).
#' @param new_data enter data for which each ID has the same beginning and end time-point (see 'stop.time').
#' @param id_name enter the name (str) of the column in your data containing subject's IDs.
#' @param time_cutoff enter the number of days, months, years that define each subject's final observation.
#' @keywords lcmm
#' @export
#' @examples
#' make.time()

make.time <- function(new_data, id_name, time_cutoff) {
  
  # prelims
  id_list <- levels(as.factor(new_data[,which(colnames(new_data) == toString(id_name))]))
  new_new_df <- data.frame()
  
  for(i in id_list) {
    
    # isolate id_df
    id_df <- new_data[new_data$id == i, ]
    
    # make list of all month values that should be present
    complete_month_list <- 0:time_cutoff
    
    # make list of present month values
    present_month_list <- id_df$months
    
    # see which copmlete list month values are not present
    missing_months <- setdiff(complete_month_list, present_month_list)
    
    # make rows in the new df containing ID, NA (for outcome), and missing month
    for(month in missing_months) {
      
      # create a row with that value
      row_to_add <- c(i, 'NA', month)
      row_to_add <- as.data.frame(t(c(row_to_add)))
      colnames(row_to_add) <- c("id", "outcomes", "months")
      # going to get warnings bc we are trying to make NAs numeric, so...
      options(warn=-1) # turn warnings off
      row_to_add <- apply(row_to_add, 2, as.numeric)
      options(warn=0) # turn back on
      
      # bind to id_df and sort
      id_df <- bind_rows(id_df, row_to_add)
      id_df <- id_df[order(id_df$id, id_df$months), ]
      
    }
    
    # now we have id_df's with complete months, but with missing dosage values
    # for imputed months. need to fill those in using the 'zoo' package
    myzoo <- zoo(id_df$outcomes)
    id_df$outcomes <- as.numeric(na.locf(myzoo))
    
    # bind into one big df
    new_new_df <- bind_rows(new_new_df, id_df)
    new_new_df <- new_new_df[order(new_new_df$id, new_new_df$months), ]
    
  }
  
  # end?
  return(new_new_df)
  
  
}



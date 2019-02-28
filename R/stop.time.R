#' A Time-Making Function
#'
#' This function helps you create final time-outcome values for known but unobserved values (e.g., dosages) at a user-specified point in time.
#' @param data enter data for which each ID does NOT have the same beginning and end time-point.
#' @param id_name enter the name (str) of the column in your data containing subject's IDs.
#' @param time_cutoff enter the number of days, months, years that define each subject's final observation.
#' @keywords lcmm
#' @export
#' @examples
#' stop.time()

stop.time <- function(data, id_name, time_cutoff) {
  
  # prelims
  id_list <- levels(as.factor(data[,which(colnames(data) == toString(id_name))]))
  new_df <- data.frame()
  time_cut <- time_cutoff + 1
  
  # dirty work
  for(i in id_list){
    
    # for each ID, isolate id_df
    id_df <- data[data$id == i, ]
    
    # identify rows eligible for the two year cohort
    n_24_rows <- nrow(id_df[id_df$months < time_cut, ])
    
    # isolate id_df with just eligible rows
    id_df_24 <- id_df[1:n_24_rows, ]
    
    # get ending dose
    end_dose <- id_df_24[nrow(id_df_24), 
                         which(colnames(id_df_24) == 'outcomes')]
    
    # add a row at the end with month = 24 and dose = previous dose
    row_to_add <- c(i, end_dose, 24)
    row_to_add <- as.data.frame(t(c(row_to_add)))
    colnames(row_to_add) <- c("id", "outcomes", "months")
    row_to_add <- apply(row_to_add, 2, as.numeric)
    
    # add row to end of id_df_24 and make sure order is in order
    id_df_24 <- bind_rows(id_df_24, row_to_add)
    id_df_24 <- id_df_24[order(id_df_24$id, id_df_24$months), ]
    
    # add to one big data frame
    new_df <- bind_rows(new_df, id_df_24)
    
  }
  
  # end
  return(new_df)
  
}

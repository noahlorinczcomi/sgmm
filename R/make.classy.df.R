#' A Data Set Modifying Function
#'
#' This function is used to assist the other functions in the package with iteratively running lcmm models.
#' @param data enter data set containing predicted latent class membership values.
#' @param x_name enter the name of the time-indicating column (str)
#' @param y_name enter the name of the time-varying outcome column (str)
#' @param p_ids enter the name of the column containing the predicted latent class membership values (str)
#' @keywords lcmm
#' @export
#' @examples
#' make.classy.df()

make.classy.df <- function(data, x_name, y_name, p_ids) {
  months <- data[ , which(colnames(data) == toString(x_name))]
  outcomes <- data[ , which(colnames(data) == toString(y_name))]
  p_ids <- data[ , which(colnames(data) == toString(p_ids))]
  new_df <- cbind(months, outcomes, p_ids)
  new_df <- as.data.frame(new_df)
  colnames(new_df) <- c('months', 'outcomes', 'p_ids')
  # end
  return(new_df)
}
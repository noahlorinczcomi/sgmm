#' A Data Set Modifying Function
#'
#' This function is used to assist the other functions in the package with iteratively running lcmm models.
#' @param data enter data set containing predicted latent class membership values.
#' @param x_name enter the name of the time-indicating column (str)
#' @param y_name enter the name of the time-varying outcome column (str)
#' @param g_name enter the name of the column indicating ID values (str)
#' @keywords lcmm
#' @export
#' @examples
#' make.prelim.df()

make.prelim.df <- function(data, x_name, y_name, g_name) {
  x <- data[ , which(colnames(data) == toString(x_name))]
  y <- data[ , which(colnames(data) == toString(y_name))]
  g <- data[ , which(colnames(data) == toString(g_name))]
  new_df <- cbind(x, y, g)
  new_df <- as.data.frame(new_df)
  colnames(new_df) <- c('months', 'outcomes', 'id')
  # end
  return(new_df)
}
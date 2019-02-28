#' A LCGA-Building Function
#'
#' This function helps you create a limited-specs latent class growth analysis model using 'lcmm.'
#' @param data enter data set.
#' @param x_name enter column name (str) of time-indicating variable.
#' @param  y_name enter column name (str) of time-varying outcome variable.
#' @param g_name enter subject grouping column name (str; usually 'ID').
#' @param ng enter number of latent classes you would like to search for.
#' @keywords lcmm
#' @export
#' @examples
#' build.lcmm()

build.lcmm <- function(data, x_name, y_name, g_name, link, ng=2) {
  # organise data
  new_df <- make.prelim.df(data, toString(x_name), toString(y_name), toString(g_name))
  # model
  model <- lcmm(
    outcomes ~ months,
    random = ~months, subject = 'id', mixture = ~months, ng = ng,
    idiag = TRUE, data = new_df, link = toString(link)
  )
  # end
  return(model)
}
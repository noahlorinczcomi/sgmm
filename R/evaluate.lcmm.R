#' A LCGA Optimal Model Searching Function
#'
#' This function helps you identify model fit values for different linear, beta, and spline models.
#' @param data enter data set with date values converted to day/month/year values.
#' @param x_name enter the time-indictating column name (str)
#' @param y_name enter the time-varying outcome variable column name (str)
#' @param g_name enter the subject grouping variable column name (str; ususally 'ID')
#' @param ng_max enter the maximum number of groups you would like to evaluate.
#' @param spline_max enter the maximum number of splines you would like the model to evaluate.
#' @param spline_type if spline_max != NULL,  enter either the spline type you would like to evaluate (either 'quant' or 'equi')
#' @keywords lcmm
#' @export
#' @examples
#' evaluate.lcmm()

evaluate.lcmm <- function(data, x_name, y_name, g_name, ng_max = 8, spline_max = NULL, spline_type=NULL) {
  # link names
  link_names <- c('linear', 'beta', 'splines')
  # blank df
  new_df <- data.frame()
  # for loop for linear
  for(i in 2:ng_max) {
    model <- build.lcmm(data, toString(x_name), toString(y_name), toString(g_name), link = toString(link_names[1]), ng = i)
    my_row <- rbind(model$ng, model$linktype, model$BIC, model$loglik, model$AIC, model$niter)
    names(my_row) <- c("Groups", "Link", "BIC", "LogLik", "AIC", "nIter")
    new_df <- bind_rows(new_df, my_row)
  }
  # for loop for beta
  for(i in 2:ng_max) {
    model <- build.lcmm(data, toString(x_name), toString(y_name), toString(g_name), link = toString(link_names[2]), ng = i)
    my_row <- rbind(model$ng, model$linktype, model$BIC, model$loglik, model$AIC, model$niter)
    names(my_row) <- c("Groups", "Link", "BIC", "LogLik", "AIC", "nIter")
    new_df <- bind_rows(new_df, my_row)
  }
  # for loop for splines
  for(i in 2:ng_max) {
    # if using equidistant splines
    if(is.null(spline_max) == FALSE & spline_type == 'equi') {
      # NOTE - you cannot do TWO equidistant splines (think about it...)
      for(k in 3:spline_max) {
        spline_call <- paste0(k, "-equi-splines")
        # model
        model <- build.lcmm(data, toString(x_name), toString(y_name), toString(g_name), link = toString(spline_call), ng = i)
        # concat values
        my_row <- rbind(model$ng, k, model$BIC, model$loglik, model$AIC, model$niter)
        names(my_row) <- c("Groups", "Link", "BIC", "LogLik", "AIC", "nIter")
        new_df <- bind_rows(new_df, my_row)
      }
      # if using quant splines
    } else if(is.null(spline_max) == FALSE & spline_type == 'quant') {
      for(k in 2:spline_max) {
        # specifying the spline call
        spline_call <- paste0(k, "-quant-splines")
        # model
        model <- build.lcmm(data, toString(x_name), toString(y_name), toString(g_name), link = toString(spline_call), ng = i)
        # for spline loop
        my_row <- rbind(model$ng, k, model$BIC, model$loglik, model$AIC, model$niter)
        names(my_row) <- c("Groups", "Link", "BIC", "LogLik", "AIC", "nIter")
        new_df <- bind_rows(new_df, my_row)
      }
    }
  }
  # change spline links to their actual call (this is suprisingly the easiest way to do this)
  if(is.null(spline_max) == FALSE & spline_type == 'quant') {
    for(i in new_df$Link) {
      if(i > 1) {
        new_df$Link[new_df$Link == i] <- paste0(i, "-quant-splines")
      }
    }
  } else if(is.null(spline_max) == FALSE & spline_type == 'equi') {
    for(i in new_df$Link) {
      if(i > 1) {
        new_df$Link[new_df$Link == i] <- paste0(i, "-equi-splines")
      }
    }
  }
  # running intercept only model
  prelim_data <- make.prelim.df(data, toString(x_name), toString(y_name), toString(g_name))
  intercept_model <- lcmm(outcomes ~ 1, subject = 'id', data = prelim_data)
  # intercept model statistics
  my_int_row <- c(intercept_model$ng, intercept_model$ng, intercept_model$BIC, intercept_model$loglik, intercept_model$AIC, intercept_model$niter)
  names(my_int_row) <- c("Groups", "Link", "BIC", "LogLik", "AIC", "nIter")
  new_df <- rbind(new_df, my_int_row)
  # rename link
  new_df$Link[new_df$Link == 0] <- 'linear'
  new_df$Link[new_df$Link == 1] <- 'beta'
  new_df$Link[new_df$Groups == 1] <- 'intercept'
  # sort by lowest BIC value
  new_df <- new_df[order(new_df$BIC), ]
  # end
  return(new_df)
}

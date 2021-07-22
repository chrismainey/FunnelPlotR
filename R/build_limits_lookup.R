#' Function to build funnel limits
#' @description Internal function for funnel plot to build the control limits prior to plotting.
#' @param min_preds Minimum predicted value for range of x-axis
#' @param max_preds Maximum predicted value for range of x-axis
#' @param min_ratio Minimum ratio value for range of y-axis
#' @param max_ratio Maximum ratio value for range of y-axis
#' @param draw_unadjusted TRUE/FALSE Draw Poisson distribution limits?
#' @param draw_adjusted TRUE/FALSE Use overdispersion adjustment
#' @param tau2 If using draw_adjusted, what is the tau2 ("between" standard error) to use?
#' @param data_type SR, PR or RC. Used to set target reference
#' @param sr_method Which adjustment method is being used, SHMI or CQC?
#' @param target target to be used to set centre line
#' @param multiplier Multiply ratio value by an amount.  Default is 1, but some mortality ratios use 100, for example.
#' @keywords internal
#' @return A data.frame with an index column and various control limits based on the index as an x-axis value.
#'
#' @importFrom stats qchisq quantile
build_limits_lookup<-function(min_x, max_x, min_y, max_y
                              , draw_adjusted, tau2, data_type, sr_method
                              , target, multiplier, denominators){
  
  
  # general limits + tau2 limits table
  set.seed(1)

  
  if(data_type=="SR"){
    number.seq <- seq(1.1, as.numeric(max_x), length.out = 1000)
  } else {
    number.seq <- seq(3, as.numeric(max_x), length.out = 1000)
  }

  if(!missing(denominators)) {
    number.seq <- sort(unique(c(number.seq, denominators)))
  }

  dfCI <- data.frame(
    number.seq
  )

  dfCI <- calculate_limits(dfCI, data_type, sr_method, multiplier, tau2, target, draw_adjusted=TRUE)
  dfCI <- calculate_limits(dfCI, data_type, sr_method, multiplier, tau2, target, draw_adjusted=FALSE)

  return(dfCI)
}



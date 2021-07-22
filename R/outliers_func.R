#' @title Label outliers
#' 
#' @param mod_plot_agg Aggregated data set for plotting
#' @param draw_adjusted Logical for drawing OD limits, takes precedence over Poisson for outliers
#' @param limit which limit to use.  Currently 95 or 99.
#' @param multiplier the amount to scale the RR / limits by. Default is 1 \(no scaling\).
#' 
#' @keywords internal
outliers_func <- function(mod_plot_agg, draw_adjusted, limit, multiplier){

  if (limit == 95) {
    if (draw_adjusted==FALSE){
      #cat('1')
       mod_plot_agg$outlier <- ifelse((multiplier*mod_plot_agg$rr) <= mod_plot_agg$UCL95 &
                                        (multiplier*mod_plot_agg$rr) >= mod_plot_agg$LCL95,
                                  0,
                                  1)
    } else {
      #cat('2')
      mod_plot_agg$outlier <- ifelse((multiplier*mod_plot_agg$rr) <= mod_plot_agg$OD95UCL &
                                       (multiplier*mod_plot_agg$rr) >= mod_plot_agg$OD95LCL,
                                     0,
                                     1)
    }
  } else {
  
    if (limit == 99) {
      
       if (draw_adjusted==FALSE){
      #   cat('3')
        mod_plot_agg$outlier <- ifelse((multiplier*mod_plot_agg$rr) <= mod_plot_agg$UCL99 &
                                         (multiplier*mod_plot_agg$rr) >= mod_plot_agg$LCL99,
                                       0,
                                       1)
      } else {
        # cat('4')
        mod_plot_agg$outlier <- ifelse((multiplier*mod_plot_agg$rr) <= mod_plot_agg$OD99UCL &
                                         (multiplier*mod_plot_agg$rr) >= mod_plot_agg$OD99LCL,
                                       0,
                                       1)
      }
    }
  }
  return(mod_plot_agg)
}
  
  

      
     
      

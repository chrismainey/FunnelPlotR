#' @title Funnel Plots drawing function
#' @description Internal function for drawing plot.  Do not use this directly, call `funnel_plot()` instead.
#'
#' @param mod_plot_agg data.frame of containing numerator, denominator, ratio/proportion, SEs and limits
#' @param limits data.frame of limits from set_plot_range().
#' @param x_label Title for the funnel plot x-axis.  Usually expected deaths, readmissions, incidents etc.
#' @param y_label Title for the funnel plot y-axis.  Usually a standardised ratio.
#' @param title Plot title
#' @param label Whether to label outliers, highlighted groups, both or none. Default is "outlier", by accepted values are: "outlier", "highlight", "both" or "NA".
#' @param multiplier Scale relative risk and funnel by this factor. Default to 1, but 100 is used for HSMR
#' @param draw_unadjusted Draw exact limits based only on data points with no iterpolation. (default=FALSE)
#' @param draw_adjusted Draw overdispersed limits using Spiegelhalter's (2012) tau2 (default=TRUE)
#' @param target the calculated target value for the data type
#' @param min_y Specify the plot range. 
#' @param max_y Specify the plot range. 
#' @param min_x Specify the plot range. 
#' @param max_x Specify the plot range. 
#' @param data_type the data type SR, PR or RC.
#' @param sr_method CQC or SHMI methods for standardised ratios
#' @param theme a ggplot theme function.
#'
#' @return A list containing [1] the funnel plot as a ggplot2 object., [2]the limits table.
#' @keywords internal
#'
#'
#' @importFrom scales comma
#' @importFrom ggrepel geom_label_repel
#' @import ggplot2


draw_plot<-function(mod_plot_agg, limits, x_label, y_label, title, label, multiplier,
                     draw_unadjusted, draw_adjusted, target, min_y, max_y, min_x, max_x
                    , data_type, sr_method, theme, plot_cols){
  
  # Bind variable for NSE
  numerator <- denominator <- number.seq <- ll95 <- ul95 <- ll998 <- ul998 <- odll95 <- odul95 <-
    odll998 <- odul998 <- rr <- UCL95 <- group <- LCL95 <- OD95UCL <- OD95LCL <-UCL99 <-LCL99 <-
    OD99UCL <- OD99LCL <- outlier <- highlight <- NULL
  
  
  
  # base funnel plot
  funnel_p <- ggplot(mod_plot_agg, aes(y = multiplier * ((numerator / denominator)), x = denominator)) +
    geom_hline(aes(yintercept = target * multiplier), linetype = 2) +
    geom_point(aes(fill=highlight, shape=highlight, size = highlight), alpha = 0.55, col=1, show.legend = FALSE) +
    scale_shape_manual(values = c("0"=21, "1"=23, 1))+
    scale_fill_manual(values = c("0"="dodgerblue","1"="yellow", 1))+
    scale_size_manual(values = c("0"=2, "1"=3, 2))+
    xlab(x_label) +
    ylab(y_label) +
    ggtitle(title) +
    theme
    


  #limits
  
    if (draw_unadjusted == TRUE & draw_adjusted == TRUE) {
    funnel_p <- funnel_p +
      geom_line(aes(x = number.seq, y = ll95, col = "95% Lower", linetype =  "95% Lower"), size = 1, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = ul95, col = "95% Upper", linetype = "95% Upper"), size = 1,  data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = ll998, col = "99.8% Lower", linetype = "99.8% Lower"), size = 1, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = ul998, col = "99.8% Upper", linetype = "99.8% Upper"), size = 1, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odll95, col = "95% Lower Overdispersed", linetype = "95% Lower Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odul95, col = "95% Upper Overdispersed", linetype = "95% Upper Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odll998, col = "99.8% Lower Overdispersed", linetype = "99.8% Lower Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odul998, col = "99.8% Upper Overdispersed", linetype = "99.8% Upper Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
      scale_color_manual(values = plot_cols, name = "Control limits")+
      scale_linetype_manual(values = c("95% Lower"=2
                                       , "95% Upper" = 2
                                       ,  "99.8% Lower" = 1
                                       , "99.8% Upper" = 1
                                       , "95% Lower Overdispersed"=2
                                       , "95% Upper Overdispersed"= 2
                                       ,  "99.8% Lower Overdispersed" = 1
                                       , "99.8% Upper Overdispersed" = 1), guide=FALSE)+
      guides(colour = guide_legend(title.theme = element_text(
        size = 10,
        face = "bold",
        colour = "black",
        angle = 0
      ), override.aes = list(linetype = c(2,2,2,2,1,1,1,1)) ))
                           
      
  } else {
    if (draw_unadjusted == TRUE & draw_adjusted == FALSE) {
      funnel_p <- funnel_p +
        geom_line(aes(x = number.seq, y = ll95, col = "95% Lower", linetype =  "95% Lower"), size = 1, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ul95, col = "95% Upper", linetype = "95% Upper"), size = 1,  data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ll998, col = "99.8% Lower", linetype = "99.8% Lower"), size = 1, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ul998, col = "99.8% Upper", linetype = "99.8% Upper"), size = 1, data = limits, na.rm = TRUE) +
        scale_color_manual(values = plot_cols[1:4], name = "Control limits")+
        scale_linetype_manual(values = c("95% Lower"=2
                                         , "95% Upper" = 2
                                         ,  "99.8% Lower" = 1
                                         , "99.8% Upper" = 1), guide=FALSE)+
        guides(colour = guide_legend(title.theme = element_text(
          size = 10,
          face = "bold",
          colour = "black",
          angle = 0
        ), override.aes = list(linetype = c(2,2,1,1)) ))
        
    }

    if (draw_unadjusted == FALSE &  draw_adjusted == TRUE) {
      funnel_p <- funnel_p +
        geom_line(aes(x = number.seq, y = odll95, col = "95% Lower Overdispersed", linetype = "95% Lower Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odul95, col = "95% Upper Overdispersed", linetype = "95% Upper Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odll998, col = "99.8% Lower Overdispersed", linetype = "99.8% Lower Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odul998, col = "99.8% Upper Overdispersed", linetype = "99.8% Upper Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
        scale_color_manual(values = plot_cols[5:8], name = "Control limits")+
        scale_linetype_manual(values = c("95% Lower Overdispersed"=2
                                         , "95% Upper Overdispersed"= 2
                                         ,  "99.8% Lower Overdispersed" = 1
                                         , "99.8% Upper Overdispersed" = 1), guide=FALSE)+
        guides(colour = guide_legend(title.theme = element_text(
          size = 10,
          face = "bold",
          colour = "black",
          angle = 0
        ), override.aes = list(linetype = c(2,2,1,1)) ))
    }
  }

  # Apply plot scaling
  if (draw_adjusted == TRUE) {
    funnel_p <- funnel_p +
      scale_y_continuous(name = y_label, limits = c(min_y, max_y))+
      scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_x -1, max_x + 1))
  } else {
    funnel_p <- funnel_p +
      scale_y_continuous(name = y_label, limits=c(min_y, max_y))+
      scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_x -1, max_x + 1))
  }

  
  
  

 # Label points
  if(!is.na(label)){
    if(label=="outlier"){
    
      funnel_p <- funnel_p +
        geom_label_repel(aes(label = ifelse(outlier == 1,
                                            as.character(group), NA))
                         , size=2.5, point.padding=0, direction = "both", force = 2
                         , min.segment.length=0, na.rm=TRUE)
    } 
    
    if(label=="outlier_lower"){
      
      funnel_p <- funnel_p +
        geom_label_repel(aes(label = ifelse(outlier == 1 & rr < 1,
                                            as.character(group), NA))
                         , size=2.5,point.padding=0, direction = "both", force = 2
                         , min.segment.length=0, na.rm=TRUE)
    } 
    
    if(label=="outlier_upper"){
      
      funnel_p <- funnel_p +
        geom_label_repel(aes(label = ifelse(outlier ==  1 & rr > 1,
                                            as.character(group), NA))
                         , size=2.5, point.padding=0, direction = "both", force = 2
                         , min.segment.length=0, na.rm=TRUE)
    } 
    
    if(label=="highlight"){
      funnel_p <- funnel_p +
        geom_label_repel(aes(label = ifelse(highlight == 1,
                                            as.character(group), NA))
                         , size=2.5, point.padding=0, direction = "both", force = 2
                         , min.segment.length=0, na.rm=TRUE)
    } 
    
    if(label=="both"){
      funnel_p <- funnel_p +
        geom_label_repel(aes(label = ifelse((highlight == 1 | outlier == 1) ,
                                            as.character(group), NA))
                         , size=2.5, point.padding=0, direction = "both", force = 2
                         , min.segment.length=0, na.rm=TRUE)
      
    }
    
    if(label=="both_lower"){
      funnel_p <- funnel_p +
        geom_label_repel(aes(label = ifelse((highlight == 1 | (outlier == 1& rr < 1)) ,
                                            as.character(group), NA))
                         , size=2.5, point.padding=0, direction = "both", force = 2
                         , min.segment.length=0, na.rm=TRUE)
     
    }
    
    if(label=="both_upper"){
      funnel_p <- funnel_p +
        geom_label_repel(aes(label = ifelse((highlight == 1 | (outlier == 1 & rr > 1)) ,
                                            as.character(group), NA))
                         , size=2.5, point.padding=0, direction = "both", force = 2
                         , min.segment.length=0, na.rm=TRUE)
     
    }
  }
     
  


 return(funnel_p)

}

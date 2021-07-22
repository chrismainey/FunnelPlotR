#' @title Funnel plots for comparing institutional performance
#' @description An implementation of funnel plots for indirectly standardised ratios, as described by Spiegelhalter (2005) <https://doi.org/10.1002/sim.1970>.
#' There are several parameters for the input, with the assumption that you will want smooth,
#' overdispersed, funnel control limits.  Limits may be inflated for overdispersion based on the methods of DerSimonian & Laird (1986), buy calculating a between unit standard deviation (\eqn{\tau}) 
#' and constructing an additive random effects models, originally used for meta-analyses of clinical trials data.
#' @encoding UTF-8
#' @param numerator  A vector of the numerator (observed events/counts) values.  Used as numerator of the Y-axis
#' @param denominator A vector of denominator (predicted/population etc.)  Used as denominator of the Y-axis and the scale of the x-axis
#' @param group A vector of group names as character or factor.  Used to aggregate and group points on plots
#' @param data_type A string identifying the type of data used for in the plot, the adjustment used and the reference point. One of: "SR" for indirectly standardised ratios, such SHMI, "PR" for proportions, or "RC" for ratios of counts. Default is "SR".
#' @param title Plot title
#' @param limit Plot limits, accepted values are: 95 or 99, corresponding to 95\% or 99.8\% quantiles of the distribution. Default=99,and applies to OD limits if both OD and Poisson are used.
#' @param label Whether to label outliers, highlighted groups, both or none. Default is "outlier", by accepted values are:\cr
#' \itemize{
#' \item{\code{"outlier"}}{ - Labels upper and lower outliers, determined in relation to the `limit` argument.}
#' \item{\code{"outlier_lower"}}{ - Labels just and lower outliers, determined in relation to the `limit` argument.}
#' \item{\code{"outlier_upper"}}{ - Labels just upper, determined in relation to the `limit` argument.}
#' \item{\code{"highlight"}}{ - Labels the value(s) given in the `highlight`argument.}
#' \item{\code{"both"}}{ - Labels both the highlighted values(s), upper and lower outliers, determined in relation to the `limit` argument.}
#' \item{\code{"both_lower"}}{ - Labels both the highlighted values(s) and lower outliers, determined in relation to the `limit` argument.}
#' \item{\code{"both_upper"}}{ - Labels both the highlighted values(s) and upper outliers, determined in relation to the `limit` argument.}
#' \item{\code{NA}}{ - No labels applied}
#' }
#' @param highlight Single or vector of points to highlight, with a different colour and point style. Should correspond to values specified to `group`. Default is NA, for no highlighting.
#' @param draw_unadjusted Draw control limits without overdispersion adjustment. (default=FALSE)
#' @param draw_adjusted Draw overdispersed limits using hierarchical model, assuming at group level, as described in Spiegelhalter (2012).
#' It calculates a second variance component ' for the 'between' standard deviation (\eqn{\tau}), that is added to the 'within' standard deviation (sigma) (default=TRUE)
#' @param sr_method Method for adjustment when using indirectly standardised ratios (type="SR") Either "CQC" or "SHMI" (default). There are a few methods for standardisation.  "CQC"/Spiegelhalter
#' uses a square-root transformation and Winsorises (rescales the outer most values to a particular percentile).
#' SHMI, instead, uses log-transformation and doesn't Winsorise, but truncates the distribution before assessing overdisperison .
#' Both methods then calculate a dispersion ratio (\eqn{\phi}) on this altered dataset.  This ratio is then used to scale the full dataset,
#' and the plot is drawn for the full dataset.
#' @param trim_by Proportion of the distribution for winsorisation/truncation. Default is 10 \% (0.1).  Note, this is applied in a two-sided
#' fashion, e.g. 10\% refers to 10\% at each end of the distribution (20\% winsorised/truncated)
#' @param multiplier Scale relative risk and funnel by this factor. Default to 1, but 100 sometime used, e.g. in some hospital mortality ratios.
#' @param x_label Title for the funnel plot x-axis.  Usually expected deaths, readmissions, incidents etc.
#' @param y_label Title for the funnel plot y-axis.  Usually a standardised ratio.
#' @param xrange Manually specify the y-axis min and max, in form c(min, max), e.g. c(0, 200). Default, "auto", allows function to estimate range.
#' @param yrange Manually specify the y-axis min and max, in form c(min, max), e.g. c(0.7, 1.3). Default, "auto", allows function to estimate range.
#' @param theme a ggplot theme function.  This can be a canned theme such as theme_bw(), a theme() with arguments, or your own custom theme function. Default is new funnel_clean(), but funnel_classic() is original format.
#' @param plot_cols A vector of 8 colours for funnel limits, in order: 95\% Poisson (lower/upper), 99.8\% Poisson (lower/upper), 95\% OD-adjusted (lower/upper), 99.8\% OD-adjusted (lower/upper). 
#' Default has been chosen to avoid red and green which can lead to subconscious value judgements of good or bad.  
#' Default is hex colours: c("#FF7F0EFF", "#FF7F0EFF", "#1F77B4FF","#1F77B4FF", "#9467BDFF", "#9467BDFF", "#2CA02CFF", "#2CA02CFF")
#'
#' @return A fitted `funnelplot` object.  A `funnelplot` object is a list containing the following components:\cr
#' \item{print}{Prints the number of points, outliers and whether the plot has been adjusted, and prints the plot}
#' \item{plot}{A ggplot object with the funnel plot and the appropriate limits}
#' \item{limits_lookup}{A lookup table with selected limits for drawing a plot in software that requires limits.}
#' \item{aggregated_data}{A data.frame of the the aggregated dataset used for the plot.}
#' \item{outlier}{A data frame of outliers from the data.}
#' \item{tau2}{The between-groups standard deviation, \eqn{\tau^2}.}
#' \item{phi}{The dispersion ratio, \eqn{\phi}.}
#' \item{draw_adjusted}{Whether overdispersion-adjusted limits were used.}
#' \item{draw_unadjusted}{Whether unadjusted Poisson limits were used.}
#'
#' @export
#' @details
#'    Outliers are marked based on the grouping, and the limits chosen, corresponding to either 95\% or 99.8\% quantiles of the normal distribution.\cr
#'    Labels can attached using the `label` argument.\cr
#'    Overdispersion can be factored in based on the methods in Spiegelhalter et al. (2012), set `draw_adjusted` to FALSE to suppress this. \cr
#'    To use Poisson limits set `draw_unadjusted=TRUE`. \cr
#'    The plot colours deliberately avoid red-amber-green colouring, but you could extract this from the ggplot object and change manually if you like.
#'    Future versions of `funnelplotr` may allow users to change this.
#'    
#' @references DerSimonian & Laird (1986) Meta-analysis in clinical trials. \url{https://doi.org/10.1016/0197-2456(86)90046-2}
#' @references Spiegelhalter (2005) Funnel plots for comparing institutional performance \url{https://doi.org/10.1002/sim.1970}
#' @references Spiegelhalter et al. (2012) Statistical methods for healthcare regulation: rating, screening and surveillance: \url{https://doi.org/10.1111/j.1467-985X.2011.01010.x}
#' @references NHS Digital (2020) SHMI Methodology v .134\url{https://digital.nhs.uk/data-and-information/publications/clinical-indicators/shmi/current}
#' 
#' @examples
#' # We will use the 'medpar' dataset from the 'COUNT' package.
#' # Little reformatting needed
#'
#' library(COUNT)
#' data(medpar)
#' medpar$provnum<-factor(medpar$provnum)
#' medpar$los<-as.numeric(medpar$los)
#'
#' mod<- glm(los ~ hmo + died + age80 + factor(type)
#'       , family="poisson", data=medpar)
#' 
#' # Get predicted values for building ratio
#' medpar$prds<- predict(mod, type="response")
#'
#' # Draw plot, returning just the plot object
#' fp<-funnel_plot(denominator=medpar$prds, numerator=medpar$los,
#' group = medpar$provnum, limit=95, title="An example funnel plot")
#' 
#' # Methods for viewing/extracting
#' print(fp)
#' plot(fp)
#' summary(fp)
#' limits(fp)
#' outliers(fp)
#' source_data(fp)
#' phi(fp)
#' tau2(fp)
#' 
#'
#'
#'
#' @importFrom scales comma
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr select filter arrange mutate summarise group_by %>% n
#' @importFrom stats na.omit
#' @import ggplot2


funnel_plot <- function(numerator, denominator, group, data_type = "SR", limit = 99, label = "outlier",
                            highlight = NA, draw_unadjusted = FALSE, draw_adjusted = TRUE, sr_method = "SHMI"
                            , trim_by = 0.1, title="Untitled Funnel Plot", multiplier = 1, x_label = "Expected"
                            , y_label ,xrange = "auto", yrange = "auto"
                            , plot_cols = c("#FF7F0EFF", "#FF7F0EFF", "#1F77B4FF","#1F77B4FF", "#9467BDFF", "#9467BDFF", "#2CA02CFF", "#2CA02CFF")
                            , theme = funnel_clean()){


#funnel_plot(medpar$los, medpar$prds, medpar$provnum)

  # build initial dataframe of obs/predicted, with error message caught here in 'try'

  if (missing(denominator)) {
    stop("Need to specify model denominator")
  }
  if (missing(numerator)) {
    stop("Need to supply numerator")
  }
  if (missing(title)) {
      title <- ("Untitled Funnel Plot")
  }
  if (missing(numerator)) {
    stop("Need to supply the column name for numerator")
  }

  if (class(denominator)[1] == "array") {
    denominator <- as.numeric(denominator)
  }
  
  if(identical(numerator,denominator)){
    stop("Numerator and denominator are the same. Please check your inputs")
  }
  
  if(length(plot_cols) < 4){
    stop("Please supply a vector of 4 colours for funnel limits, in order: 95% Poisson, 99.8% Poisson, 95% OD-adjusted, 99.8% OD-adjusted, even if you are only using one set of limits.")
  }
  
  if(!(label %in% c("outlier", "outlier_lower", "outlier_upper", "highlight"
                    , "both", "both_lower", "both_upper", NA))){
    stop("No permitted labelling specification.  See help: `?funnel_plot`")
  }
  
  if (missing(x_label)){
    if(data_type=="SR"){
      x_label <- "Expected"
      } else {
      x_label <- "n"
    }
  }
  
  if (missing(y_label)){
    if(data_type=="SR"){
      y_label <- "Standardised Ratio"
    } else if(data_type=="PR"){
      y_label <- "Proportion"
    } else {
      y_label <- "Ratio"
    }
  }
  

  # Error handling for highlight argument
  if (!(is.na(highlight))){
    if(!is.character(highlight)) {
      stop("Please supply `highlight` in character format, or a character vector")
    }
  }
  
  

  if(!is.na(highlight)){
    if (is.factor(group)){
      if((!(highlight %in% levels(group)))){
         stop("Value(s) specified to `highlight` not found in `group` variable")
      }
    } else {
      if (!(highlight %in% group)) {
        stop("Value(s) specified to `highlight` not found in `group` variable")
      }
    }
  }

  
  # Define vector for scale colours
  plot_cols<-c(
    
    "95% Lower" = plot_cols[1],
    "95% Upper" = plot_cols[2],
    "99.8% Lower" = plot_cols[3],
    "99.8% Upper" = plot_cols[4],
    "95% Lower Overdispersed" = plot_cols[5],
    "95% Upper Overdispersed" = plot_cols[6],
    "99.8% Lower Overdispersed" = plot_cols[7],
    "99.8% Upper Overdispersed" = plot_cols[8]
  )


  mod_plot <- data.frame(numerator=as.numeric(numerator)
                         ,denominator=as.numeric(denominator)
                         , group=as.factor(group))
  

  mod_plot_agg<-aggregate_func(mod_plot)
  
  # Round to two decimal places for expected SHMI expected
  if(data_type == "SR" & sr_method == "SHMI"){
    mod_plot_agg$denominator <- round(mod_plot_agg$denominator,2)
  }
  
  target <- ifelse(data_type == "SR", 1, sum(mod_plot_agg$numerator)/ sum(mod_plot_agg$denominator))
  
  #OD Adjust and return table
  # transform to z-score
  mod_plot_agg <- transformed_zscore(mod_plot_agg=mod_plot_agg, data_type = data_type, sr_method = sr_method)
  
  # Winsorise or truncate depending on method
  if(data_type=="SR" & sr_method=="SHMI"){
    mod_plot_agg <- truncation(mod_plot_agg = mod_plot_agg, trim_by=trim_by)
  } else {
    mod_plot_agg <- winsorisation(mod_plot_agg = mod_plot_agg, trim_by=trim_by)
  }
  
  # New n for winsorised/truncated values
  n <- as.numeric(sum(!is.na(mod_plot_agg$Wuzscore)))
  # Calculate Phi (the overdispersion factor)
  phi <- phi_func(n= n, zscores=na.omit(mod_plot_agg$Wuzscore))
  
  # Use phi to calculate Tau, the between group standard deviation
  # Update on 29/11/2020 to only include the S without NA values in SHMI
  tau2 <- tau_func(n=n,  phi=phi, S=mod_plot_agg[!is.na(mod_plot_agg$Wuzscore),]$s)
  
  
  if(draw_adjusted == FALSE){
    phi<-as.numeric(0)
    tau2<-as.numeric(0)
    draw_unadjusted <- TRUE
  }

  # Set limits
  # Determine the range of plots
  if(xrange[1] == "auto"){
    max_x <- as.numeric(ceiling(max(mod_plot_agg$denominator, na.rm = FALSE)))
    min_x <- as.numeric(ceiling(min(mod_plot_agg$denominator,na.rm = FALSE)))
  } else {
    min_x <- xrange[1]
    max_x <- xrange[2]
  }
  
  if(yrange[1] == "auto"){
    max_y <- max((1.3 * target *multiplier), multiplier *  (1.1 * as.numeric(max((mod_plot_agg$numerator / mod_plot_agg$denominator)))), na.rm = FALSE)
    min_y <- min((0.7 * target * multiplier), multiplier * (0.9 * as.numeric(min((mod_plot_agg$numerator / mod_plot_agg$denominator)))), na.rm = FALSE)
  
    } else {
    min_y <- yrange[1]
    max_y <- yrange[2]
  }
  
  ### Calculate funnel limits ####
  if (draw_adjusted == FALSE) {
    message("draw_adjusted set to FALSE, plotting using unadjusted limits")
  }
  
  if (draw_adjusted == TRUE & tau2 == 0) {
    draw_adjusted <- FALSE
    message("No overdispersion detected, or draw_adjusted to FALSE, plotting using unadjusted limits")
    draw_unadjusted <- TRUE
  }

  # Calculate both adjusted and unadjusted control limits. This calculates limits for
  #   for both a range of denominators for plotting as well as the denominators present
  #   in the data
  plot_limits<-build_limits_lookup(min_x=min_x, max_x=max_x, min_y=min_y, max_y=max_y, 
                                    denominators = mod_plot_agg$denominator,
                                    draw_adjusted=draw_adjusted, tau2=tau2, 
                                    data_type=data_type, sr_method=sr_method, target=target, 
                                    multiplier=multiplier)
  
  # Extract the control limits corresponding to the denominators present in the
  #   data. This drops the standard error 's' already present in mod_plot_agg
  mod_plot_agg <- merge(mod_plot_agg[,-grep("\\bs\\b",colnames(mod_plot_agg))],
                        plot_limits, by.x="denominator", by.y="number.seq")
  
  # Arrange rows by group
  mod_plot_agg <- mod_plot_agg[order(mod_plot_agg$group),]
  
  # Create named vector mapping the needed changes in variable names
  new_names = c("odll95"="OD95LCL", "odul95"="OD95UCL", "odll998"="OD99LCL",
                "odul998"="OD99UCL", "ll95"="LCL95", "ul95"="UCL95",
                "ll998"="LCL99", "ul998"="UCL99")
  
  # Rename aggregate data control limits
  names(mod_plot_agg) <- ifelse(is.na(new_names[names(mod_plot_agg)]), names(mod_plot_agg), new_names[names(mod_plot_agg)])             
  
  # Add a colouring variable 
  mod_plot_agg$highlight <- as.character(as.numeric(mod_plot_agg$group %in% highlight))
  
  # Add outliers flag
  mod_plot_agg <- outliers_func(mod_plot_agg, draw_adjusted, limit, multiplier)
  
  # Assemble plot
  fun_plot<-draw_plot(mod_plot_agg, limits=plot_limits, x_label, y_label, title, label,
                      multiplier=multiplier,  
                      draw_unadjusted=draw_unadjusted, draw_adjusted=draw_adjusted,
                      target=target, min_y, max_y, min_x, max_x, data_type=data_type,
                      sr_method = sr_method, theme = theme, plot_cols=plot_cols)
  
  
  # Subset outliers for reporting
  outliers_df<- subset(mod_plot_agg, mod_plot_agg$outlier==1)
  
  #Build return
  rtn<- new_funnel_plot(list(plot=fun_plot, limits_lookup=plot_limits, aggregated_data=mod_plot_agg
                             , phi=phi, tau2=tau2, draw_adjusted=draw_adjusted, draw_unadjusted=draw_unadjusted
                             , outliers_data=outliers_df))
  
  validate_funnel_plot(rtn)
  
  return(rtn)
}

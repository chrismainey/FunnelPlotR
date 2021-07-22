#' @title Constructor for new funnel plot object
#' @param x List of objects to convert to class 

new_funnel_plot <- function(x = list()) {
  
  stopifnot(is.list(x))
  
  structure(x,
            class = "funnelplot")
  
}



#' @title Validator for new funnel plot object
#' @param funnelplot object of class funnelplot

validate_funnel_plot <- function(funnelplot){
  
  if (!is.list(funnelplot[[1]])){
    stop(
      "Invalid ggplot object"
    )
  }
  if (!is.data.frame(funnelplot[[2]])){
    stop(
      "Invalid limits data.frame"
    )
  }
  if (!is.data.frame(funnelplot[[3]])){
    stop(
      "Invalid aggregate date.frame"
    )
  }
  if (!is.numeric(funnelplot[[4]])){
    stop(
      "Invalid phi value, requires double()"
    )
  }
  if (!is.numeric(funnelplot[[5]])){
    stop(
      "Invalid tau2 value, requires double()"
    )
  }
  if (!is.logical(funnelplot[[6]])){
    stop(
      "Invalid draw_adjusted value.  Expects logical."
    )
  }
  if (!is.logical(funnelplot[[7]])){
    stop(
      "Invalid draw_unadjusted value.  Expects logical."
    )
  }
  if (!is.data.frame(funnelplot[[8]])){
    stop(
      "Invalid outliers date.frame"
    )
  }
}



#### Methods

#'@export

print.funnelplot <- function(x, ...){
  
  print(x[[1]])
  
  cat("A funnel plot object with", nrow(x[[3]]), "points of which", nrow(x[[8]]), "are outliers. \n")
  
  if(x[[6]]==TRUE){cat("Plot is adjusted for overdispersion. \n")}
  else {cat("Plot is not adjusted for overdispersion. \n")}
  
}


#'@export

plot.funnelplot <- function(x, ...){
  
  x[[1]]
  
}


#'@encoding UTF-8
#'@export

summary.funnelplot <- function(object, ...){
  
  cat("A funnel plot object with", nrow(object[[3]]), "points of which", nrow(object[[8]]), "are outliers. \n")
  
  cat("Dispersion ratio: \u03d5 =", object[[4]],". \n")
  if(object[[6]]==TRUE){cat("Plot is adjusted for overdispersion. Between unit variance: 
  \U1D70F\u00B2, =", object[[5]], ". \n")}
  else {cat("Plot is not adjusted for overdispersion. \n")}
  
  cat("Outliers: \n")
  print(object[[8]])
  
}

#' Phi class for funnel plots
#' 
#' @title dispersion ratio, \eqn{\phi}, for Funnel plots
#' @param x object of class funnel plot
#' @export
phi <- function(x) {
  UseMethod("phi")
}



#'@export

phi.funnelplot <- function(x){
  
  x[[4]]
  
}

#' Tau2 class for funnel plots
#' 
#' @title between groups variance, \eqn{\tau^2}, for Funnel plots
#' @param x object of class funnel plot
#' @export
tau2 <- function(x) {
  UseMethod("tau2")
}


#'@export

tau2.funnelplot <- function(x){
  
  x[[5]]
  
}


#' Limits class for funnel plots
#' 
#' @title Funnel plot limits
#' @param x object of class funnel plot
#' @export
limits <- function(x) {
  UseMethod("limits")
}


#'@export

limits.funnelplot <- function(x){
  
  x[[2]]
  
}



#' Outliers class for funnel plots
#' 
#' @title Funnel plot outliers
#' @param x object of class funnel plot
#' @export
outliers <- function(x) {
  UseMethod("outliers")
}


#'@export

outliers.funnelplot <- function(x){
  
  x[[8]]
  
}


#' Source data class for funnel plots
#' 
#' @title source data used to create Funnel plots
#' @param x object of class funnel plot
#' @export
source_data <- function(x) {
  UseMethod("source_data")
}


#'@export
source_data.funnelplot <- function(x){
  
  x[[3]]
}


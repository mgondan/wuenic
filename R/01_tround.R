#' Simulate Prolog's "outward" rounding
#' 
#' @param number
#' a float
#'
#' @return
#' rounded toward next even number
#' 
tround = function(number)
  sign(number) * trunc(abs(number) + 0.5 + sqrt(.Machine$double.eps))


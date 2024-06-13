.onAttach <- function(libname, pkgname)
{
  if(!requireNamespace("rolog", quiet=TRUE))
    stop("Could not load R package rolog.")
  
  if(!rolog::rolog_ok())
    stop("Could not attach R package rolog.")
}

#' List of all vaccine names
#' 
#' @return
#' returns of a list with the abbreviated names of the vaccines used in the
#' Wuenic program
#'
#' @details
#' This function will be removed at some 
#' point (replaced by a data-driven global variable)
#' 
Vn = function()
  c("bcg", "bcgx", "dtp1", "dtp1x", "dtp3", "dtp3x", "hepb0", "hepb1",
    "hepb3", "hepb3x", "hepbb","hepbbx", "hib1", "hib3", "hib3x", "opv1",
    "ipv1", "ipv1x", "ipv2", "ipv2_frac", "ipv2x", "mcv1", "mcv1x", "mcv2", "pcv1", 
    "pcv3", "pcv3x", "pol1", "pol3", "pol3x", "rcv1", "rotac", "rotacx", "yfv",
    "menga")
#
# Todo: Generate Vn on the fly
# Todo: khm.pl has an "opv1" vaccine, I guess it is ipv1
#

#' Year range of Wuenic program
#' 
#' @return
#' 1985:2023
#' 
#' @details
#' This function will be removed at some 
#' point (replaced by a data-driven global variable)
Yn = function()
  1985:2023

#' YV.char, YV.int, YV.bool: Year x Vaccine matrices
#' 
#' @param fill
#' (default: NA) value(s) to fill the matrix
#'
#' @return
#' Year x Vaccine matrices
YV.char = function(fill=NA_character_)
  matrix(fill, length(Yn()), length(Vn()), dimnames=list(Y=Yn(), V=Vn()))

#' @rdname YV.char
#' @export
YV.int = function(fill=NA_integer_)
  matrix(fill, length(Yn()), length(Vn()), dimnames=list(Y=Yn(), V=Vn()))

#' @rdname YV.char
#' @export
YV.bool = function(fill=FALSE)
  matrix(fill, length(Yn()), length(Vn()), dimnames=list(Y=Yn(), V=Vn()))

#' @rdname YV.char
#' @export
YV.real = function(fill=NA_real_)
  matrix(fill, length(Yn()), length(Vn()), dimnames=list(Y=Yn(), V=Vn()))

#' sawtooth: jumps in reported vaccine coverage of more than 10 percentage
#' points are not plausible
#' 
#' @return
#' 10
sawtooth = function()
  10

#' svy.thrs: differences between reported coverage and survey coverage of
#' more than 10 percentage points raise flags in GoC and anchor points
#' 
#' @return
#' 10
svy.thrs = function()
  10

#' svy.scope: Average surveys across +/- 2 years
#' 
#' @return
#' 2
svy.scope = function()
  2

#' unpd.thrs: differences between reported coverage and UNPD data of
#' more than 10 percentage points raise flags in GoC
#' 
#' @return
#' 10
unpd.thrs = function()
  10

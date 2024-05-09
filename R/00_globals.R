.onAttach <- function(libname, pkgname)
{
  if(!requireNamespace("rolog", quiet=TRUE))
    stop("Could not load R package rolog.")
  
  if(!rolog::rolog_ok())
    stop("Could not attach R package rolog.")
}

# List of all vaccine names
#
# Todo: Generate Vn on the fly
# Todo: khm.pl has an "opv1" vaccine, I guess it is ipv1
#
Vn = function()
  c("bcg", "bcgx", "dtp1", "dtp1x", "dtp3", "dtp3x", "hepb0", "hepb1",
    "hepb3", "hepb3x", "hepbb","hepbbx", "hib1", "hib3", "hib3x", "opv1",
    "ipv1", "ipv1x", "ipv2", "ipv2_frac", "ipv2x", "mcv1", "mcv1x", "mcv2", "pcv1", 
    "pcv3", "pcv3x", "pol1", "pol3", "pol3x", "rcv1", "rotac", "rotacx", "yfv",
    "menga")

# Year range for estimation
Yn = function()
  1985:2023

# Convenience functions
YV.char = function(fill=NA_character_)
  matrix(fill, length(Yn()), length(Vn()), dimnames=list(Y=Yn(), V=Vn()))
YV.int = function(fill=NA_integer_)
  matrix(fill, length(Yn()), length(Vn()), dimnames=list(Y=Yn(), V=Vn()))
YV.bool = function(fill=FALSE)
  matrix(fill, length(Yn()), length(Vn()), dimnames=list(Y=Yn(), V=Vn()))
YV.real = function(fill=NA_real_)
  matrix(fill, length(Yn()), length(Vn()), dimnames=list(Y=Yn(), V=Vn()))

# The country-specific data is stored in a Prolog file data.pl. 
# 
# Todo: I consider this a temporary solution, we should skip this intermediate
# step and export the country data to a different representation, not Prolog.
# The step is not needed, and we would not depend on R package rolog anymore.

sawtooth = function()
  10

svy.thrs = function()
  10

svy.scope = function()
  2

unpd.thrs = function()
  10

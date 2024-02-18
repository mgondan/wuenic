# Simulate Prolog's "outward" rounding
tround = function(number)
  sign(number) * trunc(abs(number) + 0.5 + sqrt(.Machine$double.eps))


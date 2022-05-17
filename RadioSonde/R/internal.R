tmr <- function(w, p)
{
  #
  # Determine x-coordinate on skew-T, log p diagram given 
  # temperature (C)
  # and y-coordinate from FUNCTION SKEWTY.  X-origin at T=0c.
  #
  #            "algorithms for generating a skew-t, log p
  #            diagram and computing selected meteorological
  #            quantities."
  #            atmospheric sciences laboratory
  #            u.s. army electronics command
  #            white sands missile range, new mexico 88002
  #            33 pages
  #       baker, schlatter  17-may-1982
  #   this function returns the temperature (celsius) on a mixing
  #   ratio line w (g/kg) at pressure p (mb). the formula is 
  #   given in
  #   table 1 on page 7 of stipanuk (1973).
  #
  #   initialize constants
  c1 <- 0.0498646455
  c2 <- 2.4082965
  c3 <- 7.07475
  c4 <- 38.9114
  c5 <- 0.0915
  c6 <- 1.2035
  x <- log10((w * p)/(622. + w))
  tmrk <- 10^(c1 * x + c2) - c3 + c4 * ((10.^(c5 * x) - c6)^
                                          2.)
  return(tmrk - 273.15)
}

tda <- function(o, p)
{
  #       reference stipanuk paper entitled:
  #            "algorithms for generating a skew-t, log p
  #            diagram and computing selected meteorological
  #            quantities."
  #            atmospheric sciences laboratory
  #            u.s. army electronics command
  #            white sands missile range, new mexico 88002
  #            33 pages
  #       baker, schlatter  17-may-1982
  #   this function returns the temperature tda (celsius) 
  #   on a dry adiabat
  #   at pressure p (millibars). the dry adiabat is given by
  #   potential temperature o (celsius). the computation is 
  #   based on
  #   poisson's equation.
  ok <- o + 273.14999999999998
  tdak <- ok * ((p * 0.001)^0.28599999999999998)
  return(tdak - 273.14999999999998)
}

testP<- function( theta, target= 18.9){
  p <- seq(from = 1050, to = 100,length.out= 500)
  sy <- skewty(p)
  dry <- tda(theta, p)
  sx <- skewtx(dry, sy)
  ind<- which.min( abs( sx- target))
  return( p[ind])
}
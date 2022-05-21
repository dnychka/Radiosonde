


addAxesSkewt<-function(){ 

##############################################################
# add pressure axis     
##############################################################
standardLevels <- c(  1000, 850, 700, 500, 400,
                      300, 250, 200, 150, 100)  
ypos <- skewty(standardLevels)
axis(2, at = ypos, labels = format(standardLevels), las=2)
endL<- par()$usr[1]
mtext(side=1,line=0.2, at= endL, text="hPa", cex=1,
      col="black")
}


altitudeAxis<- function(sondeData){
 
  y <- skewty(sondeData$press)
  altKm<- sondeData$alt/1e3
# trim out missing values  
  good<- !is.na(y) & !is.na(altKm)
  y<- y[good]
  altKm<- altKm[good]
  km<- 1: round( max(altKm))
# cubic spline interpolant to get height at statndard levels
  yKm<- splint( altKm, y, km)
  axis(2, line=3, at = yKm, labels = format( km), las=2, cex=.8,
       col="grey40")
  cat( )
  mtext(2, line=3, at= skewty(100 - 10), text="km", cex=1.2,
        las=2,
        col="black")
}

makeWindPlot<- function( windplot, L, magicRatio=4){
  # create a matrix of figure coordinates for the windplot
  # the default for one is figure coordinates  .8, 1.0, 0, 1.0 
  if(is.null( windplot) ){
    windplot = cbind((magicRatio + (0 : (L-1)))/(magicRatio + L),
                     (magicRatio + (1 : (L  )))/(magicRatio + L),
                     rep( 0,L),
                     rep( 1,L)
    )
  }
  else{
    windplot<- rbind( windplot)
  }
  return( windplot)
}

horizontalIsobars<- function(){
  # Declare pressure values and x coordinates of the endpoints of each
  # isobar.  These x,y values are computed from the equations in the
  # transform functions listed at the end of this program.  Refer to
  # a skew-T diagram for reference if necessary.
  pres <- c(1050, 1000, 850, 700, 500, 400, 300, 250, 200, 150, 100)
  NPRES <- length(pres)
  # horizontal lines at ( mandatory??) pressoure levels. 
  abline( h= skewty(pres), lwd=1, col="grey")
  
}

diagonalIsotherms<- function(xmin, xmax){
temp <- seq(from = -100, to = 40, by = 10)
# TEMPERATURES
NTEMP <- length(temp)
# number of ISOTHERMS
# Determine pressures where isotherms intersect the
# edge of the skew-T diagram.
# ------------------------------------------------------
# x = 0.54*temp + 0.90692*y		SKEWTX formula
# y = 132.182 - 44.061 * log10(pres)	SKEWTY formula
#
# --- FOR ISOTHERMS TERMINATING ALONG LEFT EDGE, WE KNOW 
# --- TEMP,X = XMIN, FIND PRES
lEndt <- rep(1050, NTEMP)
inds <- seq(1, length(temp))[temp < -30]
exponent <- (132.182 - (xmin - 0.54 * temp[
  inds])/0.90692)/44.061
lEndt[inds] <- 10^exponent
# --- FOR ISOTHERMS TERMINATING ALONG TOP, WE KNOW PRESSURE ALREADY
rEndt <- rep(100, NTEMP)
# FOR ISOTHERMS TERMINATING ALONG MIDDLE EDGE, WE KNOW 
# --- TEMP,X = xmax, FIND PRES
inds <- seq(1, length(temp))[(temp >= -30)]
exponent <- (132.182 - 
               (xmax - 0.54 * temp[inds]) / 0.90692 
)/44.061
rEndt[inds] <- 10^exponent
# Declare isotherm values and pressures where isotherms intersect the
# edge of the skew-T diagram.
yr <- skewty(rEndt)
# y-coords on right
xr <- skewtx(temp, yr)
# x-coords on right
yl <- skewty(lEndt)
# y-coords on right
xl <- skewtx(temp, yl)
# x-coords on right
segments(xl, yl, xr, yr, col = "grey", lwd = 1)
mtext(side=4,line=0, at = yr[8:NTEMP],
      text = format(temp[8:NTEMP]), col = "grey30")
endR<- par()$usr[2]
mtext(side=1, line=0.2, at= endR, text="degrees C", cex=1,
      col="black")
}

saturatedMixingLevels<- function(){
mixrat <- c(20, 12, 8, 5, 3, 2, 1)
NMIX <- length(mixrat)
#     MIXING RATIOS
#yr <- skewty(rep(400., NMIX -1))
yr <- skewty(rep(100., NMIX))
#tmix <- tmr(mixrat[2:NMIX], 400.)
tmix <- tmr(mixrat[1:NMIX], 100.)
xr <- skewtx(tmix, yr)
#yl <- skewty(rep(1000., NMIX - 1))
yl <- skewty(rep(1050., NMIX ) )
#tmix <- tmr(mixrat[2:NMIX], 1000.)
tmix <- tmr(mixrat[1:NMIX], 1050.)
xl <- skewtx(tmix, yl)
segments(xl, yl, xr, yr, lty = 2, col = "red2",
         lwd = 1)     
#yl <- skewty(rep(1025., NMIX - 1))
yl <- skewty(rep(1050., NMIX ))
xl <- skewtx(tmix, yl)
mtext(side=1, line=0, at=xl, text = format(mixrat), col = "red2"
       , cex = 0.5)
}

dryAdiabats<- function(xmin, xmax, GREEN){
  # Declare adiabat values and pressures where adiabats intersect the
  # edge of the skew-T diagram.  Refer to a skew-T diagram if necessary.
  theta <- seq(from = -30, to = 170, by = 10)
  NTHETA <- length(theta)
  #  DRY ADIABATS
  lEndth <- rep(100, times = NTHETA)
  lEndth[1:8] <- c(880, 670, 512, 388, 292, 220, 163, 119)
  rEndth<- rep( NA, NTHETA)
  #	rEndth <- rep(1050, times = NTHETA)
  #	rEndth[9:NTHETA] <- c(600, 600, 500,400, 395, 334, 286, 245, 210,
  #		180, 155, 133, 115)
  # find rougly where the dry adibats cross speific x target -- xmax	
  for(  k in 1: NTHETA){
    rEndth[k]<- testP( theta[k], xmax)
  }
  
  for(itheta in 1:NTHETA) {
    p <- seq(from = lEndth[itheta], to = rEndth[itheta], length = 
               200)
    sy <- skewty(p)
    dry <- tda(theta[itheta], p)
    sx <- skewtx(dry, sy)
    lines(sx, sy, lty = 1, col = "brown3")
  }
  #---------------------------------------------------------------------
  # --- DRAW MOIST ADIABATS UP TO ~ 250hPa  
  # Declare moist adiabat values and pressures of the tops of the
  # moist adiabats.  All moist adiabats to be plotted begin at 1050 mb.
  #---------------------------------------------------------------------
  # declare pressure range
  # convert to plotter coords and declare space for x coords
  p <- seq(from = 1050, to = 240, by = -10)
  npts <- length(p)
  sy <- skewty(p)
  sx <- double(length = npts)
  # 
  # Generating the data for the curves can be time-consuming.
  # We generate them once and use them. If, for some reason you
  # need to regenerate the curves, you need to set redo to TRUE
  # 
    pseudo <- c(32, 28, 24, 20, 16, 12, 8)
    NPSEUDO <- length(pseudo)
    holdx <- matrix(0, nrow = npts, ncol = NPSEUDO)
    holdy <- matrix(0, nrow = npts, ncol = NPSEUDO)
    for(ipseudo in 1:NPSEUDO) {
      for(ilen in 1:npts) {
        # satlft is iterative
        moist <- satlft(pseudo[ipseudo], p[ilen])
        sx[ilen] <- skewtx(moist, sy[ilen])
      }
      # find the adiabats outside the plot region and
      # wipe 'em out.
      inds <- (sx < xmin)
      sx[inds] <- NA
      sy[inds] <- NA
      holdx[, ipseudo] <- sx
      holdy[, ipseudo] <- sy
    }
  # 
  # Finally draw the curves. Any curves that extend beyond
  # the left axis are clipped. Those curves only get annotated
  # at the surface.
  # 
  for(ipseudo in 1:NPSEUDO) {
    # plot the curves
    sx <- holdx[, ipseudo]
    sy <- holdy[, ipseudo]
    lines(sx, sy, lty = 2, col = GREEN)
    # annotate the curves -- at the top
    moist <- satlft(pseudo[ipseudo], 230)
    labely <- skewty(230)
    labelx <- skewtx(moist, labely)
    if (labelx > xmin) 
      text(labelx, labely, labels = as.character(pseudo[ipseudo]),
           col = GREEN, adj = 0.5, cex = 0.5)
    # annotate the curves -- at the surface
    moist <- satlft(pseudo[ipseudo], 1100)
    labely <- skewty(1100)
    labelx <- skewtx(moist, labely)
    mtext( side=1,line=.5,  at= labelx, text = format(pseudo[ipseudo]),
           col = GREEN, adj = 0.5, cex = 0.5)
  }
}

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

testP<- function( theta, target= 18.9){
  p <- seq(from = 1050, to = 100,length.out= 500)
  sy <- skewty(p)
  dry <- tda(theta, p)
  sx <- skewtx(dry, sy)
  ind<- which.min( abs( sx- target))
  return( p[ind])
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
  ok <- o + 273.15
  tdak <- ok * ((p * 0.001)^0.286)
    return(tdak - 273.15)
}




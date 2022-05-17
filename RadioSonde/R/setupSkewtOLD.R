"setupSkewtOLD" <-
function(BROWN = "brown3", GREEN = "green4", redo = FALSE, ...)
{
#
# Copyright 2001,2002 Tim Hoar, and Doug Nychka
#
# This file is part of the RadioSonde library for R and related languages.
#
# RadioSonde is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# RadioSonde is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RadioSonde; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
	

	#---------------------------------------------------------------------
	#
	# This program generates a skew-T, log p thermodynamic diagram.  This
	# program was derived to reproduce the USAF skew-T, log p diagram
	# (form DOD-WPC 9-16-1  current as of March 1978).
	#
	#---------------------------------------------------------------------
	# --- Define absoulute x,y max/min bounds corresponding to the outer
	# --- edges of the diagram. These are computed by inverting the 
	# --- appropriate
	# --- pressures and temperatures at the corners of the diagram.
	ymax <- skewty(1050)
	# actually at the bottom ~ -0.935
	ymin <- skewty(100)
	# at the top
	xmin <- skewtx((-20 - 32)*(5/9), skewty(1050))
	# was hardcoded to -19.0, is -18.66763
	TEMPMAX<- 40
	xmax <- skewtx(TEMPMAX, skewty(1050))
	# was hardcoded to  27.1, is  26.99909
	#---------------------------------------------------------------------
	 kinkx <- xmax
  #creates plotting region
	plot(c(0,1), c(0,1),
	     type = "n",
	     axes = FALSE, 
	     xlab = "",
	     ylab = ""
	     )
   par( usr= c( xmin, xmax,  ymax, ymin))

	#---------------------------------------------------------------------
	# --- DRAW HORIZONTAL ISOBARS., LABEL VERTICAL AXIS
	#---------------------------------------------------------------------
	# Declare pressure values and x coordinates of the endpoints of each
	# isobar.  These x,y values are computed from the equations in the
	# transform functions listed at the end of this program.  Refer to
	# a skew-T diagram for reference if necessary.
	pres <- c(1050, 1000, 850, 700, 500, 400, 300, 250, 200, 150, 100)
	NPRES <- length(pres)
	# horizontal lines at ( mandatory??) pressoure levels. 
	abline( h= skewty(pres), lwd=1, col="grey")
	# ISOBARS
	#xpl <- rep(xmin, times = NPRES)
	# LEFT EDGE IS STRAIGHT
	#xpr <- c(xmax, xmax, xmax, xmax, skewtx(20, skewty(500)), kinkx, kinkx,
	#	kinkx, kinkx, kinkx, kinkx)
	#y <- skewty(pres)
	
	#segments(xpl, y, xpr, y, col = "blue", lwd = 3, lty = 2)
 #	ypos <- skewty(pres[2:NPRES])
	#axis(2, at = ypos, labels = pres[2:NPRES] )
# mtext(side = 2, line = 1.5, "P (hPa)")
	
#	axis(2, at = ypos,  labels = pres[2:NPRES], pos = xmin-8,col="magenta",
#	      lwd=4)
	
	#---------------------------------------------------------------------
	# --- DRAW DIAGONAL ISOTHERMS.
	#---------------------------------------------------------------------
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
	# --- TEMP,X = KINKX, FIND PRES
	inds <- seq(1, length(temp))[(temp >= -30)]
	exponent <- (132.182 - 
	                     (kinkx - 0.54 * temp[inds]) / 0.90692 
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
	mtext(side=4, at = yr[8:NTEMP],
	     text = format(temp[8:NTEMP]), adj = 0, col = "grey40")
	#---------------------------------------------------------------------
	# --- DRAW SATURATION MIXING RATIO LINES.  
	# --- These lines run between 1050 and 400 mb. The 20 line intersects 
	# --- the sounding below 400 mb, thus a special case is made for it.  
	# --- The lines are dashed.  The temperature where each line crosses 
	# --- 400 mb is computed in order to get x,y locations of the top of
	# --- the lines.
	#---------------------------------------------------------------------
	mixrat <- c(20, 12, 8, 5, 3, 2, 1)
	NMIX <- length(mixrat)
	# --- Compute y coordinate at the top 
	# --- (i.e., right end of slanted line) and
	# --- the bottom of the lines.
	# --- SPECIAL CASE OF MIXING RATIO == 20
	yr <- skewty(440.)
	# y-coord at top (i.e. right)
	tmix <- tmr(mixrat[1], 440.)
	xr <- skewtx(tmix, yr)
	yl <- skewty(1000.)
	# y-coord at bot (i.e. left)
	tmix <- tmr(mixrat[1], 1000.)
	xl <- skewtx(tmix, yl)
	segments(xl, yl, xr, yr, lty = 2, col = "red1", lwd = 
		0.5)
	# dashed line
	# We want to stop the mixing ratio lines at 1000 and plot
	# the mixing ratio values "in-line" with where the line would continue
	yl <- skewty(1025.)
	xl <- skewtx(tmix, yl)
	text(xl, yl, labels = format(mixrat[1]), col = "green4", srt = 55,
		adj = 0.5, cex = 0.75)
	#     --- THE REST OF THE MIXING RATIOS
	yr <- skewty(rep(400., NMIX - 1))
	tmix <- tmr(mixrat[2:NMIX], 400.)
	xr <- skewtx(tmix, yr)
	yl <- skewty(rep(1000., NMIX - 1))
	tmix <- tmr(mixrat[2:NMIX], 1000.)
	xl <- skewtx(tmix, yl)
	segments(xl, yl, xr, yr, lty = 2, col = "red1", lwd = 
		0.1)
	# dashed line
	yl <- skewty(rep(1025., NMIX - 1))
	xl <- skewtx(tmix, yl)
	text(xl, yl, labels = format(mixrat), col = "green4", srt = 
		55, adj = 0.5, cex = 0.75)
	#---------------------------------------------------------------------
	# --- DRAW DRY ADIABATS.  
	# --- Iterate in 10 mb increments to compute the x,y points on the
	# ---  curve.
	#---------------------------------------------------------------------
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
	if(redo) {
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
	}
	else {
		holdx <- skewt.data$pseudox
		holdy <- skewt.data$pseudoy
                pseudo <- skewt.data$pseudo
                NPSEUDO <- skewt.data$NPSEUDO
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
		lines(sx, sy, lty = 1, col = GREEN)
                # annotate the curves -- at the top
		moist <- satlft(pseudo[ipseudo], 230)
		labely <- skewty(230)
		labelx <- skewtx(moist, labely)
                if (labelx > xmin) 
		text(labelx, labely, labels = as.character(pseudo[ipseudo]),
			col = GREEN, adj = 0.5, cex = 0.75)
                # annotate the curves -- at the surface
		moist <- satlft(pseudo[ipseudo], 1100)
		labely <- skewty(1100)
		labelx <- skewtx(moist, labely)
		mtext( side=1, at= labelx, text = format(pseudo[ipseudo]),
			col = GREEN, adj = 0.5, cex = 0.75)
	}
	# 
        # Most of the time, the only thing that needs to be returned by the 
        # routine is the plot boundaries so we know where to put the wind 
        # plot. However, if you are redrawing the curves, you need to be 
        # able to save the new curve data.
	# 
	invisible(list(pseudox=holdx, pseudoy=holdy, pseudo=pseudo, 
                 NPSEUDO=NPSEUDO, plt=par()$plt))
}

"setupSkewt" <-
function(  BROWN = "brown3", GREEN = "green4", redo = FALSE, 
           tempRangeF = c( -20, 104 ),...)
{
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

	TEMPMIN<- (tempRangeF[1] - 32)*(5/9)
	xmin <- skewtx(TEMPMIN, skewty(1050))
	TEMPMAX<- (tempRangeF[2] - 32)*(5/9)
	xmax <- skewtx(TEMPMAX, skewty(1050))
	#---------------------------------------------------------------------
  #creates plotting region
	plot(c(0,1), c(0,1),
	     type = "n",
	     axes = FALSE, 
	     xlab = "",
	     ylab = ""
	     )
   par( usr= c( xmin, xmax,  ymax, ymin))
##############################################################
# temp  (F) and pressure (hP) axes but in the skewt coordinates
##############################################################
   addAxes()

	#---------------------------------------------------------------------
	# --- DRAW HORIZONTAL ISOBARS., LABEL VERTICAL AXIS
	#---------------------------------------------------------------------
   horizontalIsobars()
	#---------------------------------------------------------------------
	# --- DRAW DIAGONAL ISOTHERMS.
	#---------------------------------------------------------------------
   diagonalIsotherms(xmin, xmax)
	#---------------------------------------------------------------------
	# --- DRAW SATURATION MIXING RATIO LINES.  
	# --- These lines run between 1050 and 400 mb. The 20 line intersects 
	# --- the sounding below 400 mb, thus a special case is made for it.  
	# --- The lines are dashed.  The temperature where each line crosses 
	# --- 400 mb is computed in order to get x,y locations of the top of
	# --- the lines.
	#---------------------------------------------------------------------
   saturatedMixingLevels()
	#---------------------------------------------------------------------
	# --- DRAW DRY ADIABATS.  
	# --- Iterate in 10 mb increments to compute the x,y points on the
	# ---  curve.
	#---------------------------------------------------------------------
	dryAdiabats(xmin, xmax, GREEN)
	# 
        # Most of the time, the only thing that needs to be returned by the 
        # routine is the plot boundaries so we know where to put the wind 
        # plot. However, if you are redrawing the curves, you need to be 
        # able to save the new curve data.
	# 
	# invisible(list(pseudox=holdx, pseudoy=holdy, pseudo=pseudo, 
  #               NPSEUDO=NPSEUDO, plt=par()$plt))
}

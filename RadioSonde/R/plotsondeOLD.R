plotsonde <- 
function (dataframe, skewT=TRUE, winds=FALSE, site = "", title = "", 
            windplot = NULL, s = 3., col = c("black", "red"), addAltitude=TRUE,
          mar.skewt = c(5.1, 5.1 , 
                         2.1 , 5.1), ... ){
#
# Copyright 2001,2002 Tim Hoar, Eric Gilleland, and Doug Nychka
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

    msg <- deparse(match.call())
 
###################################
#  BOTH PLOTS
###################################    
    if( skewT & winds){

       #
       # Plot the SKEW-T, log p diagram and the wind profile.
       #

       # Need some room for both the skewT plot and the wind profile.

       
       
       skewt.plt <- skewt.axis(mar = mar.skewt)$plt
    
       title(title)
       
       if(is.null(windplot)) {
# automatic choice for placement of wind plot          
                windplot <- skewt.plt # same vertical plot coodinates as the skewT
                windplot[1] <- 0.8
                windplot[2] <- 0.95

       } 

       first.par <- par()

       # Draw the SKEW-T, log p diagram
       # Draw background and overlay profiles

       skewt.axis(BROWN = "brown3", GREEN = "green4", redo = FALSE)
       if( addAltitude &  !is.null( dataframe$alt[1]) ){
         require(fields)
         
         y <- skewty(dataframe$press)
         altKm<- dataframe$alt/1e3
         km<- 1: round( max(altKm))
         fit<- sreg( altKm, y, lambda=1e-8)
         yKm<- predict( fit, km)
         xmin <- skewtx(-33, skewty(1050))
         axis(2, line=3, at = yKm, labels = format( km), las=2, cex=.8,
              col="grey40")
       }
       skewt.lines(dataframe$temp,  dataframe$press, col = col[1], ...)
       skewt.lines(dataframe$dewpt, dataframe$press, col = col[2], ...)
       
       #
       # Draw the windplot in the "space allocated"
        par(new = TRUE, pty = "m", plt = windplot, err = -1.)
        plotwind(dataframe = dataframe, size = s, legend = FALSE)
        par(plt = first.par$plt,
            mar = first.par$mar,
            new = FALSE, 
            pty = first.par$pty,
            usr = first.par$usr
            )
    } 
###################################
#  JUST SKEWT PLOT
###################################     
    
    if( skewT & !winds) {

       #
       # Draw the SKEW-T, log p diagram
       # Draw background and overlay profiles
       #

       skewt.axis(BROWN = "brown", GREEN = "green", redo = FALSE)
      
      if(  !is.null( dataframe$alt[1]) ){
        require(fields)
        y <- skewty(dataframe$press)
        altKm<- dataframe$alt/1e3
        km<- 1: round( max(altKm))
        fit<- sreg( altKm, y, lambda=1e-8)
        yKm<- predict( fit, km)
        xmin <- skewtx(-33, skewty(1050))
        axis(2, line=3, at = yKm, labels = format( km), las=2, cex=.8,
             col="grey40")
      }
      
       skewt.lines(dataframe$temp,  dataframe$press, col = col[1], ...)
       skewt.lines(dataframe$dewpt, dataframe$press, col = col[2], ...)
       title(title)

    } 
###################################
#  JUST WIND PLOT
###################################     
    
    if( !skewT & winds) {

       #
       # Draw the Wind profile only
       #
       plotwind(dataframe=dataframe, ...)
       title(title)

    }  # end of if else stmts

    invisible()
}

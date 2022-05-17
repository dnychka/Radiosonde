"plotWind" <-
function(sondeData, sizeBarb = NULL, ylim = c(1050, 100),
         thin=NULL, legend = FALSE, lwd=1, col="green4",
         axis=TRUE)
{
# skip some pressure levels if there are too many  
  if( is.null(thin)){
    M<- nrow(sondeData)
    if( M >= 80){
    thin<- round(M/80)
    }
    else{
      thin <- 1
    }
  }
#  
	good<- !is.na(sondeData$press)
	dataframe<- sondeData[good,]
	y <- skewty(dataframe$pres)
	ylim2 <- skewty(ylim)
#	
	theta <- dataframe$dir
	speed <- round(  dataframe$wspd, digits = 0.)
# dummy plotting region to get things going.	
	plot(c(-1, 1), c(0,1),
	     axes = FALSE, type = "n",
	     xlab = "", ylab = "")
#
# reset vertical so it is exactly from 1050 to 100
	par( usr= c( -1,1,ylim2) )
# add the y axis 
	if( axis){
	  addAxes()
	}
# maximum size of wind barbs
	if( is.null(sizeBarb)){
	  numChar<- 2/ par()$cxy[1] 
	  # 2 is from -1,1 in x dimension
	  # number of characters spanning the range -1 to 1
	  # barb size default at 1/3 of the spanning characters.
	  sizeBarb<- numChar/3
	}
	
	lines( c( -.8, .8), c(ylim2[1],ylim2[1]),
	                     col="grey", lwd=3)
	lines( c( -.8, .8), c(ylim2[2],ylim2[2]),
	      col="grey", lwd=3)
	
	
	if(legend) {
		mtext("full barb = 10 m/s", side = 1, line = 1)
	}

	lines(c(0,0 ), ylim2, lwd=4, col="grey")
# loop through thinned pressure levels. 	
	for(k in seq( 1, length(y), by=thin) ) {
		if(y[k] > ylim2[2])
			break
		if( !is.na(speed[k]) ) {
  	station.symbol(0., y[k], speed = speed[k], direction = 
					theta[k], circle=FALSE, cex = sizeBarb,
					spdcolor=col, lwd=lwd)
		}
	}
}

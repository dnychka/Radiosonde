skewtPlot <- 
  function ( ...,  
                 winds = FALSE,
               colTemp = c( "red2", "blue2"),
                colDew = colTemp,
                lty = c( 1,1),
                lwd = 1.5,
             skewplot = NULL,
             windplot = NULL, 
             windAxis = FALSE, 
             sizeBarb = NULL, 
                 thin = NULL,
            mar.skewt = c(3.1, 5.1 ,
                          4.1 , 2.1)
            ) {

# save previous values for graphics parameters
    def.par0 <- par(no.readonly = TRUE)
# the data to plot    
    sondeData<- list(...) 
    L<- length( sondeData )
# repeat the colors and line types if needed    
    if( length(colDew)<L){
    colDew<- rep(colDew,L)}
    if( length(colTemp)<L){
      colTemp<- rep(colTemp,L)}
    
# if wind barbs are to be added divide up plotting region 
    if( winds){
      magicRatio<- 15
      if( is.null( skewplot)){
      skewplot = c( 0, magicRatio/(magicRatio + L), 0,1)
      }
      
      windplot<- makeWindPlot( windplot, L, magicRatio)
# margins for windplot next to have the same vertical ones      
      mar.windplot<- c(mar.skewt[1], 0, mar.skewt[3], 0)
      par(  fig = skewplot )
      print( skewplot)
      par( mar = mar.skewt )
    }
# standard plot background that does not depend on data    
# draw base skewt plot with the reference lines 
# and pressure and temperature axes    
# default for temperature range is -20, 104 F
    setupSkewt()
#    box(col="blue1", lwd=4)
   
##############################################################
# add altitude axis if altitude info is present
##############################################################
    if( !is.null( (sondeData[[1]])$alt[1]) ){
      altitudeAxis( sondeData[[1]] )
    }
##############################################################
# overlay  data
##############################################################
  for( k in (1 : L) ){
    # temperature
    hold<- sondeData[[k]]
    skewt.lines(hold[,"temp"],  hold[,"press"],
                col = colTemp[k], lty = lty[1], lwd=lwd)
    # dew point temperature
    skewt.lines(hold$dewpt, hold$press,
                col = colDew[k],  lty = lty[2] , lwd=lwd)
  }
   print( par()$fig)
#
##############################################################
# add wind barbs if this is asked for
##############################################################
   print( windplot)
   
   
    if (winds) {
      # Draw the windplot in the "space allocated"
      for( k in (1 : L) ){
      par(new = TRUE,  fig = windplot[k,], err = -1.)
       
        
       
      # margins need to be the same in vertical so pressure scales match
      par( mar=mar.windplot )
      hold <-  sondeData[[k]]
# only draw axis if it is the first one and windAxis is TRUE      
      addAxes<- ifelse( k==1, TRUE, FALSE) & windAxis
      
      plotWind( hold,
              sizeBarb = sizeBarb,
                legend = FALSE, thin=thin, col=colTemp[k],
               lwd=.5,
               axis=addAxes)
      }
      # restore the original graphics parameters      
      par(def.par0)   
    }
  }

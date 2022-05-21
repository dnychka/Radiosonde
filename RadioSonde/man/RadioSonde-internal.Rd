\name{RadioSonde internal }

\alias{addAxesSkewt}
\alias{altitudeAxis}
\alias{diagonalIsotherms}
\alias{dryAdiabats}
\alias{horizontalIsobars}
\alias{makeWindPlot}
\alias{satlft}
\alias{saturatedMixingLevels}
\alias{setupSkewt}
\alias{skewt.data}
\alias{skewtx}
\alias{skewty}
\alias{tda}
\alias{tmr}
\alias{testP}
\alias{wobf}

\title{
  RadioSonde internal and secondary functions
}
\description{
Listed below are supporting functions for the major methods in RadioSonde.
}
\usage{

addAxesSkewt()

altitudeAxis(sondeData)

diagonalIsotherms(xmin, xmax)

dryAdiabats(xmin,xmax, GREEN)

horizontalIsobars()

makeWindPlot(windplot, L, magicRatio = 4) 

satlft(thw, p)
setupSkewt(BROWN = "brown3", GREEN = "green4", redo = FALSE,
                 tempRangeF = c(-20, 104), ...)

skewtx(temp, ycoord)

skewty(pres)

tda(o, p)

tmr(w, p)
testP( theta, target= 18.9)
wobf(temp)


}

\keyword{internal}
\keyword{misc}

\name{skewtPlot}
\alias{skewtPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A skewt plot wind wind barbs.
}
\description{
Creates a SKEW-T, log p diagram with dry-bulb temperature and 
   dewpoint temperature traces versus (log) pressure. Optionally plots
   the vertical wind profile using wind barbs.

}
\usage{
skewtPlot(
..., winds = FALSE, colTemp = c("grey40", "blue2"),
                 colDew = colTemp, lty = c(1, 1), lwd = 1.5, windplot =
                 NULL, windAxis = FALSE, sizeBarb = NULL, thin = NULL,
                 magicRatio = 13, skewplot = NULL, mar.skewt = c(3.1,
                 5.1, 4.1, 2.1)
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
  One or more data frames where rows indexes the time ( or height) and the columns are the variables measured.
   The variables must be names as:  \code{c( "press", "temp",  "dewpt")} corresponding to pressure (hPa), temperature (C) and dew point temperature (C).
  

}
  \item{winds}{
  If TRUE will add wind barbs to the side of the skewt plot. 
%%     ~~Describe \code{winds} here~~
}
  \item{colTemp}{
Colors(s) to plot temperature. 
}
  \item{colDew}{
  Colors(s) to plot dew point temperature.

}
  \item{lty}{
  Line type for the plotted curves. 

}
  \item{lwd}{
Line width for the curves. 
}
  \item{skewplot}{ Vector of 4 figure coordinates that define the skewt plot portion of figure.
  By default \code{c( 0, magicRatio/(magicRatio + L), 0,1)}.
  

}
  \item{windplot}{ A  matrix of the 4 figure coodinates where each row
  defines the figure region for the wind barbs.
}
  \item{windAxis}{If TRUE include a height axis alongside the wind barbs. 

}
  \item{sizeBarb}{Controls relative size of the wind barbs. 
}
  \item{thin}{ Total number of equally select3ed wind barbs to plots.
  This avoids too much overplotting.

}
  \item{mar.skewt}{
  Margins for the labels of the skewt plot. 
}
  \item{magicRatio}{Relative space in horizontal for the skewt plot
  and the panel with vertical wind barbs.
}

}

\details{

We recommend that the plots be drawn using the the pdf output devide with
a 8 by 6 inch size. 

}
\references{ 
	1.  Department of Defense, 1969, "USAF SKEW-T, log p DIAGRAM,"
	DOD-WPC-9-16-1, Aeronautical Chart and Information Center,
	United States Air Force, St. Louis, Missouri 63118.

	2.  List, R.J. (editor), 1958, \emph{Smithsonian Meteorological Tables},
	Smithsonian Institute, Washington, D.C.

	3.  Nordquist, W.S., 1973, "Numerical Approximations of Selected
	Meteorological Parameters for Cloud Physics Problems," ECOM-5475,
	Atmospheric Sciences Laboratory, US Army Electronics Command,
	White Sands Missile Range, New Mexico 88002.

	4.  Stipanuk, G.S., 1973, "Algorithms for Generating a SKEW-T, log p
	Diagram and Computing Selected Meteorological Quantities," American
	Sciences Laboratory, US Army Electronics Command, White Sands Missile
	Range, New Mexico 88002.

	5. \url{http://www.atd.ucar.edu/dir_off/tc_corr/index.html}

}
\author{  Doug Nychka, Eric Gilleland, Tim Hoar}
\seealso{ 
          \code{\link{skewt.lines}} }

\examples{


data(sonde1)
# skewt with wind barbs. 

skewtPlot( sonde1, winds=TRUE)

# mulitple profiles

data(sonde2)
skewtPlot( sonde1, sonde2)

}

\keyword{ hplot }

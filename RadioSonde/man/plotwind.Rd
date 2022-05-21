\name{plotWind}
\alias{plotWind}
\title{ Winds Profile Plot }
\description{ Creates a wind profile in the standard atmospheric 
              notation. Each full barb = 10 m/s, half barb = 5 m/s, 
              and a filled triangle for 50 m/s.  The maximum wind 
              speed that can be plotted without amending the program 
              is 65 m/s. }

\usage{
plotWind(sondeData, sizeBarb = NULL, ylim = c(1050, 100),
         thin=NULL, legend = FALSE, lwd=1, col="green4",
         axis=TRUE)
	 }

%- maybe also `usage' for other objects documented here.

\arguments{
  \item{sondeData}{ Data frame for sounding data, must have components for 
                    wind speed \code{wspd}, wind direction \code{dir}, 
                    and pressure \code{press}.}
  \item{sizeBarb}{change the thickness of the plotted lines}
  \item{thin}{ Max number of wind barbs to draw. }
  \item{axis}{If TRUE add an axis in pressure. }
  \item{col}{Color of barbs}
  \item{lwd}{Line width for barbs}
  
  \item{ylim}{Plot extent, in units of pressure. Note this is a reversed axis. }
  \item{legend}{explains wind barb convention}
}

\details{ the dataframe must have components for wind speed \code{wspd}, 
          wind direction \code{dir}, and pressure \code{press}.  
          Missing values may be coded as either NA or 999. and are 
          not plotted.\cr
          The standard atmospheric wind sybmol is a vector of fixed length 
          with barbs proportional to wind speed. A full barb for each 10 m/s,  
          half barbs for 5 m/s and a triangular barb for 50 m/s. }

\value{None -- creates a plot.}

\author{  Doug Nychka, Eric Gilleland}

\seealso{ \code{\link{skewtPlot}},  \code{\link{station.symbol}}  }

\examples{
# Example 1:
data(sonde1)

plotWind(sonde1, sizeBarb =1.0, legend=TRUE )

}
\keyword{ hplot }          % at least one, from doc/KEYWORDS

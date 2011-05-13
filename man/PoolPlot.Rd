\name{PoolPlot}
\alias{PoolPlot}

\title{Draws a 4-plot graph of pools}

\description{
  An exploratory function that draws the lateral surface area, volume, dry-matter biomass, and carbon biomass over time.
}

\usage{
PoolPlot(SA)
}

\arguments{
	\item{SA}{a \code{data.frame} holding the stem.analysis table.}
}

\details{
Lateral surface area is plotted as m$^2$, volume as m$^3$. The units of measure of dry-matter biomass and carbon biomass depend on the unit of measure of the user-given wood-density values. Default unit of measures are Mg (t) for the former and MgC (tC) for the latter.
}

\seealso{\code{\link{StemAnalysis}}}

\author{Marco Bascietto}

\examples{
# Load and attach Lei301 data
data(Lei301)
attach(Lei301)

PoolPlot(SA.1.a)
detach(Lei301)

}

\keyword{aplot}

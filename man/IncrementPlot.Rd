\name{IncrementPlot}
\alias{IncrementPlot}

\title{Draws a 4-plot graph of increments}

\description{
  An exploratory function that draws the lateral surface area, volume, dry-matter biomass, and carbon biomass increment over time.
}

\usage{
IncrementPlot(SA)
}

\arguments{
	\item{SA}{a \code{data.frame} holding the stem.analysis table.}
}

\details{
Lateral surface area increment is plotted as m$^2$/yr, volume increment as m$^3$/yr. The units of measure of increment of dry-matter biomass and carbon biomass depend on the unit of measure of the user-given wood-density values. Default unit of measures are Mg/yr (t/yr) for the former and MgC/yr (tC/yr) for the latter.
}

\seealso{\code{\link{StemAnalysis}}}

\author{Marco Bascietto}

\examples{
# Load and attach Lei301 data
data(Lei301)
attach(Lei301)

IncrementPlot(SA.1.a)

detach(Lei301)
}

\keyword{aplot}

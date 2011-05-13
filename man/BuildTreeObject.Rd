\name{BuildTreeObject}
\alias{BuildTreeObject}

\title{Builds a data.frame holding tree data}

\description{
  Builds a \code{data.frame} holding conversion coefficients and tree physical properties including. 
The creation of this object is a mandatory step before any call to \code{\link{StemAnalysis}} and to \code{\link{BuildAgeTable}}. It is passed as an argument to both the above functions.
}

\usage{
BuildTreeObject(tom = 1000000, tocm = 10000, vols = 1.0635, vols.ste = 0.0, basd = 0.51783, basd.ste = 0.04281, carb = 0.4577,carb.ste = 0.00243)
}

\arguments{
	\item{tom}{a numeric value holding the multiplicative coefficients to get to [m] from ring-width measures. This is used in \code{\link{StemAnalysis}}.}
	\item{tocm}{a numeric value holding the multiplicative coefficients to get to [cm] from ring-width measures. This is used in \code{\link{BuildAgeTable}}.}
	\item{vols}{a numeric value holding the multiplicative coefficients to get to fresh stem volume [m$^3$] from the computed volume.}
	\item{vols.ste}{a numeric value holding the standard error of the multiplicative coefficients to get to fresh stem volume [m$^3$] from the computed volume.}
	\item{basd}{a numeric value holding the wood basic density (\emph{i.e.} dry-matter wood weight over fresh wood volume).}
	\item{basd.ste}{a numeric value holding the standard error of wood basic density.}
	\item{carb}{a numeric value holding the amount of carbon stored in wood. It is expressed as a [0..1] number.}
	\item{carb.ste}{a numeric value holding the standard error of the amount of carbon stored in wood. It is expressed as a [0..1] number.}
}

\details{
Volume shrinks from wet wood to dry wood. if RWs are measured at room temperature and humidity, the computed volume must be expanded to fresh volume
If not measured, fetch the values for volume reduction in the literature (see below). If ring-widths are measured at room temperature and humidity, the tabled shrink coefficient has to be halved:
\emph{Picea abies} shrinkage coefficient from dry-matter to fresh volume is 1.127. (1.127-1)/2+1 becomes 1.0635. As a result, the computed volume of \emph{Picea abies} must be expanded of 6.35%
}

\seealso{\code{\link{StemAnalysis}}\code{\link{BuildAgeTable}}}

\note{
\code{BuildTreeObject} is just a wrapper for \code{data.frame}.
}

\references{
Nardi Berti R., La struttura anatomica del legno ed il riconoscimento dei legnami italiani di più comune impiego, Contributi Scientifico-Pratici per una migliore conoscenza ed utilizzazione del legno, CNR 24 (1979) 117-155.\cr
in  Bascietto M., Scarascia-Mugnozza G., A collection of functions to determine annual tree Carbon increment via stem analysis, Annals of Forest Science (accepted)\cr
Giordano, G., Variazioni della densità del legno, in: Tecnologia del legno,UTET,Torino, Italy, pp. 832-869.
}

\author{Marco Bascietto}

\examples{
# Build a tree object
Tree.features.1 <- BuildTreeObject(vols=1.085, vols.ste=0.0, basd=0.5792, basd.ste=0.016, carb=0.4424,carb.ste=0.0011)
}

\keyword{attribute}


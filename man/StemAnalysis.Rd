\name{StemAnalysis}
\alias{StemAnalysis}

\title{Performs stem-analysis}

\description{
  Performs stem-analysis on ring-widths on stem cross-sections.
}

\usage{
StemAnalysis(RWdata, Tree.height=23.91, Tree.object)
}

\arguments{
	\item{RWdata}{a numeric value holding the multiplicative coefficients to get to [m] from ring-width measures. This is used in \code{\link{StemAnalysis}}.}
	\item{Tree.height}{a numeric value holding the height of the tree tip.}
	\item{Tree.object}{a \code{data.frame} holding tree data. This object is easily created through \code{\link{BuildTreeObject}}.}
}

\details{
The function first interpolates tree heights using the Carmean's algorithm, to yield past annual estimates of tree height.
Yealry log volumes are computed using the Smalian's formula (trapezium formula): \deqn{V=\frac{Gl+Gu}{2} (Hu-Hl)}{V=[(Gl+Gu)/2] * (Hu-Hl)} where Gl if basal area of the lower section, Gu is the basal area of the upper section, Hu is the height of the upper section, Hl is the height of the lower section.
The volume of the terminal cone is: \deqn{V=\frac{Gl}{3} (Htree-Hl)}{V=Gl/3*(Htree-Hl)} where Htree is the height of the tree.
Dry-matter biomass is computed by converting volume to weight by means of basic density. Carbon biomass is given by dry-matter biomass multiplied by wood carbon ratio.
The standard errors of volume coefficient, basic density and wood carbon ratio are combined using the standard formula of multiplicative error propagation for variables with uncorrelated variances:  \deqn{se\left(W_C\right)=W_C \cdot \sqrt{\left(\frac{se\left(S_V\right)}{S_V}\right)^2 + \left(\frac{se\left(D_W\right)}{D_W}\right)^2 + \left(\frac{se\left(R_C\right)}{R_C}\right)^2}}{se(Wc)=Wc[(se(Sv)/Sv)^2+(se(Dw)/Dw)^2+(se(Rc)/Rc)^2]^0.5} where se() is standard error, Wc is wood carbon biomass, Sv is the volume coefficient from fresh matter to mean water-content matter, Dw is wood basic density, and Rc is wood carbon ratio.
Yearly lateral surface area of the logs is: \deqn{L=\pi (Ru+Rl) (Hu-Hl)}{L=pi * (Ru+Rl) * (Hu-Hl)} where Ru and Rl are the radia on the upper and lower cross-sections, Hu and Hl are the heights of the upper and lower cross-sections.for the lateral surface area of logs
The lateral surface area of the terminal cone is: \deqn{L=\pi Rl \sqrt{Rl^2 + (Htree - Hu)}}{L=pi * Rl * (Rl^2 + (Htree-Hl))^0.5}

Volume and surface area increments are computed by difference of the consecutive stem volumes.
Dry-matter biomass is computed by multiplying volume by basic density, carbon biomass is obtained by multiplyng dry-matter biomass by carbon content.
}

\seealso{\code{\link{BuildTreeObject}}\code{\link{BuildAgeTable}}}

\references{
Carmean W.H., Site index curves for Upland Oaks in the Central States, Forest Science 18 (1972) 109-120.\cr
Fabbio G., Frattegiani M., Manetti M.C., Il metodo di analisi del fusto. Confronto tra cinque metodi di stima della relazione altezza-età, Annali dell'Istituto Sperimentale per la Selvicoltura, Arezzo 19 (1988) 117-154.\cr
Bascietto M., Scarascia-Mugnozza G., A collection of functions to determine annual tree Carbon increment via stem analysis, Annals of Forest Science (accepted).
}

\author{Marco Bascietto}

\examples{
# Recall and attach RW data
data(Lei301)
attach(Lei301)

# Perform stem analysis
SA.1.a <- StemAnalysis(RWdata = RW.1A, Tree.height=12.33, Tree.object=Tree.features.1)
SA.1.b <- StemAnalysis(RWdata = RW.1B, Tree.height=5.5, Tree.object=Tree.features.1)

detach(Lei301)
}

\keyword{manip}


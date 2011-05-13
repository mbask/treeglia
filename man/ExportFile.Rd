\name{ExportFile}
\alias{ExportFile}

\title{Export an object into a csv text file}

\description{
  Saves a csv text file containing the object passed as argument.
}

\usage{
ExportFile(SAdata, filename, ...)
}

\arguments{
	\item{...}{any argument to be passed to \code{write.table}}
	\item{SAdata}{a \code{data.frame} object to be saved to disk, may be either a stem-analysis, age table, or whatever \R object.}
	\item{filename}{a string giving the name of the file to save}
}

\details{
The function creates a csv text file. Csv files can easily be openend by any spreadsheet program.
}

\seealso{\code{\link{ImportRW}}}

\note{
\code{ExportFile} is just a wrapper for \code{write.table}.
}

\author{Marco Bascietto}

\keyword{file}


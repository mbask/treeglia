\name{ImportRW}
\alias{ImportRW}

\title{Import a ring-width file}

\description{
  Loads a csv text file containing the ring widths measured on tree cross-sections.
}

\usage{
ImportRW(filename)
}

\value{
  Returns a data.frame object holding the ring-widths. Calendar years are coded as row names, and can be accessed via \code{rownames()}. 
Cross-section heights along the stem are coded column names, they can be accessed via \code{colnames()} and converted to numeric values via \code{as.numeric()}.
}

\arguments{
	\item{filename}{a string giving the name of the file to load}
}

\details{
Ring-widths and their calendar years, as well the heights of the cross-sections they were measured from must be stored in a csv file. Ring-widths will be ordered in columns, from the lowest cross-section, to the highest (nearest to tree tip) cross-section. 
The first row will hold cross-sections heights (in m). 
The first column will hold the calendar years whose ring refer.
Csv files can be created through any spreadsheet program. As an example of what a future ring-width csv file might look in a spreadsheet consider the following:
\tabular{ccccc}{
	\tab	0.0	\tab	1.30	\tab	3.00	\tab	5.00	\cr
1990	\tab	1234	\tab		\tab		\tab		\cr
1991	\tab	1234	\tab		\tab		\tab		\cr
1992	\tab	1234	\tab	4232	\tab		\tab		\cr
1993	\tab	1234	\tab	4232	\tab	353	\tab		\cr
1994	\tab	1234	\tab	4232	\tab	353	\tab		\cr
1995	\tab	1234	\tab	4232	\tab	353	\tab		\cr
1996	\tab	1234	\tab	4232	\tab	353	\tab	3567	\cr
1997	\tab	1234	\tab	4232	\tab	353	\tab	3567	\cr
1998	\tab	1234	\tab	4232	\tab	353	\tab	3567	\cr
}
}

\seealso{\code{\link{ExportFile}}}

\note{
Decimals should be dot-separated.
}

\author{Marco Bascietto}

\keyword{file}


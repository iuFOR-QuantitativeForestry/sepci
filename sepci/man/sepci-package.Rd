\encoding{UTF-8}
\name{sepci-package}
\alias{sepci-package}
\alias{sepci}
\docType{package}
\title{
Spatial Explicit Plant Competition Indices
}
\description{
 Computation of spatially explicit forest competition indices
}
\details{
\tabular{ll}{
Package: \tab sepci\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
License: \tab GPL\cr
}
%~~ An overview of how to use the package, including the most important functions ~~
The package is a extension to the \pkg{siplab} package. The main functions are competition
kernels for the \code{\link{pairwise}()} function in \pkg{siplab}. Some of the included kernels
are simple and ready to use out of the box (e.g \code{\link{bella}()}). Others are more complex
and need some preprocessing that can be eases through the use of helper functions also included
in the package (e.g the kernel \code{\link{crown_cross_fk}()} and the helper function \code{\link{ccs_cparea_cone}()})

A sample data set is included, see link below.
}
\author{
\enc{Andrés}{Andres} Bravo

Maintainer: A. Bravo <andres.bravo@uva.es>
%~~ The author and/or maintainer of the package ~~
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
%\keyword{ package }
\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
Example \pkg{sepci} data set: \code{\link{valdepoza}}

Some \pkg{siplab} and \pkg{spatstas} standard data sets may also be of interest: \code{\link{boreasNP}}, \code{\link{boreasNS}}, \code{\link{boreasSA}}, \code{\link{boreasSP}}, \code{finpines}, \code{longleaf}, \code{spruces}, \code{waka}.

For tutorials try the vignettes. E. g., in R type \code{help.start()} to open the help browser, and navigate to Packages > sepci > Vignettes.
}

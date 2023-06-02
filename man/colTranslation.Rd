\name{colTranslation}
\alias{colTranslation}
\docType{data}
\title{Color translation table}
\description{A data frame relating RHS codes to color names, color subnames, 
  HCL codes, and UPOV codes.}
\usage{data("colTranslation")}
\format{
  A data frame with 969 observations on the following 7 variables.
  \describe{
    \item{\code{RHS}}{a character vector}
    \item{\code{Name}}{a factor with levels corresponding to color names}
    \item{\code{Subname}}{a factor with levels corresponding to color
      subclasses }
    \item{\code{red, green, blue}}{coordinates of the RHS color in RGB space}
    \item{\code{hue, chroma, luminance}}{coordinates of the RHS color in
    HCL space}
    \item{\code{UPOV}}{a factor with UPOV codes for every RHS code.}
  }
}
\source{Naktuinbouw}
\examples{
data(colTranslation)
head(colTranslation)
}
\keyword{datasets}

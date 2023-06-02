\name{colorTransformations}
\alias{colorTransformations}
\alias{rgb2rhs}
\alias{rhs2rgb}
\alias{hex2rgb}
\alias{hex2colClass}
\alias{hcl2hex}
\alias{D2Ecol}
\title{Color transformations}
\usage{
rgb2rhs(rgb, colTable)
rhs2rgb(rhs, colTable)
hex2colClass(hexcols, colTable)
hcl2hex(hclmat)
D2Ecol(colname)
}
\description{Functions to convert colors to different
  representations. In the Modoma project we use color names (in Dutch
  and English), RHS codes, plain matrices of RGB or HCL values, and
  color objects from the colorspace package. For the translation to and
  from color names and/or RHS codes to coordinates in a color space a
  translation table is used.
}
\arguments{
  \item{rgb}{}
  \item{colTable}{Typically the \code{colTranslation} object, although
    also other similar data tables can be used.}
  \item{rhs}{An RHS code which is present in the colTranslation object.}
  \item{hexCols}{A color vector consisting of hexadecimal RGB codes.}
  \item{hclmat}{Either a three-column matrix (luminance, chroma, hue) or
    a colorspace object of class "polarLUV".}
  \item{colname}{A Dutch color name like "roze".}
}
\value{The functions return different types of objects - see the Details
  section.}
\details{Function rgb2rhs takes a matrix of RGB codes, calculates the
  Euclidean distance to all known RHS codes from the color table, and
  returns the closest one.

  Function rhs2rgb takes a list of rhs codes and translates that
  into a matrix of RGB values. Multiple RHS codes within one record
  are translated individually and then averaged. This leads to the
  possibility of non-integer RGB codes, which can easily be remedied
  by rounding.

  Function hex2rgb, when given a set of RGB colors in hexadecimal
  format, returns the rgb matrix. This function already exists in
  package colorspace (hex).

  Transforming a vector of hexadecimal RGB colors to color names is done
  through function hex2colClass.

  Function hcl2hex converts a matrix of HCL color coordinates into
  hexadecimal RGB codes.

  Function D2Ecol translates Dutch colors to their English
  equivalents. If a color is not recognized, a warning is printed.
}
\author{Ron Wehrens}
\seealso{
  \code{\link{colTranslation}}
}

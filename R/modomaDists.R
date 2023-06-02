#' @export
mydist <- function(x, ...) UseMethod("mydist")

#' @export
mydist.default <- function(x, ...) dist(x, ...)

#' mydist.ordered
#'
#' for ordered factors special care is needed in case NA values are
#' present. Some factor levels may be missing, but I do take them into
#' account here. This is one way to provide information about level
#' spacings: simply add a number of levels in between that are not
#' used. Missing values are replaced by the maximum possible
#' distance. In an ordered factor with three levels, for example, the
#' maximum distance of the middle level to an NA is 1, the maximum
#' distance of the two extreme levels to an NA is 2. One could make a
#' case of imputing by half the maximum distance, which can be done by
#' simply dividing all elements in imputevec by 2. Let's see...
#'
#' @export
mydist.ordered <- function(x, ...) {
  if (sum(is.na(x)) == 0) return(dist(as.numeric(x), ...)) # easy

  xn <- as.numeric(x)
  imputevec <- apply(cbind(xn - 1, nlevels(x) - xn), 1, max)
  imputevec[is.na(imputevec)] <- nlevels(x) - 1

  dmat <- as.matrix(dist(xn))
  idx <- which(apply(dmat, 1, function(xx) sum(!is.na(xx)) == 1))
  dmat[, idx] <- imputevec
  dmat <- t(dmat) # not the most efficient way, but hey, I'm lazy...
  dmat[, idx] <- imputevec

  as.dist(dmat)
}

#' @export
mydist.factor <- function(x, ...) {
  nclasses <- nlevels(x)
  outmat <- matrix(0, length(x), nclasses)
  dimnames(outmat) <- list(NULL, levels(x))
  for (i in 1:nclasses)
    outmat[which(as.integer(x) == i), i] <- 1

  idx <- which(is.na(x))

  result <- as.matrix(dist(outmat, ...))
  result[idx, idx] <- 1
  diag(result) <- 0

  as.dist(result)
}

#' @export
mydist.RHS <- function(x, ...) {
  data("colTranslation")
  rhscodes <- cleanRHS(x, colTranslation$RHS)
  rgbcols <- rhs2rgb(rhscodes, colTranslation)

  dmat <- as.matrix(dist(rgbcols))
  dmat[is.na(dmat)] <- sqrt(3 * 256^2)

  as.dist(dmat)
}

#' @export
mydist.color <- function(x, ...) {
  as.dist(matrix(0, length(x), length(x)))
}


#' rgb2rhs
#'
#' Function rgb2rhs takes a matrix of RGB codes, calculates the Euclidean
#' distance to all known RHS codes from the color table, and returns
#' the closest one
#'
#' @noRd
#' @export
rgb2rhs <- function(rgb, colTable) {
  RGB <- data.matrix(colTable[, c("red", "green", "blue")])
  rgb <- data.matrix(rgb)

  idx <- sapply(1:nrow(rgb),
                function(ii)
                  which.min(colSums((t(RGB) - rgb[ii,])^2)))

  colTable$RHS[idx]
}

#' cleanRHS
#'
#' Function cleanRHS removes all non-RHS items from the input vector
#' RHS codes are basically a combination of an optional letter (or
#' even two), a number of 1-3 digits, and a letter. Sometimes two
#' codes are indicated as 56 C-D
#'
#' @export
cleanRHS <- function(rhsvec, rhs) {
  ## first split on the terms like "RHS", "and", "en", "tussen", some
  ## colours, and single characters. Very ad hoc!
  rhsvec.split <- strsplit(rhsvec, "ca.|RHS|NA|and|Between|dark|brown|more|much|between|Blue|tussen|lopend|en|ring|op|maar|geel|rose|roze|paars|oranje|light|orange|rood|red|purple|yellow|white|blue|pink|zalm|bruin|wit|uit|rand|met|naar|creme|basis|,|\\+|\\(|\\)|/|-|\\%|\\;|\\:")

  ## get rid of empty strings, remove leading zeros and spaces, and
  ## convert to upper
  rhsvec.split2 <- lapply(rhsvec.split,
                          function(x){
                            y <- x[x != "" & x != " "]
                            y <- sub("^0+", "", y)
                            toupper(gsub("[[:space:]]", "", y))
                          })

  ## Check: N030A should remain N030A!
  removeLeadingZeros <- function(strset) {
    ## split into character and digit parts
    setelements <-
      strsplit(strset,
               "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])",
               perl=TRUE)
    ## remove leading zeros everywhere
    setelements2 <- lapply(setelements,
                           function(id) sub("^0+", "", id))
    ## put everything back together again
    sapply(setelements2, paste, sep = "", collapse = "")
  }

  rhs2 <- removeLeadingZeros(rhs)
  list2 <- lapply(rhsvec.split2,
                  function(x) match(removeLeadingZeros(x), rhs2))

  lapply(list2, function(xx) rhs[xx])
}

#' modocol2rgb
#'
#' standard R function col2rgb gives the translation for named colours
#' in R - several color indications in the naktuinbouw data are
#' composite. If all elements are individual colours then the average
#' RGB is returned, if words like light or dark are present then the
#' lighter or darker shades of the corresponding colours are used.
#'
#' modocol2rgb takes one string, and a vector of sepchars
#'
#' @examples
#' modocol2rgb(c("dark yellow dark purple red"))
#' modocol2rgb(c("dark yellow", "dark purple", "red", "dark whatever"))
#' modocol2rgb("dark yellow")
#'
#' @export
modocol2rgb <- function(colstr, sepchars = c(" ", "_")) {
  colstr <- tolower(colstr) ## you never know...
  ## separate any composite strings into words
  sepchars <- paste("[", paste(sepchars, sep = "", collapse = ""),
                    "]+", sep = "", collapse = "")
  indivCols <- strsplit(colstr, split = sepchars)

  for (ii in 1:length(indivCols)) {
    ## merge dark and light to subsequent color
    dl.idx <- match(indivCols[[ii]], c("dark", "light"))
    idx <- which(!is.na(dl.idx))
    if (length(idx) > 0) {
      indivCol2 <- indivCols[[ii]]
      for (jj in idx) {
        indivCol2[jj + 1] <-
          paste(indivCols[[ii]][jj], indivCols[[ii]][jj + 1], sep = "")
        tmprgb <- try(col2rgb(indivCol2[jj + 1]), silent = TRUE)
        if (class(tmprgb)[1] == "try-error") {
          suffix <- switch(indivCols[[ii]][jj],
                           dark = "4",
                           "1")
          indivCol2[jj + 1] <- paste(indivCols[[ii]][jj + 1], suffix, sep = "")
        }
      }

      indivCols[[ii]] <- indivCol2[!(indivCol2 %in% c("light", "dark"))]
    }
  }

  allRGBs <-
    lapply(indivCols,
           function(xx)
             lapply(xx, function(yy) try(col2rgb(yy), silent = TRUE)))
  names(allRGBs) <- colstr

  ## get all errors
  conversion.errors <-
    lapply(allRGBs,
           function(xx) which(sapply(xx, class) == "try-error"))

  error.idx <- sapply(conversion.errors, length)
  if (!all(error.idx == 0)) {
    warning("Invalid color name(s): ",
            paste(
              names(conversion.errors[sapply(conversion.errors, length) > 0]),
              collapse = ", "))
    allRGBs[error.idx > 0] <- NA
  }

  t(sapply(allRGBs,
           function(xx)
             if (any(is.na(xx))) {
               rep(NA, 3)
             } else {
               if (is.list(xx)) {
                 round(rowMeans(do.call(cbind, xx)))
               } else {
                 xx
               }
             }))

}

#' D2Ecol
#'
#' Dutch colour names need to be translated to English ones
#'
#' @noRd
#' @export
D2Ecol <- function(colstr) {
  colstr <- tolower(colstr)

  dcols <- c("bruin", "geel", "groen", "oranje", "paars", "rood",
             "roze", "wit", "blauw", "grijs", "lichtgroen")
  ecols <- c("brown", "yellow", "green", "orange", "purple", "red",
             "pink", "white", "blue", "gray", "lightgreen")

  idx <- match(colstr, dcols)
  if (any(is.na(idx)))
    warning("Unknown color(s): ", paste(colstr[is.na(idx)], collapse = ", "))

  ecols[idx]
}

#' @export
dfdist <- function(dframe, weights = 1, individual = FALSE,
                   scaling = c("range", "none"),
                   maxNAfrac = 0.5) {
  ## should also work on simple vectors
  if (!is.list(dframe)) dframe <- list(dframe)

  result <- lapply(dframe, mydist)
  if (individual) return(result)

  scaling <- match.arg(scaling)
  if (scaling == "range") {
    result2 <-
      do.call(cbind,
              lapply(result, function(ii) 100*ii/max(ii, na.rm = TRUE)))
  } else {
    result2 <- do.call(cbind, result)
  }

  N <- ncol(result2)
  if (length(weights) == 1) weights <- rep(1/N, N)
  if (length(weights) != N)
    stop("A weight is needed for each trait in dframe")

  mymerge <- function(x, wghts) {
    idx <- !is.na(x)
    if (sum(idx) < 0.5*N) return(NA)
    sum(x[idx] * wghts[idx]) / sum(wghts[idx])
  }

  mergeddist <- apply(result2, 1, mymerge, weights)
  attributes(mergeddist) <- attributes(result[[1]])

  mergeddist
}

#' @export
getClosestPair <- function(indat,
                           idCol) {

  columnMeta <- read.csv("metadata/metadataColumns.csv")
  orderedFactMeta <- read.csv("metadata/metadataOrderedFactors.csv")
  nonTraits <- columnMeta[columnMeta$trait == "No", "colname"]

  dmat <- as.matrix(dfdist(indat[, !colnames(indat) %in% nonTraits]))
  dimnames(dmat) <- list(indat[, idCol], indat[, idCol])

  minIdx <- which.min(dmat[dmat > 0])
  minVal <- dmat[dmat > 0][minIdx]
  closest <- rownames(which(dmat == minVal, arr.ind = TRUE))

  indat[indat[[idCol]] %in% closest, ]
}

#' getClosestRef
#'
#' Get n flowers closest to reference flower based on
#' selected subset of traits.
#'
#' @export
getClosestRef <- function(ref,
                          indat,
                          traits = NULL,
                          n = 1) {

  columnMeta <- read.csv("metadata/metadataColumns.csv")
  orderedFactMeta <- read.csv("metadata/metadataOrderedFactors.csv")
  nonTraits <- columnMeta[columnMeta$trait == "No", "colname"]

  isFlor <- hasName(indat, "VKCNr")
  refVar <- if (isFlor) "VKCNr" else "RVPnr"

  ## Checks.
  if (!ref %in% indat[[refVar]]) {
    stop("ref is not in column ", refVar, " in input file.\n")
  }
  if (is.null(traits)) {
    traits <- colnames(indat)[!colnames(indat) %in% nonTraits]
  } else {
    missTraits <- traits[!traits %in% colnames(indat)]
    if (length(missTraits) > 0) {
      stop("The following traits are not in the data:\n",
           paste(missTraits, collapse = ", "), "\n")
    }
  }

  idx <- which(indat[[refVar]] == ref)

  dmat <- as.matrix(dfdist(indat[, colnames(indat) %in% traits]))

  ## Add distance to data.
  indat$distance <- round(dmat[idx, ], 1)
  indat <- indat[, c(colnames(indat)[1], "distance", refVar,
                     setdiff(colnames(indat)[-c(1, ncol(indat))], refVar))]

  ## Assure ref is always first, even if multiple distances are 0.
  closest <- names(sort(dmat[idx, ]))
  closest <- c(idx, closest[closest != idx])
  closest <- closest[1:(n + 1)]
  idxMinDist <- as.numeric(closest)

  return(indat[idxMinDist, ])
}

#' getClosestTraits
#'
#' Get n flowers closest to reference flower based on
#' selected subset of traits.
#'
#' @export
getClosestTraits <- function(indat,
                             traitValues = NULL,
                             n = 1) {
  isFlor <- hasName(indat, "VKCNr")
  refVar <- if (isFlor) "VKCNr" else "RVPnr"

  ## Add fake observation to indat with traits we want to compare.
  newObs <- data.frame(id = "FAKEOBS",
                       traitValues,
                       check.names = FALSE)
  colnames(newObs)[1] <- refVar

  ## Restrict indat to traits we want to match on.
  matchDat <- indat[, colnames(newObs)]
  matchDat <- rbind(matchDat, newObs)

  ## Get closest flower by matching on reference.
  resDat <- getClosestRef(ref = "FAKEOBS", indat = matchDat,
                          traits = names(traitValues), n = n)

  ## Subset from input data to get all columns.
  ## Using merge keeps the order from the result intact.
  resDatTot <- merge(resDat[-1, c(refVar, "distance")], indat, sort = FALSE)
  resDatTot <- resDatTot[, c(colnames(indat)[1], "distance", refVar,
                             setdiff(colnames(indat)[-1], refVar))]

  ## Add trait values used for selection as first row.
  firstRow <- sapply(colnames(resDatTot), FUN = function(x) {
    if (x %in% names(traitValues)) {
      traitValues[[x]]
    } else {
      NA
    }
  })

  resDatTot <- rbind(firstRow, resDatTot)

  return(resDatTot)
}


## Create a simple data.frame for testing
# set.seed(7)
# nsample <- 20
# huhn <-
#   data.frame(width = sample(1:3, nsample, replace = TRUE),
#              species = factor(sample(c(LETTERS[1:3],NA), nsample,
#                                      replace = TRUE)),
#              size = factor(sample(c("small", "medium", "large", "NA"), nsample,
#                                   replace = TRUE),
#                            levels = c("small", "medium", "large"),
#                            ordered = TRUE))
#
# huhn
# dfdist(huhn)
# dfdist(huhn[1], scaling = "range") ## numeric
# dfdist(huhn[1], scaling = "none") ## numeric
# mydist(huhn[[1]]) # same as previous line
# mydist(huhn[[2]]) # factor
# mydist(huhn[[3]]) # ordered factor
# dfdist(huhn[3])
# dfdist(huhn[[3]])

# 0.25 * (2*mydist(huhn[[1]]) + mydist(huhn[[2]]) + mydist(huhn[[3]]))
# dfdist(huhn, weights = c(2, 1, 1), scaling = "none") # same as previous line
#
# ## numeric vector with NAs
# egcol <- data.frame(red  = as.numeric(c(NA,NA,0,0,0, 0,  255)),
#                     green = as.numeric(c(NA,0,0,0,0,  255,255)),
#                     blue = as.numeric(c(NA,0,0,0,255,255,255)))
# dfdist(egcol) # not what we want: we treat R, G and B separately
# dfdist(as.matrix(egcol)) # one distance for the RHS trait
# ## The difference is in the scaling: in the first line the distances
# ## for R, G and B are scaled separately, and in the matrix form one
# ## distance is calculated in three dimensions, which then is scaled.
#



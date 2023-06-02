## Todo:
## - segmentImage: fast = TRUE seems to be slower for a large image

#' showFlowerResult
#'
#' Function showing the original flower image on the left, the color
#' histograms of RGB and HCL approaches in the middle, and the ground
#' truth on the right
#'
#' @export
showFlowerResult <- function(floName, rgbSolution, hclSolution, dbcol,
                             mainColRGB = NULL, mainColHCL = NULL,
                             format = c("tif", "jpg")) {
  format <- match.arg(format)
  if (format == "tif") {
    flower <- readTIFF(floName)
  } else {
    flower <- readJPEG(floName)
  }

  layout(matrix(c(1, 1, 2, 3, 4, 4), ncol = 3), widths = c(3, 3, 1))

  opar <- par(mar = c(0, 0, 2, 0))
  plot(0:10, type = "n", xlab = "", ylab = "", axes = FALSE, asp = 1,
       main = basename(file_path_sans_ext(floName)))
  rasterImage(flower, xleft = 0, ybottom = 0, xright = 10, ytop = 10)

  par(opar)
  pal(rgbSolution$clcols, rgbSolution$dens$pro,
      main = "RGB", labels = FALSE, mainCol = mainColRGB,
      cex.main = 1.5)
  pal(hclSolution$clcol, hclSolution$dens$pro,
      colSpace = "polarLUV", main = "HCL", labels = FALSE,
      mainCol = mainColHCL, cex.main = 1.5)

  dbrgb <- do.call(rgb, list(dbcol/255))
  pal(dbrgb, 1, dbrgb, main = "Ground\ntruth", labels = TRUE,
      cex.main = 1.5)
}

#' showColTab
#'
#' Function to create a figure with coloured rectangles, one for every
#' colour in the colour table that is the first argument
#'
#' @export
showColTab <- function(colTab, nr, nc, nrmax = 10, ncmax = 10,
                       labels, cex = 1, ...) {
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mar = c(.3, .3, .3, .3))

  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
       xlab = "", ylab = "", ...)
  if (missing(nr))
    nr <- min(nrmax, ceiling(sqrt(nrow(colTab))))
  if (missing(nc))
    nc <- min(ncmax, ceiling(sqrt(nrow(colTab))))

  if (missing(labels)) labels <- 1:nrow(colTab)

  coords <- expand.grid(head(seq(0, 1, length = nc + 2)[-1], -1),
                        head(seq(1, 0, length = nr + 2)[-1], -1))
  blocksepc <- .45 / (nc + 1)
  blocksepr <- .45 / (nr + 1)

  coords <- coords[1:nrow(colTab),]
  rect(coords[,1] - blocksepc,
       coords[,2] - blocksepr,
       coords[,1] + blocksepc,
       coords[,2] + blocksepr,
       col = rgb(colTab[, "red"],
                 colTab[, "green"],
                 colTab[, "blue"], maxColorValue = 255))
  text(coords[,1], coords[,2],
       paste(labels,
             apply(colTab[, 1:2], 1, paste, collapse = " "),
             apply(colTab[, c("red", "green", "blue")], 1, paste,
                   collapse = " "),
             sep = "\n"),
       cex = cex)
}

#' pal
#'
#' Function to show a colour histogram, obtained by, e.g., clustering
#' image pixels. The first argument is an argument from class color,
#' (from colorspace) with color definition in a three-column matrix in
#' the coords element. Argument colSpace should be one
#' of the color-generating functions from package colorspace, or the
#' rgb() function. If the former, the color matrix will be transformed
#' into rgb colors. A bit clumsy, but the result of using two
#' different color representations: the S4 one from colorspace, and
#' the one from the regular graphics system.
#'
#' @export
pal <- function(col, heights, border = "light gray",
                colSpace = c("rgb", "RGB", "HLS", "HSV", "LAB", "LUV",
                             "polarLAB", "polarLUV"),
                mainCol = NULL, labels = is.null(mainCol), ...) {
  colSpace <- match.arg(colSpace)

  if (colSpace != "rgb") {
    myrgbmat <- as(col, "RGB")@coords
    myrgbmat[myrgbmat < 0] <- 0
    myrgbmat[myrgbmat > 1] <- 1
    col <- rgb(myrgbmat[,1], myrgbmat[,2], myrgbmat[,3])
  }
  n <- length(col)
  if (missing(heights)) heights <- rep(1, n)
  heights <- heights / max(heights)

  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
       xlab = "", ylab = "", ...)
  if (labels) {
    mtext(col, side = 1, at = 1:n/n - .5/n, cex = .8)
  } else {
    if (!is.null(mainCol))
      mtext(col[mainCol], side = 1, cex = .8, at = (mainCol - .5)/n)
  }
  rect(0:(n-1)/n, 0, 1:n/n, heights, col = col, border = border)
}

#' segmentImage
#'
#' Function to return the segmented colors in an image
#' Background pixels are supposed to be black, with an
#' intensity not more than minInt. To fit the mixture model we use
#' sampling. If the model is to be fitted on the complete data then
#' nsamp should be set to NULL.
#'
#' This version: the function that does all, analysis in RGB or HCL
#' space, kmeans (fast) or mclust. The function always takes a 3D
#' array that will provide the dimensions variable. Optimised somewhat
#' for speed: we avoid doing things on the matrix as a whole if it is
#' needed only on the sample.
#'
#' cspace is rgb leads to hexadecimal colors in clcols; any other
#' valid value for cspace leads to a three-column matrix.
#'
#' @export
segmentImage <- function(imagemat,
                         cspace = c("rgb", "RGB", "HLS", "HSV", "LAB", "LUV",
                                    "polarLAB", "polarLUV"),
                         minInt = 5, nsamp = 1000, quiet = FALSE,
                         segmentImage = TRUE, fast = TRUE, ...) {
  cspace <- match.arg(cspace)
  if (length(dim(imagemat)) != 3)
    stop("imagemat should be a three-dimensional array (an image)")

  dimensions <- dim(imagemat)[1:2]
  imagemat <- matrix(imagemat, prod(dim(imagemat)[1:2]), dim(imagemat)[3])

  if (ncol(imagemat) != 3)
    stop("imagemat should have R, G and B information")
  if (is.null(nsamp)) nsamp <- nrow(imagemat)

  ## If we do not need to provide segmentation for the whole image, we
  ## may just as well pick nsamp pixels directly and kick the rest
  ## out. To be sure we have enough foreground pixels we initially
  ## consider ten times the number of nsamp (or the whole image) and
  ## select the first nsamp pixels that correspond to foreground

  pixintensity <- apply(imagemat, 1, sum)
  idx <- which(pixintensity*255 > minInt)
  smpl <- sample(1:length(idx), nsamp)
  imagemat <- imagemat[idx,] # only containing foreground pixels

  if (cspace != "rgb" & cspace != "RGB") {
    imagemat <- as(RGB(R = imagemat[,1], G = imagemat[,2], B = imagemat[,3]),
                   cspace)@coords
  }

  if (fast) {
    densObj <- kmeans(imagemat[smpl,], centers = 9, nstart = 10)
    ccovs <- by(imagemat[smpl, ], list(densObj$cluster), cov)
    woppa <- list(mean = t(densObj$centers),
                  pro = densObj$size / sum(densObj$size),
                  variance = array(t(do.call(rbind, ccovs)),
                                     c(3, 3, length(ccovs))))

  } else {
    if (quiet) sink("/dev/null")
    densObj <- try(densityMclust(imagemat[smpl, ], ..., plot = FALSE))
    if (quiet) sink()
    if(inherits(densObj, "try-error")) return(NA)

    woppa <- summary(densObj, parameters = TRUE)
  }

  myfun <- get(cspace)
  clcols <- myfun(woppa$mean[1,], woppa$mean[2,], woppa$mean[3,])

  if (segmentImage) {
    if (fast) {
      predict.kmeans <- function(x, newdata)
        apply(newdata, 1, function(r) which.min(colSums((t(x$centers) - r)^2)))
      classif <- predict(densObj, newdata = imagemat)
    } else {
      huhn <- predict(densObj, what = "z", newdata = imagemat)
      classif <- apply(huhn, 1, which.max)
    }
    predicted <- t(woppa$mean[,classif])
    deviation <- sqrt(mean((imagemat - predicted)^2))

    if (cspace == "rgb") {
      clcolsrgb <- clcols
    } else {
      clcolsRGB <- as(clcols, "RGB")@coords
      clcolsRGB[clcolsRGB < 0] <- 0
      clcolsRGB[clcolsRGB > 1] <- 1
      clcolsrgb <- rgb(clcolsRGB[,1], clcolsRGB[,2], clcolsRGB[,3])
    }

    rgbcols <- rep(rgb(0, 0, 0), prod(dimensions))
    dim(rgbcols) <- dimensions
    rgbcols[idx] <- clcolsrgb[classif]

    list(dens = woppa, segImage = rgbcols,
         clcols = clcols, deviation = deviation)
  } else {
    list(dens = woppa, clcols = clcols)
  }
}

#' bhattaCol
#'
#' Calculate Bhattacharyya distances for the colour classes
#'
#' @export
bhattaCol <- function(densObj) {
  nClus <- length(densObj$pro)
  clusDist <- matrix(0, nClus, nClus)
  ## The try function should catch singular cases which otherwise lead to an error
  for (i in 2:nClus)
    for (j in 1:(i-1)) {
      huhn <-
        try(bhattacharyya.dist(densObj$mean[,j],
                               densObj$mean[,i],
                               densObj$variance[,,j],
                               densObj$variance[,,i]),
            silent = TRUE)
      if (inherits(huhn, "try-error") | !is.finite(huhn)) {
        clusDist[j, i] <- -1
      } else {
        clusDist[j, i] <- huhn
      }
    }

  clusDist[clusDist < 0] <- 100*max(clusDist)

  as.dist(clusDist + t(clusDist))
}

#' mainCol
#'
#' Given a colour histogram, return the main colour. We can restrict
#' the choice: include takes precedence over exclude. Returns the
#' number of the main color.
#'
#' @export
mainCol <- function(densObj, what = c("count", "both", "bright"),
                    hierClust = TRUE, hierCutree = 1.5,
                    include, exclude, plot = FALSE,
                    colorSpace = c("RGB", "HCL"), ...) {
  what <- match.arg(what)
  colorSpace <- match.arg(colorSpace)

  if (length(densObj$pro) == 1) return(1)

  if (hierClust) {
    cDists <- bhattaCol(densObj)
    cClus <- hclust(cDists, method = "single")
    if (plot) {
      plot(cClus, ...)
      abline(h = hierCutree, lty = 2)
      dev.new()
    }
    cClas <- cutree(cClus, h = hierCutree)
    classCounts <- aggregate(densObj$pro, list(cClas), sum)$x
    maxClass <- which.max(classCounts)
    return(mainCol(densObj, what = what, hierClust = FALSE,
                   plot = plot, include = which(cClas == maxClass)))
  }

  if (colorSpace == "HCL") {
    brightness <- densObj$mean[1,]
  } else { # RGB
    brightness <- colSums(densObj$mean)
  }
  counts <- densObj$pro
  names(counts) <- NULL
  N <- length(counts)

  if (missing(include) & missing(exclude)) {
    include <- 1:N
  } else {
    if (missing(include)) {
      include <- (1:N)[-exclude]
    }
  }

  if (plot) {
    pllabels <- 1:length(brightness)
    cols <- rep("gray", N)
    cols[include] <- "black"
    plot(brightness, counts, col = cols)
    text(brightness, counts, labels = pllabels, pos = 1, col = cols)
  }

  switch(what,
         count = include[which.max(counts[include])],
         bright = include[which.max(brightness[include])],
         both = include[which.min(order(counts[include]) +
                                  order(brightness[include]))])
}

hcl2hex <- function(hclobj) {
  if (!inherits(hclobj, "polarLUV") &
      all(colnames(hclobj) == c("L", "C", "H")))
    hclobj <- polarLUV(hclobj[,1], hclobj[,2], hclobj[,3])
  rgbobj <- as(hclobj, "RGB")@coords
  rgb(rgbobj[,1], rgbobj[,2], rgbobj[,3])
}

rhs2rgb <- function(rhs, colTable) {
  t(sapply(rhs,
           function(cell) {
             idx <- match(cell, colTable$RHS)
             if (all(is.na(idx))) return(rep(NA, 3))

             ##          if (any(nacases <- is.na(idx))) {
             ##   warning("Did not find an RHS entry for",
             ##           paste(rhs[idx], collapse = ", "))
             ## }

             idx <- idx[!is.na(idx)]
             colMeans(colTable[idx, c("red", "green", "blue")])
           })
  )
}

compareRHS <- function(rhs1, rhs2, colTable, space = c("RGB", "HCL")) {
  space <- match.arg(space)
  c1 <- rhs2rgb(rhs1, colTable)
  c2 <- rhs2rgb(rhs2, colTable)

  if (space == "HCL") {
    c1 <- as(RGB(c1[,1], c1[,2], c1[,3]), "polarLUV")@coords
    c2 <- as(RGB(c2[,1], c2[,2], c2[,3]), "polarLUV")@coords
  }

  sqrt(rowMeans((c1 - c2)^2)  )
}

#' rgb2idx
#'
#' Function rgb2idx takes a matrix of RGB codes, calculates the Euclidean
#' distance to all RGB triplets in the color table, and returns
#' the closest one
#'
#' @export
rgb2idx <- function(rgb, colTable) {
  RGB <- data.matrix(colTable[, c("red", "green", "blue")])
  rgb <- data.matrix(rgb)

  mat2min(rgb, RGB, scale = FALSE, what = "dist")
}

mat2min <- function(mat, tabMat, scale = TRUE, byrow = TRUE,
                    what = c("dist", "idx", "both")) {
  what <- match.arg(what)

  if (ncol(mat) != ncol(tabMat))
    stop("Unequal numbers of columns in first two arguments")
  colnames(mat) <- colnames(tabMat)

  if (scale) {
    tabMat <- scale(tabMat)
    mat <- scale(mat, center = attr(tabMat, "scaled:center"),
                 scale = attr(tabMat, "scaled:scale"))
  }

  distmat <- as.matrix(dist(rbind(mat, tabMat)))
  indices <- list(1:nrow(mat), (nrow(mat) + 1):(nrow(mat) + nrow(tabMat)))

  distmat <- distmat[ indices[[1]], indices[[2]] ]
  if (byrow) { # one outcome for each row in mat
    minval <- apply(distmat, 1,
                    function(x)
                      if (!all(is.na(x)))
                        which.min(x) else NA)
    mincol <- apply(distmat, 1,
                    function(x)
                      if (!all(is.na(x))) which.min(x) else NA)
    result <- cbind(mincol, minval)
  } else { # one outcome for mat as a whole
    minval <- min(distmat, na.rm = TRUE)
    mincol <- which(abs(distmat - minval) < 1e-8, arr.ind = TRUE)[1,2]
    result <- c(mincol, minval)
  }

  switch(what,
         dist = minval,
         idx = mincol,
         result)
}

#' hex2rgb
#'
#' Given a set of hexadecimal colours, return the rgb matrix
#' This function already exists in package colorspace: hex()
#'
#' @noRd
#' @export
hex2rgb <- function(colcode) {
  rgbcols <- strtoi(c(substr(colcode, 2, 3),
                      substr(colcode, 4, 5),
                      substr(colcode, 6, 7)),
                    base = 16)
  matrix(rgbcols, ncol = 3, byrow = FALSE,
         dimnames = list(NULL, c("R", "G", "B")))
}

#' hex2colClass
#'
#' Given a set of hexadecimal colours, return the colour names
#' (calculated through the RGB matrix, and then looking for the
#' closest colour).
#'
#' @noRd
#' @export
hex2colClass <- function(colest, colTable)
{
  colrgb <- hex2rgb(colest)
  dimnames(colrgb) <- list(NULL, c("R", "G", "B"))
  idx <- rgb2idx(colrgb, colTable)
  cbind(colest, colrgb, colTable[idx,])
}

#' cleanColorGroup
#'
#' Function to split composite colors into elements consisting only of
#' elements in the second argument, colorSet.
#'
#' @export
cleanColorGroup <- function(colors, colorSet, splitchar = c(" ", "_")) {
  splitchar <- match.arg(splitchar)
  colorElements <- strsplit(colors, splitchar)
  sapply(colorElements,
         function(x) {
           idx <- match(x, colorSet)
           if (is.na(tail(idx, 1)))
             stop(paste("Main color undefined (", tail(x, 1),
                        "), consider expanding colorSet argument",
                        sep = ""))
           if (length(x) == 2 & all(!is.na(idx))) {
             return(paste(x, collapse = " "))
           } else {
             tail(x, 1)
           }
         }
         )
}

#' countCorrect
#'
#' The following function counts the number of cases where the row
#' label matches the column label. Row labels may contain two
#' elements, both are counted as correct.
#'
#' @export
countCorrect <- function(xtable) {
  rowColors <- strsplit(rownames(xtable), " ")
  sum(sapply(seq(along = rowColors),
             function(ii)
               sum(xtable[ii, rowColors[[ii]]])))
}

#' getRMSquantile
#'
#' compare individual pixel color values with the ground truth, and
#' return the specified quantile
#'
#' @export
getRMSquantile <- function(RHS, imagename, colTable,
                           cspace = c("rgb", "HCL"),
                           quant = .25, minInt = 5,
                           plot = FALSE) {
  cspace <- match.arg(cspace)

  flower <- switch(file_ext(imagename),
                   tif = readTIFF(imagename),
                   readJPEG(imagename))
  dimensions <- dim(flower)
  flower <- matrix(flower, prod(dimensions[1:2]), dimensions[3])
  flower <- flower[rowSums(flower) > minInt/255,]

  rhs <- c(rhs2rgb(RHS, colTable))
  if (cspace == "HCL") {
    rhs <- rhs/255
    flower <- as(RGB(flower[,1], flower[,2], flower[,3]),
                 "polarLUV")@coords
    rhs <- c(as(RGB(rhs[1], rhs[2], rhs[3]), "polarLUV")@coords)
  } else {
    flower <- flower * 255
  }

  diffs <- colMeans((t(flower) - rhs)^2)

  if (plot)
    qqnorm(sqrt(diffs))

  sqrt(quantile(diffs, probs = quant, na.rm = TRUE))
}

#' showRHSquantile
#'
#' Visualisation function for a matrix of quantile outcomes
#'
#' @export
showRHSquantile <- function(rhsq, id = 2, plot = TRUE, ...) {
  limit <- quantile(rhsq[,id], .95, na.rm = TRUE)

  rhsq <- rhsq[apply(rhsq, 1, function(x) !any(is.na(x))),]
  if (plot) {
    matplot(rhsq[order(rhsq[,id]),], type = "l", lty = 1,
            xlab = "Image number (ordered)", ylab = "RMS quantile", ...)
    abline(h = quantile(rhsq[,id], .95, na.rm = TRUE), lty = 2)
    legend("topleft", legend = colnames(rhsq), lty = 1, col = 1:3)

    invisible(limit)
  } else {
    limit
  }
}


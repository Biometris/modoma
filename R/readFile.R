## Helper functions for reading external files and processing image data.

#' @export
readFile <- function(infile,
                     metafile,
                     orderedFactMetafile) {
  ## Get meta data.
  columnMeta <- read.csv(metafile)
  orderedFactMeta <- read.csv(orderedFactMetafile)
  ## Input file.
  indat <- read.csv(infile)

  for (colname in colnames(indat)) {
    idx <- match(colname, columnMeta$colname)
    if (length(columnMeta[idx, "colType"]) > 0 &&
        columnMeta[idx, "colType"] == "trait") {
      ## Convert traits to correct class.
      colClass <- columnMeta[idx, "traitType"]
      if (length(colClass) > 0) {
        if (colClass == "factor") {
          indat[[colname]] <- factor(indat[[colname]])
        } else if (colClass == "ordered") {
          ## Ordered factors get their levels from meta data.
          idx.levs <- orderedFactMeta$levels == columnMeta[idx, "levels"]
          indat[[colname]] <- factor(indat[[colname]], ordered = TRUE,
                                     levels = orderedFactMeta[idx.levs, "value"])
        } else if (colClass %in% c("color", "RHS")) {
          ## Custom classes for color columns.
          class(indat[[colname]]) <- colClass
        }
      }
    }
    colnames(indat)[colnames(indat) == colname] <-
      columnMeta[idx, "colnameDisplay"]
  }

  attr(indat, "filenameVar") <-
    columnMeta[columnMeta$colname == "filename", "colnameDisplay"]

  return(indat)
}


#' @export
imgUri <- function(x,
                   imageFolder) {
  sapply(x, function(y) {
    yPath <- file.path(imageFolder, y)

    if (is.na(y)) return(NA)

    if (tools::file_ext(y) %in% c("tif", "tiff")) {
      yJpg <- paste0(tools::file_path_sans_ext(yPath), ".jpg")
      if (file.exists(yJpg)) {
        ## tif files were converted to jpg.
        ## Convert the file paths as well to assure the files can still be
        ## matched to the filename in the input
        yPath <- yJpg
      } else if (file.exists(yPath)) {
        yPng <- tempfile(fileext = ".png")

        tiff <- tiff::readTIFF(normalizePath(yPath))
        png::writePNG(tiff, target = yPng)
        yPath <- yPng
      }
      else return(NA)
    } else if (!file.exists(yPath)) return(NA)
    return(sprintf('<img src="%s"/ height="25">', knitr::image_uri(yPath)))
  })
}

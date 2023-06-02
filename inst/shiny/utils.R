## Helper function for creating the result table.
## The colors with the RGB information for the RHS columns are in the table
## since they are used to provide the actual background colors.
## They should always be hidden and can also not appear in the dropdown
## menu for column selection.
##
## Except for input all arguments should be isolated when the function is
## called to avoid unnecessary updating of the table before computations
## are finished.
renderResultTable <- function(input,
                              inputDat,
                              matchRes,
                              refVar,
                              columnVis) {

  colTarget <- which(startsWith(colnames(matchRes), "colors")) - 1

  if (!is.null(columnVis)) {
    visTarget <- which(!colnames(matchRes) %in% columnVis) - 1
  } else {
    visTarget <- colTarget
  }

  dat <- datatable(matchRes,
                   rownames = FALSE,
                   escape = FALSE,
                   selection = "none",
                   extensions = "Buttons",
                   plugins = "input",
                   options = list(
                     dom = "Bf<\"datatables-scroll\"t>tip",
                     ordering = FALSE,
                     autoWidth = FALSE,
                     pagingType = "input",
                     stateSave = TRUE,
                     # lengthMenu = list(c(11, 26, 51, 101),
                     #                   c("10", "25", "50", "100")),
                     pageLength = 25,
                     columnDefs = list(
                       list(className = "dt-center", targets = 0),
                       list(bSearchable = FALSE, targets = 0),
                       list(visible = FALSE, targets = visTarget),
                       list(className = "noVis", targets = colTarget)),
                     buttons = list(
                       list(extend = "pageLength"),
                       list(extend = "colvis",
                            columns = I(":not(.noVis)"),
                            collectionLayout = "three-column"),
                       list(extend = "colvisGroup",
                            text = "Show all",
                            show = I(":not(.noVis)")),
                       list(extend = "colvisGroup",
                            text = "Show none",
                            hide = I(":visible"))
                     )
                   )) |>
    formatStyle(which(colnames(matchRes) == refVar),
                target = "row",
                backgroundColor = styleEqual(levels = c(isolate(input$ref), NA),
                                             values = "lightblue"))

  RHScols <- colnames(inputDat)[sapply(inputDat, class) == "RHS"]

  for(RHScol in RHScols) {
    dat <- dat |>
      formatStyle(columns = RHScol,
                  valueColumns = paste0("colors", RHScol),
                  backgroundColor = styleValue())
  }

  return(dat)
}

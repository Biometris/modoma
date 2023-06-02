shinyServer(function(input,
                     output,
                     session) {

  reacVals <- reactiveValues()

  # shinyDirChoose(input = input,
  #                id = "imageDir",
  #                roots = getVolumes()(),
  #                filetypes = c("", "txt", "bigWig", "tsv", "csv", "bw"),
  #                allowDirCreate = FALSE)

  ## Read the input data.
  ## The image files are processed in this part.
  ## Also extra columns are added for the columns containing RHS values
  ## The columns contain the corresponding hex color used as background.
  inputDat <- reactive({
    # req(input$infile, !inherits(input$imageDir, "shinyActionButtonValue"))
    #
    # ## Read input.
    # inputDat <- try(readFile(input$infile$datapath))
    # if (inherits(inputDat, "try-error")) {
    #   shinyjs::alert("Error trying to read file.\n Please check the settings.")
    # }
    #
    # reacVals$filenameVar <- attr(inputDat, "filenameVar")
    #
    # ## Convert filename to image.
    # if (isTRUE(hasName(inputDat, reacVals$filenameVar))) {
    #   inputDat[[reacVals$filenameVar]] <-
    #     imgUri(inputDat[[reacVals$filenameVar]],
    #            imageFolder = parseDirPath(getVolumes()(), input$imageDir))
    #
    #   ## Move picture to first position.
    #   inputDat <-
    #     inputDat[c(reacVals$filenameVar,
    #                colnames(inputDat)[colnames(inputDat) != reacVals$filenameVar])]
    # }
    #
    # data("colTranslation")
    #
    # rgb2hex <- function(vec) {
    #   if (any(is.na(vec))) return(NA)
    #   rgb(vec[1], vec[2], vec[3], maxColorValue = 255)
    # }
    #
    # RHScols <- colnames(inputDat)[sapply(inputDat, class) == "RHS"]
    # for (RHScol in RHScols) {
    #   rhscodes <- cleanRHS(inputDat[[RHScol]], colTranslation$RHS)
    #   rgbcols <- rhs2rgb(rhscodes, colTranslation)
    #
    #   inputDat[[paste0("colors", RHScol)]] <- apply(rgbcols, 1, rgb2hex)
    # }
    #
    # ## Reset inputs.
    # shinyjs::reset("tabs")
    # removeUI("#resTab")
    # removeUI("#resTabTr")

    data(florGerb)
    inputDat <- florGerb
    RHScols <- colnames(inputDat)[sapply(inputDat, class) == "RHS"]
    reacVals$filenameVar <- "image"

    reacVals$inserted <- reacVals$insertedTr <- FALSE
    reacVals$colVisInTab <-
      colnames(inputDat)[!colnames(inputDat) %in% paste0("colors", RHScols)]
    reacVals$PCoARes <- NULL

    inputDat
  })

  ## Get the column name that contains the reference variable.
  ## This differs for Floricode and nak.
  refVar <- reactive({
    req(inputDat())
    ## Check if file is Floricode or nak.
    isFlor <- hasName(inputDat(), "VKCNr")
    if (isFlor) "VKCNr" else "RVPnr"
  })

  nonTraits <- reactive({
    c("rasnummer", "RVPnr", "VBN_CODE","VBN_PRODUCTNAAM_NL", "VKCNr",
      reacVals$filenameVar)
  })

  ## Extract all trait columns from input data.
  traitCols <- reactive({
    req(inputDat())

    colnames(inputDat())[!colnames(inputDat()) %in% nonTraits() &
                           !startsWith(colnames(inputDat()), "colors")]
  })

  observe({
    req(input$inTab_state$columns)

    visCols <- colnames(inputDat())[
      sapply(X = isolate(input$inTab_state$columns), FUN = `[[`, "visible")]

    reacVals$colVisResTab <- c("distance", visCols)
    reacVals$colVisResTabTr <- c("distance", visCols)
  })

  ## Create table containing full input data set.
  output$inTab <- renderDataTable({
    req(inputDat())

    colTarget = which(startsWith(colnames(inputDat()), "colors")) - 1
    visTarget <- which(!colnames(inputDat()) %in% reacVals$colVisInTab) - 1

    dat <- datatable(inputDat(),
                     rownames = FALSE,
                     filter = "top",
                     escape = FALSE,
                     selection = "none",
                     extensions = c("Buttons", "FixedHeader"),
                     plugins = "input",
                     options = list(
                       dom = "Bf<\"datatables-scroll\"t>tip",
                       autoWidth = FALSE,
                       pagingType = "input",
                       fixedHeader = TRUE,
                       columnDefs = list(list(className = "dt-center", targets = 0),
                                         list(bSearchable = FALSE, targets = 0),
                                         list(visible = FALSE, targets = visTarget),
                                         list(className = "noVis", targets = colTarget)),
                       buttons = list(
                         list(extend = "pageLength"),
                         list(extend = "colvis",
                              columns = I(':not(.noVis)'),
                              collectionLayout = "three-column"),
                         list(extend = "colvisGroup",
                              text = "Show all",
                              show = I(":not(.noVis)")),
                         list(extend = "colvisGroup",
                              text = "Show none",
                              hide = I(":visible"))
                       )
                     ))

    RHScols <- colnames(inputDat())[sapply(inputDat(), class) == "RHS"]

    for(RHScol in RHScols) {
      dat <- dat |> formatStyle(columns = RHScol,
                                valueColumns = paste0("colors", RHScol),
                                backgroundColor = styleValue())
    }

    dat
  })

  ## Server side for matching by reference.

  ## ref should be filled with the varieties from the input data.
  observe({
    req(inputDat(), refVar())

    updateSelectizeInput(inputId = "ref",
                         label = paste("Select reference", refVar()),
                         choices = c("Select one" = "", inputDat()[[refVar()]]),
                         server = TRUE)
  })

  ## Fill columns for comparison.
  observe({
    req(traitCols())

    updateCheckboxGroupInput(inputId = "compCols",
                             choices = traitCols(),
                             selected = NULL)
  })

  ## Select or unselect all columns for comparison.
  observe({
    req(traitCols())

    if (input$selectall > 0) {
      if (input$selectall %% 2 == 1) {
        updateCheckboxGroupInput(inputId = "compCols",
                                 selected = traitCols())
      } else {
        updateCheckboxGroupInput(inputId = "compCols",
                                 selected = "")
      }
    }
  })

  ## Toggle button for comparison.
  ## Button can only be clicked if both a reference variety is selected and
  ## at least one column is selected for comparison.
  observe({
    toggleState("matchBtn", input$ref != "" & length(input$compCols) > 0)
  })

  ## After clicking match button behind the placeholder in the UI
  ## an output is inserted for the datatable with the results of the match.
  ## This allows for completely removing this output after reading a
  ## different input file.
  observeEvent(input$matchBtn, {
    if (!reacVals$inserted) {
      insertUI("#placeholderResTab",
               where = "afterEnd",
               ui = div(dataTableOutput("resTab")))
      reacVals$inserted <- TRUE
    }
  })

  ## The actual matching using the getClosest function from
  ## the package with the inputs as they are selected.
  matchRes <- eventReactive(input$matchBtn, {
    withBusyIndicatorServer("matchBtn", session = session, {
      res <- getClosestRef(ref = input$ref,
                           indat = inputDat(),
                           traits = input$compCols,
                           n = input$numComp)
    })
    res
  })

  observe({
    req(input$resTab_state$columns)

    visCols <- colnames(matchRes())[
      sapply(X = isolate(input$resTab_state$columns), FUN = `[[`, "visible")]

    reacVals$colVisInTab <- visCols
    reacVals$colVisResTabTr <- visCols
  })

  observeEvent(input$matchBtn, {
    reacVals$colVisResTab <- c(input$compCols, nonTraits(), "distance")
  })

  ## Generate result table.
  output$resTab <- renderDataTable({
    req(input$matchBtn)

    renderResultTable(input = input,
                      inputDat = isolate(inputDat()),
                      matchRes = isolate(matchRes()),
                      refVar = refVar(),
                      columnVis = reacVals$colVisResTab)
  })

  ## Server side for matching by traits.

  ## trait should be filled with the traits in the input data.
  observe({
    req(inputDat(), traitCols())

    updateSelectizeInput(inputId = "traits",
                         choices = c("Select one or more" = "", traitCols()),
                         server = TRUE)
  })

  ## Toggle button for comparison.
  ## Button can only be clicked if both a reference variety is selected and
  ## at least one column is selected for comparison.
  observe({
    toggleState("matchBtnTr", input$traits != "")
  })

  ## After clicking match button behind the placeholder in the UI
  ## an output is inserted for the datatable with the results of the match.
  ## This allows for completely removing this output after reading a
  ## different input file.
  observeEvent(input$matchBtnTr, {
    if (!reacVals$insertedTr) {
      insertUI("#placeholderResTabTr",
               where = "afterEnd",
               ui = dataTableOutput("resTabTr"))
      reacVals$insertedTr <- TRUE
    }
  })

  output$traitInputs <- renderUI({
    req(input$traits, inputDat())

    lapply(X = input$traits, FUN = function(trait) {
      choices <- if (is.factor(inputDat()[[trait]])) {
        levels(inputDat()[[trait]])
      } else if (inherits(inputDat()[[trait]], "RHS")) {
        colTranslation$RHS
      } else {
        sort(unique(inputDat()[[trait]]))
      }

      selectizeInput(inputId = trait,
                     label = paste("Select value for", trait),
                     choices = choices,
                     width = "80%")
    })
  })

  ## The actual matching using the getClosestTraits function from
  ## the package with the inputs as they are selected.
  matchResTr <- eventReactive(input$matchBtnTr, {
    withBusyIndicatorServer("matchBtnTr", session = session, {
      traitValues <- sapply(X = input$traits, FUN = function(trait) {
        input[[trait]]
      }, simplify = FALSE)

      res <- getClosestTraits(indat = inputDat(),
                              traitValues = traitValues,
                              n = input$numCompTr)
    })
    res
  })

  observe({
    req(input$resTabTr_state$columns)

    visCols <- colnames(matchResTr())[
      sapply(X = isolate(input$resTabTr_state$columns), FUN = `[[`, "visible")]

    reacVals$colVisInTab <- visCols
    reacVals$colVisResTab <- visCols
  })

  observeEvent(input$matchBtnTr, {
    reacVals$colVisResTabTr <- c(input$traits, nonTraits(), "distance")
  })

  ## Generate result table.
  output$resTabTr <- renderDataTable({
    req(input$matchBtnTr)

    renderResultTable(input = input,
                      inputDat = isolate(inputDat()),
                      matchRes = isolate(matchResTr()),
                      refVar = refVar(),
                      columnVis = reacVals$colVisResTabTr)
  })

  ### PCoA.

  ## Fill columns for PCoA.
  observe({
    req(traitCols())

    updateCheckboxGroupInput(inputId = "PCoACols",
                             choices = traitCols(),
                             selected = NULL)
  })

  ## Select or unselect all columns for PCoA.
  observe({
    req(traitCols())

    if (input$selectallPCoA > 0) {
      if (input$selectallPCoA %% 2 == 1) {
        updateCheckboxGroupInput(inputId = "PCoACols",
                                 selected = traitCols())
      } else {
        updateCheckboxGroupInput(inputId = "PCoACols",
                                 selected = "")
      }
    }
  })

  ## Fill colorPCoA with the traits in the input data.
  observe({
    req(inputDat(), traitCols())

    colOpts <- traitCols()[sapply(X = traitCols(), FUN = function(x) {
      is.factor(inputDat()[[x]]) &&
        length(unique(na.omit(inputDat()[[x]]))) > 1 &&
        length(unique(na.omit(inputDat()[[x]]))) <= 15
    })]

    updateSelectizeInput(inputId = "colorPCoA",
                         choices = colOpts,
                         server = TRUE)
  })

  ## Toggle button for PCoA.
  ## Button can only be clicked if at least one column is selected for PCoA
  observe({
    toggleState("PCoABtn", length(input$PCoACols) > 0)
  })

  ## The actual PCoA using the dfdist and cmdscale functions from the package.
  observeEvent(input$PCoABtn, {
    withBusyIndicatorServer("PCoABtn", session = session, {
      XDistances <- as.matrix(dfdist(inputDat()[, isolate(input$PCoACols),
                                                drop = FALSE]))
      XMds <- cmdscale(XDistances)
      colnames(XMds) <- c("PC1", "PC2")

      res <- cbind(inputDat(), XMds)
    })
    colnames(res) <- make.names(colnames(res))
    reacVals$PCoARes <- res
  })

  output$PCoAPlot <- renderPlotly({
    req(reacVals$PCoARes)

    plotDat <- reacVals$PCoARes
    plotDat[[reacVals$filenameVar]] <-
      gsub(pattern = '\"/ height=\"25\">',
           replacement = "",
           x = gsub(pattern = '<img src=\"',
                    replacement = "" ,
                    x = plotDat[[reacVals$filenameVar]],
                    fixed = TRUE),
           fixed = TRUE)

    plot_ly(data = plotDat,
            x = ~PC1,
            y = ~PC2,
            color = as.formula(paste("~", make.names(input$colorPCoA))),
            text = as.formula(paste("~", refVar())),
            type = "scatter",
            mode = "markers",
            colors = "Paired",
            showlegend = TRUE,
            hoverinfo = "none",
            #source = "hoverplotsource",
            customdata = as.formula(paste("~", reacVals$filenameVar))
            # hovertemplate = paste0("<b>", refVar(), "</b>: %{text}<br>",
            #                        "<extra></extra>")
    ) |>
      htmlwidgets::onRender(readLines("js/tooltip-image.js")) |>
      # event_register("plotly_hover") |>
      # event_register("plotly_unhover") |>
      # event_register("plotly_relayout") |>
      config(displaylogo = FALSE,
             modeBarButtonsToRemove =
               c("zoom", "pan", "select", "lasso2d", "zoomIn", "zoomOut",
                 "autoScale", "hoverClosestCartesian", "hoverCompareCartesian",
                 "toggleSpikelines", "resetScale2d"))

  })

  # hover_event <- reactive({
  #   req(reacVals$PCoARes)
  #   event_data(event = "plotly_hover", source = "hoverplotsource")
  # })
  #
  # unhover_event <- reactive({
  #   req(reacVals$PCoARes)
  #   event_data(event = "plotly_unhover", source = "hoverplotsource")
  # })
  #
  # relayout_event <- reactive({
  #   req(reacVals$PCoARes)
  #
  #   browser()
  #
  #   event_data(event = "plotly_relayout", source = "hoverplotsource")
  # })
  #
  # hoverplotlyProxy <- plotlyProxy("PCoAPlot", session)
  #
  # observeEvent(unhover_event(), {
  #   hoverplotlyProxy %>%
  #     plotlyProxyInvoke("relayout", list(images = list(NULL)))
  # })
  #
  # observeEvent(hover_event(), {
  #   sizeX <- 20
  #   sizeY <- 20
  #   posX <- hover_event()$x
  #   posY <- hover_event()$y
  #
  #   hoverplotlyProxy %>%
  #     plotlyProxyInvoke("relayout", list(images = list(
  #       list(
  #         source = hover_event()$customdata,
  #         xref = "x",
  #         yref = "y",
  #         x = if (posX < 0) posX else posX - sizeX,
  #         y = if (posY > 0) posY else posY + sizeY,
  #         sizex = sizeX,
  #         sizey = sizeY,
  #         opacity = 1
  #       )
  #     )))
  # })

})


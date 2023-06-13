# Define UI.
shinyUI(
  navbarPage(
    # Application title
    title = "Modoma",
    id = "tabs",
    theme = shinytheme("cerulean"),
    #htmltools::includeScript("./www/text.js"),
    tabPanel(title = "Introduction",
             ## The lines below have to be somewhere in the ui part of the app.
             ## Putting them here is somewhat random.
             shinyjs::useShinyjs(),
             tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                 href = "custom.css")),
             tags$script(src = "//d3js.org/d3.v3.min.js"),
             p("In the project Modoma (MOrphological Descriptors of Ornamentals through MAchine learning, 2021-2023) Floricode, Naktuinbouw and Biometris/Wageningen University & Research investigated the potential of Artificial Intelligence for obtaining flower characteristics for rose and gerbera varieties. These characteristics are important for growers as well as consumers, forming an unambiguous and full description of flower varieties. In addition, such characteristics play an important role in distinctness, uniformity and stability (DUS) testing. Automation, even partially, of the process has many benefits, such as objectivity and stability, and would allow the process to be decentralized to a large extent."),
             p("Once a database of flower characteristics is available, it can be used in a variety of ways. This application provides some examples, using gerbera data from Floricode. In the next tabs you can inspect the data, find flowers in the database that are most similar to a particular flower, picked by the user, or find flowers in the database that are closest to a set of values for characteristics, selected by the user. The final tab provides a full overview of all flowers in the database, arranged in two dimensions. It is based on a technique called Principal Coordinate Analysis, which aims to reproduce the calculated distances between flowers (based on the flower characteristics) in two dimensions."),
             HTML("If you are interested to learn more about this app or about the Modoma project, feel free to contact ", '<a href="mailto:ron.wehrens@wur.nl">Ron Wehrens</a>')
    ),
    tabPanel(title = "Data",
             fluidRow(
               column(width = 2 - demoApp ,
                      conditionalPanel(condition = "!output.demoApp",
                                       h1("Load your data"),
                                       ## Select input file.
                                       fileInput(inputId = "infile",
                                                 label = "Choose file with data",
                                                 accept = c(".csv", ".txt"),
                                                 width = "80%"),
                                       fileInput(inputId = "infilemeta",
                                                 label = "Choose file with meta data",
                                                 accept = c(".csv", ".txt"),
                                                 width = "80%"),
                                       fileInput(inputId = "infileOrderedFactmeta",
                                                 label = "Choose file with meta data on ordered factors",
                                                 accept = c(".csv", ".txt"),
                                                 width = "80%"),
                                       shinyDirButton(id = "imageDir",
                                                      label = "Select image directory",
                                                      title = "Selected",
                                                      class = "btn-primary") #,
                                       # tags$div(class="form-group shiny-input-container",
                                       #          tags$div(tags$label("File input")),
                                       #          tags$div(tags$label("Choose folder",
                                       #                              class = "btn btn-primary",
                                       #                              tags$input(id = "fileIn",
                                       #                                         webkitdirectory = TRUE, type = "file", style="display: none;", onchange = "pressed()"))),
                                       #          tags$label("No folder choosen", id = "noFile"),
                                       #          tags$div(id="fileIn_progress", class="progress progress-striped active shiny-file-input-progress",
                                       #                   tags$div(class="progress-bar")
                                       #          )
                                       # )
                      )),
               column(width = 10 + demoApp,
                      div(#style = "width:100%;",
                        shinycssloaders::withSpinner(dataTableOutput("inTab")))
               )
             ),
             #HTML("<script type='text/javascript' src='getFolders.js'></script>")
    ),
    tabPanel(title = "Matching by reference",
             column(width = 2,
                    selectizeInput(inputId = "ref",
                                   label = "Select reference",
                                   choices = NULL,
                                   width = "80%"),
                    numericInput(inputId = "numComp",
                                 label = "Number of closest flowers",
                                 value = 10,
                                 min = 1,
                                 max = 100,
                                 step = 1,
                                 width = "80%"),
                    hr(),
                    p(strong("Select variables for comparison")),
                    actionButton("selectall",
                                 label = "Select / Deselect all",
                                 class = "btn-primary"),
                    checkboxGroupInput(inputId = "compCols",
                                       label = NULL,
                                       choices = NULL,
                                       width = "100%"),
                    hr(),
                    withBusyIndicatorUI(
                      actionButton(inputId = "matchBtn",
                                   label = "Find closest flowers",
                                   class = "btn-primary")
                    )
             ),
             column(width = 10,
                    tags$div(id = "placeholderResTab")
             )
    ),
    tabPanel(title = "Matching by trait",
             column(width = 2,
                    selectizeInput(inputId = "traits",
                                   label = "Select traits",
                                   choices = NULL,
                                   multiple = TRUE,
                                   width = "80%"),
                    uiOutput(outputId = "traitInputs"),
                    numericInput(inputId = "numCompTr",
                                 label = "Number of closest flowers",
                                 value = 10,
                                 min = 1,
                                 max = 100,
                                 step = 1,
                                 width = "80%"),
                    hr(),
                    withBusyIndicatorUI(
                      actionButton(inputId = "matchBtnTr",
                                   label = "Find closest flowers",
                                   class = "btn-primary")
                    )
             ),
             column(width = 10,
                    tags$div(id = "placeholderResTabTr")
             )
    ),
    tabPanel(title = "PCoA",
             column(width = 3,
                    p(strong("Select variables for PCoA")),
                    actionButton("selectallPCoA",
                                 label = "Select / Deselect all",
                                 class = "btn-primary"),
                    checkboxGroupInput(inputId = "PCoACols",
                                       label = NULL,
                                       choices = NULL,
                                       width = "100%"),
                    hr(),
                    withBusyIndicatorUI(
                      actionButton(inputId = "PCoABtn",
                                   label = "Perform PCoA",
                                   class = "btn-primary")
                    )
             ),
             column(width = 9,
                    selectizeInput(inputId = "colorPCoA",
                                   label = "Select variable for coloring",
                                   choices = NULL,
                                   width = "400px"),
                    hr(),
                    helpText(p("Hover over a point to see an image of the",
                               "flower."),
                             p("By clicking in the legend, categories can be",
                               "turned on and off. Double clicking in the",
                               "legend will show the clicked category only.",
                               "Double clicking again will undo this."),
                             p("It is possible to zoom by drawing a rectangle",
                               "in the plot. To zoom out again, double click at",
                               "a random spot in the plot."),
                             p("The plot can be saved by clicking on the",
                               "camera symbol in the top right of the plot.")),
                    plotlyOutput(outputId = "PCoAPlot",
                                 width = "800px",
                                 height = "600px")
             )
    )
  )
)

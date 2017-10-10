#' Shiny server
#'
#' Run the Shiny App IrisViewer
#' @export
ui <- navbarPage(
    title = "IrisViewer",
    tabPanel(
        title = "File loading",
        fluidRow(
            includeCSS(
                system.file('www', 
                            'IrisViewer', 
                            package = 'IrisViewer')),
            includeScript(
                system.file('www', 
                            "window_dimensions.js", 
                            package="IrisViewer")),
        column(2,
            tags$h4('Select IF channel colors'),
            tags$div(id = 'IFColorSelect')   
               
        ),
        column(2,
            tags$h4('Select marker colors'),
            tags$div(id = 'MarkerColorSelect')   
               
        ),
        column(8,
            h4('Load configuration'),
               fileInput("rdsFile", "Choose .RDS file",
                           accept = c(".rds", ".RDS")),
            h4('Save configuration'),
            downloadButton("SaveConfig", "Save configuration")
        ))
    ),
    tabPanel(
        title = "Nearest Neighbor",
        fluidRow(
            column(4,
                   d3BarplotOutput(
                       "nn_panel", width = "100%", height = "400px"
                   )),
            column(4,
                   d3BarplotOutput(
                       "nnt_panel", width = "100%", height = "400px"
                   )),
            column(2,align='bottom',
                selectInput("first_marker",
                            label = "Marker 1:",
                            choices = NULL),
                selectInput("second_marker",
                            label = "Marker 2:",
                            choices = NULL),
                selectInput("coord_select",
                            label = "Coordinate",
                            choices = NULL)
            ),
            column(2,
                tags$div(id = 'ChannelSelect')         
            )
        ),
        fluidRow(column(
            6,
            plotOutput("rayplot_panel", height = "100%")
        ),
        column(6,
               imageOutput("IF_image")))
    )
)

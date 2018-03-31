#' Launch IrisViewer
#'
#' Run the Shiny App IrisViewer
#' @export
IrisViewer <- function (iris_set, images) {
    require(colourpicker)
    require(d3Toolbox)
    require(IrisSpatialFeatures)
    require(shiny)
    require(shinyjs)
    require(tiff)
    .GlobalEnv$iris_set = iris_set
    .GlobalEnv$images = images
    app <- list(
        ui = ui,
        server = server
    )
    runApp(app,launch.browser = T)
}

shinyApp(
    ui = fluidPage(
        includeCSS(system.file('www', 'magnifier.css')),
        actionButton('button','Press button'),
        tags$div(id = 'MarkerColorSelect'),
        useShinyjs()
        ,extendShinyjs(script = 'www/Magnifier.js')

    ),
    server = function(input, output, session) {
        values <- reactiveValues(img=NULL)
        output$plot <- renderPlot({
            plot(1:10, col = input$col)
        })
        
        observeEvent(input$button,{
            insertUI(
                selector = "#MarkerColorSelect",
                where = "afterBegin",
                ui = tags$div(
                    tags$div(
                        class="magnifier-thumb-wrapper",
                        tags$img(
                            id="thumb",
                            src = "S00-52910 G_[39703,10578]_composite_image_small.jpg",
                            alt = "IF_image",
                            vspace="50,0",
                            width = 800
                        )
                    ),
                    tags$div(
                        class="magnifier-preview",
                        id="preview"    
                        )                
                    )
            )
            js$attachZoom()
            
            
        })
        
    }
)




require(tiff)

tif <-  readTIFF('component_tiffs/1049_[52474,10131]_component_data.tif',all=T)
tif <- tif[1:4]

cols <- c('red','green','yellow','cyan')


#sum up the colors into one single image
img <- array(0,dim=c(dim(tif[[1]]),3))
for (i in 1:length(tif)){
    rgb <- col2rgb(cols[i])
    for (j in 1:3){
        img[,,j] <- img[,,j] * (1.0 - tif[[i]]) + (tif[[i]] * rgb[j]/255)
    }
}








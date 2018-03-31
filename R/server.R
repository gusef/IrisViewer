#' Shiny server
#'
#' Run the Shiny App IrisViewer
#' @export
server <- function(input, output, session) {
    options(shiny.maxRequestSize=1000*1024^2)

    #reactive values
    values <- reactiveValues(verbose = NULL,
                             current_sample = NULL,
                             from_cell = NULL,
                             to_cell = NULL,
                             IF_colors = NULL,
                             colors=NULL,
                             current_tiffstack = NULL,
                             img_file = NULL)

    #Initialization
    observe({
    
        #set the marker selects
        singles <- iris_set@markers
        allmarkers <- singles
        doubles <- sub('[+-]$','',singles)
        singles <- singles[!doubles %in% doubles[duplicated(doubles)]]
        doubles <- table(doubles)
        doubles <- names(doubles)[doubles == 2]

        updateSelectInput(session, 'first_marker',
                          choices = allmarkers)
        updateSelectInput(session, 'second_marker',
                          choices = doubles)

        #color palette to choose from
        colpalette <- c('blue','red','green','yellow','orange','purple','grey')
        
        #figure out the channels
        channels <- read.csv(file.path(images,'channels.csv'),as.is = T)[,1]
        names(channels) <- sub('\\.\\.Opal.+','',channels)
        dapi_pos <- grep('DAPI',channels)
        channels <- c(channels[dapi_pos],channels[-dapi_pos])
        values$channels <- channels

        insertUI(
            selector = "#IFColorSelect",
            where = "afterBegin",
            ui =  lapply(1:length(channels),
                function(idx,channels,colpalette){
                    colourpicker::colourInput(
                        paste0("IF_col_",idx),
                        names(channels)[idx],
                        showColour = 'background',
                        value = colpalette[idx])},
                 channels,
                 colpalette))
        
        #colors
        values$IF_colors <- data.frame(
            name=names(channels),
            channel=channels,
            colors=colpalette[1:length(channels)],
            stringsAsFactors = F)
        
        #add observers to the color selectors
        lapply(1:length(channels),
               function(id){
                   nam <- paste0("IF_col_",id)
                   observeEvent(input[[nam]], {
                        values$IF_colors$colors[id] <- input[[nam]]
                   })
               })
        
        #make selectors for the marker colors 
        marker_colors <- c('#7fc97f', '#6a3d9a', '#bc80bd', '#e31a1c',
                 '#beaed4', '#33a02c', '#fdc086', '#386cb0', '#f0027f',
                 '#bf5b17', '#666666', '#ffff99')
        
        markers <- iris_set@markers
        insertUI(
            selector = "#MarkerColorSelect",
            where = "afterBegin",
            ui =  lapply(1:length(markers),
                         function(idx,markers,marker_colors){
                             colourpicker::colourInput(
                                 paste0("Marker_col_",idx),
                                 markers[idx],
                                 showColour = 'background',
                                 value = marker_colors[idx])},
                         markers,
                         marker_colors))
        
        #set the color markers
        values$colors <- data.frame(marker=markers,
                                    colors=marker_colors[1:length(markers)],
                                    stringsAsFactors = F)
        
        #add observers to the color selectors
        lapply(1:length(markers),
               function(id){
                   nam <- paste0("Marker_col_",id)
                   observeEvent(input[[nam]], {
                       values$colors$colors[id] <- input[[nam]]
                   })
               })
        
        #add a channel selection panel 
        temp <- channels
        names(temp) <- NULL
        insertUI(
            selector = "#ChannelSelect",
            where = "afterBegin",
            ui = checkboxGroupInput("ChannelSelectBox", 
                                    label = 'Select channels', 
                                    choiceNames = as.list(names(channels)),
                                    choiceValues = as.list(temp),
                                    selected = temp[1])  
        )
   })
    
    
##############################################################################
#### When changing the marker panels let's reset the plots and images
    
observeEvent(input$first_marker,{
    values$current_sample <- NULL
    values$current_tiffstack <- NULL
})
    
observeEvent(input$second_marker,{
    values$current_sample <- NULL
    values$current_tiffstack <- NULL
})
    
    

##############################################################################
#### Plot nearest neighbor panels
    plot_nn <- function(input, output, session,
                        transpose = FALSE, callback){

        if (!is.null(input$second_marker)){

            #use the Iris plot function to extract all relevant values
            vals <- plot_nearest_neighbor(iris_set,
                                          from = input$first_marker,
                                          to = input$second_marker,
                                          transposed = transpose)

            means <- t(vals$means)
            colnames(means) <- c('x','y')
            se <- t(vals$ses)
            colnames(se) <- NULL

            #set up a legend
            legend <- data.frame(col=c('grey','black'),
                                 name=c(paste0(input$second_marker,'-'),
                                        paste0(input$second_marker,'+')))
            margins <- list(top = 40,
                            right = 20,
                            bottom = 70,
                            left = 80)

            #plot the barplot
            d3Barplot(data=means,
                      se=se,
                      margins=margins,
                      beside=T,
                      las=2,
                      col=c('grey','black'),
                      xlab='',
                      ylab=vals$ylab,
                      title=vals$label,
                      title_size=20,
                      legend=legend,
                      subtitle=paste('Paired signed rank test:', format(vals$pval,digits=4)),
                      callback=callback)
        }
    }


    output$nn_panel <- renderd3Barplot({
        plot_nn(input, output, session,
                transpose = FALSE, callback = 'NN_select')
    })

    output$nnt_panel <- renderd3Barplot({
        plot_nn(input, output, session,
                transpose = TRUE, callback = 'NN_transpose')
    })


    #if an element on the first NN was clicked
    observeEvent(input$NN_select, {
        values$from_cell <- input$first_marker
        if (input$NN_select$group == 'x'){
            values$to_cell <- paste0(input$second_marker,'-')
        }else{
            values$to_cell <- paste0(input$second_marker,'+')
        }
        display_coordinates(input, values, session, input$NN_select$x_value)
    })

    #if an element on the transposed NN was clicked
    observeEvent(input$NN_transpose, {
        values$to_cell <- input$first_marker
        if (input$NN_transpose$group == 'x'){
            values$from_cell <- paste0(input$second_marker,'-')
        }else{
            values$from_cell <- paste0(input$second_marker,'+')
        }
        display_coordinates(input, values, session, input$NN_transpose$x_value)

    })
    
    display_coordinates <- function(input, values, session, selector){
        current <- iris_set@samples[[selector]]
        
        #update the sample
        values$current_sample <- current
        
        #add the coordinate selector
        updateSelectInput(session, 'coord_select',
                          choices = names(current@coordinates))
        
        #extract all the images
        extract_tiffstack(selector,
                          names(values$current_sample@coordinates)[1])
        
    }

    #extracting all tiffs related to the current sample / coordinate
    extract_tiffstack <- function(samp,coord){
        #access the right images
        img_dir <- dir(images)
        samp_names <- gsub('^(.*)_[^_]+_component_data.tif$', '\\1', img_dir)
        coord_names <- gsub('^.*_([^_]+)_component_data.tif$', '\\1', img_dir)
        img <- img_dir[(startsWith(samp_names,samp)) & (substring(gsub(samp,'',img_dir),1,1)=='_') & (coord_names == coord)]
        #load the tiffstack
        img <- file.path(images,img)
        if (file.exists(img)){
            maps <- readTIFF(img,all = T)
            #add the channels
            names(maps) <- read.csv(file.path(images,'channels.csv'),as.is = T)[,1]
            values$current_tiffstack <- maps[match(values$channels,names(maps))]
        } else {
            showModal(modalDialog(
                title = "An error occured",
                paste(img, 'does not exist.')
            ))
        }
    }
    
##############################################################################
#### Rayplot and image output

    output$rayplot_panel <- renderPlot({
        if (!is.null(values$current_sample) &&
            !is.null(values$to_cell) &&
            input$coord_select %in% names(values$current_sample@coordinates)){

            #figure out the coloring to be consistent
            from_col <- values$colors$colors[match(values$from_cell,values$colors$marker)]
            to_col <- values$colors$colors[match(values$to_cell,values$colors$marker)]

            #plot a ray plot
            rayplot_single_coordinate(x = values$current_sample@coordinates[[input$coord_select]],
                                      samp_name = values$current_sample@sample_name,
                                      from_type = values$from_cell,
                                      from_col = from_col,
                                      to_type = values$to_cell,
                                      to_col = to_col)
        }else{
            return(NULL)
        }
    }, height = function() {
        min(700, session$clientData$rayplot_panel_width)
    })

    #first removes the old image and then adds a new one
    observeEvent(input$coord_select, {
        if (!is.null(values$current_sample) &&
            input$coord_select %in% names(values$current_sample@coordinates)){
            
            #extract all the images
            extract_tiffstack(values$current_sample@sample_name,
                              input$coord_select)
        }
    })

    #simple rendering of an multiplex IF image
    output$IF_image <- renderShinyMagnifier({
        #extract_tiffstack(samp='1049',coord='52474,10131')
        if (!is.null(values$current_tiffstack)){
    
            #get colors
            selection <- input$ChannelSelectBox
            channels <- values$IF_colors
            channels <- channels[match(selection,channels$channel),]
            cols <- channels$colors
            
            #and tiffs         
            tif <- values$current_tiffstack[selection]
            

            #sum up the colors into one single image
            img <- array(0,dim=c(dim(tif[[1]]),3))
            for (i in 1:length(tif)){
                rgb <- col2rgb(cols[i])
                for (j in 1:3){
                    #alpha blending
                    img[,,j] <- img[,,j] * (1.0 - tif[[i]]) + (tif[[i]] * rgb[j]/255)
                }
            }
            
            #and save it as a jpeg
            temp_dir <- tempdir()
            temp_file <- paste0('temp',sample(1000000,1),'.jpg')

            values$img_file <- temp_file
            unlink(file.path(temp_dir,temp_file))
            writeJPEG(img, file.path(temp_dir,temp_file), color.space='RGBA')
            addResourcePath('img', temp_dir)
            
            ShinyMagnifier(file.path('img',temp_file), 
                           file.path('img',temp_file), 
                           zoom = 4,
                           width = 0.8 * input$dimension[1] / 2,
                           vspace = '50 0')
        }
    })
}




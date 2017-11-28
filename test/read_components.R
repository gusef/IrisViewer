if (read_component_tiff) {
    if (length(grep('_component_data.tif', img_names)) == 0){
        stop('There is no _component_data.tif file for',
             x@coordinate_name,
             ' in ',
             sample_dir)
    }

    #filename of the component tiff
    filename <- file.path(sample_dir,
                          img_names[grep('_component_data.tif', img_names)])
    #figure out the channels
    channels <- colnames(dat)[grep('Nucleus',colnames(dat))]
    channels <- channels[grep('Counts',channels)]
    channels <- channels[grep('Counts',channels)]
    channels <- channels[grep('Min\\.\\.',channels)]
    channels <- sub('^Nucleus.','',channels)
    channels <- sub('\\.Min\\.\\..+','',channels)
    #drop the autofluorescent layer
    channels <- channels[-length(channels)]

    #temporary file name
    temp <- tempfile(fileext = '.tif')
    component <- lapply(1:length(channels),
                        function(idx,filename, temp){
                            #save the sub images and read the single frame
                            gdalUtils::gdal_translate(filename, temp, sd_index=idx)
                            component <- readTIFF(temp)

                            #scale between 0 and 1 based on the 99th percentile
                            component <- component / quantile(component,0.99)
                            component[component > 1] <- 1

                            #get a raw byte array of a jpg compressed tiff file
                            temp_raw <- writeTIFF(component,raw(),compression="JPEG")

                            return(temp_raw)
                        },
                        filename,
                        temp)
    names(component) <- channels
    x@raw@component_tiff <- component
}

#passed parameters
read_component_tiff <- function(raw_dir,
                                h5_file,
                                overwrite_h5 = FALSE,
                                Autofluorescence = FALSE){
    #get all the component_data files
    fnames <- dir(raw_dir,recursive = T)
    fnames <- fnames[grep('_component_data.tif',fnames)]

    #craet the h5 file
    if (file.exists(h5_file)) {
        if (overwrite_h5) {
            unlink(h5_file)
        } else {
            stop('h5 file already exists, set overwrite_h5=TRUE to remove it.')
        }
    }
    h5createFile(h5_file)


    dummy <- lapply(fnames,function(fname, raw_dir, h5_file){
        print(paste('Working on ...', fname))
        #figure out coordinate name and create a h5 group
        group <- strsplit(fname,'/')[[1]][2]
        group <- sub('\\]_.+',']',group)
        h5createGroup(h5_file, group)

        #add the directory
        fname <- file.path(raw_dir,fname)

        #figure out how many subsets there are
        num_subsets <- length(grep('SUBDATASET_[0-9]+_NAME',gdalinfo(fname)))

        #figure out the channels we can read
        channels <- sapply(1:num_subsets,function(x,fname){
            info <- gdalinfo(fname,sd=x)
            info <- info[grep('<Name>',info)]
            if (length(info) == 0 ){
                info = 'Overview'
            } else {
                info <- gsub('<(/)?Name>(\\\r)?','',info[1])
            }
        },fname)

        #drop the overview image
        channels <- channels[channels != 'Overview']

        #if not specified otherwise also drop the autofluorescence channel
        if (!Autofluorescence){
            channels <- channels[channels != 'Autofluorescence']
        }

        #reformat so the names match the seg output
        channels <- gsub('[ \\(\\)-]','.',channels)

        #write the img tiff into the hdf file
        temp <- tempfile(fileext = '.tif')
        dummy <- lapply(1:length(channels),
            function(idx, fname, channels, temp, group, h5_file){
                #save the sub images and read the single frame
                gdal_translate(fname, temp, sd_index=idx)
                img <- readTIFF(temp)

                #scale between 0 and 1 based on the 99th percentile
                img <- img / quantile(img,0.99)
                img[img > 1] <- 1

                #get a raw byte array of a jpg compressed tiff file
                temp_raw <- writeTIFF(img,raw(),compression="JPEG")

                #read the now jpg compressed image in again
                img <- readTIFF(temp_raw)

                #save the image as an hdf file
                suppressWarnings(
                    h5write(img, h5_file,
                        paste(group,channels[idx],sep='/')))
            },
            fname,
            channels,
            temp,
            group,
            h5_file)
    },
    raw_dir,
    h5_file)

}

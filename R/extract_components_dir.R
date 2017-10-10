require(gdalUtils)
require(tiff)

#passed parameters
read_component_tiff <- function(raw_dir,
                                out_dir,
                                overwrite = FALSE,
                                Autofluorescence = FALSE){
    #get all the component_data files
    fnames <- dir(raw_dir,recursive = T)
    fnames <- fnames[grep('_component_data.tif',fnames)]

    #craet the h5 file
    if (dir.exists(out_dir)) {
        if (overwrite) {
            unlink(out_dir, recursive=T)
        } else {
            stop('Output directory already exists, set overwrite=TRUE to remove it.')
        }
    }
    dir.create(out_dir)


    dummy <- lapply(fnames,function(fname, raw_dir, out_dir){
        print(paste('Working on ...', fname))
        
        #add the directory
        full_name <- file.path(raw_dir,fname)

        #figure out how many subsets there are
        num_subsets <- length(grep('SUBDATASET_[0-9]+_NAME',gdalinfo(full_name)))

        #figure out the channels we can read
        channels <- sapply(1:num_subsets,function(x,full_name){
            info <- gdalinfo(full_name,sd=x)
            info <- info[grep('<Name>',info)]
            if (length(info) == 0 ){
                info = 'Overview'
            } else {
                info <- gsub('<(/)?Name>(\\\r)?','',info[1])
            }
        },full_name)

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
        tiff <- lapply(1:length(channels),
            function(idx, fname, temp){
                #save the sub images and read the single frame
                gdal_translate(fname, temp, sd_index=idx)
                img <- readTIFF(temp)

                #scale between 0 and 1 based on the 99th percentile
                img <- img / quantile(img,0.99)
                img[img > 1] <- 1
                return(img)
            },
            full_name,
            temp)
        
        names(tiff) <- channels
        
        #write the component tiff jpeg compressed
        outfile <- strsplit(fname,'/')[[1]]
        outfile <- outfile[length(outfile)]
        outfile <- file.path(out_dir,outfile)
        writeTIFF(tiff,outfile,compression="JPEG")
    },
    raw_dir,
    out_dir)
}

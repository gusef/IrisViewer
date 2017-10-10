setwd("C:/Users/Daniel Gusenleitner/Dropbox (Partners HealthCare)/github_repos/IrisViewer/preprocess")
require(tiff)
library(jpeg)

raw_dir <- 'Z:/pathology/TCHRLBCL/EXPORT DATA_initial'
raw_files <- dir(raw_dir, recursive = T)
raw_files <- raw_files[grep('_composite_image.tif', raw_files)]

for (file in raw_files){
    path <- file.path(raw_dir,file)
    tif <- readTIFF(path)
    out <- file.path('Images',strsplit(file,'/')[[1]][2])
    out <- sub('.tif$','.jpg',out)
    writeJPEG(tif, target = out, quality = 0.7)
}



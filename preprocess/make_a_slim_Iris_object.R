setwd("C:/Users/Daniel Gusenleitner/Dropbox (Partners HealthCare)/github_repos/IrisViewer/preprocess")
rm(list=ls())
gc()
require(IrisSpatialFeatures)

set <- readRDS('Iris/example_set.rds')
for (idx in 1:length(set@samples)){
    for (jdx in 1:length(set@samples[[idx]]@coordinates)){
        #extract the raw data
        dat <- set@samples[[idx]]@coordinates[[jdx]]@raw@data
    
        #extract all relevant markers
        scores <- set@samples[[idx]]@coordinates[[jdx]]@raw@score
        markers <- scores[grep('Component',rownames(scores)),]
        markers <- gsub('[ ()-]','.',markers)
        markers <- colnames(dat)[unlist(lapply(markers,function(x,y)grep(x,y),colnames(dat)))]
        markers <- markers[grep('Mean',markers)]
        
        markers <- c('Phenotype',
            'Cell.ID',
            'Cell.X.Position', 
            'Cell.Y.Position',
            'Nucleus.Area..pixels.', 
            'Nucleus.Compactness', 
            'Nucleus.Minor.Axis', 
            'Nucleus.Major.Axis', 
            'Nucleus.Axis.Ratio',
            markers,
            'Phenotype.combined')
        set@samples[[idx]]@coordinates[[jdx]]@raw@data <- dat[,markers]
    }
}

set <- extract_nearest_neighbor(set)
plot_nearest_neighbor(set, from = 'Tumor', to = 'CD68+ PDL1')

saveRDS(set,file='Iris/reduced_set.rds')


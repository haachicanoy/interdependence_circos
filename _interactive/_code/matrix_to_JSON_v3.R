# Matrix to JSON format using R: Extract top 5 of important crops by flow
# H. Achicanoy & C. Khoury
# CIAT, 2016

library(jsonlite)

work_dir <-'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
all_elements <- read.csv(paste(work_dir, '/_interactive/_useful_info/fs_average_diet_regions_and_origins.csv', sep=''))
all_elements <- all_elements[,c('Element','Item','Average','Region_crops','Region')]
names(all_elements)[4:5] <- c('R_origin', 'R_recipients')

regions <- sort(unique(as.character(all_elements$R_recipients)))
all.combinations <- as.data.frame(expand.grid(regions, regions))
fmeas <- sort(unique(as.character(all_elements$Element)))

top5_element <- lapply(1:length(fmeas), function(i)
{
  measData <- all_elements[which(all_elements$Element==fmeas[[i]]),]; rownames(measData) <- 1:nrow(measData)
  top5 <- lapply(1:nrow(all.combinations), function(j)
  {
    subData <- measData[which(measData$R_origin==measData$R_origin[j] & measData$R_recipients==measData$R_recipients[j]),]
    subData <- subData[order(subData$Average, decreasing=TRUE),]; rownames(subData) <- 1:nrow(subData)
    subData <- subData[subData$Average>0,]
    if(nrow(subData)!=0)
    {
      if(nrow(subData)>5){
        subData <- subData[1:5,]
        rownames(subData) <- 1:nrow(subData)
      } else {
        subData <- subData
        rownames(subData) <- 1:nrow(subData)
      }
      return(subData)
    } else {
      return(cat('Combination from origin region:', as.character(measData$R_origin[j]), 'to recipient region:', as.character(measData$R_recipients[j]), ' has not flows\n'))
      subData <- data.frame(Element=measData$Element[i], Item=NA, Average=NA, R_origin=measData$R_origin[j],  R_recipients=measData$R_recipients[j])
      return(subData)
    }
  })
  top5 <- Reduce(function(...) rbind(..., deparse.level=1), top5)
  rownames(top5) <- 1:nrow(top5)
  return(top5)
})
top5_element <- Reduce(function(...) rbind(..., deparse.level=1), top5_element)

write.csv(top5_element, paste(work_dir, '/_interactive/_useful_info/most_important_crop_flows.csv', sep=''), row.names=FALSE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Food supplies without hierarchical level
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(jsonlite)

### Load flow matrix

# Matrix should have macro regions and regions. All flows should be in the same table.
work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
flows <- list.files(path=paste(work_dir, '/_interactive/_flows_matrix/fs_without_hierarchical_level', sep=''), pattern='.csv$', full.names=TRUE)
flows <- flows[c(1,4,2,3)]
flowsFiles <- lapply(flows, function(x)
{
  z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]
  colnames(z) <- rownames(z)
  z <- round(z, 1)
  z2 <- z
  rownames(z2) <- paste(rownames(z2), '_rep', sep='')
  colnames(z2) <- rownames(z2)
  pos <- seq(1, 46, 2)
  pos2 <- seq(2, 46, 2)
  zFinal <- matrix(data=NA, nrow=46, ncol=46); zFinal <- as.data.frame(zFinal)
  rownames(zFinal)[pos] <- colnames(zFinal)[pos] <- rownames(z)
  rownames(zFinal)[pos2] <- colnames(zFinal)[pos2] <- rownames(z2)
  zFinal[pos, pos] <- z
  zFinal[pos2, pos2] <- z2
  zFinal[pos, pos2] <- z
  zFinal[pos2, pos] <- z
  return(zFinal)
})

### Define elements to construct JSON file

# {names} for JSON file
mat.labels <- rownames(flowsFiles[[1]])

# {matrix} for JSON file
matrices <- lapply(flowsFiles, as.matrix)
names(matrices) <- c('calories','protein','fat','food_weight')

# {regions} for JSON file
regions <- setdiff(1:length(mat.labels),grep(pattern='*_rep$', x=mat.labels)) - 1

# Redo {names} for JSON file
mat.labels <- gsub(pattern='*_rep$', replacement='', x=mat.labels)
# mat.labels <- gsub(pattern='\n', replacement='<br />', x=mat.labels)

### Making JSON file

# Put all elements together in a list, after that apply toJSON function
# Sublist can contain different type of information to show
json.file <- list(names=mat.labels,
                  regions=regions,
                  matrix=matrices
)

sink(paste(work_dir, '/_interactive/_json/fs_interdependence.json', sep='')) # redirect console output to a file
toJSON(json.file, pretty=TRUE)
sink()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Production systems without hierarchical level
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(jsonlite)

### Load flow matrix

# Matrix should have macro regions and regions. All flows should be in the same table.
work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
flows <- list.files(path=paste(work_dir, '/_interactive/_flows_matrix/prod_without_hierarchical_level', sep=''), pattern='.csv$', full.names=TRUE)
flows <- flows[c(2,1,3)]
flowsFiles <- lapply(flows, function(x)
{
  z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]
  colnames(z) <- rownames(z)
  z <- round(z, 1)
  z2 <- z
  rownames(z2) <- paste(rownames(z2), '_rep', sep='')
  colnames(z2) <- rownames(z2)
  pos <- seq(1, 46, 2)
  pos2 <- seq(2, 46, 2)
  zFinal <- matrix(data=NA, nrow=46, ncol=46); zFinal <- as.data.frame(zFinal)
  rownames(zFinal)[pos] <- colnames(zFinal)[pos] <- rownames(z)
  rownames(zFinal)[pos2] <- colnames(zFinal)[pos2] <- rownames(z2)
  zFinal[pos, pos] <- z
  zFinal[pos2, pos2] <- z2
  zFinal[pos, pos2] <- z
  zFinal[pos2, pos] <- z
  return(zFinal)
})

### Define elements to construct JSON file

# {names} for JSON file
mat.labels <- rownames(flowsFiles[[1]])

# {matrix} for JSON file
matrices <- lapply(flowsFiles, as.matrix)
names(matrices) <- c('production quantity','harvested area','production value')

# {regions} for JSON file
regions <- setdiff(1:length(mat.labels),grep(pattern='*_rep$', x=mat.labels)) - 1

# Redo {names} for JSON file
mat.labels <- gsub(pattern='*_rep$', replacement='', x=mat.labels)
# mat.labels <- gsub(pattern='\n', replacement='<br />', x=mat.labels)

### Making JSON file

# Put all elements together in a list, after that apply toJSON function
# Sublist can contain different type of information to show
json.file <- list(names=mat.labels,
                  regions=regions,
                  matrix=matrices
)

sink(paste(work_dir, '/_interactive/_json/prod_interdependence.json', sep='')) # redirect console output to a file
toJSON(json.file, pretty=TRUE)
sink()

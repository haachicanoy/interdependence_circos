# Matrix to JSON format using R
# H. Achicanoy & C. Khoury
# CIAT, 2016

library(jsonlite)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Load flow matrix
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Matrix should have macro regions and regions. All flows should be in the same table.
work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
flows <- list.files(path=paste(work_dir, '/_interactive/_flows_matrix', sep=''), pattern='.csv$', full.names=TRUE)
flowsFiles <- lapply(flows, function(x)
{
  z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]
  colnames(z) <- rownames(z)
  return(z)
})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Define elements to construct JSON file
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# {names} for JSON file
mat.labels <- rownames(flowsFiles[[1]])

# {matrix} for JSON file
matrices <- lapply(flowsFiles, as.matrix)
names(matrices) <- c('calories','fat','food_weight','protein')

# {regions} for JSON file
regions <- setdiff(1:length(mat.labels),grep(pattern='*_reg$', x=mat.labels)) - 1

# Redo {names} for JSON file
mat.labels <- gsub(pattern='*_reg$', replacement='', x=mat.labels)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Making JSON file
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Put all elements together in a list, after that apply toJSON function
# Sublist can contain different type of information to show
json.file <- list(names=mat.labels,
                  regions=regions,
                  matrix=matrices
)

sink(paste(work_dir, '/_interactive/_json/all_interdependences.json', sep='')) # redirect console output to a file
toJSON(json.file, pretty=TRUE)
sink()

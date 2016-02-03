# Construct complete flow matrix
# H. Achicanoy & C. Khoury
# CIAT, 2016

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'

macro <- list.files(path=paste(work_dir, '/_macro/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)
region <- list.files(path=paste(work_dir, '/_regions/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)
macro_to_region <- list.files(path=paste(work_dir, '/_macro_to_region/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)
region_to_macro <- list.files(path=paste(work_dir, '/_region_to_macro/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)

macroFiles <- lapply(macro, function(x){z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]; colnames(z) <- rownames(z); return(z)})
regionFiles <- lapply(region, function(x){z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]; colnames(z) <- rownames(z); return(z)})
macro_to_regionFiles <- lapply(macro_to_region, function(x)
{
  z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]
  colnames(z) <- c("N\nAmerica","C\nAmerica","Caribbean","Andes","Trop. S.\nAmerica","Temp. S.\nAmerica","W\nAfrica","C\nAfrica","E\nAfrica",
                   "S\nAfrica","IOI","NW\nEurope","SW\nEurope","NE\nEurope","SE\nEurope","SE\nMediterranean","W\nAsia","C\nAsia","S\nAsia",
                   "E\nAsia","SE\nAsia","Pacific","ANZ")
  return(z)
})
region_to_macroFiles <- lapply(region_to_macro, function(x)
{
  z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]
  colnames(z) <- c("North\nAmerica","Latin\nAmerica","Africa","Europe","W Asia and\nSE Mediterranean","Central\nAsia")
  return(z)
})
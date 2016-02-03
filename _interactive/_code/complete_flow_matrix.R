# Construct complete flow matrix
# H. Achicanoy & C. Khoury
# CIAT, 2016

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'

macro <- list.files(path=paste(work_dir, '/_macro/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)
region <- list.files(path=paste(work_dir, '/_regions/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)
macro_to_region <- list.files(path=paste(work_dir, '/_macro_to_region/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)
region_to_macro <- list.files(path=paste(work_dir, '/_region_to_macro/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)

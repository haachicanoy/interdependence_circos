# Flow matrix by regions using information about crops
# H. Achicanoy & C. Khoury
# CIAT, 2016

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'

fs_information <- read.csv(paste(work_dir, '/_regions/_outputs/food_supplies_countries_regions_all_merge.csv', sep=''))
fs_information <- fs_information[,c("Item", "Element", "Average", "Region", "Region_crops")]

elements <- sort(as.character(unique(fs_information$Element)))
lapply(1:length(elements), function(i)
{
  sub_fs_info <- subset(fs_information, fs_information$Element==elements[[i]]); rownames(sub_fs_info) <- 1:nrow(sub_fs_info)
  colnames(sub_fs_info)[4:5] <- c("R_recipients", "R_origin")
  dim(unique(sub_fs_info[,4:5]))
  length(sort(as.character(unique(sub_fs_info$R_recipients))))
  length(sort(as.character(unique(sub_fs_info$R_origin))))
  return(cat("Done!\n"))
})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Recycling code
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_regions'
fs_data_elements <- read.csv(paste(work_dir, "/_outputs/food_supplies_countries_regions_all_merge.csv", sep=''))

fs_data_elements <- fs_data_elements[,c("Element", "Item", "Country", 'Average', 'Region')]
fs_data_elements <- unique(fs_data_elements)

fmeas <- as.character(unique(fs_data_elements$Element))
fitem <- as.character(unique(fs_data_elements$Item))
fcoun <- as.character(unique(fs_data_elements$Country))

library(dplyr)

# Average of population by country

popData <- read.csv(paste(work_dir, "/_inputs/Pop_177countriesFSandprod.csv", sep=''))
popData[,grep("Y",names(popData))] <- popData[,grep("Y",names(popData))]*1000
popData <- popData[,c('Country','Yaverage')]
popData <- unique(merge(popData, fs_data_elements[,c('Country','Region')], by.x='Country')); rownames(popData) <- 1:nrow(popData)

all_elements <- lapply(1:length(fmeas), function(i) # Measurement filter
{
  
  measData <- fs_data_elements[which(fs_data_elements$Element==fmeas[[i]]),]
  regions <- sort(as.character(unique(measData$Region)))
  
  regionsInfo <- lapply(1:length(regions), function(j) # Region filter
  {
    regData <- measData[which(measData$Region==regions[[j]]),]
    items <- sort(as.character(unique(regData$Item)))
    
    totpop_reg <- sum(popData$Yaverage[popData$Region==regions[[j]]], na.rm=TRUE)
    
    itemInfo <- lapply(1:length(items), function(k)
    {
      itemData <- regData[which(regData$Item==items[[k]]),]
      aux_coun <- as.character(itemData$Country)
      grep2 <- Vectorize(grep, vectorize.args='pattern')
      popCoun  <- popData$Yaverage[unlist(lapply(grep2(pattern=aux_coun, x=popData$Country, fixed=TRUE), function(x){z <- x[[1]]; return(z)}))]
      avg <- sum(itemData$Average*popCoun, na.rm=TRUE)/totpop_reg
      tb_item <- data.frame(item=items[[k]], Average=avg)
      return(tb_item)
    })
    itemInfo <- Reduce(function(...) rbind(..., deparse.level=1), itemInfo)
    itemInfo$Region <- paste(regions[[j]])
    return(itemInfo)
  })
  regionsInfo <- Reduce(function(...) rbind(..., deparse.level=1), regionsInfo)
  regionsInfo$Element <- fmeas[[i]]
  return(regionsInfo)
})
all_elements <- Reduce(function(...) rbind(..., deparse.level=1), all_elements)
write.csv(all_elements, paste('C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_interactive/_useful_info/fs_average_diet_regions.csv', sep=''), row.names=FALSE)


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
popData <- merge(popData, fs_data_elements[,c('Country','Region')], by.x='Country')

cOrgData <- data_country
names(cOrgData)[4:5] <- c('R_origin','R_recipients')
rownames(cOrgData) <- 1:nrow(cOrgData)

elements <- sort(as.character(unique(cOrgData$Element)))
lapply(1:length(elements), function(i)
{
  elemData <- cOrgData[which(cOrgData$Element==elements[[i]]),]
  r_recipients <- sort(as.character(unique(elemData$R_recipients)))
  lapply(1:length(r_recipients), function(j)
  {
    reciData <- elemData[which(elemData$R_recipients==r_recipients[[j]]),]
    items <- sort(as.character(unique(reciData$Item)))
    
    totpop_reg <- sum(popData$Yaverage[popData$Region==r_recipients[[j]]], na.rm=TRUE)
    
    lapply(1:length(items), function(k)
    {
      itemData <- reciData[which(reciData$Item==items[[k]]),]
      
    })
  })
})

for (coun in ucoun) {
  #coun <- ucoun[1]
  cat("Country:", paste(coun), "\n")
  counData <- data_country[which(data_country$Country==paste(coun)),]
  rownames(counData) <- 1:nrow(counData)
  umeas <- sort(as.character(unique(data_country$Element)))
  
  for (meas in umeas) {
    # meas <- umeas[1]
    cat("Measurement:", paste(meas), "\n")
    measData <- counData[which(counData$Element==paste(meas)),]
    rownames(measData) <- 1:nrow(measData)
    uitem <- sort(as.character(unique(measData$Item)))
    
    for (reg in ureg) {
      
      itemData <- measData[which(measData$Item==paste(it)),]
      #comm <- ucomm[145]
      #cat("Commodity:",paste(comm),"\n")
      regData <- measData[which(measData$Region_crops==paste(reg)),]
      iniCol <- grep(paste("Y",iniYear,sep=""),names(regData))
      finCol <- grep(paste("Y",endYear,sep=""),names(regData))
      if (nrow(regData)==0) {regData[1,iniCol:finCol] <- NA; ismissing<-T} else {ismissing <- F}
      k <- data.frame(YEAR=seq(iniYear,endYear,by=1),VALUE=as.numeric(t(regData[,iniCol:finCol])))
      names(k) <- c("YEAR",paste(coun))
      k <- unique(k)
      
      wnames <- popData[which(popData$Country==paste(coun)),]
      wnames <- sort(as.character(unique(wnames$Country))) # Revisar!!!
      
      mergeSel <- popData[which(popData$Country==paste(coun)),]
      
      minYear <- iniYear
      maxYear <- endYear
      
      #loop through years that need population data to be merged
      for (yr in minYear:maxYear) {
        #yr <- (minYear:maxYear)[1]
        #totpop <- sum(as.numeric(popSelData[which(popSelData$YEAR == yr),3:ncol(popSelData)]),na.rm=T)
        totval <- 0
        for (wname in wnames) {
          
          dta <- k[which(k$YEAR==yr),paste(wname)]
          eval(parse(text=paste('pop <- mergeSel$Y',yr,sep='')))
          pop <- unique(pop)
          totpop <- popData_region[which(popData_region$Region==as.character(mergeSel$Region)[1]),paste('Y',yr,sep='')]
          
          if (is.na(dta)) {dta <- 0}
          if (is.na(pop)) {pop <- 0}
          
          val <- (dta*pop)/totpop
          
          if (wname==wnames[1]) {
            totval <- val
          } else {
            totval <- totval+val
          }
          
        }
        k[which(k$YEAR==yr),paste(coun)] <- totval
      }
      
      outrow <- regData
      outrow[,iniCol:finCol] <- k[,paste(coun)]
      outrow$Country <- coun
      outrow$Element <- meas
      outrow$Region_crops <- reg
      
      if (ismissing) {
        cOrgData <- rbind(cOrgData,outrow)
      } else {
        cOrgData[which(cOrgData$Country == paste(coun)
                       & cOrgData$Region_crops == paste(reg)
                       & cOrgData$Element == paste(meas)),iniCol:finCol] <- k[,paste(coun)]
      }
    }
  }
}



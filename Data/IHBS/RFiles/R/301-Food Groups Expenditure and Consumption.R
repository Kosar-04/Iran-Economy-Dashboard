
#Food Groups Expenditure and consumption 

rm(list=ls())
starttime <- proc.time()
library(yaml)

Settings <- yaml.load_file("Settings.yaml")

leapyears <- c(seq(1280,1308,by=4),
               seq(1313,1341,by=4),
               seq(1346,1370,by=4),
               seq(1375,1403,by=4),
               seq(1408,1469,by=4))

library(data.table)
library(stringr)
library(readxl)


cat("\n\n================ FoodGroups =====================================\n")
TFoodGroups <- data.table(read_excel(Settings$MetaDataNewFilePath,Settings$MDS_FoodGroupsTotalNutritionNew))
FoodTypeTables <- list()
for(i in 1:nrow(TFoodGroups))
  FoodTypeTables[[i]] <- data.table(read_excel(Settings$MetaDataNewFilePath,sheet=TFoodGroups[i,SheetName]))


for(year in (93:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
 
  BigFDataNew <- data.table()
  
  
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  if (year==100 | year==101| year==102) {
    HHBase[,Month:=NA]
  }
  if(length(which(is.na(HHBase$Month)))>1){     # For years withouout month info`
    DayCount <- HHBase[,.(HHID,Days=ifelse(Quarter<=2,31,30))]
  }else{
    DayCount <- HHBase[,.(HHID,
                          Days=ifelse(Month<=6,31,
                                      ifelse(Month<=11,30,
                                             ifelse(Year %in% leapyears,30,29))))]
  }
  #i <- 4
  for(i in 1:nrow(TFoodGroups)){
    cat(paste0(TFoodGroups[i,SheetName],", "),"\t")
    
    ThisFoodTypeTable <- FoodTypeTables[[i]] 
    ft <- ThisFoodTypeTable[Year==year]
    ft[, ExtraCode := lapply(strsplit(ExtraCode, ","), as.numeric)]
    
    tab <- ft$Table
    if(is.na(tab))
      next

    # Merge urban and rural tables
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    if(year %in% 63:82){
      pcols <- intersect(names(TF),c("HHID","Code","Kilos","Expenditure","Price"))
      TF <- TF[,pcols,with=FALSE]
      TF <- TF[Code %in% ft$StartCode:ft$EndCode]
      TF <- TF[!(is.na(Kilos) & is.na(Expenditure) & is.na(Price))] 
      
      TF[,Kilos:=as.numeric(Kilos)]
  
      TF[,FGrams:=(Kilos*1000)]
      
    } else if(year >= 83){
      pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos","Expenditure","Price"))
      TF <- TF[,pcols,with=FALSE]
      
      valid_ranges1 <- unlist(mapply(seq, ft$StartCode, ft$EndCode))
      valid_ranges2 <- unlist(mapply(seq, ft$StartCode2, ft$EndCode2))
      
      valid_values <- unlist(ft$ExtraCode)
      
      valid_codes <- unique(c(valid_ranges1,valid_ranges2, valid_values))
      
      TF <- TF[Code %in% valid_codes]
      


      
      TF <- TF[!(is.na(Grams) & is.na(Kilos) & is.na(Expenditure) & is.na(Price))] 
      TF[,Price:=as.numeric(Price)]
      TF[,Expenditure:=as.numeric(Expenditure)]

      TF <- TF[(is.na(Grams) & is.na(Kilos)),Kilos:=Expenditure/Price]
      TF <- TF[!is.infinite(Kilos)]
      TF[,Price:=as.numeric(Price)]
      TF[,Kilos:=as.numeric(Kilos)]
      TF[,Grams:=as.numeric(Grams)]
      TF[,Expenditure:=as.numeric(Expenditure)]

      # Impute missing prices and FGrams if possible
      TF[is.na(Grams) & !is.na(Kilos),Grams:=0]
      TF[is.na(Kilos) & !is.na(Grams),Kilos:=0]
      TF[,FGrams:=(Kilos*1000+Grams)]
    }
    
    TF[is.na(Price), Price:= Expenditure/(FGrams/1000)]
    TF[is.na(FGrams),FGrams:=Expenditure/Price*1000]
    
    TF[is.na(TF)] <- 0
    
    FData <- TF[,lapply(.SD,sum),by=.(HHID,Code)]
    FData[!is.na(FGrams), Price:=Expenditure/(FGrams/1000)]
    FData[, FoodType:=TFoodGroups[i,FoodType]]
    FData <- merge(FData,DayCount)
    

    
    FData[is.infinite(Price),Price:=NA]
    
    BigFDataNew <- rbind(BigFDataNew,
                      FData[,.(HHID,FoodCode=Code,FoodType,Price,Expenditure,FGrams)])
    
  }
  
  save(BigFDataNew, file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFDataTotalNew.rda"))
  

}
endtime <- proc.time()
cat("\n\n==============Finish==============\nIt took ",(endtime-starttime)[3],"seconds.")

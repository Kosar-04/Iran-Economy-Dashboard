# 111-HHBase.R
# Builds the base data.table for households
#
# Copyright © 2016-2020: Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBase =====================================\n")

library(yaml)

Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)

# Loop over years 1383 (2004) to 1402 (2023)
for(year in (83:102)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # Load raw survey data for the year
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))

  # Load county code mapping data for specific years (1387 to 1391)
  if(year >86 & year < 92 ){ 
  load(file=paste0(Settings$HEISCountyCodePath,"Y",year,
                   Settings$HEISCountyCodeFileName,".rda"))
  }

   # Handle years before 1387 separately due to different structure of data
  if(year < 87){           # RxxData & UxxData tables are provided Since 1387
    RData <- Tables[[paste0("R",year,"P2")]][,1,with=FALSE]
    RData[, Region:=factor(x="Rural",levels=c("Urban","Rural"))]
    UData <- Tables[[paste0("U",year,"P2")]][,1,with=FALSE]
    UData[, Region:=factor(x="Urban",levels=c("Urban","Rural"))]
    HHBase <- rbind(RData, UData)
    rm(RData,UData)
    setnames(HHBase,c("HHID","Region"))
    HHBase[,Year:=year]
     # Format household IDs to standardized string length
    if(year==74){
      HHBase[,HHIDs:=formatC(HHID, width = 8, flag = "0")]
    }else if(year<77){
      HHBase[,HHIDs:=formatC(HHID, width = 7, flag = "0")]
    }else if(year %in% 77:86){
      HHBase[,HHIDs:=formatC(HHID, width = 9, flag = "0")]
    }
    # Extract Quarter from HHID string depending on year structure
    if(year < 77){
      HHBase[,Quarter:=as.integer(str_sub(HHIDs,4,4))]
    }else{
      HHBase[,Quarter:=as.integer(str_sub(HHIDs,6,6))]
    }
    # For early years, month information is unavailable
    HHBase[,Month:=NA_integer_]
    
  }else{
    # For years 1387 and later, structure is consistent
    RData <- Tables[[paste0("R",year,"DATA")]][,c(1:2),with=FALSE]
    RData[, Region:=factor(x="Rural",levels=c("Urban","Rural"))]
    UData <- Tables[[paste0("U",year,"DATA")]][,c(1:2),with=FALSE]
    UData[, Region:=factor(x="Urban",levels=c("Urban","Rural"))]
    HHBase <- rbind(RData, UData)
    rm(RData,UData)
    setnames(HHBase,c("HHID","Month","Region"))
    # Adjust month values (HEIS uses 1=Farvardin but survey is one month behind)
    HHBase[,Month:=ifelse(Month==1,12,Month - 1)]
    # Correct data error for one specific HHID in year 1397
    HHBase[HHID=="10107019605" & year==97,Month:=2]  # Odd Month (-1) in 1397
    if(length(which(HHBase$Month<=0))>0)
      stop("Odd Month Number Here!")
     # Compute quarter from month
    HHBase[,Quarter:=(Month-1)%/%3+1]
    HHBase[,HHIDs:=as.character(HHID)]
  }
  
  if(year >86 & year < 92 ){ 
    HHBase<-merge(HHBase,ShCode,by="HHID",all.x = TRUE)
  }
  
  HHBase <- HHBase[!is.na(HHID)]
 # HHBase[,ProvinceCode:=as.integer(str_sub(HHIDs,2,3))]

  # Extract CountyCode differently depending on year and availability of SHCode
  if(year <= 86 | year >= 92 ){
    HHBase[,CountyCode:=as.integer(str_sub(HHIDs,2,5))]
  }
  
  if(year >= 87 & year <= 91 ){ 
    HHBase[,CountyCode:=as.integer(SHCode)]
  }
  
  # Update CountyCode for cities moved to Alborz Province
  if(year >76 & year < 92 ){ 
    HHBase[CountyCode==2305, CountyCode:=3001] # Karaj
    HHBase[CountyCode==2308, CountyCode:=3002] # Savojbolagh 
  }
  
  
  # Handle Khorasan provincial splits and re-mappings for old years
  if(year >76 & year < 87 ){ 
    HHBase[CountyCode==901, CountyCode:=2801] # Esfarayen 
    HHBase[CountyCode==902, CountyCode:=2802] # Bojnourd 
    HHBase[CountyCode==909, CountyCode:=2804] # Shirvan 
    HHBase[CountyCode==924, CountyCode:=2803] # Jajarm 
    HHBase[CountyCode==925, CountyCode:=2806] # Maneh & Samalqan 
    
    HHBase[CountyCode==903, CountyCode:=2901] # Birjand 
    HHBase[CountyCode==911, CountyCode:=2907] # Ferdows
    HHBase[CountyCode==912, CountyCode:=2904] # Qaenat
    HHBase[CountyCode==921, CountyCode:=2905] # Nehbandan
    
    
    HHBase[CountyCode==2809, CountyCode:=2804] # Shirvan
  }
   # Handle Khorasan mapping for 1384–1386 specifically
  if(year %in% 84:86){
      HHBase[CountyCode==2824, CountyCode:=2803] # Jaram
      HHBase[CountyCode==2809, CountyCode:=2804] # Shirvan
      HHBase[CountyCode==2813, CountyCode:=2805] # Faruj     # Guess!
      HHBase[CountyCode==2825, CountyCode:=2806] # Mane & Semelqan
      
      HHBase[CountyCode==2903, CountyCode:=2901] # Birjand
      HHBase[CountyCode==2912, CountyCode:=2906] # Sarbishe  # Just a guess
      HHBase[CountyCode==2921, CountyCode:=2905] # Nehbandan
      HHBase[CountyCode==2911, CountyCode:=2903] # Serayan   # Just a guess
      #HHBase[CountyCode==0911, CountyCode:=2907] # Ferdows
    }
    
 
  # Handle counties that changed provinces  
  HHBase[CountyCode==2110, CountyCode:=2911] # Tabas
  HHBase[CountyCode==2315, CountyCode:=3003] # Nazarabad
  

  # #Ghazvin
  # if(year >76 & year < 82 ){ 
  #   HHBase[CountyCode %in% c(2311),
  #          NewArea:=26]
  # }
  # 
  #Golestan
  # if(year >76 & year < 82 ){ 
  #   HHBase[CountyCode %in% c(212,203,209,211,213,217),
  #          NewArea:=27]
  # }
  
  HHBase[,ProvinceCode:=CountyCode %/% 100]
  
  HHBase[,Year:=year]

  # Merge with external metadata to get Province and County names
  Geo2 <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_Geo2))
  Geo2 <- Geo2[,.(ProvinceCode=SCINo,ProvinceName=NameEnglish)]
  HHBase <- merge(HHBase,Geo2,by="ProvinceCode")
  
  Geo4 <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_Geo4))
  Geo4 <- Geo4[,.(CountyCode=as.numeric(Geo4),CountyName=CountyEn)]
  

  HHBase <- merge(HHBase,Geo4,by="CountyCode",all.x = TRUE)

  HHBase <- HHBase[,.(HHID,Year,Quarter,Month,
                      Region,ProvinceCode,ProvinceName,
                      CountyCode,CountyName)]
  

  # Define NewArea: for most urban regions, it is the county
  HHBase[,NewArea:=ProvinceCode]
  HHBase[Region=="Urban" & 
           CountyCode %in% c(2301,303,             # Tehran (County), Tabriz,
                             603,707,              # Ahvaz, Shiraz
                             916,1002,             # Mashhad, Isfahan (County)
                             3001,502,             # Karaj, Kermanshah (County),
                             2202,401,             # Bandarabbas, Urmia
                             808,1105,             # Kerman (County), Zahedan
                             1304,                 # Hamedan (County)
                             2105,105),            # Yazd (County), Rasht
         NewArea:=CountyCode]
  HHBase[Region=="Urban" & CountyCode ==1, 
         NewArea:=-1]                              # Arak [0001] to not be 
                                                   #  confused with [01] Gilan
  

  HHBase[,NewArea_Name:=NA_character_]
  HHBase[,NewArea_Name:=ifelse(NewArea==ProvinceCode,
                               ProvinceName,
                               paste0("Sh_",CountyName))]
  HHBase[,NewArea_Name:=as.factor(NewArea_Name)]

  
  save(HHBase, file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))

  cat(HHBase[,.N])
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])

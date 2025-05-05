# NonFood Expenditure

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ NonFood Expenditures =============================\n")

library(yaml)

Settings <- yaml.load_file("Settings.yaml")

library(rlang)
library(readxl)
library(data.table)
library(stringr)

# Define all non-food expenditure categories
sections_names <- c("Tobacco",
                    "MenCloth" , "WomenCloth","KidsCloth","OtherCloth",
                    "ActualRent","ImputedRent","Water_WasteWater","OtherHouse","EnergyNew",
                    "Furniture_HouseAppliance",
                    "Medicine_MedicalProducts", "MedicalServices","DentalServices", "ParamedicalServices",
                    "OwnTransportation", "PublicTransportation",
                    "CommunicationNew",
                    "AmusementNew",
                    "PreparedFoods", "HotelNew",
                    "MiscellGoodsSerivces",
                    "DurableWomenCloth", "DurableOtherCloth", "DurableOtherHouse", "DurableFurniture_HouseAppliance",
                    "DurableMedicine_MedicalProducts","DurableCosmeticMedical", "DurableMedicalServices",
                    "DurableOwnTransportation",
                    "DurableOtherCommunication","DurablePhone",
                    "DurableComputer",
                    "DurableAmusement", 
                    "EducationNew",
                    "Jewelry_Stocks",
                    "DurableMiscellGoodsSerivces",
                    "Insurance", "Loans", "OtherFinancials",
                    "InvestmentApartment", "InvestmentOther", "InvestmentJewelry_Stocks")

for(section in sections_names){
  section_sheet <- section
  cat("\n\n================",section,"=====================================\n")
  SectionTables <- data.table(read_excel(Settings$MetaDataNewFilePath,
                                         sheet=section_sheet))
  
  for(year in (90:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    st <- SectionTables[Year==year]
    tab <- st$Table
    if(is.na(tab))
      next
    UTS <- Tables[[paste0("U",year,tab)]]
    RTS <- Tables[[paste0("R",year,tab)]]
    TS <- rbind(UTS,RTS)
    for(n in names(TS)){
      x <- which(st==n)
      if(length(x)>0)
        setnames(TS,n,names(st)[x])
    }
    pcols <- intersect(names(TS),c("HHID","Code",paste0(section,"_Exp")))
    TS <- TS[,pcols,with=FALSE]
    if(!is.na(st$StartCode)){
      
      valid_ranges1 <- unlist(mapply(seq, st$StartCode, st$EndCode))
      valid_ranges2 <- unlist(mapply(seq, st$StartCode2, st$EndCode2))
      
      valid_values <- unlist(st$ExtraCode)
      valid_values <- strsplit(valid_values, ",")[[1]]
      
      valid_codes <- unique(c(valid_ranges1,valid_ranges2, valid_values))
      
      TS <- TS[Code %in% valid_codes]
      
      #TS <- TS[Code %in% st$StartCode:st$EndCode]
    }
    
    TS[,(paste0(section,"_Exp")):=as.numeric(get(paste0(section,"_Exp")))]

    # For monthly-reported sections
    if(tab=="P3S13" | tab=="P3S14"){
      TS[,(paste0(section,"_Exp")):=get(paste0(section,"_Exp"))/12]
      #cat("======*****========")
    }
    
    TS[,Code:=NULL]
    TS[is.na(TS)] <- 0
    
    eval(parse(text = paste0(section,"Data <- TS[,lapply(.SD,sum),by=HHID]")))
    Savelocation <- paste0(Settings$HEISProcessedPath)
    
    eval(parse(text = paste0("save(",section,
                             "Data, file =paste0(Savelocation,\"Y\",year,\"",
                             section,"s.rda\"))")))
    
    eval(parse(text = paste0("cat(section,\":\",",section,
                             "Data[,mean(",section,"_Exp)])")))
  }
}


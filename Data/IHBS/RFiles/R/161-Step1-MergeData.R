#161-Step1-MergeData.R
# 
# Copyright © 2018-2022: Majid Einian & Arin Shahbazian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Merge Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)


# Loop through each year defined in Settings
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodNutritionData.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodExpData.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))

  # Load data for all non-food expenditure categories
  for(G in c("Cigars","Cloths","Amusements","Communications", 
             "Educations", "Furnitures","Hotels","Energys","House", "Medicals",
             "Hygienes","Transportations","Others", "Restaurants",
             "Durable_4Groups","OwnedDurableItemsDepreciation","Investments"))
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))
  
  FoodNutritionData <- FoodNutritionData[FoodKCaloriesHH>0]
  
  # Merge all datasets into a master data table (MD) by household ID (HHID)
  MD<-merge(HHBase,HHI ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FoodNutritionData ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FoodExpData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CigarData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ClothData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,AmusementData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CommunicationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,EducationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,EnergyData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HouseData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FurnitureData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HotelData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,RestaurantData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HygieneData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,TransportationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,OtherData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,MedicalData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,Durable_4Groups,by =c("HHID"),all=TRUE)
  MD<-merge(MD,OwnedDurableItemsDepreciation,by =c("HHID"),all=TRUE)
  MD<-merge(MD,InvestmentData,by =c("HHID"),all=TRUE)

  # Calculate Monthly Total Expenditures 
  for (col in union(Settings$ExpenditureCols,Settings$ConsumptionCols))
    MD[is.na(get(col)), (col) := 0]
  
  MD[is.na(Investment_Exp),Investment_Exp:=0]
  
  # Calculate total monthly expenditures by summing all relevant expenditure columns
  MD[,Total_Expenditure_Month := Reduce(`+`, .SD), .SDcols=Settings$ExpenditureCols]
  # Calculate total monthly consumption by summing all relevant consumption columns
  MD[,Total_Consumption_Month := Reduce(`+`, .SD), .SDcols=Settings$ConsumptionCols]
  
  # Normalize total monthly expenditures by household equivalent size (OECD scale)
  MD[,Total_Expenditure_Month_per:=Total_Expenditure_Month/EqSizeOECD]
  # Normalize total monthly consumption by household equivalent size (OECD scale)
  MD[,Total_Consumption_Month_per:=Total_Consumption_Month/EqSizeOECD]
  
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN1.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)[3],"seconds.")

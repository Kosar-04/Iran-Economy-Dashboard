# 153-Specification of food original groups.R

#
# Copyright © 2019: Arin Shahbazian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)

Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(readxl)

cat("\n\n================ FoodGroups =====================================\n")
# Loop over each year in the specified range
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  # Load the file for the current year which contains food expenditure data
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodExp.rda"))
  
  # Create OriginalFoodExpenditure column to store item-level food expenditure
  BigFData[,OriginalFoodExpenditure:=Expenditure]
  # Select household ID and OriginalFoodExpenditure to calculate household-level totals
  NfoodExp<-BigFData[,.(HHID,OriginalFoodExpenditure)]     
  # Aggregate OriginalFoodExpenditure at household level
  NfoodExp <- NfoodExp[,lapply(.SD,sum),by=HHID]  
  # Merge total food expenditure with aggregated item-level expenditure
  FoodExpData<-merge(TotalFoodExpData,NfoodExp,all.x = TRUE)
  FoodExpData[is.na(FoodExpData)] <- 0
  # Calculate FoodOtherExpenditure as the difference between total and original food expenditures
  FoodExpData[,FoodOtherExpenditure:=FoodExpenditure-OriginalFoodExpenditure]     
  save(FoodExpData, file = paste0(Settings$HEISProcessedPath,"Y",year,"FoodExpData.rda"))
}

cat("\n\n==============Finish==============\nIt took ")
endtime <- proc.time()
cat((endtime-starttime)[3],"seconds.")

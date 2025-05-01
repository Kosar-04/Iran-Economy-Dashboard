#163- Step 3.R
# 
# Copyright © 2018-2020: Majid Einian & Arin Shahbazian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================  =====================================\n")

library(yaml)

Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(spatstat) # For weighted median
year <- 92
# Loop through each survey year defined in the settings
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
   # Load data that has calorie-equivalent sizes computed
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN2.rda"))
  
 # Normalize expenditures and nutrient values by calorie-based equivalence size
  MD[,FoodExpenditure_Per :=FoodExpenditure/EqSizeCalory]
  MD[,OriginalFoodExpenditure_Per :=OriginalFoodExpenditure/EqSizeCalory]
  MD[,FoodKCaloriesHH_Per:=FoodKCaloriesHH/EqSizeCalory]
  MD[,FoodProtein_Per:=FoodProteinHH/EqSizeCalory]
  
  # Calculate per_Calory from Restaurants
  MD[,Calory_Price:=(OriginalFoodExpenditure_Per/FoodKCaloriesHH_Per)]
  # Compute area-level median price per calorie (weighted by sample weight)
  MD[,Calory_Price_Area:=weighted.median(Calory_Price,Weight),by=.(Region,NewArea)]
  # Estimate calories from restaurant food using expenditure and calorie price
  MD[,RestaurantKCalories:=(Settings$OutFoodKCXShare*Restaurant_Exp)/Calory_Price_Area]
  MD[is.na(RestaurantKCalories),RestaurantKCalories:=0]
  
  MD[,TFoodKCaloriesHH:=FoodKCaloriesHH+RestaurantKCalories]
  # Add restaurant expenditure to original food expenditure
  MD[,TOriginalFoodExpenditure:=OriginalFoodExpenditure+Restaurant_Exp]
  
  MD[,TOriginalFoodExpenditure_Per :=TOriginalFoodExpenditure/EqSizeCalory]
  MD[,TFoodKCaloriesHH_Per:=TFoodKCaloriesHH/EqSizeCalory]
  
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ",(endtime-starttime)[3],"seconds.")

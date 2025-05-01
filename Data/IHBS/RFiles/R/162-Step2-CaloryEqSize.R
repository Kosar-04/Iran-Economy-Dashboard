#162- Step 2.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n============== Calculationg Calorie Equal size  ===================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  cat("\n")
  
  # Load the merged household data for the given year
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN1.rda"))
  
  # Filter out invalid or incomplete data: households with size=0, or missing essential food expenditure or calorie data
  MD<-MD[Size!=0 & OriginalFoodExpenditure!=0 & !is.na(FoodKCaloriesHH)]

  # EqSizeCalory: Based on World Bank standard adult calorie need
  MD[,EqSizeCalory:=Calorie_Need_WorldBank/
       Settings$KCaloryNeed_Adult_WorldBank]
  # EqSizeCalory2: Based on Iran Nutrition Institute’s estimate
  MD[,EqSizeCalory2:=Calorie_Need_NutritionInstitute/
       Settings$KCaloryNeed_Adult_NutritionInstitute]
  # EqSizeCalory3: A simple weighted sum using different needs for adults and children
  MD[,EqSizeCalory3 :=(Size-NKids) +
       NKids*(Settings$KCaloryNeed_Child/Settings$KCaloryNeed_Adult)]
 # EqSizeCalory4: Possibly a misnamed or alternate Nutrition Institute reference 
  MD[,EqSizeCalory4 :=Calorie_Need_NutritionInstitute/
       Settings$KCaloryNeed_Adult_Institute]
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN2.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)

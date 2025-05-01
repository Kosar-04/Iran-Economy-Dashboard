#165-Step 5-AddClusterInfo.R ----- Clusters are predefined now, not calculated here
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Saving Cluster Info===============================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

# Read the predefined cluster information from the specified Excel sheet into a data.table
ClusterInfo <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_GeoX_New))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  # Load the previously processed household data (with initial poverty status) for the given year
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))

  # Merge household data (MD) with cluster information by geographical keys
  MD<-merge(MD,ClusterInfo,by=c("NewArea","NewArea_Name","Region"))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,
                      "InitialPoorClustered.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took",
    (endtime-starttime)["elapsed"],"seconds.")

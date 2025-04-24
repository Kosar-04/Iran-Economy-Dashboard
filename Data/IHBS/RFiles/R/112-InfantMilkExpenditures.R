# 112-InfantMilkExpenditures.R
# Builds a column for Infant Milk (Baby Formula) Expenditures
# This is used later to identify lactating women
#
# Copyright © 2019-2020: Zahra Shahidi, Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# License: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n========== Infant Milk (Baby Formula) Expenditures ================\n")

library(yaml)

Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

# Loop over each survey year defined in Settings
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))

# Load raw data file for the given year
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  InfantMilkTables <- data.table(read_excel(Settings$MetaDataFilePath,
                                            sheet=Settings$MDS_InfantMilk))
# Filter the metadata for the current year
  imt <- InfantMilkTables[Year == year]
  tab <- imt$Table
  if (is.na(tab))
    next
   # Read urban and rural tables for the given year and table
  UTInfantMilk <- Tables[[paste0("U",year,tab)]]
  RTInfantMilk <- Tables[[paste0("R",year,tab)]]
  # Combine urban and rural data into one table
  TInfantMilk <- rbind(UTInfantMilk,RTInfantMilk)
  # Rename columns to standard names based on metadata
  for(n in names(TInfantMilk)){
    x <- which(imt==n)
    if(length(x)>0)
      setnames(TInfantMilk,n,names(imt)[x])
  }
  # Select only needed columns: HHID, Code, and InfantMilkExpenditure
  pcols <- intersect(names(TInfantMilk),c("HHID","Code",
                                          "InfantMilkExpenditure"))
  TInfantMilk <- TInfantMilk[Code %in% imt$StartCode:imt$EndCode,
                             pcols,with=FALSE]
  TInfantMilk <- TInfantMilk[,Code:=NULL]
  TInfantMilk <- TInfantMilk[,InfantMilkExpenditure:=
                               as.numeric(InfantMilkExpenditure)]
  TInfantMilk[is.na(TInfantMilk)] <- 0
  # Aggregate expenditures by household ID (HHID)
  TInfantMilk <- TInfantMilk[,lapply(.SD,sum),by=HHID]

  save(TInfantMilk,file = paste0(Settings$HEISProcessedPath,
                                 "Y",year,"InfantMilk.rda"))
} 

endTime <- proc.time()
cat("\n\n============================\nIt took",(endTime-startTime)[3], "seconds.")

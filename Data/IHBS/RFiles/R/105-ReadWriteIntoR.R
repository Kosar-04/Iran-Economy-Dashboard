# 105-ReadWriteIntoR
# Read and Save All Tables into R Format
#
# Copyright © 2015: Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ ReadWriteIntoR =====================================\n")
# Load the YAML settings file for dynamic configuration
library(yaml)

Settings <- yaml.load_file("Settings.yaml")

# Copy special database link file for year 80 (1380) to a new location to avoid overwriting source
file.copy(from = Settings$D80LinkSource, to = Settings$D80LinkDest, overwrite = TRUE)

# Load necessary packages for reading Access files and handling data
library(RODBC)
library(foreign)
library(data.table)


# Loop through each survey year (from 1383 to 1402, saved as 83 to 102)
for(year in (83:102)){

  # Print the current year being processed
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  l <- dir(path=Settings$HEISAccessPath, pattern=glob2rx(paste0(year,".*")),ignore.case = TRUE)
   # Special handling for year 80 (1380), using the D80LinkDest path
  if(year==80){
    cns<-odbcConnectAccess2007(Settings$D80LinkDest)
  }else{
    cns<-odbcConnectAccess2007(paste0(Settings$HEISAccessPath, l))
  }
  tbls <- data.table(sqlTables(cns))[TABLE_TYPE %in% c("TABLE","SYNONYM"),]$TABLE_NAME
  print(tbls)
  Tables <- vector(mode = "list")#, length = length(tbls))
  
  for (tbl in tbls) {
    D <- data.table(sqlFetch(cns,tbl,stringsAsFactors=FALSE))
    Tables[[tbl]] <- D
  }
  names(Tables) <- toupper(names(Tables))
  ### 1400 change to 100
   # Fix inconsistencies in table naming for years 1400, 1401, and 1402
  if (year==100){
    names(Tables) <- gsub('1400','100',names(Tables))
  }
  
  if (year==101){
    names(Tables) <- gsub('1401','101',names(Tables))
  }
  
  if (year==102){
    names(Tables) <- gsub('1402','102',names(Tables))
  }
  close(cns)
  
  # Save the list of tables into a .rda file using a standardized file name format
  save(Tables,file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

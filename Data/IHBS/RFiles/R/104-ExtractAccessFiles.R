# 104-ExtractAccessFiles.R
# downloads the RAR (and zip) files that are not present in the folder 
# specified in Settings file
#
# Copyright © 2015: Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ ExtractAccessFiles =====================================\n")


library(yaml)
library(readxl)
library(tools)

Settings <- yaml.load_file("Settings.yaml")

# Load metadata containing compressed filenames for each survey year
compressed_file_names_df <- read_excel(path = Settings$MetaDataFilePath,
                                       sheet = Settings$MDS_CFN)

# Get list of years for which Access database files already exist
existing_years <- file_path_sans_ext(list.files(Settings$HEISAccessPath))
# Identify the target years for which data should be extracted
needed_years <- Settings$startyear:Settings$endyear
years_to_extract <- setdiff(needed_years,existing_years)

files_to_extract <- compressed_file_names_df[
  compressed_file_names_df$Year %in% years_to_extract,]$CompressedFileName

cmdline <- paste0(normalizePath("../exe/7z/7z.exe")," e -y ")      # Use 7-zip binary

# Save current working directory and set up a temporary folder
cwd <- getwd()
dir.create("temp")
setwd("temp")

# Loop through each year to extract its corresponding Access file
for(year in years_to_extract){
  filename <- compressed_file_names_df[
    compressed_file_names_df$Year ==year,]$CompressedFileName
  file.copy(from = paste0(Settings$HEISCompressedPath,filename),to = ".")
  shell(paste0(cmdline,filename))
  l <- dir(pattern=glob2rx("*.mdb"),ignore.case = TRUE)
  if(length(l)>0){
    file.rename(from = l,to = paste0(year,".mdb"))
    file.copy(from = paste0(year,".mdb"),
              to = paste0(Settings$HEISAccessPath,year,".mdb"))
  }
  l <- dir(pattern=glob2rx("*.accdb"),ignore.case = TRUE)
  if(length(l)>0){
    file.rename(from = l,to = paste0(year,".accdb"))
    file.copy(from = paste0(year,".accdb"),
              to = paste0(Settings$HEISAccessPath,year,".accdb"))
  }
  unlink("*.*")
}
# Restore original working directory and delete temp folder
setwd(cwd)
unlink("temp",recursive = TRUE,force = TRUE)

existing_file_list <- file_path_sans_ext(list.files(Settings$HEISAccessPath))
years <- Settings$startyear:Settings$endyear
years_had_error <- setdiff(years,existing_file_list)
if (length(years_had_error)>0 ){
  cat("\n------------------\nFollowing years had errors; you have to provide")
  cat(" the *.mdb files manually for these years:\n")
  cat(years_had_error)
}

# Display execution time
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

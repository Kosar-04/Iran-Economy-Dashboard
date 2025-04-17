# 103-DownloaCompressedFiles.R
# downloads the RAR files that are not present in the folder 
# specified in Settings file
#
# Copyright © 2015: Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3


################################################################################
################################################################################
###                                                                          ###
###                                                                          ###
###           THIS CODE DOES NOT WORK FOR NOW.                               ###
###           SCI PROVIDES DATA IN A MARKET.                                 ###
###           (FOR FREE NOW).                                                ###
###           I'VE NOT FOUND DIRECT DOWNLOAD LINKS YET.                      ###
###           PUT YOUR DOWNLAODED COMPRESSED (RAR) FILES                     ###
###           IN THE HEIS COMPRESSED FILES PATH                              ###
###           (SPECIFIED ON SETTINGS FILE)                                   ###
###                                                                          ###
###                                                                          ###
################################################################################
################################################################################



rm(list=ls())

# Track the start time for performance logging
starttime <- proc.time()
cat("\n\n================ DownloaCompressedFiles =====================================\n")

# Load required libraries
library(yaml)
library(readxl)

# Load project settings from YAML configuration file
Settings <- yaml.load_file("Settings.yaml")

# Read metadata sheet from Excel file
# This sheet contains the list of compressed file names and corresponding years
compressed_file_names_df <- read_excel(path = Settings$MetaDataFilePath,
                                       sheet = Settings$MDS_CFN)

# Get a list of currently available compressed files in the directory
present_compressed_file_list <- list.files(Settings$HEISCompressedPath)

# Remove folder names from the list if any exist
x <- list.dirs(Settings$HEISCompressedPath, recursive = FALSE, full.names = FALSE)
present_compressed_file_list <- setdiff(present_compressed_file_list, x)

years <- Settings$startyear:Settings$endyear

existing_file_list <- list.files(Settings$HEISCompressedPath)

# Identify the files needed for the selected years
ys <- compressed_file_names_df$Year %in% years
needed_compressed_files_list <- compressed_file_names_df[ys,]$CompressedFileName

# Determine which files are missing and need to be downloaded
files_to_download <- setdiff(needed_compressed_files_list,existing_file_list)

# Attempt to download the missing files if any
if(length(files_to_download)>0){
  urls <- paste0(Settings$RawDataWebAddress,files_to_download)
  for(i in 1:length(files_to_download)){
    cat(paste0("Downloading file ",i," / ",
               length(files_to_download)," : ",files_to_download[i]),"\n")
    try(download.file(urls[i], paste0(Settings$HEISCompressedPath, files_to_download[i]),mode="wb"))
  }
}else{
    cat("All files in the range specified in Setting.yaml file are present, no need to download.")
}

# Track and display end time and duration
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

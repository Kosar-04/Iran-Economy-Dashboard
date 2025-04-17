# 101-BuildDirectoryStructure
# Builds the base data.table for households
#
# Copyright © 2016: Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ BuildDirectoryStructure =====================================\n")


# Load YAML library to read project-wide settings from external config file
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

# Create all required folders based on config settings
dir.create(Settings$HEISPath,showWarnings = FALSE)
dir.create(Settings$HEISCompressedPath,showWarnings = FALSE)
dir.create(Settings$HEISAccessPath,showWarnings = FALSE)
dir.create(Settings$HEISRawPath,showWarnings = FALSE)
dir.create(Settings$HEISProcessedPath,showWarnings = FALSE)
dir.create(Settings$HEISResultsPath,showWarnings = FALSE)


endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

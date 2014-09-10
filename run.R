#Shiny launch script

#the CSVs have to be in a subfolder of the following directory called "data"
path <- "C:/Users/PS-309/Desktop/Outfittery_IMT-test"

setwd(path)

#Mac
#Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")

#Windows
Sys.setlocale("LC_ALL","English")

library(caret)
library(data.table)
library(datasets)
library(e1071)
library(forecast)
library(graphics)
library(grDevices)
library(lattice)
library(lubridate)
library(MASS)
library(plyr)
library(stats)
library(stinepack)
library(timeDate)
library(utils)
library(zoo)
library(reshape2)
library(tree)
library(robust)
library(shiny)

source("import.R")
source("functions.R")
source("extra.R")
shiny::runApp()
#runGitHub("Outfittery_IMT","knusperman")
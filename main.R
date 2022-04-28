# Research Project MAPS
# Analyze CTQ in relationship to various SCID dx

# clear environment
  rm(list=ls())

# Package loading
if(!require(haven)) {install.packages("haven")}; library(haven)
if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if(!require(lavaan)){install.packages("lavaan")};library(lavaan)
if(!require(psych)) {install.packages("psych")}; library(psych)
if(!require(plyr)) {install.packages("plyr")}; library(plyr)
                                                        
# import SPSS master data file for MAPS
# note: this takes FOREVER! only load this once :)
source("import.R")

#Once importing SPSS dataset, run here and below 

# data cleaning
source("fn.R")
#call functions
removeNa()
recodeCTQ()
summarizeSCID()

# Research Project MAPS
# Analyze CTQ in relationship to various SCID dx

# clear environment
  rm(list=ls())

# Package loading
if(!require(haven)) {install.packages("haven")}; library(haven)
if(!require(plyr)) {install.packages("plyr")}; library(plyr)
if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if(!require(lavaan)){install.packages("lavaan")};library(lavaan)
if(!require(psych)) {install.packages("psych")}; library(psych)
if(!require(apaTables)) {install.packages("apaTables")}; library(apaTables)
if(!require(sjPlot))  {install.packages("sjPlot")}; library(sjPlot)
if(!require(reghelper))  {install.packages("reghelper")}; library(reghelper)
if(!require(parameters))  {install.packages("parameters")}; library(parameters)
if(!require(ggstatsplot))  {install.packages("ggstatsplot")}; library(ggstatsplot)
                                                        
# import SPSS master data file for MAPS
# note: this takes FOREVER! only load this once :)
source("import.R")

#Once importing SPSS dataset, run here and below 

# data cleaning #these dont work for some reason. Sad :(  Just run them directly from "fn.r" 
source("fn.R")
#call functions
removeNa()
recodeCTQ()
splitMAPS()
summarizeSCID()

#descriptive stats and correlations
source("descriptives.R")


## Clear the model workspace.
rm(list = ls())

## This line needs to be changed to the appropriate library path.
.libPaths("C:/Users/smyrna/Documents/R/win-library/3.3")

## This version of the model depends on some of the tidyverse packages.
library(dplyr)
library(ggplot2)
library(shiny)

## The wideScreen function is useful for debugging.
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}

## load.R contains definitions specifying where and which data to load.
## model.R contains the definitions of runModel() and its components.
## output.R contains the definition of generateBasicOutput().
source("load.R")
source("model.R")
source("output.R")

## Run 6-digit BACI model for 2014 (including AB) and generate output.
baci_1996_6digit_2014_ab <- runModel("baci", "1996", "6", 2014, TRUE)
generateBasicOutput(baci_1996_6digit_2014_ab)

## Run 4-digit BACI model for 2014 (including AB) and generate output.
baci_1996_4digit_2014_ab <- runModel("baci", "1996", "4", 2014, TRUE)

generateBasicOutput(baci_1996_4digit_2014_ab)

## Run 4-digit OEC model for 2014 (excluding AB).
oec_1996_6digit_2014_ab <- runModel("oec", "1996", "4", 2014, FALSE)

runApp("simple-app")

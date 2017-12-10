## This version of the model depends on some of the tidyverse packages.
library(dplyr)
library(ggplot2)
library(shiny)

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

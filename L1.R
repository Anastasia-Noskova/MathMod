library(tidyverse)
library(readr)
greendb <- read_csv("greendb.csv")
data = greendb

rad = data$d_trunk_m / 2

basal = rad * rad * 3.14
basal
greendb$basal = basal

data$height_m
v = greendb$basal * data$height_m
greendb$v = v
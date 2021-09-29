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

library(readr)
greendb <- read_csv("greendb.csv")
View(greendb)

library(dplyr)

unique(greendb$species_ru)
greendb$species_ru %>% unique
greendb$species_ru |> unique()

greendb$species_ru = factor(greendb$species_ru)
summary(greendb$species_ru)

#формиров табл с заданными переменными
sam_table = greendb %>% group_by(species_ru) %>%
  summarise (
             diam_m = mean(d_trunk_m, na.rm=T),
             num = n(),
             height_m = mean(height_m, na.rm=T)
  )

# кол-во видов в каждом районе
divers = greendb %>% group_by(adm_region, species_ru) %>%
  summarise (
             nspecies = n()
  )          %>% select(-nspecies) %>%
                 ungroup() %>% group_by(adm_region) %>%
             summarise(
               nspecies = n()
             )

#ДЗ

sam_table1 = greendb %>% group_by(adm_region, species_ru) %>%
  summarise (adm_region = adm_region) %>%
  ungroup() %>% group_by (adm_region,species_ru) %>%
  tally() %>%
  group_by(adm_region) %>%
  top_n(n=3)
  
  

  










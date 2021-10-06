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
  
  
sam_table2 = greendb %>% group_by(adm_region, species_ru) %>%
  group_by(adm_region) %>%
  arrange(adm_region, desc(nspecies)) %>%
  mutate (order = order(nspecies, decreasing = T)) %>%
  filter (order <= 3) %>% select(-order)

  z = c(3,7,6,8)
  order(z, decreasing = T)
  
    
  
#пара 29 сентября
  
transp = greendb %>% group_by(adm_region, species_ru) %>%
  summarise (nspecies = n())
  %>% pivot_wider()


   #2
library(sf)
adm_sf = st_read("boundary-polygon-lvl8", 
                 options ="ENCODING=UTF-8")
plot(map)


library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)

transp = greendb %>% group_by(adm_region, species_ru) %>%
  summarise(
    nspecies = n()
  ) %>% pivot_wider(names_from = species_ru, values_from = nspecies) %>%
  select(starts_with("Липа"))

library(sf)
library(ggplot2)
library(ggthemes)

map = st_read("boundary-polygon-lvl8",
              options = "ENCODING=UTF-8")
plot(map) 

domin = greendb %>%group_by(adm_region, species_ru) %>% 
  summarise(
    nspecies = n()
  ) %>% group_by(adm_region) %>%
  arrange(adm_region, desc(nspecies)) %>%
  mutate(order = order(nspecies, decreasing = T)) %>%
  filter( order == 1) %>% select(-order, -nspecies) %>%
  rename(NAME = adm_region)

map = left_join(map,domin, by="NAME")

ggplot() + geom_sf(data = map, aes(fill=species_ru))+
  theme_foundation() + theme(legend.title = element_blank())


heights = greendb %>%group_by(adm_region) %>% 
  summarise(
    num = mean(height_m, na.rm=T),
    height_m = mean(height_m, na.rm=T)
  ) %>% group_by(adm_region) %>%
  arrange(adm_region, desc(height_m)) %>%
  mutate(order = order(height_m, decreasing = T)) %>%
  filter( order == 1) %>% select(-order, -height_m) %>%
  rename(NAME = adm_region)

map2 = left_join(map,heights, by="NAME")
ggplot() + geom_sf(data = map2, aes(fill=num))+
  theme_foundation() + theme(legend.title = element_blank())








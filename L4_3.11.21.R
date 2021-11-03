library(tidyverse)
library(sf)
library(ggplot2)
library(ggthemes)

A = c(1,5,3)
B = c(3,-6,7)

diff_A = A - mean(A)
diff_B = B - mean(B)

cov = sum(diff_A * diff_B)/(length(A)-1)

var_A = sum(diff_A^2)/(length(A)-1)
var_B = sum(diff_B^2)/(length(B)-1)

sd_A = (sum(diff_A^2)/(length(A)-1))^.5
sd_B = (sum(diff_B^2)/(length(B)-1))^.5

cor = cov / (sd_A * sd_B)
# чило cor может быть (-1;1)
cor(A,B)

det = cor^2
#может быть (0;1)

data = read_csv("greendb.csv")

oak = data %>% filter(species_ru = "Дуб черешчатый") %>% filter (heidgt_m >10)
oak = oak %>% select(species_ru, height_m, d_trunk_m) %>% na.exclude()

cor(oak$height_m, oak$d_trunk_m)
plot(oak$height_m, oak$d_trunk_m)
plot(iris$Sepal.Length, iris$Sepal.Width)
cor(iris$Sepal.Length, iris$Sepal.Width)
iris2 = iris %>% filter (Species = "setosa")


        ### REGRESSION ###
### y = x
### y = k*x + b
#### k  результат
#### b  регрессии 

x = -100:100

y = 6*x+18
plot (x,y)

cor(x,y)
lm(y~x)
plot (x,y)

model1 = lm(data = iris2, Sepal.Length~Sepal.Width)
summary(model1)
plot (model1)
RSS = sum(model1$residuals^2)

ggplot(iris2)+
  geom_point(aes(Sepal.Length~Sepal.Width))+
  geom_smooth(aes(Sepal.Length~Sepal.Width), method = "lm")+
  theme_bw()



      ### NULL HYPOTHESIS: r,k,b = 0, ALTERNATIVE HYPOTHESISL: r,k,b /= 0
      ### ERROR = PREDICTED - REAL = RESIDUAL
      ### SUM(RESIDUAL^2) = RESIDUAL Sam of Squares = RSS
      ### regression detects linear models with minimal RSS


data = data %>% filter (height_m ,50, d_trunk_m <10)
part = data %>% filter (adm_region = "Тимирязевский район")
  
ggplot(data)+
  geom_point(aes(log(d_trunk_m), height_m))+
  geom_smooth(aes(log(d_trunk_m), height_m), method = "lm")+
  theme_bw()

ggplot(data)+
  geom_point(aes(d_trunk_m, log(height_m)))+
  geom_smooth(aes(d_trunk_m, log(height_m)), method = "lm")+
  theme_bw()

model_part = lm(height_m ~ d_trunk_m, data = part)
summary(model_part)
sum(model_part$residuals^2)

model_part_log = lm(height_m ~ log(d_trunk_m), data = part)
summary(model_part_log)
anova(model_part_log)

ggplot(populus %>% filter(
  adm_region == "Тимирязевский район",
  d_trunk_m < 0.6,
  d_trunk_m > 0.2))+
  geom_point(aes(d_trunk_m,height_m, color=adm_region))+
  geom_smooth(aes(d_trunk_m,height_m), method ="lm")+
  theme_bw()

###
### 

#ДЗ
# 1) Построить карту средних высот сирени по районам 
# 2) Показать, что район произрастания значимо влияет на высоты произрастания Сирени обыкновенной 
# 3) Посчитать регрессионную зависимость высоты от диаметра ствола сирени для района Внуково  

#1
library(tidyverse)
library(sf)
library(ggplot2)
library(ggthemes)
library(dplyr)

map = st_read ("greendb.csv")
plot(map)

average_heights = greendb %>% filter(species_ru == "Сирень обыкновенная", adm_region != "NA") %>% group_by(adm_region )%>%
  summarise(
    mean_height = mean(height_m, na.rm = TRUE)
  )

average_heights_data = average_heights %>% group_by(adm_region, mean_height) %>%
  arrange(adm_region, desc(mean_height)) %>%
  rename(NAME = adm_region)

map = left_join(map, average_heights_data, by="NAME")

ggplot() + geom_sf(data = map, aes(fill=mean_height))+
  theme_foundation() + theme(legend.title = element_blank())



#2 
data = read_csv(file = "greendb.csv")

data = data %>% filter(species_ru == "Сирень обыкновенная")
model = lm(height_m ~ adm_region, data)

anova(model)

# Analysis of Variance Table
# Response: height_m
#                 Df     Sum Sq Mean Sq  F value  Pr(>F)    
# adm_region     145 1.4154e+09 9761514  18.189 < 2.2e-16 ***
# Residuals  1047972 5.6241e+11  536667                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# вывод к 2 заданию: при P=0  можно сделать вывод, что район произростания 
# влияет на высоту сирени обыкновенной


#3

fit_data = greendb %>% 
  filter(species_ru == "Сирень обыкновенная", adm_region == "район Внуково") %>% select(
    d_trunk_m, height_m)

ggplot(fit_data, aes(x=d_trunk_m, y=height_m)) + 
  geom_point()+
  geom_smooth(method=lm)

summary(model1)
anova(model)

# d_trunk_m = 0.2234 * height_m = 0.015561
# p = 0.1519, R = 0.014 

# Analysis of Variance Table
# Response: d_trunk_m
#               Df  Sum Sq Mean Sq F value    Pr(>F)    
# height_m       1     2.2  2.2438  97.962 < 2.2e-16 ***
# Residuals 886817 20312.1  0.0229                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# p = 0, значит зависит





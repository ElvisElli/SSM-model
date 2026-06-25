library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(car)

#Reading the df with all the data
data_all <- read_excel("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/input/soybean_data_input.xlsx", 
                       sheet = "all mm")

df_harvest_all <- data_all %>% 
  filter(Harvest=="final") %>% 
  mutate(Genotype=as.factor(Genotype)) %>% 
  mutate(Rep=as.factor(Rep)) %>% 
  mutate(Harvest= case_when(Harvest=="final"~"4", TRUE~Harvest)) %>% 
  mutate(Harvest= as.factor(Harvest)) %>% 
  mutate(Year= as.factor(Year))

#Filtering only sarec

df_harvest_sarec <- data_all %>% 
  filter(Location=="sarec") %>% 
  filter(Harvest=="final") %>% 
  mutate(Year=as.factor(Year))
#Clearly to see if the water treatment impact HI
ggplot(df_harvest_sarec,aes(x = Treatment,y = HI))+
  geom_boxplot(alpha=0.4)+
  facet_wrap(~Year)
#Clearly to see if the water treatment impact on yield
ggplot(df_harvest_sarec,aes(x = Treatment,y = yield_kg_ha))+
  geom_boxplot(alpha=0.4)+
  facet_wrap(~Year)
#Clearly to see if the water treatment impact on biomass
ggplot(df_harvest_sarec,aes(x = Treatment,y = TotalBiomass_kg_ha))+
  geom_boxplot(alpha=0.4)+
  facet_wrap(~Year)


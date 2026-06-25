library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

#Reading the df with all the data
data_all <- read_excel("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/input/soybean_data_input.xlsx", 
                       sheet = "all mm")

#Selecting data from 2025 and selecting columns that will need for the QUALITY CONTROL.
df_2025 <- data_all %>% 
  filter(Year=="2025") %>% 
  select(Location,Harvest,Genotype,Treatment,Rep,Plot,Date,DOY,DAP,TotalBiomass_kg_ha,GreenBiomass,YellowBiomass,StemBiomass,ReprodBiomass,SeedBiomass,yield_kg_ha,HI,yield_combine
  )

#Mutate Genotype and Treatment as factor
df_25 <-df_2025 %>%
  mutate(Genotype = as.factor(Genotype)) %>% 
  mutate(Treatment=as.factor(Treatment))


#Summary to check the df
summary(df_25)

df_final25 <-df_25 %>% 
  filter(Harvest=="final")

#General plot
plot(df_final25$TotalBiomass_kg_ha, df_final25$yield_kg_ha) #Yield vs total biomass
plot(df_25$yield_kg_ha, df_2025$yield_combine)

#Plotting the relationship between the combine and 1 linear meter harvested
ggplot(data = df_final25,aes(x = yield_kg_ha,y = yield_combine)) +
  geom_point() +
  geom_abline(slope = 1,linetype="dashed", color="red") #at low yield values is not consistent the combine and manual harvest. 
                                                       #At higher yield values the combine is underestimating yield.
#Yield vs Total biomass
ggplot(data = df_final25,aes(x = TotalBiomass_kg_ha,y = yield_kg_ha,color=Treatment))+
  geom_point()+
  geom_abline(slope = 0.5,linetype="dashed", size=0.7,color="black")+
 # geom_abline(slope = 0.4,linetype="dashed", size=0.5, color="darkgreen")+
  #geom_abline(slope = 0.6,linetype="dashed", size=0.5, color="darkgreen")+
  scale_x_continuous(breaks = seq(0,12000, by= 1000), limits = c(0,12000))+
  scale_y_continuous(breaks = seq(0,6000, by= 1000),limits = c(0,6000))

mod <- lm (yield_kg_ha~TotalBiomass_kg_ha, data= df_final25)
coef(mod)

#Harvest information  from both years
df_harvest_all <- data_all %>% 
  filter(Harvest=="final") %>% 
  mutate(Genotype=as.factor(Genotype)) %>% 
  mutate(Rep=as.factor(Rep)) %>% 
  mutate(Harvest= case_when(Harvest=="final"~"4", TRUE~Harvest)) %>% 
  mutate(Harvest= as.factor(Harvest)) %>% 
  mutate(Year= as.factor(Year))

df_harvest_improve <- df_harvest_all %>% 
  unite("Site", Location, Year, sep = "_", remove=FALSE) %>% 
  mutate(Site=as.factor(Site))

f1 <- 
  ggplot(df_harvest_improve, aes(x = TotalBiomass_kg_ha,y = yield_kg_ha, shape = Treatment, color= Site))+ 
  geom_point()+
  geom_abline(slope = 0.5,linetype="dashed", size=0.7,color="black")+
  geom_abline(slope = 0.4,linetype="dashed", size=0.7,color="orange")+
  geom_abline(slope = 0.6,linetype="dashed", size=0.7,color="orange")+
  scale_x_continuous(breaks = seq(0,12000, by= 1000), limits = c(0,12000))+
  scale_y_continuous(breaks = seq(0,6000, by= 1000),limits = c(0,6000))
f1

ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/f1.tiff", dpi = 600, height = 20, width = 20, units = "cm")

#Same plot as f1 but coloring only the treatments, not identifying the site 

f1bw <- 
  ggplot(df_harvest_improve, aes(x = TotalBiomass_kg_ha,y = yield_kg_ha, shape = Treatment))+ 
  geom_point()+
  geom_abline(slope = 0.5,linetype="dashed", size=0.7,color="black")+
  geom_abline(slope = 0.4,linetype="dashed", size=0.7,color="orange")+
  geom_abline(slope = 0.6,linetype="dashed", size=0.7,color="orange")+
  scale_x_continuous(breaks = seq(0,12000, by= 1000), limits = c(0,12000))+
  scale_y_continuous(breaks = seq(0,6000, by= 1000),limits = c(0,6000))+
  scale_shape_manual(values = c(1,16))

ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/f1bw.tiff", dpi = 600, height = 20, width = 20, units = "cm")

ggplot(df_harvest_improve, aes(x = HI, color=Location, fill = Treatment))+
  facet_grid(~Location)+
  geom_density(alpha=0.1)

ggplot(df_harvest_sarec, aes(x = HI, color=Treatment))+
  geom_density(alpha=0.1)
#h1 <- 
  ggplot(df_harvest_all, aes(x = HI, color=Treatment))+
  geom_density(alpha=0.1)
h1
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/h1.tiff", dpi = 600, height = 20, width = 20, units = "cm")

ggplot(df_harvest_improve, aes(x = HI,y = yield_kg_ha, shape = Treatment))+
  geom_point()+
  scale_shape_manual(values = c(1,16))
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/h2.tiff", dpi = 600, height = 20, width = 20, units = "cm")

df_site_wt <- df_harvest_improve %>% 
  unite("Site_wt", Site, Treatment, sep = "_", remove=FALSE) %>% 
  mutate(Site=as.factor(Site_wt))

#f2 <- 
  ggplot(df_site_wt,aes(x = Site,y = yield_kg_ha, fill = Site))+
  geom_boxplot(alpha=0.4)
  #geom_jitter()
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/f2.tiff", dpi = 600, height = 20, width = 25, units = "cm")
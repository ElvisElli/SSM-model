library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

#Reading the df with all the data
data_all <- read_excel("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/input/soybean_data_input.xlsx", 
                                        sheet = "all mm")

#Selecting data from 2024 and selecting columns that will need for the QUALITY CONTROL.
df_2024 <- data_all %>% 
  filter(Year=="2024") %>% 
  select(Location,Harvest,Genotype,Treatment,Rep,Plot,Date,DOY,DAP,TotalBiomass_kg_ha,GreenBiomass,YellowBiomass,StemBiomass,ReprodBiomass,SeedBiomass,yield_kg_ha,HI
                )


#Mutate Genotype and Treatment as factor
df_24 <-df_2024 %>%
  mutate(Genotype = as.factor(Genotype)) %>% 
  mutate(Treatment=as.factor(Treatment))
str(df_24)

#Summary to check the df
summary(df_24)   #144 plots irrigated and 288 rain-fed for all three locations in 2024.
                #HI lowest value 0.10 but the 50% of the values are between 0.43 and 0.48

df_finalH <-df_24 %>% 
  filter(Harvest=="final")

#General plot
plot(df_finalH$TotalBiomass_kg_ha, df_finalH$yield_kg_ha)
plot(df_finalH$HI, df_finalH$yield_kg_ha)
plot(df_finalH$Treatment, df_finalH$yield_kg_ha)

#Physiological outlier
ggplot(df_finalH, aes(x = TotalBiomass_kg_ha,y = yield_kg_ha))+
  geom_point(na.rm=T)+
  geom_smooth(method = "lm", na.rm = T)
 # facet_wrap(~Location)

#Statistical outlier
df_finalH %>% 
  ggplot(aes(y = HI))+
  geom_boxplot()
  
summary(df_finalH$HI)

#Variation- In 2024 irrigated-sarec has a similar typical values than rainfed- rohwer and pinetree.
#There are some plots in rainfed conditions 
#as high as irrigated conditions. Rainfed-Sarec shows the lowest yield values.

ggplot(df_finalH, aes(x = yield_kg_ha))+
  geom_histogram(binwidth = 500)+
  facet_wrap(Location~Treatment)

#Biomass over time - sarec

df_sarec <- data_all %>% 
  filter(Location=="sarec", Year=="2024")

#General plot for SAREC 2024
plot(df_sarec$TotalBiomass_kg_ha, df_sarec$yield_kg_ha)

#
df_sarec_biomass <- df_sarec %>% 
filter(!is.na(Date)) %>%     #Removing the NAs
  filter(!is.na(StemBiomass)) %>%  
  mutate(Genotype=as.factor(Genotype)) %>% 
  mutate(Rep=as.factor(Rep)) %>% 
  mutate(Harvest= case_when(Harvest=="final"~"4", TRUE~Harvest)) %>% 
  mutate(Harvest= as.factor(Harvest))

ggplot(df_sarec_biomass, aes(x = DAP,y = TotalBiomass_kg_ha, color=Rep))+
  geom_line()+
  facet_wrap(Treatment~Genotype)+
  labs(title = "Biomass (kg/ha) per rep 2024")

str(df_sarec_biomass)
  
ggplot(df_sarec_biomass, aes(x = DOY,y = TotalBiomass_kg_ha, color=Rep))+
  geom_point()+
  geom_line()+
    #scale_y_continuous(breaks = df_sarec_biomass$DAP)+
  facet_wrap(Treatment~Genotype)+
  labs(title = "Biomass (kg/ha) per rep 2024")
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/biomass24-rep.tiff", dpi = 600, height = 20, width = 20, units = "cm")

summary(df_sarec_biomass)

#Ploting GreenBiomass over time

df_sarec_partition <- df_sarec_biomass %>% 
  filter(Genotype %in% c("R19C-1012","R18-14502","PI603457A","PI548431","PI471938","P52A14SE","P48A14E","P42A84E")) %>% 
  filter(!is.na(GreenBiomass)) %>%     #Removing the NAs
  mutate(RelGreenLeaves= GreenBiomass/TotalBiomass_kg_ha) %>%
  mutate(RelStemLeaves= StemBiomass/TotalBiomass_kg_ha)

ggplot(df_sarec_partition, aes(x = DOY,y = GreenBiomass, color=Rep))+  ###Green biomass over time expressed as kg/ha.
  geom_point()+
  geom_line()+
  #scale_y_continuous(breaks = df_sarec_biomass$DAP)+
  facet_wrap(Treatment~Genotype)+
  labs(title = "GreenBiomass (kg/ha) per rep 2024")
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/GreenBiomas-rep.tiff", dpi = 600, height = 20, width = 20, units = "cm")

###GreenBiomass as relative to total biomass.
ggplot(df_sarec_partition, aes(x = DOY,y = RelGreenLeaves, color=Rep))+
  geom_point()+
  geom_line()+
  #scale_y_continuous(breaks = df_sarec_biomass$DAP)+
  facet_wrap(Treatment~Genotype)+
  labs(title = "GreenBiomass (kg/ha) per rep 2024")
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/RelativeGreenBiomas-rep.tiff", dpi = 600, height = 20, width = 20, units = "cm")
  
ggplot(df_sarec_partition, aes(x = DOY,y = StemBiomass, color=Rep))+  ###Stem biomass over time expressed as kg/ha.
  geom_point()+
  geom_line()+
  facet_wrap(Treatment~Genotype)+
  labs(title = "StemBiomass (kg/ha) per rep 2024")

###Stem Biomass as relative to total biomass.
ggplot(df_sarec_partition, aes(x = DOY,y = RelStemLeaves, color=Rep))+
  geom_point()+
  geom_line()+
  facet_wrap(Treatment~Genotype)+
  labs(title = "StemBiomass (kg/ha) per rep 2024")

###Rohwer

df_rohwer <- data_all %>% 
  filter(Location=="rohwer") %>% 
filter(!is.na(Date)) %>%     #Removing the NAs
  mutate(Genotype=as.factor(Genotype)) %>% 
  mutate(Rep=as.factor(Rep)) %>% 
  mutate(Harvest= case_when(Harvest=="final"~"4", TRUE~Harvest)) %>% 
  mutate(Harvest= as.factor(Harvest))

str(df_rohwer)
 
plot(df_rohwer$TotalBiomass_kg_ha, df_rohwer$yield_kg_ha)
plot(df_finalH$HI, df_finalH$yield_kg_ha)
plot(df_finalH$Treatment, df_finalH$yield_kg_ha)

#Biomass over time

R1 <- ggplot(df_rohwer, aes(x = DAP,y = TotalBiomass_kg_ha, color=Rep))+
  geom_point()+
  geom_line()+
  facet_wrap(~Genotype)+
  labs(title = "Biomass (kg/ha) per rep 2024")
R1  
#What happen if I remove the lowest values considering them as ooutliers due to be wrong written.

df_rohwer_draft <- df_rohwer %>% 
  filter(yield_kg_ha >= 1500)

ggplot(df_rohwer_draft, aes(x = TotalBiomass_kg_ha,y = yield_kg_ha))+
  geom_point()+
  labs(title = "yield (kg/ha) 2024 - Rohwer")

ggplot(df_rohwer, aes(x = TotalBiomass_kg_ha,y = yield_kg_ha))+
  geom_point()+
  scale_y_continuous(breaks = seq(1000,6000, by= 1000),limits = c(1000,6000))+
  labs(title = "yield (kg/ha) 2024 - Rohwer")


###Pinetree

df_pinetree <- data_all %>% 
  filter(Location=="pinetree") %>% 
  filter(!is.na(Date)) %>%     #Removing the NAs
  mutate(Genotype=as.factor(Genotype)) %>% 
  mutate(Rep=as.factor(Rep)) %>% 
  mutate(Harvest= case_when(Harvest=="final"~"4", TRUE~Harvest)) %>% 
  mutate(Harvest= as.factor(Harvest))

str(df_pinetree)

summary(df_pinetree)

plot(df_pinetree$TotalBiomass_kg_ha, df_pinetree$yield_kg_ha)
plot(df_finalH$HI, df_finalH$yield_kg_ha)
plot(df_finalH$Treatment, df_finalH$yield_kg_ha)

#Biomass over time

 ggplot(df_pinetree, aes(x = DAP,y = TotalBiomass_kg_ha, color=Rep))+
  geom_point()+
  geom_line()+
  facet_wrap(~Genotype)+
  labs(title = "Biomass (kg/ha) per rep 2024 - Pinetree")


ggplot(df_pinetree, aes(x = TotalBiomass_kg_ha,y = yield_kg_ha))+
  geom_point()+
  scale_y_continuous(breaks = seq(1000,6000, by= 1000),limits = c(1000,6000))+
  labs(title = "yield (kg/ha) 2024 - Pinetree")


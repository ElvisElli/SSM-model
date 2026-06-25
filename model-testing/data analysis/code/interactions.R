library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

#Reading the df with all the data
data_all <- read_excel("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/input/soybean_data_input.xlsx", 
                       sheet = "all mm")

#First I will bring the data from both years -2024 and 2025

df_interaction <- data_all %>% 
  filter(Location=="sarec") %>% 
  filter(Harvest=="final") %>% 
  mutate(Year=as.factor(Year)) %>% 
  mutate(Treatment= as.factor(Treatment)) %>% 
  mutate(Plot=as.factor(Plot)) %>% 
  filter(Genotype !=" R19-42848")

df_mean <- df_interaction %>% 
  group_by(Genotype,Year, Treatment) %>%  
  filter(Genotype !="PI548431")%>%
  summarise(mean_hi=mean(HI, na.rm=T),
            sd_hi=sd(HI, na.rm=T),
            mean_biomass=mean(TotalBiomass_kg_ha,na.rm=T),
            sd_biomass=sd(TotalBiomass_kg_ha,na.rm=T),
            mean_yield=mean(yield_kg_ha, na.rm=T),
            sd_yield=sd(yield_kg_ha, na.rm=T),
            .groups = "drop")


#Interaction plots-HI

ggplot(data = df_interaction, aes(x = Treatment,y = HI))+
         geom_point()+
         facet_wrap(Genotype~Year)

#Face_wrap by Genotype to see the variability
ggplot(data = df_interaction, aes(x = Treatment,y = HI, group = Genotype, color=Genotype))+
  geom_point(alpha=0.4)+
  stat_summary(fun = mean,geom = "line")+   #Linea conecta los promedios
  stat_summary(fun = mean,geom = "point")+   #el punto mas oscuro es el de la media
  facet_wrap(Genotype~Year)
  
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/In1.tiff", dpi = 600, height = 20, width = 25, units = "cm")
#Aca ploteo todos los genotipos juntos, y sus medias
ggplot()+
  geom_point(data = df_mean,aes(x = Treatment,y = mean_hi,fill=Genotype),
             shape =21,
             color= "black",
             size = 3, stroke = 0.9)+
  geom_line(data = df_mean,aes(x = Treatment,y = mean_hi, group = Genotype, color=Genotype))+
  facet_wrap(~Year)
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/In2.tiff", dpi = 600, height = 20, width = 25, units = "cm")
    
#Interaction plots-yield
ggplot(data = df_interaction, aes(x = Treatment,y = yield_kg_ha, group = Genotype, color=Genotype))+
  geom_point(alpha=0.4)+
  stat_summary(fun = mean,geom = "line")+   #Linea conecta los promedios
  stat_summary(fun = mean,geom = "point")+   #el punto mas oscuro es el de la media
  facet_wrap(Genotype~Year)

ggplot()+
  geom_point(data = df_mean,aes(x = Treatment,y = mean_yield,fill=Genotype),
             shape =21,
             color= "black",
             size = 3, 
             stroke = 0.9)+
  geom_line(data = df_mean,aes(x = Treatment,y = mean_yield, group = interaction(Genotype,Year), color=Genotype))+ 
  facet_wrap(~Year)

  
#Interaction plots-biomass
ggplot(data = df_interaction, aes(x = Treatment,y = TotalBiomass_kg_ha, group = Genotype, color=Genotype))+
  geom_point(alpha=0.4)+
  stat_summary(fun = mean,geom = "line")+   #Linea conecta los promedios
  stat_summary(fun = mean,geom = "point")+   #el punto mas oscuro es el de la media
  facet_wrap(Genotype~Year)

ggplot()+
  geom_point(data = df_mean,aes(x = Treatment,y = mean_biomass,fill=Genotype),
             shape =21,
             color= "black",
             size = 3, 
             stroke = 0.9)+
  geom_line(data = df_mean,aes(x = Treatment,y = mean_biomass, group = interaction(Genotype,Year), color=Genotype))+
  facet_grid(~Year)

#Hierarchical analysis
#First i will need a data frame without the genotype label columns for the hierarchical analysis
matrix_hi_2024 <- df_mean %>% 
  filter(Year=="2024") %>% 
  mutate(Condition= paste0("HI_", Year,"_",Treatment)) %>% 
  select(Genotype,Condition,mean_hi) %>% 
  pivot_wider(names_from = Condition,
              values_from = mean_hi) %>% 
  column_to_rownames(var = "Genotype") 

matrix_hi_2025 <- df_mean %>% 
  filter(Year=="2025") %>% 
  mutate(Condition= paste0("HI_", Year,"_",Treatment)) %>% 
  select(Genotype,Condition,mean_hi) %>% 
  pivot_wider(names_from = Condition,
              values_from = mean_hi) %>% 
  column_to_rownames(var = "Genotype")

#But I will create this matrix again, keeping the genotype columns for better visualization
matrix_hi_ID <- df_mean %>% 
  mutate(Condition= paste0("HI_", Year,"_",Treatment)) %>% 
  select(Genotype,Condition,mean_hi) %>% 
  pivot_wider(names_from = Condition,
              values_from = mean_hi)
# let's see If I can plot the data- for 2024
ggplot(data = matrix_hi_ID,aes(x =HI_2024_Rainfed,y = HI_2024_Irrigated,color=Genotype))+
  geom_point(size=3)+
scale_x_continuous(breaks = seq(0.35,0.5, by= 0.05), limits = c(0.35,0.5))+
  scale_y_continuous(breaks = seq(0.35,0.5, by= 0.05),limits = c(0.35,0.5))+
  coord_fixed()


# I'll try to add yield information by point size. The yield information is the mean of the four reps.

df_matrix_2024 <- matrix_hi_ID %>%
  select(Genotype,HI_2024_Irrigated,HI_2024_Rainfed,) %>% 
left_join(df_mean, by = "Genotype") %>% 
  filter(Year=="2024") %>% 
  mutate(Yield_Group = case_when(
    mean_yield <1000 ~ "Low (>1000)",
    mean_yield >= 1000 & mean_yield < 1500 ~ "Medium (1000-1500)",
    mean_yield >= 1500 & mean_yield < 1700 ~ "High (1500-1700)",
    mean_yield >= 1700 ~ "More high (>1700)"
  )) %>%
  mutate(Yield_Group = factor(Yield_Group, 
                              levels = c("Low (>1000)", "Medium (1000-1500)", 
                                         "High (1500-1700)", "More high (>1700)")))

ggplot(data = df_matrix_2024,aes(x =HI_2024_Rainfed,y = HI_2024_Irrigated))+
  geom_point(aes(shape=Yield_Group, color=Genotype), alpha = 0.6,
             size=10,
             alpha=0.8)+
  scale_shape_manual(values = c(16, 17, 15, 8), name = "Yield") +
  scale_x_continuous(breaks = seq(0.35,0.5, by= 0.05), limits = c(0.35,0.5))+
  scale_y_continuous(breaks = seq(0.35,0.5, by= 0.05),limits = c(0.35,0.5))+
  coord_fixed()
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/24_yield_shape.tiff", dpi = 600, height = 20, width = 25, units = "cm")

ggplot(data = df_matrix_2024,aes(x =HI_2024_Rainfed,y = HI_2024_Irrigated, fill=Genotype))+
  geom_point(aes(size=mean_biomass), alpha = 0.6,
             shape=21,
             alpha=0.8)+
  scale_shape_manual(values = c(16, 17, 15, 8), name = "Yield") +
  scale_x_continuous(breaks = seq(0.35,0.5, by= 0.05), limits = c(0.35,0.5))+
  scale_y_continuous(breaks = seq(0.35,0.5, by= 0.05),limits = c(0.35,0.5))+
  coord_fixed()

#For 2025
df_matrix_2025 <- matrix_hi_ID %>%
  select(Genotype,HI_2025_Irrigated,HI_2025_Rainfed,) %>% 
  left_join(df_mean, by = "Genotype") %>% 
  filter(Year=="2025") %>% 
  mutate(Yield_Group = case_when(
    mean_yield <1000 ~ "Low (>1000)",
    mean_yield >= 1000 & mean_yield < 1500 ~ "Medium (1000-1500)",
    mean_yield >= 1500 & mean_yield < 1700 ~ "High (1500-1700)",
    mean_yield >= 1700 ~ "More high (>1700)"
  )) %>%
  mutate(Yield_Group = factor(Yield_Group, 
                              levels = c("Low (>1000)", "Medium (1000-1500)", 
                                         "High (1500-1700)", "More high (>1700)")))

ggplot(data = df_matrix_2025,aes(x =HI_2025_Rainfed,y = HI_2025_Irrigated))+
  geom_point(aes(shape=Yield_Group, color=Genotype), alpha = 0.6,
             size=10,
             alpha=0.8)+
  scale_shape_manual(values = c( 17, 15,8), name = "Yield") +
  scale_x_continuous(breaks = seq(0.45,0.56, by= 0.05), limits = c(0.45,0.56))+
  scale_y_continuous(breaks = seq(0.45,0.56, by= 0.05),limits = c(0.45,0.56))+
  coord_fixed()
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/25_yield_shape.tiff", dpi = 600, height = 20, width = 25, units = "cm")

ggplot(data = matrix_hi_ID,aes(x =HI_2025_Rainfed,y = HI_2025_Irrigated,color=Genotype))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0.45,0.56, by= 0.05), limits = c(0.45,0.56))+
  scale_y_continuous(breaks = seq(0.45,0.56, by= 0.05),limits = c(0.45,0.56))+
  coord_fixed()

# Función rápida para no repetir código
ejecutar_hc <- function(matriz) {
  dists <- dist(scale(matriz), method = "euclidean")
  return(hclust(dists, method = "ward.D2"))
}

hc_2024 <- ejecutar_hc(matrix_hi_2024)
hc_2025 <- ejecutar_hc(matrix_hi_2025)
plot(hc_2024,main = 2024)
plot(hc_2025,main=2025)

matrix_improve <- scale(matrix_hi)
distance <- dist(matrix_improve, method = "euclidean")  #Calculating euclidean distance
hc <- hclust(distance,method = "ward.D2") #Grouping by Ward method
plot(hc,main = "Hierarchical analysis")

##After seeing the interactions-Are the genotypes with a stable HI the high yielding ones?
ggplot(data = df_interaction,aes(x = Genotype))+
  geom_point(aes(y=yield_kg_ha),na.rm=T, alpha=0.3)+
  geom_point(data = df_mean, aes(y = mean_yield), size = 3, color = "red") +
  facet_wrap(Treatment~Year)+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) 

#Going back to the hierarchical analysis I want to check if the clusters are significant different 
#Clustering by 3 
grouping_2024 <- cutree(hc_2024, k=3)
grouping_2025 <-cutree(hc_2025,k=3)
#Cluster labels
hc_labeled_2024 <- matrix_hi_2024 %>% 
  mutate(Cluster=as.factor(grouping_2024))

#Trying to calculate the R2 to check how much variability I am accounting
sst <- sum(scale(matrix_hi_2024, scale=FALSE)^2)
ssw <- sum(sapply(unique(grouping_2024), function(g) {
  residuos <- scale(matrix_hi_2024[grouping_2024 == g, ], scale = FALSE)
  sum(residuos^2)
}))
r2 <- (sst - ssw) / sst
#My clusterisation account for 0.71 of the variability in 2024
###for 2025
sst25 <- sum(scale(matrix_hi_2025, scale=FALSE)^2)
ssw25 <- sum(sapply(unique(grouping_2025), function(g) {
  residuos <- scale(matrix_hi_2025[grouping_2025 == g, ], scale = FALSE)
  sum(residuos^2)
}))
r2_25 <- (sst25 - ssw25) / sst25

#My clusterisation account for 0.81 of the variability in 2025
#ANOVA (but do not know how to deal with both conditions)
aov_rain_2024 <- aov(HI_2024_Rainfed ~ Cluster:Genotype, data = hc_labeled_2024)
summary(aov_rain_2024)
aov_irrig_2024 <- aov(HI_2024_Irrigated ~ Cluster, data = hc_labeled_2024)
summary(aov_irrig_2024)
#Clusters are significant different in both conditions in 2024
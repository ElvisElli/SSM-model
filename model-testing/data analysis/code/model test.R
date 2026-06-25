library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(car)

library(emmeans)
library(agricolae)
library(multcompView)
#Readingcar#Reading the df with all the data
data_all <- read_excel("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/input/soybean_data_input.xlsx", 
                       sheet = "all mm")
df_harvest_all <- data_all %>% 
  filter(Harvest=="final") %>% 
  mutate(Genotype=as.factor(Genotype)) %>% 
  mutate(Rep=as.factor(Rep)) %>% 
  mutate(Harvest= case_when(Harvest=="final"~"4", TRUE~Harvest)) %>% 
  mutate(Harvest= as.factor(Harvest))


###YIELD

#First I will bring the data from both years -2024 and 2025

df_harvest_sarec <- data_all %>% 
  filter(Location=="sarec") %>% 
  filter(Harvest=="final") %>% 
  mutate(Year=as.factor(Year))

#Model to get the coefficient from the data and understand the value from the slope from the observed data.
modall <- lm (yield_kg_ha~TotalBiomass_kg_ha, data= df_harvest_sarec)
coef(modall)
#Model only 2024
mod24 <- lm (yield_kg_ha~TotalBiomass_kg_ha, data= df_sarec)
coef(mod)

#Both years together to see the distribution of HI
#Biomass and yield, and by color I represented the water treatments and with different shpes different years.
#From the data, under rainfed conditions all of the harvest index values are under the 0.5
#In irrigated conditions, harvest index is more dependent of the year conditions. On 2025, there are more values higher than 0.5 compared to 2024.
f1 <- ggplot(df_harvest_sarec, aes(x = TotalBiomass_kg_ha,y = yield_kg_ha, color=Treatment, shape = Year))+ 
  geom_point()+
  geom_abline(slope = 0.5,linetype="dashed", size=0.7,color="black")+
  scale_x_continuous(breaks = seq(0,12000, by= 1000), limits = c(0,12000))+
  scale_y_continuous(breaks = seq(0,6000, by= 1000),limits = c(0,6000))
f1

ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/f1.tiff", dpi = 600, height = 20, width = 20, units = "cm")

#Starting with anova analysis.
#Visually see the distribution of the independent variables
p1 <- ggplot(df_harvest_sarec, aes(x = yield_kg_ha, color=Treatment, fill = Treatment))+
  geom_density(alpha=0.1)
p1
#Cheching the distribution of yield between irrigated and rainfed, visually there are big differences between the means. 
#There is interaction between yield and water treatment
p2 <- ggplot(df_harvest_sarec, aes(x = yield_kg_ha, color=Year, fill = Year))+
  geom_density(alpha=0.1)
p2

#Visually the different between means of 2024 and 2025 is not that clear.
#Normality data
hist(df_harvest_sarec$yield_kg_ha)
boxplot(df_harvest_sarec$yield_kg_ha)

hist(df_harvest_sarec$TotalBiomass_kg_ha)
boxplot(df_harvest_sarec$TotalBiomass_kg_ha)

hist(df_harvest_sarec$HI)
boxplot(df_harvest_sarec$HI)
#Normality model - residuals.
res_aov <- aov(yield_kg_ha~Genotype, data=df_harvest_sarec)
res_aov
plot(res_aov$residuals)
hist(res_aov$residuals)
qqPlot(res_aov$residuals) # Many point outside the normal distribution

shapiro.test(res_aov$residuals)

#Homocedasticidad
mod1 <- aov(yield_kg_ha ~ Genotype*Treatment * Year, data = df_harvest_sarec)
leveneTest(mod1)

plot(mod1, 3)
summary(mod1)
qqPlot(mod1$residuals)

#Checking homogeneity of variances for biomass

mod2 <- aov(TotalBiomass_kg_ha ~ Genotype*Treatment * Year, data = df_harvest_sarec)
leveneTest(mod2)
#Checking normality for biomass
plot(mod2, 1)
qqPlot(mod2$residuals)
summary(mod2)

#Checking homogeneity of variances for HI

mod3 <- aov(HI ~ Genotype*Treatment * Year, data = df_harvest_sarec)
leveneTest(mod3)
#Checking normality for HI
plot(mod3, 1)
qqPlot(mod3$residuals)
summary(mod3)

####Grouping location per year to have Site column.

df_harvest_improve <- df_harvest_all %>% 
  unite("Site", Location, Year, sep = "_", remove=FALSE) %>% 
  mutate(Site=as.factor(Site))
str(df_harvest_improve)

#Homocedasticidad
mod3 <- aov(yield_kg_ha ~ Site*Genotype*Treatment , data = df_harvest_improve)
leveneTest(mod3)

plot(mod3, 1)  #This is to see the distribution of the residuals from the model.
summary(mod3)  ##I run  summary to see which factor from the model are significant determining the response variable.
qqPlot(mod1$residuals)

#Checking homogeneity of variances for biomass

mod4 <- aov(TotalBiomass_kg_ha ~ Genotype*Treatment * Site, data = df_harvest_improve)
leveneTest(mod4)

#Checking normality for biomass
plot(mod4, 1)
qqPlot(mod4$residuals)
summary(mod4)

#Checking homogeneity of variances for HI

mod5 <- aov(HI ~ Genotype*Treatment * Site, data = df_harvest_improve)
leveneTest(mod5)
#Checking normality for HI
plot(mod5, 1)
qqPlot(mod5$residuals)
summary(mod5)


################Considering only sarec to see differences in the variance when adding year as factor 
mod6 <- aov(yield_kg_ha ~ Year*Genotype*Treatment , data = df_harvest_sarec)
leveneTest(mod6)

plot(mod6, 1)  #This is to see the distribution of the residuals from the model.
summary(mod6)  ##I run  summary to see which factor from the model are significant determining the response variable.
qqPlot(mod6$residuals)
#Post hoc analysis.
TukeyHSD(mod6, "Year")
TukeyHSD(mod6, "Treatment")
#Checking homogeneity of variances for biomass

mod7 <- aov(TotalBiomass_kg_ha ~ Year*Genotype*Treatment, data = df_harvest_sarec)
leveneTest(mod7)

#Checking normality for biomass
plot(mod7, 1)
qqPlot(mod7$residuals)
summary(mod7)
TukeyHSD(mod7, "Year")
TukeyHSD(mod7, "Treatment")

#Checking homogeneity of variances for HI

mod8 <- aov(HI ~ Year*Genotype*Treatment , data = df_harvest_sarec)
leveneTest(mod8)
#Checking normality for HI
plot(mod8, 1)
qqPlot(mod8$residuals)
summary(mod8)



# Comparaciones para cada factor independiente
TukeyHSD(mod8, "Genotype")
TukeyHSD(mod8, "Treatment")
TukeyHSD(mod8, "Year")
out <- HSD.test(mod8, "Genotype", group = TRUE)
print(out$groups)

#I will try to fit a model for yield depending on biomass and another one for harvets index

plot(df_harvest_sarec$TotalBiomass_kg_ha, df_harvest_sarec$yield_kg_ha)


######What about the grain weight?
##the harvest in 2024 was manual, so there is a correlation between yield and seed weight.
df_2025w <- df_harvest_sarec %>% 
  filter(Year=="2025")

mod_weight <- lm (yield_kg_ha~SeedTB, data = df_2025w)
coef(mod_weight)
plot(mod_weight)
(plot(mod_weight, which = 2))
shapiro.test(resid(mod_weight))
hist(resid(mod_weight), breaks = 20, main = "Distribución de Residuos")

plot(df_2025w$SeedTB, df_2025w$yield_combine)


#Graphs
model1<- lm(yield_kg_ha~TotalBiomass_kg_ha, data = df_harvest_sarec)
summary(model1)
par(mfrow = c(2, 2))
plot(model1)

r2_valueMOD1 <- paste0("R^2 ==",round(summary(model1)$r.squared, 3))
interceptmod1 <-coef(model1)[1]
slopemod1 <- coef(model1)[2]

model2<- lm(yield_kg_ha~HI, data = df_harvest_sarec)
summary(model2)
par(mfrow = c(2, 2))
plot(model2)

r2_valueMOD2 <- paste0("R^2 ==",round(summary(model2)$r.squared, 3))
interceptmod2 <-coef(model2)[1]
slopemod2 <- coef(model2)[2]

#Now I will check the correlation between biomass and HI
model3<- lm(TotalBiomass_kg_ha~HI, data = df_harvest_sarec)
summary(model3)
par(mfrow = c(2, 2))
plot(model3)
r2_valueMOD3 <- paste0("R^2 ==",round(summary(model3)$r.squared, 3))
interceptmod3 <-coef(model3)[1]
slopemod3 <- coef(model3)[2]

#ggplot with the model coefficients and the r square

lm1 <- ggplot(data = df_harvest_sarec,aes(x = TotalBiomass_kg_ha,y = yield_kg_ha))+
  geom_point()+
  geom_abline(intercept = interceptmod1,slope = slopemod1)+
  annotate("text",label=r2_valueMOD1,x = 9000,y = 1000, parse=T)
lm1
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/lm1.tiff", dpi = 600, height = 20, width = 20, units = "cm")

lm2 <- ggplot(data = df_harvest_sarec,aes(x = HI,y = yield_kg_ha))+
  geom_point()+
  geom_abline(intercept = interceptmod2,slope = slopemod2)+
  annotate("text",label=r2_valueMOD2,x = 0.3,y = 3000, parse=T)
lm2
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/lm2.tiff", dpi = 600, height = 20, width = 20, units = "cm")

ggplot(data = df_harvest_sarec,aes(x = HI,y = TotalBiomass_kg_ha))+
  geom_point()+
  geom_abline(intercept = interceptmod3,slope = slopemod3)+
  annotate("text",label=r2_valueMOD3,x = 0.3,y = 9000, parse=T)
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/lm3.tiff", dpi = 600, height = 20, width = 20, units = "cm")

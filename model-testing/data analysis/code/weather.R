library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)
install.packages("lubridate")

#Reading the df with the data from 2024
weather_2024 <- read_csv("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/input/daily-data_weather_2024..csv" 
                     )
#I make all the corrections in the 2024 df
w_24 <- weather_2024 %>% 
  rename(Tmax="MaxAirTemp") %>% 
  rename(Tmin="MinAirTemp") %>% 
  mutate(Tmax=as.numeric(Tmax)) %>% 
  mutate(Tmin=as.numeric(Tmin)) %>% 
  mutate(pp_mm=as.numeric(pp_mm)) %>% 
  mutate(Date=mdy(Date))
str(w_24)

#To simpler view de pp, I separate the info in month
df_monthly24 <- w_24 %>% 
  mutate(.before = Date, Month=month(Date, label=TRUE)) %>% 
  group_by(Month) %>% 
  summarise(Total_pp=sum(pp_mm))
#pp plot
ggplot(data = df_monthly24, aes(x =Month,y = Total_pp))+
  geom_bar(stat = "identity", fill = "skyblue")+
  labs(title = "Precipitacion (mm) 2024")

pp_accumulated <- w_24 %>% 
  mutate(.before=Tmin, pp_all=cumsum(pp_mm))

#Accumulated pp
p1 <- ggplot(pp_accumulated, aes(x = DAP, y = pp_all)) +
  geom_area(fill = "steelblue", alpha = 0.4) + # Relleno azul transparente
  geom_line(color = "darkblue", size = 1) +    # Línea superior más marcada
  labs(title = "Cumulative pp during cop cycle",
       x = "DAP",
       y = "accumulated mm") +
  theme_minimal()
p1
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/w24_1.tiff", dpi = 600, height = 20, width = 25, units = "cm")


p2 <- ggplot(data=w_24)+
  geom_line(aes(x = DAP,y = pp_mm), color="blue")
p2
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/w24_2.tiff", dpi = 600, height = 20, width = 25, units = "cm")

#Daily temperatures for 2024
p3 <- ggplot(data=w_24)+
  geom_line(aes(x = as.Date(Date, format("%Y-%m-%d")),y = Tmax), color="red")+
  geom_line(aes(x = as.Date(Date, format("%Y-%m-%d")),y = Tmin), color="blue")+
  labs(x="Month", y="Daily temperature (C)", title = "Daily temperatures for 2024")
p3
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/w24_3.tiff", dpi = 600, height = 20, width = 25, units = "cm")

###ANALYSIS FOR 2025
weather_2025 <- read_xlsx("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/input/weather_all.xlsx")
#I make all the corrections in the 2025 df
w_25 <- weather_2025 %>%
  mutate(Date=ymd(Date)) %>% 
  filter(Date >= as.Date("2025-06-03")) %>% 
  rename(Tmax="MaxAirTemp") %>% 
  rename(Tmin="MinAirTemp") %>% 
  mutate(Tmax=as.numeric(Tmax)) %>% 
  mutate(Tmin=as.numeric(Tmin)) %>% 
  mutate(pp_mm=as.numeric(pp_mm)) 
str(w_24)


pp_accumulated_25 <- w_25 %>% 
  mutate(.before=Tmin, pp_all=cumsum(pp_mm))

#Accumulated pp
p4 <-
  ggplot(pp_accumulated_25, aes(x = DAP, y = pp_all)) +
  geom_area(fill = "steelblue", alpha = 0.4) + # Relleno azul transparente
  geom_line(color = "darkblue", size = 1) +    # Línea superior más marcada
  labs(title = "Cumulative pp during cop cycle",
       x = "DAP",
       y = "accumulated mm") +
  theme_minimal()
p4
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/w25_1.tiff", dpi = 600, height = 20, width = 25, units = "cm")


p5 <- ggplot(data=w_25)+
  geom_line(aes(x = DAP,y = pp_mm), color="blue")
p5
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/w25_2.tiff", dpi = 600, height = 20, width = 25, units = "cm")

#Daily temperatures for 2024
p6 <- ggplot(data=w_25)+
  geom_line(aes(x = as.Date(Date, format("%Y-%m-%d")),y = Tmax), color="red")+
  geom_line(aes(x = as.Date(Date, format("%Y-%m-%d")),y = Tmin), color="blue")+
  labs(x="Month", y="Daily temperature (C)", title = "Daily temperatures for 2024")
p6
ggsave("C:/Users/mmeira/Box/Martina/soybean field experiment/data analysis/output/w25_3.tiff", dpi = 600, height = 20, width = 25, units = "cm")

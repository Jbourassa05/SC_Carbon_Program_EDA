library(tidyverse)

library(readr)
EM_CanEconSec <- read_csv("EM_CanEconSec.csv")
View(EM_CanEconSec)

Data<-EM_CanEconSec%>%
  filter(Category == "Total",
         Region != "British Columbia",
         Region != "New Brunswick",
         Region != "Newfoundland and Labrador",
         Region != "Nova Scotia",
         Region != "Prince Edward Island",
         Region != "Quebec", 
         Year >= 2000)

Data%>%
  ggplot(aes(Year, CO2.eq))+
  geom_point(aes(color = Region))+
  geom_line(aes(color = Region), alpha=.5)+
  geom_vline(xintercept=2005, alpha = .25)+
  geom_smooth(method = lm, se = FALSE, aes(color = Region))+
  ggtitle("Total Agricultural Emissions: 2000-2019")+
  ylab("Mt CO2eq")

Data<-EM_CanEconSec%>%
  filter(Category == "On Farm Fuel Use",
         Region != "British Columbia",
         Region != "New Brunswick",
         Region != "Newfoundland and Labrador",
         Region != "Nova Scotia",
         Region != "Prince Edward Island",
         Region != "Quebec",
         Year >= 2000)
Data%>%
  ggplot(aes(Year, CO2.eq))+
  geom_point(aes(color = Region))+
  geom_line(aes(color = Region), alpha=.5)+
  geom_vline(xintercept=2005, alpha = .25)+
  geom_smooth(method = lm, se = FALSE, aes(color = Region))+
  ggtitle("Total On Farm Fuel Emissions: 2000-2019")+
  ylab("Mt CO2 eq")

Data<-EM_CanEconSec%>%
  filter(Category == "Crop Production",
         Region != "British Columbia",
         Region != "New Brunswick",
         Region != "Newfoundland and Labrador",
         Region != "Nova Scotia",
         Region != "Prince Edward Island",
         Region != "Quebec",
         Year >= 2000)
Data%>%
  ggplot(aes(Year, CO2.eq))+
  geom_point(aes(color = Region))+
  geom_line(aes(color = Region), alpha=.5)+
  geom_vline(xintercept=2005, alpha = .25)+
  geom_smooth(method = lm, se = FALSE, aes(color = Region))+
  ggtitle("Total Emissions from Crop Production: 2000-2019")+
  ylab("Mt CO2 eq")

Data<-EM_CanEconSec%>%
  filter(Category == "Animal Production",
         Region != "British Columbia",
         Region != "New Brunswick",
         Region != "Newfoundland and Labrador",
         Region != "Nova Scotia",
         Region != "Prince Edward Island",
         Region != "Quebec",
         Year >= 2000)
Data%>%
  ggplot(aes(Year, CO2.eq))+
  geom_point(aes(color = Region))+
  geom_line(aes(color = Region), alpha=.5)+
  geom_vline(xintercept=2005, alpha = .25)+
  geom_smooth(method = lm, se = FALSE, aes(color = Region))+
  ggtitle("Total Emissions from Animal Production: 2000-2019")+
  ylab("Mt CO2 eq")

Data<-EM_CanEconSec%>%
  filter(Region == "Alberta",
         Year >= 2000)
Data%>%
  ggplot(aes(Year, CO2.eq))+
  geom_point(aes(color = Category))+
  geom_line(aes(color = Category), alpha=.5)+
  geom_vline(xintercept=2005, alpha = .25)+
  geom_smooth(method = lm, se = FALSE, aes(color = Category))+
  ggtitle("Alberta Agricultural Emissions: 2000-2019")+
  ylab("Mt CO2 eq")
  

Data<-EM_CanEconSec%>%
  group_by(Category, Year)%>%
  mutate(Total = sum(CO2.eq))

T.Data <- Data %>%
  select(Year, Category, Total)%>%
  filter(Year >= 2005)

T.Data %>%
  ggplot(aes(Year, Total))+
  geom_point(aes(color = Category))+
  geom_line(aes(color = Category))+
  ggtitle("Total Agricultural Emissions: Canadian economic sector measurments")+
  ylab("Mt CO2e")

Data<-EM_CanEconSec%>%
  filter(Category =="Total",
         Year >= 2000)

Data %>%
  ggplot(aes(Year, CO2.eq))+
  geom_point(aes(color = Region))+
  geom_line(aes(color = Region))+
  ggtitle("Total Agricultural Emissions by Province")+
  ylab("Mt CO2e")
  


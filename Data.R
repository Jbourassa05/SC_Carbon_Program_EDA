library(tidyverse)
library(readr)
Can_Econ_Sec_Ag <- read_csv("SPP/Data/Can_Econ_Sec_Ag.csv")
View(Can_Econ_Sec_Ag)

GDP_AG <- read_csv("SPP/Data2/GDP.AG.csv")
View(GDP_AG)

Total_Cash <- read_csv("SPP/Data2/Total.Cash.csv")
View(Total_Cash)

EM.GDP.Cash <- Can_Econ_Sec_Ag%>%
  select("Year", "Region", "Category", "CO2eq")%>%
  filter(Year >= 2000)
  
Total_Cash<- Total_Cash%>%
  filter(Region != "Canada")

GDP_AG<-GDP_AG%>%
  filter(Region != "Canada")

EM.GDP.Cash<- full_join(EM.GDP.Cash,GDP_AG, by=c("Year"="Year","Region"="Region", "Category"="Category"))
EM.GDP.Cash<- full_join(EM.GDP.Cash,Total_Cash, by=c("Year"="Year","Region"="Region", "Category"="Category"))

EM<-EM.GDP.Cash%>%
  filter(Category!="On Farm Fuel Use",
         Region %in% c("Alberta", "Manitoba", "Ontario", "Quebec", "Saskatchewan"))%>%
  mutate(FarmGate = (CO2eq/VALUE.M.y)*1000,
         GDP = (CO2eq/VALUE.M.x)*1000)


farmGate<-EM%>%
  ggplot(aes(Year, FarmGate,color = Category))+
  geom_point(aes(group = Region))
  

  Prod.2018.EM<- Area.Production%>%
  filter(Year==2018,
         Region != "Canada",
         Region != "British Columbia",
         Category %in% c("Wheat", "Canola", "Barley", "Oats", "Soybeans"))%>%
  mutate(Category = fct_reorder(Category, desc(EM)),
         Region = fct_reorder(Region, EM),
         KtCO2e = EM/1000)%>%
  ggplot(aes(Category, KtCO2e, fill= Region))+
  geom_col(position = "dodge")+
  scale_fill_brewer(direction = 1)+
  ggtitle("Estimated Emissions by Crop type: 2018")+
  ylab("KtCO2e")+
  labs(fill = "Province")+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.title = element_blank())
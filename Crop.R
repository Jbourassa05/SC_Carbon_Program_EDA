# Crop Data
library(tidyverse)
library(readr)
Crop_EF <- read_csv("SPP/Crop.EM/Crop.EF.csv")
View(Crop_EF)

Crop_EF<-Crop_EF%>%gather(Crop, EF, 2:18)


Crop_Production <- read_csv("SPP/Crop.EM/Crop.Production.csv")
View(Crop_Production)

Area<-Crop_Production%>%
  filter(Measurment == "Seeded area (hectares)")%>%
  rename(Seeded.Ha = VALUE)%>%
  select("REF_DATE", "GEO", "Crop", "Seeded.Ha")%>%
  spread(Crop, Seeded.Ha)%>%
  rename(Beans.Dry = `Beans, all dry (white and coloured)`,
         Canola = `Canola (rapeseed)`,
         Corn.Grain = `Corn for grain`,
         Corn.Silage = `Corn for silage`,
         Mixed.Grain = `Mixed grains`,
         Peas.Dry = `Peas, dry`,
         Rye = `Rye, all`,
         Sugar.Beets = `Sugar beets`,
         Tame.Hay = `Tame hay`,
         Wheat = `Wheat, all`)%>%
  gather(Crop, Seeded.Ha, 3:16)%>%
  spread(GEO, Seeded.Ha)%>%
  replace(is.na(.), 0)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, Seeded.Ha, 3:13)%>%
  filter(GEO != "Newfoundland and Labrador",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick")
  
Crop<-full_join(Area, Crop_EF, by=c("GEO"="GEO", "Crop"="Crop"))

Crop<-Crop%>%
  mutate(EM.Crop = Seeded.Ha*EF)%>%
  replace(is.na(.), 0)


Production <- Crop_Production%>%
  filter(Measurment != "Seeded area (hectares)")%>%
  spread(Measurment, VALUE)%>%
  mutate(Total.Production = `Harvested area (hectares)`*`Average yield (kilograms per hectare)`)%>%
  select(REF_DATE, GEO, Crop, Total.Production)%>%
  spread(Crop, Total.Production)%>%
  rename(Beans.Dry = `Beans, all dry (white and coloured)`,
         Canola = `Canola (rapeseed)`,
         Corn.Grain = `Corn for grain`,
         Corn.Silage = `Corn for silage`,
         Mixed.Grains = `Mixed grains`,
         Peas.Dry = `Peas, dry`,
         Rye = `Rye, all`,
         Sugar.Beets = `Sugar beets`,
         Tame.Hay = `Tame hay`,
         Wheat = `Wheat, all`)%>%
  gather(Crop, Total.Production, 3:15)%>%
  spread(GEO, Total.Production)%>%
  replace(is.na(.), 0)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, Total.Production, 3:13)%>%
  filter(GEO != "Newfoundland and Labrador",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick")

Crop<-full_join(Crop, Production, by = c("REF_DATE" = "REF_DATE", "Crop" = "Crop", "GEO"="GEO"))

Crop<-mutate(Crop, EM.Production= EM.Crop/Total.Production )

Potato <- read_csv("SPP/Crop.EM/Potato.csv")
View(Potato)

P.Seeded<-Potato%>%
  filter(Measurement == "Seeded area, potatoes")%>%
  spread(Measurement, Metric.Value)%>%
  rename(Potatoes =`Seeded area, potatoes`)%>%
  gather(Crop, Seeded.Ha, 4)%>%
  select(!Value)%>%
  spread(GEO, Seeded.Ha)%>%
  replace(is.na(.), 0)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, Seeded.Ha, 3:13)%>%
  filter(GEO != "Newfoundland and Labrador",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick")
  

P.EF<-Crop_EF%>%
  filter(Crop == "Potatoes")

Potatoes<-full_join(P.Seeded,P.EF, by = c("GEO"="GEO", "Crop"="Crop"))
  

P.Production<-Potato%>%
  filter(Measurement != "Seeded area, potatoes")%>%
  select(!Value)%>%
  spread(Measurement, Metric.Value)%>%
  mutate(Total.Production =`Average yield, potatoes`*`Harvested area, potatoes`)%>%
  select(REF_DATE, GEO, Total.Production)%>%
  spread(GEO, Total.Production)%>%
  replace(is.na(.), 0)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, Total.Production, 2:12)%>%
  filter(GEO != "Newfoundland and Labrador",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick")

Potatoes<-full_join(Potatoes,P.Production, by = c("REF_DATE"="REF_DATE", "GEO"="GEO"))

Potatoes<-Potatoes%>%
  mutate(EM.Crop = Seeded.Ha*EF,
         EM.Production = EM.Crop/Total.Production )

Field_Crop<-full_join(Crop,Potatoes, by=c("REF_DATE"="REF_DATE",
                                          "GEO"= "GEO",
                                          "Crop"="Crop",
                                          "Seeded.Ha" = "Seeded.Ha",
                                          "EF"="EF", 
                                          "EM.Crop" = "EM.Crop",
                                          "Total.Production" = "Total.Production",
                                          "EM.Production" = "EM.Production" ))
Field_Crop1<-Field_Crop%>%
  filter(REF_DATE !=0,
         EM.Production !=0)
Field_Crop_2019<-Field_Crop1%>%
  filter(REF_DATE==2019)



Crop1<-Field_Crop_2019%>%
  filter(Crop %in% c("Wheat", "Barley","Oats","Canola", "Corn.Grain"))%>%
  mutate(EF = EF/1000)%>%
  ggplot(aes(Crop, EF, fill= GEO))+
  geom_col(position = "dodge", color = "black")+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Carbon Footprint of Canadian Field Crops(t CO2eq/ha)")+
  theme(title = element_text(size = 20),
    axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 4, .1), breaks = seq(0, 4, .5))
  
 

Crop2<-Field_Crop_2019%>%
  filter(Crop %in% c("Wheat", "Barley","Oats","Canola", "Corn.Grain"))%>%
  ggplot(aes(Crop, EM.Production, fill= GEO))+
  geom_col(position = "dodge", color = "black")+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Emissions Per Unit Produced (kg Co2eq / kg Production)")+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 1.5, .1), breaks = seq(0, 1.5, .20))

Crop_Farm_Cash_2019 <- read_csv("SPP/Crop.EM/Crop.Farm.Cash.2019.csv")
View(Crop_Farm_Cash_2019)

Cash_2019<-Crop_Farm_Cash_2019%>%
  spread(`Type of cash receipts`, VALUE)%>%
  replace(is.na(.), 0)%>%
  mutate(Wheat = `Durum wheat [112111211]`+`Wheat (except durum wheat) [1121111]`)%>%
  rename(Barley = `Barley [1151141]`,
         Canola = `Canola (including rapeseed) [113111]`,
         Corn.Grain = `Corn for grain [1151111]`,
         Oats = `Oats [115113111]`)%>%
  gather(Crop, Farm.Cash, 4:10)%>%
  filter(Crop !="Durum wheat [112111211]",
         Crop !="Wheat (except durum wheat) [1121111]")%>%
  spread(GEO, Farm.Cash)%>%
  mutate(Atlantic = `Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, Farm.Cash, 4:14)%>%
  filter(GEO != "Canada",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick")


Field_Crop_2019<-full_join(Field_Crop_2019, Cash_2019, by=c("REF_DATE"="REF_DATE", "GEO"="GEO", "Crop"="Crop"))
  
Crop3<-Field_Crop_2019%>%
  filter(Crop %in% c("Wheat", "Barley","Oats","Canola", "Corn.Grain"),
         Farm.Cash > 0)%>%
  mutate(EM.Cash = EM.Production/Farm.Cash,
         GEO = fct_reorder(GEO,desc(Farm.Cash)))%>%
  ggplot(aes(Crop, EM.Cash, fill= GEO))+
  geom_col(position = "dodge") 



install.packages("viridis")
library(viridis)
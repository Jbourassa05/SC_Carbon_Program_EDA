library(readr)
Field <- read_csv("SPP/Land.Use/Field.csv", 
                  na = "0")
View(Field)

Field<-Field%>%
  replace(is.na(.), 0)%>%
  group_by(REF_DATE, GEO)%>%summarise(Field.Crop.Acres = sum(VALUE))

Sod <- read_csv("SPP/Land.Use/Sod.csv")
View(Sod)

Sod<-Sod%>%
  replace(is.na(.), 0)%>%
  group_by(REF_DATE, GEO)%>%summarise(Sod.Acres = sum(VALUE))

Potatoes <- read_csv("SPP/Land.Use/Potatoes.csv")
View(Potatoes)

Potatoes<-Potatoes%>%
  rename(Potatoes.Acres = VALUE)%>%
  select(REF_DATE, GEO, Potatoes.Acres)

Fruit <- read_csv("SPP/Land.Use/Fruit.csv")
View(Fruit)

Fruit<-Fruit%>%
  replace(is.na(.), 0)%>%
  group_by(REF_DATE, GEO)%>%summarise(Fruit.Acres = sum(VALUE))

Veg <- read_csv("SPP/Land.Use/Veg.csv")
View(Veg)

Veg<-Veg%>%
  replace(is.na(.), 0)%>%
  group_by(REF_DATE, GEO)%>%summarise(Veg.Acres = sum(VALUE))

Land.Area<-full_join(Field,Sod, by=c("REF_DATE"="REF_DATE", "GEO"="GEO"))%>%
  replace(is.na(.), 0)

Land.Area<-full_join(Land.Area,Potatoes, by=c("REF_DATE"="REF_DATE", "GEO"="GEO"))%>%
  replace(is.na(.), 0)

Land.Area<-full_join(Land.Area,Fruit, by=c("REF_DATE"="REF_DATE", "GEO"="GEO"))%>%
  replace(is.na(.), 0)

Land.Area<-full_join(Land.Area,Veg, by=c("REF_DATE"="REF_DATE", "GEO"="GEO"))%>%
  replace(is.na(.), 0)%>%
  mutate(Total.Acres = Field.Crop.Acres+Sod.Acres+Potatoes.Acres+Fruit.Acres+Veg.Acres)%>%
  select(REF_DATE, GEO, Total.Acres)%>%
  spread(GEO, Total.Acres)%>%
  replace(is.na(.), 0)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, Total.Acres, 2:14)%>%
  filter(GEO != "Newfoundland and Labrador",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick",
         GEO != "Prairie provinces",
         GEO != "Canada",
         REF_DATE <2020)

Land<-Land.Area%>%
  mutate(Total.Acres = Total.Acres/1000000)%>%
  ggplot(aes(REF_DATE, Total.Acres, color = GEO))+
  geom_line(size = 2, alpha = .75)+
  geom_point(aes (fill = GEO), shape = 21, color = "black", size=4)+
  scale_color_brewer(palette = "RdYlBu")+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Land Used For Crop Production (Million Acres)")+
  ylab(element_blank())+
  xlab(element_blank())+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_y_continuous(minor_breaks = seq(0 , 50, 5), breaks = seq(0, 50, 10))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))

Prov_IPCC_A <- read_csv("SPP/EM/Prov.IPCC.A.csv")
View(Prov_IPCC_A)

Prov <- Prov_IPCC_A%>%
  filter(Year >=1990)%>%
  spread(Region, CO2eq)%>%
  replace(is.na(.), 0)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(Region, CO2eq, 4:14)%>%
  filter(Region != "Newfoundland and Labrador",
         Region != "Prince Edward Island",
         Region != "Nova Scotia",
         Region != "New Brunswick",
         Category %in% c("Agricultural Soils", "Field Burning of Agricultural Residues", "Liming, Urea Application and Other Carbon-containing Fertilizers"))%>%
  spread(Category, CO2eq)%>%
  mutate(Total.EM = `Agricultural Soils`+ `Field Burning of Agricultural Residues`+`Liming, Urea Application and Other Carbon-containing Fertilizers`,
         Total.EM = Total.EM*1000,
         Ag.Soils = `Agricultural Soils`*1000)%>%
  select(Year, Region, Total.EM, Ag.Soils)

Land.Area<-full_join(Land.Area,Prov, by=c("REF_DATE"="Year", "GEO"="Region"))%>%
  replace(is.na(.), 0)
EM.Acre<-Land.Area%>%
  mutate(EM.Acre = Total.EM/Total.Acres)%>%
  ggplot(aes(REF_DATE, EM.Acre, color = GEO))+
  geom_line(size = 2, alpha = .75)+
  geom_point(aes (fill = GEO), shape = 21, color = "black", size=4)+
  scale_color_brewer(palette = "RdYlBu")+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Emissions per Acre of Crop Land by Province: 1990-2019 (Tonnes an Acre)")+
  ylab(element_blank())+
  xlab(element_blank())+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_y_continuous(minor_breaks = seq(0 , .8, .1), breaks = seq(0, .8, .2))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))

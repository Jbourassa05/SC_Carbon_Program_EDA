#Project 2A Script
setwd("~/SPP/CAP")

library(readr)
Field_Crops <- read_csv("Data/Field Crops1.csv")
View(Field_Crops)

Area <- Field_Crops %>%
  mutate(VALUE = replace_na(VALUE,0))%>%
  filter(Harvest_disposition == "Seeded area (acres)",
         GEO != "Canada")%>%
  group_by(REF_DATE, GEO)%>%
  summarise(Total_C = sum(VALUE))

potatoes <- read_csv("Data/potatoes.csv")
View(potatoes)
Area2 <- potatoes %>%
  mutate(VALUE = replace_na(VALUE,0))%>%
  filter(UOM == "Acres",
         GEO != "Canada")%>%
  group_by(REF_DATE, GEO)%>%
  summarise(Total_P = sum(VALUE))

fruit <- read_csv("Data/fruit.csv")
View(fruit)

Area3 <- fruit %>%
  mutate(VALUE = replace_na(VALUE,0))%>%
  filter(UOM == "Acres",
         GEO != "Canada")%>%
  group_by(REF_DATE, GEO)%>%
  summarise(Total_F = sum(VALUE))

Veg <- read_csv("Data/Veg.csv")
View(Veg)
Area4 <- Veg %>%
  mutate(VALUE = replace_na(VALUE,0))%>%
  filter(UOM == "Acres",
         GEO != "Canada")%>%
  group_by(REF_DATE, GEO)%>%
  summarise(Total_V = sum(VALUE))

View(Area4)

Area <- Area %>% merge(Area2, by=c("REF_DATE","GEO"))
Area <- Area %>% merge(Area3, by=c("REF_DATE","GEO"))
Area <- Area %>% merge(Area4, by=c("REF_DATE","GEO"))

Area <- Area%>%
  mutate(Other = Total_P+Total_F+Total_V,
         Total = Total_C+Other,
         Share = Total_C/Total)

# by Crop Type

C_Area_Wide <- Field_Crops %>%
  spread(Type, VALUE)%>%
  replace(is.na(.), 0)%>%
  filter(UOM == "Acres",
         GEO != "Canada")%>%
  mutate(Cereals = Barley +`Canary seed` + `Corn for grain` + `Corn for silage` + `Mixed grains` + `Rye, all` + `Triticale` + `Wheat, all` + Oats,
         Oilseeds = `Canola (rapeseed)` + `Safflower` + `Soybeans` + `Flaxseed` + `Sunflower seed` + `Mustard seed`,
         Pulses = `Beans, all dry (white and coloured)`+ `Chick peas` + `Faba beans` + `Lentils` + `Peas, dry`,
         Hay = `Tame hay`,
         Others_FC = `Borage seed`+ Buckwheat + `Caraway seed` + `Coriander seed`+ `Sugar beets`,
         Fallow = Summerfallow)%>%
  rename(Canola = `Canola (rapeseed)`,
         Wheat = `Wheat, all`)%>%
  select(REF_DATE, GEO, Canola, Wheat, Barley, Cereals, Oilseeds, Pulses, Hay, Others_FC,Fallow)

Area <- Area %>%
  mutate(Vegetables = Total_P + Total_V)%>%
  rename(Fruit = Total_F)

Area_Use <- Area %>%
  merge(C_Area_Wide, by=c("REF_DATE","GEO"))%>%
  select(REF_DATE, GEO, Total, Canola, Wheat, Barley, Cereals, Oilseeds, Pulses, Hay, Others_FC,Fallow, Vegetables, Fruit )%>%
  gather(Type, Value, 3:14)

C_Area_Use <- Area_Use %>%
  spread(GEO, Value)%>%
  mutate(Atlantic = `New Brunswick` + `Newfoundland and Labrador`+ `Prince Edward Island` + `Nova Scotia`)%>%
  select(-`New Brunswick`, -`Newfoundland and Labrador`, -`Prince Edward Island`, -`Nova Scotia`)%>%
  gather(GEO, Value, 3:9)
  
# Visualization Total Seeded Area
library(RColorBrewer)

Canadian_Area <- C_Area_Use %>%
  filter(Type=="Total")%>%
  mutate(GEO = fct_reorder(GEO, desc(Value)),
         Value = Value/1000000)%>%
  ggplot(aes(REF_DATE, Value))+
  geom_area(aes(fill = GEO),alpha=0.6 , size=.5, colour="white")+
  theme_bw()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Total Seeded Acres by Province")+
  xlab("Year")+
  ylab("Million Acres")
Canadian_Area

Canadian_Area_Type <- C_Area_Use %>%
  filter(Type != "Wheat",
         Type != "Canola",
         Type != "Barley",
         Type != "Total")%>%
  group_by(REF_DATE, Type)%>%
  summarise(Total = sum(Value))%>%
  spread(Type, Total)%>%
  mutate(Other = Vegetables+Fruit+Others_FC)%>%
  select(-Vegetables, -Fruit, -Others_FC)%>%
  gather(Type, Total, 2:7)%>%
  mutate(Type = fct_reorder(Type, desc(Total)),
         Total = Total/1000000)%>%
  ggplot(aes(REF_DATE, Total))+
  geom_area(aes(fill = Type),alpha=0.6 , size=.5, colour="white")+
  theme_bw()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Total Seeded Acres by Crop type")+
  xlab("Year")+
  ylab("Million Acres")
Canadian_Area_Type

# Alberta Production

Alberta_Area_Type <- C_Area_Use %>%
  filter(Type != "Wheat",
         Type != "Canola",
         Type != "Barley",
         Type != "Total",
         GEO == "Alberta")%>%
  group_by(REF_DATE, Type)%>%
  summarise(Total = sum(Value))%>%
  spread(Type, Total)%>%
  mutate(Other = Vegetables+Fruit+Others_FC)%>%
  select(-Vegetables, -Fruit, -Others_FC)%>%
  gather(Type, Total, 2:7)%>%
  mutate(Type = fct_reorder(Type, desc(Total)),
         Total = Total/1000000)%>%
  ggplot(aes(REF_DATE, Total))+
  geom_area(aes(fill = Type),alpha=0.6 , size=.5, colour="white")+
  theme_bw()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Total Seeded Acres by Crop type")+
  xlab("Year")+
  ylab("Million Acres")
Alberta_Area_Type

Alberta_Area_Type_D <- C_Area_Use %>%
  filter(GEO == "Alberta",
         Type != "Total")%>%
  group_by(REF_DATE, Type)%>%
  spread(Type, Value)%>%
  mutate(Other = Vegetables+Fruit+Others_FC,
         Cereals.Other = Cereals - Wheat - Barley,
         Oilseeds.Other = Oilseeds - Canola)%>%
  select(-Vegetables, -Fruit, -Others_FC, - Cereals, -Oilseeds)%>%
  gather(Type, Total, 3:11)%>%
  mutate(Type = fct_reorder(Type, desc(Total)),
         Total = Total/1000000)%>%
  ggplot(aes(REF_DATE, Total))+
  geom_area(aes(fill = Type),alpha=0.6 , size=.5, colour="white")+
  theme_bw()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Total Seeded Acres by Crop type")+
  xlab("Year")+
  ylab("Million Acres")
Alberta_Area_Type_D

# Fertilizer
Fertilizer_Data <- read_csv("Data/Fertilizer Data.csv")
View(Fertilizer_Data)

Fertilizer <- Fertilizer_Data %>%
  select(REF_DATE, GEO, `Fertilizer nutrient content`,VALUE)%>%
  spread(GEO, VALUE)%>%
  replace(is.na(.), 0)%>%
  mutate(Atlantic = Canada - Quebec - Ontario - Manitoba - Saskatchewan - Alberta-`British Columbia`)%>%
  gather(GEO, Value, 3:10)%>%
  filter(GEO == "Canada")%>%
  mutate(`Fertilizer nutrient content`= fct_reorder(`Fertilizer nutrient content`, desc(Value)))%>%
  ggplot(aes(REF_DATE, Value))+
  geom_area(aes(fill = `Fertilizer nutrient content`),alpha=0.6 , size=.5, colour="white")+
  theme_bw()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Total Fertilizer Use by Nutrient Content")+
  xlab("Year")+
  ylab("Metric Tonnes")
Fertilizer

Fertilizer_Province <- Fertilizer_Data %>%
  select(REF_DATE, GEO, `Fertilizer nutrient content`,VALUE)%>%
  spread(GEO, VALUE)%>%
  replace(is.na(.), 0)%>%
  mutate(Atlantic = Canada - Quebec - Ontario - Manitoba - Saskatchewan - Alberta-`British Columbia`)%>%
  gather(GEO, Value, 3:10)%>%
  group_by(REF_DATE, GEO)%>%
  summarise(Total = sum(Value))%>%
  filter(GEO != "Canada")%>%
  mutate(GEO = fct_reorder(GEO, desc(Total)))%>%
  ggplot(aes(REF_DATE, Total))+
  geom_area(aes(fill = `GEO`),alpha=0.6 , size=.5, colour="white")+
  theme_bw()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Total Fertilizer Use by Province")+
  xlab("Year")+
  ylab("Metric Tonnes")

Alberta_Fertilizer <- Fertilizer_Data %>%
  select(REF_DATE, GEO, `Fertilizer nutrient content`,VALUE)%>%
  filter(GEO == "Alberta")%>%
  mutate(`Fertilizer nutrient content`= fct_reorder(`Fertilizer nutrient content`, desc(VALUE)))%>%
  ggplot(aes(REF_DATE, VALUE))+
  geom_area(aes(fill = `Fertilizer nutrient content`),alpha=0.6 , size=.5, colour="white")+
  theme_bw()+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Total Fertilizer Use by Nutrient Content")+
  xlab("Year")+
  ylab("Metric Tonnes")
Alberta_Fertilizer

# Beef Production 
cattle <- read_csv("Data/cattle2.csv")

cattle<-cattle%>%
  mutate(GEO = fct_reorder(GEO, desc(VALUE)))%>%
  filter(`Farm type` =="On all cattle operations")%>%
  ggplot( aes(REF_DATE, VALUE))+
  geom_area(aes(fill=GEO),alpha=0.6 , size=.5, colour="white")+
  scale_fill_brewer(palette = "Spectral")+
  theme_bw()+
  ggtitle("Total Cattle On All Cattle Operations by Province")+
  xlab("Year")+
  ylab("Head of Cattle (1000)")
cattle

cattle_Type <- read_csv("Data/cattle2.csv")

cattle_Type<- cattle_Type%>%
  filter(`Farm type` !="On all cattle operations",
         `Farm type` != "On beef operations")%>%
  spread(GEO, VALUE)%>%
  mutate(Total = Alberta+`Atlantic provinces`+`British Columbia`+ Quebec+ Ontario+ Manitoba+ Saskatchewan,
         `Farm type` = fct_reorder(`Farm type`, desc(Total)))%>%
  ggplot( aes(REF_DATE, Total))+
  geom_area(aes(fill=`Farm type`),alpha=0.6 , size=.5, colour="white")+
  scale_fill_brewer(palette = "Spectral")+
  theme_bw()+
  ggtitle("Total Cattle by Cattle Operation Type")+
  xlab("Year")+
  ylab("Head of Cattle (1000)")

Beef <- read_csv("Data/cattle2.csv")

Beef<-Beef%>%
  mutate(GEO = fct_reorder(GEO, desc(VALUE)))%>%
  filter(`Farm type` =="On beef operations")%>%
  ggplot( aes(REF_DATE, VALUE))+
  geom_area(aes(fill=GEO),alpha=0.6 , size=.5, colour="white")+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Total Cattle On All Beef Operations by Province")+
  xlab("Year")+
  ylab("Head of Cattle (1000)")+
  theme_bw()
Beef

Beef <- read_csv("Data/cattle2.csv")

Alberta_Beef<-Beef%>%
  filter(`Farm type` !="On beef operations",
         `Farm type` !="On all cattle operations",
         GEO == "Alberta")%>%
  mutate(`Farm type` = fct_reorder(`Farm type`, desc(VALUE)))%>%
  ggplot( aes(REF_DATE, VALUE))+
  geom_area(aes(fill=`Farm type`),alpha=0.6 , size=.5, colour="white")+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Alberta Cattle Population by Farm Type")+
  xlab("Year")+
  ylab("Head of Cattle x1000")+
  theme_bw()
Alberta_Beef

# Economy Stat Can Sector 


# Emissions Stat Can Sector

StatCan_EM <- read_csv("Data/StatCan_EM.csv")
View(StatCan_EM)

Data <- StatCan_EM%>%
  mutate(VALUE = replace_na(VALUE,0))%>%
  filter(Sector != "Seafood product preparation and packaging [BS31170]")%>%
  spread(Sector, VALUE)%>%
  mutate(Food_Manifacturing = `Sugar and confectionery product manufacturing [BS31130]`+
           `Fruit and vegetable preserving and specialty food manufacturing [BS31140]`+
           `Dairy product manufacturing [BS31150]`+
           `Meat product manufacturing [BS31160]`+
           `Miscellaneous food manufacturing [BS311A0]`+
           `Soft drink and ice manufacturing [BS31211]`+
           `Breweries [BS31212]`+
           `Wineries and distilleries [BS3121A]`+
           `Tobacco manufacturing [BS31220]`,
         Agro_Chemicals = `Pesticide, fertilizer and other agricultural chemical manufacturing [BS32530]`,
         Feed_Production = `Animal food manufacturing [BS31110]`)
         
Sector_EM <- Data %>%
  select(REF_DATE, GEO, Food_Manifacturing, Agro_Chemicals, Feed_Production )%>%
  gather(Type, VALUE,  3:5)
  

SectorGDP <- read_csv("Data/SectorGDP2.csv")

GDP<-SectorGDP%>%
  mutate(VALUE = replace_na(VALUE,0))%>%
  rename(NAICS = `North American Industry Classification System (NAICS)`)%>%
  select(REF_DATE, GEO, NAICS, VALUE)%>%
  filter(NAICS != "Potash mining [212396]",
         NAICS != "Agricultural, construction and mining machinery manufacturing [3331]")%>%
  spread(GEO, VALUE)%>%
  mutate(Atlantic = `Newfoundland and Labrador`+`Prince Edward Island`+`Nova Scotia`+`New Brunswick`,
         Territories = Yukon+`Northwest Territories`+Nunavut)%>%
  select(-`Newfoundland and Labrador`, -`Prince Edward Island`, -`Nova Scotia`, -`New Brunswick`, -Yukon, -`Northwest Territories`,-Nunavut)%>%
  gather(GEO, VALUE, 3:10)%>%
  spread(NAICS, VALUE)%>%
  select(-`Agriculture, forestry, fishing and hunting [11]`, -`Animal food manufacturing [3111]`, -`Animal production (except aquaculture) [112A]`, -`Bakeries and tortilla manufacturing [3118]`, -`Breweries [31212]`, -`Food and beverage stores [445]`, -`Fruit and vegetable preserving and specialty food manufacturing [3114]`, -`Grain and oilseed milling [3112]`, -`Other food manufacturing [3119]`, -`Sugar and confectionery product manufacturing [3113]`, -`Support activities for agriculture and forestry [115]`, -`Wineries and distilleries [3121A]`)%>%
  mutate(Food_Systems = `Crop and animal production [11A]`+`Beverage and tobacco product manufacturing [312]`+`Pesticide, fertilizer and other agricultural chemical manufacturing [3253]`+`Food manufacturing [311]`,
         Total_Share = Food_Systems/`All industries [T001]`,
         Production_Share = `Crop and animal production [11A]`/`All industries [T001]`,
         Manufacturing_Share = (`Beverage and tobacco product manufacturing [312]`+`Food manufacturing [311]`)/`All industries [T001]`,
         Agro_Chemical_Share = `Pesticide, fertilizer and other agricultural chemical manufacturing [3253]`/`All industries [T001]`)%>%
  gather(Industry, Value, 3:11)

GDP2<-GDP%>%
  filter(Industry == "Food_Systems")%>%
  mutate(GEO = fct_reorder(GEO, desc(Value)))%>%
  ggplot(aes(REF_DATE,Value))+
  geom_area(aes(fill = GEO), alpha=0.6, size=.5, color="white")+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Food System GDP by Province")+
  xlab("Year")+
  ylab("Million Dollars")+
  theme_bw()
GDP2

GDP3<-GDP%>%
  group_by(REF_DATE, Industry)%>%
  summarise(Value = sum(Value))%>%
  filter(Industry != "All industries [T001]",
         Industry != "Crop production [111]",
         Industry != "Support activities for crop and animal production [115A]",
         Industry != "Animal production and aquaculture [112]",
         Industry != "Food_Systems")%>%
  mutate(Industry = fct_reorder(Industry, desc(Value)))

Industry <- GDP3 %>%
  ggplot(aes(REF_DATE, Value))+
  geom_area(aes(fill = Industry), alpha=0.6, size=.5, color="white")+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Agricultural GDP by Sector")+
  xlab("Year")+
  ylab("Million Dollars")+
  theme_bw()
Industry

Alberta_GDP_Sector<-GDP%>%
  filter(GEO == "Alberta",
         Industry != "All industries [T001]",
         Industry != "Crop production [111]",
         Industry != "Support activities for crop and animal production [115A]",
         Industry != "Animal production and aquaculture [112]",
         Industry != "Food_Systems")%>%
  mutate(Industry = fct_reorder(Industry, desc(Value)))%>%
  ggplot(aes(REF_DATE, Value))+
  geom_area(aes(fill = Industry), alpha=0.6, size=.5, color="white")+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Alberta's Agricultural GDP by Sector")+
  xlab("Year")+
  ylab("Million Dollars")+
  theme_bw()
  
         



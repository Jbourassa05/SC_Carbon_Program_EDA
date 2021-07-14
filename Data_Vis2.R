# Work in progress Data visualisations

library(readr)
library(tidyverse)
library(RColorBrewer)
install.packages("gghighlight")
library(gghighlight)

Can_Econ_Sec <- read_csv("SPP/Data/Can_Econ_Sec.csv", 
                         col_types = cols(Index = col_skip()))
View(Can_Econ_Sec)

Can_Econ_Sec1<-Can_Econ_Sec%>%
  spread(Source, CO2eq)%>%
  replace(is.na(.), 0)%>%
  mutate(Other = `Light Manufacturing, Construction and Forest Resources`+ Waste +`Coal Production`)%>%
  gather(Source, CO2eq, 4:16)%>%
  filter(Source != "Light Manufacturing, Construction and Forest Resources",
         Source != "Waste",
         Source != "National Inventory Total",
         Source != "Provincial Inventory Total",
         Source != "Territorial Inventory Total",
         Source != "Coal Production",
         Region == "Canada")%>%
  group_by(Year)%>%
  mutate(Source = fct_reorder(Source, desc(CO2eq)))%>%
  ungroup()

NDC_level <- 511

P1<-Can_Econ_Sec1%>%
  ggplot(aes(Year, CO2eq))+
  geom_col(aes(fill = Source), alpha=0.80)+
  scale_fill_viridis(discrete = TRUE)+
  ggtitle("Canada's GHG Emissions by Canadian Economic Sector, 1990-2019")+
  ylab("Mt CO2 eq")+
  labs(fill = "Economic Sector")+
  theme(
    legend.position = c(.05, .05),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 800, 50), breaks = seq(0, 800, 150))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))+
  geom_hline(yintercept=NDC_level, colour = "Black", size = 2, linetype = "longdash")+
  geom_text(aes( 1990, NDC_level, label = "NDC Commitment", vjust = -1), size = 3,colour = "black")

library(readr)
Can_Econ_Sec_Ag <- read_csv("SPP/Data/Can_Econ_Sec_Ag.csv", 
                            col_types = cols(Source = col_skip(), 
                                             `Sub-category` = col_skip(), `Sub-sub-category` = col_skip(), 
                                             Total = col_skip()))
View(Can_Econ_Sec_Ag)

Can<-Can_Econ_Sec_Ag%>%
  filter(Region == "Canada",
         Category != "Total")

P3<-Can%>%
  ggplot(aes(Year, CO2eq))+
  geom_col(aes(fill = Category), alpha=0.80)+
  scale_fill_viridis(begin = .33, end = .66, discrete = TRUE)+
  ggtitle("Canada's Ag GHG Emissions by Production Category, 1990-2019")+
  ylab("Mt CO2 eq")+
  labs(fill = "Sector")+
  theme(
    legend.position = c(.05, .05),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 80, 5), breaks = seq(0, 80, 15))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))

Can2<-Can_Econ_Sec_Ag%>%
  filter(Region != "Canada",
         Category == "Total")%>%
  spread(Region, CO2eq)%>%
  replace(is.na(.), 0)%>%
  mutate(Atlantic = `New Brunswick`+ `Nova Scotia`+`Prince Edward Island` + `Newfoundland and Labrador` )%>%
  gather(Region, CO2eq, 5:18)%>%
  filter(Region != "New Brunswick",
         Region != "Nova Scotia",
         Region != "Prince Edward Island",
         Region != "Newfoundland and Labrador",
         Region != "Yukon",
         Region != "Northwest Territories and Nunavut",
         Region != "Northwest Territories")%>%
  group_by(Year)%>%
  mutate(Region = fct_reorder(Region, desc(CO2eq)))%>%
  ungroup()
P2<-Can2%>%
  ggplot(aes(Year, CO2eq))+
  geom_area(aes(fill = Region), alpha=0.80)+
  scale_fill_brewer(direction = -1)+
  ggtitle("Canada's Ag GHG Emissions by Province and Region, 1990-2019")+
  ylab("Mt CO2 eq")+
  labs(fill = "Sector")+
  theme_bw()+
  theme(
    legend.position = c(.05, .05),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 80, 5), breaks = seq(0, 80, 15))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))

alta<-Can_Econ_Sec_Ag%>%
  filter(Region == "Alberta")%>%
  select(!(Index))%>%
  spread(Category, CO2eq)%>%
  replace(is.na(.), 0)%>%
  mutate(`Animal Production` = `Animal Production`/ Total,
         `Crop Production` = `Crop Production` / Total,
         `On Farm Fuel Use` = `On Farm Fuel Use` / Total)%>%
  gather(Category, CO2eq, 4:7)

A1<-alta%>%
  filter(Category != "Total")%>%
  ggplot(aes(Year, CO2eq))+
  geom_area(aes(fill = Category), alpha=0.80)+
  scale_fill_brewer(direction = -1)+
  ggtitle("Share of Ag Emissions in Alberta by Production Type, 1990-2019")+
  ylab("Share of Agricultural Emissions")+
  labs(fill = "Production Type ")+
  theme_bw()+
  theme(
    legend.position = c(.05, .05),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 1, .05), breaks = seq(0, 1, .25))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))
  
alta1<-Can_Econ_Sec_Ag%>%
  filter(Region == "Alberta",
         Category != "Total")
A2<-alta1%>%
  ggplot(aes(Year, CO2eq))+
  geom_area(aes(fill = Category), alpha=0.80)+
  scale_fill_brewer(direction = -1)+
  ggtitle("Alberta's Ag GHG Emissions by Production Type, 1990-2019")+
  ylab("Mt CO2 eq")+
  labs(fill = "Production Type")+
  theme_bw()+
  theme(
    legend.position = c(.05, .05),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 25, 1), breaks = seq(0, 25, 5))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))
alta2 <-Can_Econ_Sec%>%
  spread(Source, CO2eq)%>%
  replace(is.na(.), 0)%>%
  mutate(Other = `Light Manufacturing, Construction and Forest Resources`+ Waste +`Coal Production`)%>%
  gather(Source, CO2eq, 4:16)%>%
  filter(Source != "Light Manufacturing, Construction and Forest Resources",
         Source != "Waste",
         Source != "National Inventory Total",
         Source != "Provincial Inventory Total",
         Source != "Territorial Inventory Total",
         Source != "Coal Production",
         Region == "Alberta")%>%
  group_by(Year)%>%
  mutate(Source = fct_reorder(Source, desc(CO2eq)))%>%
  ungroup()
A3<-alta2%>%
  ggplot(aes(Year, CO2eq))+
  geom_area(aes(fill = Source), alpha=0.80)+
  scale_fill_brewer(direction = -1)+
  ggtitle("Alberta's GHG Emissions by Canadian Economic Sector, 1990-2019")+
  ylab("Mt CO2 eq")+
  labs(fill = "Sector")+
  theme_bw()+
  theme(
    legend.position = c(.05, .05),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 300, 25), breaks = seq(0, 300, 50))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))

GHG_IPCC_Can_Prov_Terr2 <- read_csv("SPP/Data/GHG_IPCC_Can_Prov_Terr2.csv")
View(GHG_IPCC_Can_Prov_Terr2)

IPCC <- GHG_IPCC_Can_Prov_Terr2%>%
  filter(Region == "Alberta",
         Rollup == FALSE)
A_IPCC<-IPCC%>%
  select(Year,Category,`CH4 (CO2eq)`,`N2O (CO2eq)`, CO2eq)%>%
  filter(Category != "Direct Sources",
         Category != "Indirect Sources",
         Category != "Field Burning",
         Category != "Carbon-containing Fertilizers")
A_IPCC_A<- A_IPCC %>%
  ggplot(aes(Year, CO2eq))+
  geom_col(aes(fill = Category),alpha=0.80)+
  scale_fill_brewer(direction = -1)+
  ggtitle("Animal Production EM by Source, 1990-2019")+
  ylab("Kt CO2 eq")+
  labs(fill = "Source")+
  theme_bw()+
  theme(
    legend.position = c(.05, .95),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 15000, 250), breaks = seq(0, 15000, 1000))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))

c_IPCC<-IPCC%>%
  select(Year,Category,`CH4 (CO2eq)`,`N2O (CO2eq)`, CO2eq)%>%
  filter(Category != "Enteric Fermentation",
         Category != "Manure Management")
c_IPCC_c<- c_IPCC %>%
  mutate(Category = fct_reorder(Category, desc(CO2eq)))%>%
  ggplot(aes(Year, CO2eq))+
  geom_col(aes(fill = Category),alpha=0.80)+
  scale_fill_brewer(direction = -1)+
  ggtitle("Crop Production EM by Source, 1990-2019")+
  ylab("Kt CO2 eq")+
  labs(fill = "Source")+
  theme_bw()+
  theme(
    legend.position = c(.05, .95),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 15000, 250), breaks = seq(0, 15000, 1000))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))

# Crop Production Data 
FarmGate.A<-FarmGate2%>%
  rename(Region = GEO)%>%
  filter(Region == "Alberta")

Alta_Econ_Farm <- Can_Econ_Sec_Ag%>%
  filter(Category!= "On Farm Fuel Use",
         Region == "Alberta")%>%

Farm.Gate.Alberta <- inner_join(FarmGate.A, Alta_Econ_Farm, by = "Year")

  
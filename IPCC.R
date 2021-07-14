library(readr)
Canada_IPCC <- read_csv("SPP/EM/Canada.IPCC.csv")
View(Canada_IPCC)

NDC_level<-511

Canada_IPCC%>%
  filter(Category!="TOTAL")%>%
  mutate(CO2eq = CO2eq/1000,
         Category = fct_reorder(Category, desc(CO2eq)))%>%
  ggplot(aes(Year, CO2eq))+
  geom_col(aes(fill = Category))+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Canada's GHG Emissions by IPCC Sector, 1990-2019 (Mt CO2 eq)")+
  xlab(element_blank())+
  ylab(element_blank())+
  labs(fill = "IPCC Sector")+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_y_continuous(minor_breaks = seq(0 , 800, 50), breaks = seq(0, 800, 150))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))+
  geom_hline(yintercept=NDC_level, colour = "Black", size = 2, linetype = "longdash")+
  geom_text(aes( 1990, NDC_level, label = "NDC Commitment", vjust = -1), size = 3,colour = "black")

Canada_IPCC_A <- read_csv("SPP/EM/Canada.IPCC.A.csv")
View(Canada_IPCC_A)

Canada_IPCC_A%>%
  filter(Category!="AGRICULTURE",
         Category!="Direct Sources",
         Category!="Indirect Sources")%>%
  mutate(CO2eq = CO2eq/1000,
         Category = fct_reorder(Category, desc(CO2eq)))%>%
  ggplot(aes(Year, CO2eq))+
  geom_col(aes(fill = Category), alpha=0.80)+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Canada's Agricultural GHG Emissions by IPCC Sector, 1990-2019 (Mt CO2 eq)")+
  xlab(element_blank())+
  ylab(element_blank())+
  labs(fill = "IPCC Sector")+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_y_continuous(minor_breaks = seq(0 , 65, 5), breaks = seq(0, 65, 15))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))
  
Stat_CAN <- read_csv("SPP/EM/Stat.CAN.csv")%>%
  spread(Sector,VALUE)%>%
  replace(is.na(.), 0)%>%
  mutate(Food.Manufacturing = `Sugar and confectionery product manufacturing [BS31130]`+
           `Fruit and vegetable preserving and specialty food manufacturing [BS31140]`+
           `Dairy product manufacturing [BS31150]`+
           `Meat product manufacturing [BS31160]`+
           `Miscellaneous food manufacturing [BS311A0]`+
           `Soft drink and ice manufacturing [BS31211]`+
           `Breweries [BS31212]`+
           `Wineries and distilleries [BS3121A]`+
           `Tobacco manufacturing [BS31220]`+
           `Seafood product preparation and packaging [BS31170]`,
         Feed.Manufacturing = `Animal food manufacturing [BS31110]`,
         Ag.Production = `Crop and animal production [BS11A00]`+`Crop and animal production (except cannabis) [BS11B00]`,
         Ag.Chemicals = `Pesticide, fertilizer and other agricultural chemical manufacturing [BS32530]`,
         Ag.Plus =Food.Manufacturing+Feed.Manufacturing+Ag.Chemicals+`Support activities for agriculture and forestry [BS11500]`)%>%
  rename(Ag.Forest.Support = `Support activities for agriculture and forestry [BS11500]`)
Stat_CAN<-Stat_CAN%>%
  select(REF_DATE, GEO, `Total, industries and households`, Ag.Production, Ag.Forest.Support, Ag.Chemicals, Food.Manufacturing, Feed.Manufacturing,Ag.Plus)%>%
  mutate(Other.Sectors = `Total, industries and households`- (Ag.Production + Ag.Forest.Support+Ag.Chemicals +Food.Manufacturing +Feed.Manufacturing))%>%
  select(!`Total, industries and households`)%>%
  filter(GEO == "Canada")%>%
  gather(Category, kt.CO2eq, 3:9)

Stat.Can.EM <-Stat_CAN%>%
  filter(Category %in% c("Other.Sectors","Ag.Plus","Ag.Production"))%>%
  mutate(Category = fct_reorder(Category, desc(kt.CO2eq)),
         kt.CO2eq = kt.CO2eq/1000)%>%
  ggplot(aes(REF_DATE, kt.CO2eq, fill=Category))+
  geom_col()+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Canada's GHG Emissions by Physical flow Account, 1990-2019 (Mt CO2 eq)")+
  xlab(element_blank())+
  ylab(element_blank())+
  labs(fill = "Sector")+
  theme(title = element_text(size = 20),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.x = element_text(size = 15),
              legend.position = "bottom",
              legend.title = element_blank())+
  scale_y_continuous(minor_breaks = seq(0 , 800, 50), breaks = seq(0, 800, 100))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,2))

Stat.Can.EM <-Stat_CAN%>%
  mutate(Category = fct_reorder(Category, desc(kt.CO2eq)),
         kt.CO2eq = kt.CO2eq/1000)%>%
  filter(Category!="Other.Sectors",
         Category!= "Ag.Plus")%>%
  ggplot(aes(REF_DATE, kt.CO2eq, fill=Category))+
  geom_col()+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Agri-Food Emissions by Physical flow Account, 1990-2019")+
  ylab("Mt CO2 eq")+
  xlab(element_blank())+
  labs(fill = "Sector")+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_y_continuous(minor_breaks = seq(0 , 100, 5), breaks = seq(0, 100,10))+
  scale_x_continuous(minor_breaks = seq(2009,2018,1), breaks = seq(2009,2018,3))
  

Can_Econ_Sec_Ag <- read_csv("SPP/Data/Can_Econ_Sec_Ag.csv", 
                            col_types = cols(Source = col_skip(), 
                                             `Sub-category` = col_skip(), `Sub-sub-category` = col_skip(), 
                                             Total = col_skip()))
alta1<-Can_Econ_Sec_Ag%>%
  filter(Region == "Alberta",
         Category != "Total",
         Year >=2005)
A2<-alta1%>%
  ggplot(aes(Year, CO2eq))+
  geom_col(position = "dodge", aes(fill = Category),color="black")+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Alberta's Agricultural GHG Emissions by Source, 2005-2019 (Mt CO2 eq)",
          subtitle = "Canadian Economic Sectors")+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank())


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
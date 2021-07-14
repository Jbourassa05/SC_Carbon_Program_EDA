Prov_IPCC_A <- read_csv("SPP/EM/Prov.IPCC.A.csv")
View(Prov_IPCC_A)

Prov <- Prov_IPCC_A%>%
  filter(Category == "AGRICULTURE",
         Year >=1990)%>%
  spread(Region, CO2eq)%>%
  replace(is.na(.), 0)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(Region, CO2eq, 4:14)%>%
  filter(Region != "Newfoundland and Labrador",
         Region != "Prince Edward Island",
         Region != "Nova Scotia",
         Region != "New Brunswick")

EM.Total<-Prov%>%
  mutate(CO2eq = CO2eq/1000)%>%
  ggplot(aes(Year ,CO2eq, color = Region))+ geom_line(size = 2, alpha = .75)+geom_point(aes (fill = Region), shape = 21, color = "black", size=4)+
  scale_color_brewer(palette = "RdYlBu")+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Agricultural Emissions by Province", subtitle = "IPCC Sector")+
  ylab("Mt CO2 eq")+
  xlab(element_blank())+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_y_continuous(minor_breaks = seq(0 , 20, 2.5), breaks = seq(0, 20, 5))+
  scale_x_continuous(minor_breaks = seq(1990,2020,1), breaks = seq(1990,2020,5))
  
Alta <- Prov_IPCC_A%>%
  filter(Category != "AGRICULTURE",
         Region == "Alberta",
         Year >=2005)%>%
  spread(Category, CO2eq)%>%
  replace(is.na(.), 0)%>%
  mutate(Other = `Field Burning of Agricultural Residues`+ `Liming, Urea Application and Other Carbon-containing Fertilizers`)%>%
  gather(Category, kt.CO2, 4:11)%>%
  filter(Category %in% c("Agricultural Soils", "Enteric Fermentation", "Manure Management", "Other"))%>%
  mutate(Mt.CO2eq = kt.CO2/1000)

Alta1 <- Alta%>%
  mutate(Category = fct_reorder(Category, desc(Mt.CO2eq)))%>%
  ggplot(aes(Year, Mt.CO2eq, fill = Category))+geom_col(position = "dodge", color = "black")+
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Alberta's Agricultural GHG Emissions by Source, 2005-2019 (Mt CO2 eq)",
          subtitle = "IPCC Sector")+
  xlab(element_blank())+
  ylab(element_blank())+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_y_continuous(minor_breaks = seq(0 , 12, 1), breaks = seq(0, 12, 3))+
  scale_x_continuous(minor_breaks = seq(2005,2020,1), breaks = seq(1005,2020,5))



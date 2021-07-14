library(readr)
library(tidyverse)
Cattle_Pop <- read_csv("SPP/Cattle/Cattle.Pop.csv")
View(Cattle_Pop)
Cattle_EM_EF <- read_csv("SPP/Cattle/Cattle.EM.EF.csv")
View(Cattle_EM_EF)

Cattle_EM_EF<-Cattle_EM_EF%>%
  gather(livestock, EM, 2:9)

#ggplot(Cattle_EM_EF, aes(Year, EM, color = livestock))+geom_line()

Cattle_Data <- full_join(Cattle_Pop,Cattle_EM_EF, by=c("REF_DATE"="Year", "Livestock" = "livestock"))%>%
  mutate(Pop = VALUE*1000,
         kg.EM = Pop*EM,
         t.EM = kg.EM/1000,
         t.CO2e = t.EM*25)%>%
  select(REF_DATE, GEO, Livestock, t.CO2e)%>%
  spread(Livestock, t.CO2e)%>%
  mutate(Total = Dairy.Cows+Dairy.Heifers+Bulls+Beef.Cows+Beef.Heifers+Heifers.Slaughter+Steers+Calves,
         Dairy.Cattle = Dairy.Cows+Dairy.Heifers,
         Non.Dairy.Cattle = Total-Dairy.Cattle)%>%
  gather(Livestock, t.CO2e, 3:13)

Cattle_Group <- Cattle_Data%>%
  filter(Livestock %in% c("Dairy.Cattle", "Non.Dairy.Cattle"))%>%
  spread(GEO, t.CO2e)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, t.CO2e, 3:13)%>%
  filter(GEO != "Newfoundland and Labrador",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick",
         REF_DATE == 2019)
Cattle_EM <- Cattle_Group%>%
  mutate(GEO = fct_reorder(GEO,desc(t.CO2e)),
         Mt.CO2e = t.CO2e/1000000 )%>%
  ggplot(aes(GEO, Mt.CO2e, fill=Livestock))+geom_col()


Cattle_Pop2<-Cattle_Pop%>%
  spread(Livestock, VALUE)%>%
  mutate(Total = Dairy.Cows+Dairy.Heifers+Bulls+Beef.Cows+Beef.Heifers+Heifers.Slaughter+Steers+Calves,
         Dairy.Cattle = Dairy.Cows+Dairy.Heifers,
         Non.Dairy.Cattle = Total-Dairy.Cattle)%>%
  gather(Livestock, Pop, 3:13)%>%
  filter(Livestock %in% c("Dairy.Cattle", "Non.Dairy.Cattle"))%>%
  spread(GEO, Pop)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, Pop, 3:13)%>%
  filter(GEO != "Newfoundland and Labrador",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick",
         REF_DATE == 2019)
library(RColorBrewer)

cattle <- Cattle_Pop2%>%
  mutate(GEO = fct_reorder(GEO,desc(Pop)))%>%
  ggplot(aes(GEO, Pop, fill=Livestock))+geom_col()+
  scale_fill_manual(values = c("#fdae61","#abd9e9"))

Cattle_Farm_Cash <- read_csv("SPP/Cattle/Cattle.Farm.Cash.csv")
View(Cattle_Farm_Cash)

Cattle_Cash<-Cattle_Farm_Cash%>%
  spread(`Type of cash receipts`, VALUE)%>%
  mutate(Non.Dairy.Cattle = Cattle+Calve)%>%
  rename(Dairy.Cattle = Unprocessed.Milk)%>%
  select(REF_DATE, GEO,Non.Dairy.Cattle, Dairy.Cattle)%>%
  filter(REF_DATE == 2019)%>%
  gather(Livestock, Farm.Cash, 3:4)%>%
  spread(GEO,Farm.Cash)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, Farm.Cash, 3:13)%>%
  filter(GEO != "Newfoundland and Labrador",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick",
         REF_DATE == 2019)
Cattle_Group2019<-full_join(Cattle_Group, Cattle_Cash, by=c("REF_DATE"="REF_DATE", "Livestock"="Livestock", "GEO" = "GEO"))%>%
  mutate(EM.Value = t.CO2e/Farm.Cash)

EM.Cash <- Cattle_Group2019%>%
  ggplot(aes(GEO, EM.Value, fill = Livestock))+geom_col(position = "dodge")+
  scale_fill_manual(values = c("#d7191c","#2c7bb6"))+
  ggtitle("Emission Intensity by Cattle Group (t.CO2.eq/$1000 Farm Cash Receipts)")+
  ylab("Mt Co2 eq")+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 15),
        legend.position = c(.0, 1),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        legend.background = element_rect(alpha("white", 0.75)))+
        
  

Cattle_CH4_Manure <- read_csv("SPP/Cattle/Cattle.CH4.Manure.csv")
View(Cattle_CH4_Manure)

Cattle_CH4_Manure<-Cattle_CH4_Manure%>%
  gather(Livestock, EM.CH4, 2:9)

Cattle_Pop_M<-full_join(Cattle_Pop,Cattle_CH4_Manure, by=c("REF_DATE"="Year", "Livestock"="Livestock"))%>%
  mutate(CH4.Manure.EM = VALUE*EM.CH4)%>%
  
Cattle_Pop_M<-full_join(Cattle_Pop_M, Cattle_EM_EF, by= c("REF_DATE"="Year","Livestock"="livestock"))

Cattle_Pop_M<-Cattle_Pop_M%>%
  mutate(t.CH4.EF = EM*VALUE)
  
Cattle_Full<-Cattle_Pop_M

Cattle_Sub<-Cattle_Full%>%
  gather(Measure,Value, 4:8)%>%
  spread(Livestock, Value)%>%
  mutate(Total = Dairy.Cows+Dairy.Heifers+Bulls+Beef.Cows+Beef.Heifers+Heifers.Slaughter+Steers+Calves,
       Dairy.Cattle = Dairy.Cows+Dairy.Heifers,
       Non.Dairy.Cattle = Total-Dairy.Cattle)%>%
  gather(Livestock, Value, 4:14)%>%
  filter(Livestock %in% c("Dairy.Cattle", "Non.Dairy.Cattle"))%>%
  spread(GEO, Value)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, Value, 4:14)%>%
  filter(GEO != "Newfoundland and Labrador",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick")%>%
  spread(Measure, Value)

Cattle_N2O_Manure_2010 <- read_csv("SPP/Cattle/Cattle.N2O.Manure.2010.csv")
View(Cattle_N2O_Manure_2010)

Cattle_Sub<-full_join(Cattle_Sub, Cattle_N2O_Manure_2010, by= c("REF_DATE"="Year", "Livestock"="Livestock"))

Cattle_Sub<-Cattle_Sub%>%
  mutate(N2O.EM = VALUE*N2O.EF,
         CO2eq.EF = t.CH4.EF*25,
         CO2eq.MM.CH4 = CH4.Manure.EM*25,
         CO2eq.MM.N2O = N2O.EM*298,
         CO2eq.MM = CO2eq.MM.CH4+CO2eq.MM.N2O,
         CO2eq.Total = CO2eq.EF+CO2eq.MM)

Cattle_Farm_Cash <- read_csv("SPP/Cattle/Cattle.Farm.Cash.csv")
View(Cattle_Farm_Cash)

Cattle_Cash<-Cattle_Farm_Cash%>%
  spread(`Type of cash receipts`, VALUE)%>%
  mutate(Non.Dairy.Cattle = Cattle+Calve)%>%
  rename(Dairy.Cattle = Unprocessed.Milk)%>%
  select(REF_DATE, GEO,Non.Dairy.Cattle, Dairy.Cattle)%>%
  gather(Livestock, Farm.Cash, 3:4)%>%
  spread(GEO,Farm.Cash)%>%
  mutate(Atlantic = `Newfoundland and Labrador` +`Prince Edward Island` +`Nova Scotia`+`New Brunswick`)%>%
  gather(GEO, Farm.Cash, 3:13)%>%
  filter(GEO != "Newfoundland and Labrador",
         GEO != "Prince Edward Island",
         GEO != "Nova Scotia",
         GEO != "New Brunswick")

Cattle_Data<-full_join(Cattle_Sub,Cattle_Cash, by=c("REF_DATE"="REF_DATE","GEO"="GEO", "Livestock"="Livestock"))

Cattle_Data<-mutate(Cattle_Data, EM.Cash = CO2eq.Total/Farm.Cash)

Cattle_Data_2019<-Cattle_Data%>%
  filter(REF_DATE == 2019)

Cattle1<-Cattle_Data_2019%>%
  mutate(GEO = fct_reorder(GEO,desc(CO2eq.Total)),
         CO2eq.Total = CO2eq.Total/1000000)%>%
  ggplot(aes(GEO, CO2eq.Total, fill = Livestock))+geom_col()+
  scale_fill_viridis(begin = .25, end = .40,direction = -1, discrete = TRUE)+
  ggtitle("Total CO2eq emissions from Cattle")+
  ylab("Mt Co2 eq")+
  theme(axis.title.x=element_blank(),
        legend.title = element_blank(),
        legend.position = c(.975, .975),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 11, .5), breaks = seq(0, 11, 1))

Cattle2<-Cattle_Data_2019%>%
  mutate(GEO = fct_reorder(GEO,desc(EM.Cash)))%>%
  ggplot(aes(GEO, EM.Cash, fill = Livestock))+geom_col(position = "dodge")+
  scale_fill_viridis(begin = .25, end = .40,direction = -1, discrete = TRUE)+
  ggtitle("Emissions per $1000 Farm Cash Receipts")+
  ylab("t.CO2eq/$1000")+
  theme(axis.title.x=element_blank(),
        legend.title = element_blank(),
        legend.position = c(.975, .975),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 5, .25), breaks = seq(0, 5, 1))


cattle3<-Cattle_Sub%>%
  filter(REF_DATE ==2019)%>%
  select(REF_DATE, GEO, Livestock, CO2eq.EF, CO2eq.MM.CH4, CO2eq.MM.N2O)%>%
  rename(Enteric.Fermentation = CO2eq.EF,
         Manure.Management.CH4 = CO2eq.MM.CH4,
         Manure.Management.N2O = CO2eq.MM.N2O)
 
  mutate(GEO = fct_reorder(GEO, desc(CO2.eq)))%>%
  ggplot(aes(Emission.Source, CO2.eq, fill = GEO))+geom_col(position = "dodge")+
  scale_fill_viridis(begin = 0, end = 1,direction = -1, discrete = TRUE)+
  ggtitle("Emissions By Source and Animal Type")+
  ylab("t.CO2eq")+
  theme(axis.title.x=element_blank(),
        legend.title = element_blank(),
        legend.position = c(.975, .975),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(alpha("white", 0.75)))+
  scale_y_continuous(minor_breaks = seq(0 , 5, .25), breaks = seq(0, 5, 1))+
  facet_wrap(~Livestock)

Data <- EME_Data_Cattle %>%
  filter(Type == "Non-dairy cattle")

ggplot(Data,aes(x = Year))+
  geom_point(aes(y = Population), size = 4, alpha = .75, color="#69b3a2")+
  geom_line(aes(y = Population), size = 1, alpha = .5)+
  ggtitle("Population of Non-Dairy Cattle in Canada")+
  ylab("Non-Dairy Cattle (x1000)")

ggplot(Data,aes(x = Year))+
  geom_point(aes(y = EF.IE.CH4), size = 4, alpha = .75, color="#69b3a2")+
  geom_line(aes(y = EF.IE.CH4), size = 1, alpha = .5)+
  ggtitle("Implied Emission Factors: Non-Dairy Cattle")+
  ylab("kgCH4/Head/year")
 
ggplot(Data,aes(x = Year))+
  geom_point(aes(y = T.EM), size = 4, alpha = .75, color="#69b3a2")+
  geom_line(aes(y = T.EM), size = 1, alpha = .5)+
  ggtitle("Total Non-Dairy Cattle CH4 Emissions")+
  ylab("kt CH4")

ggplot(EME_Data_N20,aes(x = Year))+
  geom_point(aes(y = kgN, color = EM.NO2), size = 4, alpha = .75)+
  geom_line(aes(y = kgN, color = EM.NO2), size = 1, alpha = .5)+
  ggtitle("Total Nitrogen Use/Nitrogen Activity")+
  ylab("kg N/Year")

ggplot(EME_Data_N20,aes(x = Year))+
  geom_point(aes(y = kgN20.kgN, color = EM.NO2), size = 4, alpha = .75)+
  geom_line(aes(y = kgN20.kgN, color = EM.NO2), size = 1, alpha = .5)+
  ggtitle("Implied Emission Factors: Nitrogen")+
  ylab("kg N20-N/kg N")

ggplot(EME_Data_CropLand,aes(x = Year))+
  geom_point(aes(y = CO2.Area, color = Land.Use), size = 4, alpha = .75)+
  geom_line(aes(y = CO2.Area, color = Land.Use), size = 1, alpha = .5)+
  ggtitle("Net Carbon Emission by Land Use")+
  ylab("t CO2/ha")

ggplot(EME_Data_CropLand,aes(x = Year))+
  geom_point(aes(y = Net.CO2, color = Land.Use), size = 4, alpha = .75)+
  geom_line(aes(y = Net.CO2, color = Land.Use), size = 1, alpha = .5)+
  ggtitle("Net CO2 Emissions by Land Use")+
  ylab("kt")

Data <- EME_Data_CropLand %>%
  filter(Land.Use == "Cropland remaining cropland")
ggplot(Data,aes(x = Year))+
  geom_point(aes(y = Area), size = 4, alpha = .75, color="#69b3a2")+
  geom_line(aes(y = Area), size = 1, alpha = .5)+
  ggtitle("Total Cropland")+
  ylab("kha")
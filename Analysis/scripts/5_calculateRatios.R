# This script calculates social and ecological performance with respect to boundaries

#read in latest dataset, if needed
#myData8 <- read_csv("./myData/4_20240614_doughnutData-boundaries.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice")))

#---------------------------------------------------------------------------------------

#Calculate overshoot and shortfall ratios
myData10 <- myData8 %>%
  rowwise() %>%
  mutate(ratio = ifelse(indicator == "co2_ppm", (value - 280)/(boundary - 280), #normalised to pre-industrial 280 ppm
	  ifelse(indicator == "omega_a", (1-value/3.44)/(1-boundary/3.44), #normalised to pre-industrial 3.44 from Rockstrom et al
	  ifelse(indicator == "totalOzone", (1-value/290)/(1-boundary/290), #normalised to pre-industrial 290 from Rockstrom et al
	  ifelse(indicator == "forestAreaMKM2", (1 - value/63.87)/(1 - boundary/63.87),#note 47.9 million km2 is from Steffen et al
	  ifelse(indicator == "interhemAOD", (value - 0.03)/(boundary - 0.03), #normalised to pre-industrial 0.03 from Richardson et al 
	  ifelse(domain == "social", (100-value)/100, value/boundary)))))),
	ratio_low = ifelse(indicator == "co2_ppm", (BAU_low - 280)/(boundary - 280), #normalised to pre-industrial 280 ppm
	  ifelse(indicator == "omega_a", (1-BAU_low/3.44)/(1-boundary/3.44), #note 3.44 from Rockstrom et al
	  ifelse(indicator == "totalOzone", (1-(BAU_low/299))/(1-boundary/299), #note 310 is from weber et al 10-year average in 1960
	  ifelse(indicator == "forestAreaMKM2", (1 - BAU_low/63.87)/(1 - boundary/63.87),  #note 47.9 million km2 is from Steffen et al
	  ifelse(indicator == "interhemAOD", (BAU_low - 0.03)/(boundary - 0.03), #normalised to pre-industrial 0.03 from Richardson et al 
	  ifelse(domain == "social", (100-BAU_low)/100, BAU_low/boundary)))))),
	ratio_high = ifelse(indicator == "co2_ppm", (BAU_high - 280)/(boundary - 280), #normalised to pre-industrial 280 ppm
	  ifelse(indicator == "omega_a", (1-BAU_high/3.44)/(1-boundary/3.44), #note 3.44 from Rockstrom et al
	  ifelse(indicator == "totalOzone", (1-(BAU_high/299))/(1-boundary/299), #note 310 is from weber et al 10-year average in 1960
	  ifelse(indicator == "forestAreaMKM2", (1 - BAU_high/63.87)/(1 - boundary/63.87),  #note 47.9 million km2 is from Steffen et al
	  ifelse(indicator == "interhemAOD", (BAU_high - 0.03)/(boundary - 0.03), #normalised to pre-industrial 0.03 from Richardson et al 
	  ifelse(domain == "social", (100-BAU_high)/100, BAU_high/boundary))))))) %>%
  ungroup()

rm(myData8)

#---------------------------------------------------------------------------------------
#Look at normalised ecological values
ecoData <- myData10 %>%
  filter(date <= 2021, domain == "ecological", !indicator %in% c("chemicalsMt", "EUshare_hzdHealth")) %>%
  mutate(indicator = factor(indicator, levels = c("co2_ppm", "erf_wm2", "omega_a", "chemicalsMt_Hzd", "phosphorusMt", "nitrogenMt",
    "blueDev", "soilDev", "forestAreaMKM2", "extinction1900", "hanppGtC", "interhemAOD", "totalOzone")))
  
ggplot(data=ecoData, aes(x=date)) +
  geom_hline(yintercept = 1, col = "black", linetype=6, lwd=0.8, alpha=0.5) +
  geom_line(data=ecoData, aes(y=ratio, colour=indicator), lwd=0.8) +
  geom_point(date=ecoData, aes(y=ratio, colour=indicator), size=1.5) +
  #geom_ribbon(data=ecoData %>% filter(date > 2021), aes(ymin=ratio_low, ymax=ratio_high, fill=indicator), alpha=0.3) +
  #geom_line(data=ecoData %>% filter(date > 2021), aes(y=ratio, colour=indicator), alpha=0.6, lwd=0.8) +
  scale_y_continuous(labels=scales::percent_format(big.mark=",")) +
  coord_cartesian(ylim=c(0,4)) +
  facet_wrap(~dimension, ncol=4) +
  labs(subtitle = "The ecological ceiling and its indicators of overshoot", x = "Year", y = "Ecological overshoot (100% = planetary boundary)") +
  theme_chart_SMALLM

#ggsave("./figures/5_ecologicalIndicators_400pctCut.png", device="png")

#Look at normalised social values
socData <- myData10 %>%
  filter(date <= 2021, domain == "social") %>%
  mutate(indicator = factor(indicator, levels=c("undernourishment", "foodInsecurity", "under5death", "noUniversalHealth", "adultLiteracy",
    "secondarySchool", "societalPoverty", "youthNEET", "drinkingH2O", "sanitation", "energyAccess", "energyIndoor", "internet", "publicTrans",
    "urbanSlums", "genderGapIndex", "lifeSatisfaction", "PalmaOver2_share", "voiceAccount", "controlCorruption", "homicideOver5")),
  ratio = 1-ratio)
  
ggplot(data=socData, aes(x=date)) +
  geom_hline(yintercept = 0, col = "black", linetype=6, lwd=0.8, alpha=0.5) +
  geom_line(data=socData, aes(y=ratio, colour=indicator), lwd=0.8) +
  geom_point(date=socData, aes(y=ratio, colour=indicator), size=1.3) +
  #geom_ribbon(data=socData %>% filter(date > 2021), aes(ymin=ratio_low, ymax=ratio_high, fill=indicator), alpha=0.3) +
  #geom_line(data=socData %>% filter(date > 2021), aes(y=ratio, colour=indicator), alpha=0.6, lwd=0.8) +
  scale_y_reverse(labels=scales::percent) +
  facet_wrap(~dimension, ncol=4) +
  labs(subtitle = "The social foundation and its indicators of shortfall", x = "Year", y = "Social shortfall (0% = social foundation)") +
  theme_chart_SMALLM

#ggsave("./figures/5_socialIndicators.png", device="png")


rm(ecoData, socData)
#---------------------------------------------------------------------------------------
#Add placeCodes and indicatorCodes
myData11 <- myData10 %>%
  add_column(place = "World", placeCode = "WLD") %>%
  relocate(place, placeCode, .after=scenario) %>%
  rowwise() %>%
  mutate(indicatorCode = ifelse(indicator == "hanppGtC", "bio1",
	ifelse(indicator == "extinction1900", "bio2",
	ifelse(indicator == "chemicalsMt_Hzd", "chem",
	ifelse(indicator == "co2_ppm", "clim1",
	ifelse(indicator == "erf_wm2", "clim2",
	ifelse(indicator == "nitrogenMt", "fertN",
	ifelse(indicator == "phosphorusMt", "fertP",
	ifelse(indicator == "blueDev", "H2Oblu",
	ifelse(indicator == "soilDev", "H2Ogrn",
	ifelse(indicator == "forestAreaMKM2", "land",
	ifelse(indicator == "interhemAOD", "airPol",
	ifelse(indicator == "omega_a", "acid",
	ifelse(indicator == "totalOzone", "O3dep",
	ifelse(indicator == "adultLiteracy", "edu1",
	ifelse(indicator == "secondarySchool", "edu2",
	ifelse(indicator == "energyAccess", "enrgy1",
	ifelse(indicator == "energyIndoor", "enrgy2",
	ifelse(indicator == "genderGapIndex", "eqDi1",
	ifelse(indicator == "foodInsecurity", "food1",
	ifelse(indicator == "undernourishment", "food2",
	ifelse(indicator == "under5death", "hlth2",
	ifelse(indicator == "noUniversalHealth", "hlth1",
	ifelse(indicator == "urbanSlums", "hous",
	ifelse(indicator == "societalPoverty", "inWk1",
	ifelse(indicator == "youthNEET", "inWk2",
	ifelse(indicator == "publicTrans", "con1",
	ifelse(indicator == "internet", "con2",
	ifelse(indicator == "lifeSatisfaction", "socCo1",
	ifelse(indicator == "PalmaOver2_share", "socCo2",
	ifelse(indicator == "controlCorruption", "peJu1",
	ifelse(indicator == "homicideOver5", "peJu2",
	ifelse(indicator == "voiceAccount", "poVo",
	ifelse(indicator == "drinkingH2O", "watr1",
	ifelse(indicator == "sanitation", "watr2", "ERROR"))))))))))))))))))))))))))))))))))) %>%
  ungroup() %>%
  relocate(indicatorCode, .after=indicator)

rm(myData10)

#add racial inequality missing values
racialEquity <- tibble(date = as.numeric(2000:2030),
  scenario = ifelse(date <= 2021, "historical", "bau"), 
  place = "World",
  placeCode = "WLD",
  domain = "social",
  dimension = "equality",
  indicator = "racialEquality",
  indicatorCode = "eqDi2",
  value = as.numeric(NA),
  BAU_low = as.numeric(NA),
  BAU_high = as.numeric(NA),
  boundary = as.numeric(NA),
  ratio = 1.00001,
  ratio_low = as.numeric(NA),
  ratio_high = as.numeric(NA)) %>%
  relocate(date, .after=indicatorCode)

myData12 <- rbind(myData11, racialEquity) %>%
  arrange(domain, dimension, indicator, indicatorCode)

rm(racialEquity)

#-----------------------------------------------------------------------------------
#					write to file
#-------------------------------------------------------------------------------------
write_csv(myData12, "./myData/5_20240614_doughnutDat-boundariesRatios.csv")




#Create database and load global social and ecological data files

#--------------------------------------------------------------------
#		ecological indicators
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#		climate change
#--------------------------------------------------------------------

co2 <- read_csv("./cleanData/eco-1_climateChange_clean.csv") %>%
	filter(date >= 2000, date <= 2021) %>%
	gather(indicator, value, c(4:5))

ggplot(co2, aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~indicator, scales="free") +
  #scale_y_continuous(limits=c(350, 450)) +
  #geom_hline(yintercept=350, color="green") +
  #geom_hline(yintercept=450, color="red") +
  theme_chart

#--------------------------------------------------------------------
#		ocean acidification
#--------------------------------------------------------------------

oceanAcid <- read_csv("./cleanData/eco-2_oceanAcid_clean.csv") %>%
  filter(date >= 2000) %>%	
  gather(indicator, value, omega_a)

ggplot(oceanAcid, aes(x=date, y=value)) +
  geom_line() +
  scale_y_continuous(limits=c(2.4, 3.15))  +
  geom_hline(yintercept=2.75, color="green") +  #80% of 3.44
  geom_hline(yintercept=2.41, color="red") +	#70% of 3.44
  theme_chart

#join
myData <- rbind(co2, oceanAcid)

rm(co2, oceanAcid)

#--------------------------------------------------------------------
#		chemical pollution
#--------------------------------------------------------------------

chemPollution <- read_csv("./cleanData/eco-3_chemPollution_clean.csv") %>%
	gather(indicator, value, c(4:5)) %>%
	group_by(indicator) %>%
	mutate(value = na.approx(value, na.rm=F)) %>%
	ungroup()

ggplot(chemPollution, aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~indicator, scales="free") + 
  theme_chart

#join
myData <- rbind(myData, chemPollution)

rm(chemPollution)

#--------------------------------------------------------------------
#		nutrient pollution
#--------------------------------------------------------------------

fertilizer <- read_csv("./cleanData/eco-4_nutrientPollution_clean.csv") %>%
	gather(indicator, value, c(4:5)) %>%
	filter(date >= 2000)

ggplot(fertilizer %>% filter(indicator == "nitrogenMt"), aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~indicator) + 
  scale_y_continuous(limits=c(5, 200))  +
  geom_hline(yintercept=6.2, color="green") +  
  geom_hline(yintercept=11.2, color="red") +
  theme_chart

#join
myData <- rbind(myData, fertilizer)

rm(fertilizer)

#--------------------------------------------------------------------
#		air pollution
#--------------------------------------------------------------------

airPollution <- read_csv("./cleanData/eco-8_airPollution_clean.csv") %>%
  filter(date >= 2000) %>%	
  gather(indicator, value, interhemAOD) %>%
  group_by(indicator) %>%
  mutate(value = na.approx(value, na.rm=F)) %>%
  ungroup()

ggplot(airPollution, aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~indicator) +
  scale_y_continuous(limits=c(0, 0.3))  +
  geom_hline(yintercept=0.1, color="green") +  
  geom_hline(yintercept=0.25, color="red") +
  theme_chart


#join
myData <- rbind(myData, airPollution)

rm(airPollution)

#--------------------------------------------------------------------
#		freshwater disruption
#--------------------------------------------------------------------

freshwater <- read_csv("./cleanData/eco-5_freshwaterDisruption_clean.csv") %>%
	gather(indicator, value, c(4:5)) %>%
	filter(date >= 2000)

ggplot(freshwater, aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~indicator) +
  #scale_y_continuous(limits=c(350, 450)) +
  #geom_hline(yintercept=350, color="green") +
  #geom_hline(yintercept=450, color="red") +
  theme_chart

#join
myData <- rbind(myData, freshwater)

rm(freshwater)

#--------------------------------------------------------------------
#		land conversion
#--------------------------------------------------------------------

landCon <- read_csv("./cleanData/eco-6_landConversion_clean.csv") %>%
  filter(date >= 2000) %>%	
  gather(indicator, value, forestAreaMKM2)

ggplot(landCon, aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~indicator) +
  scale_y_continuous(limits=c(33, 50))  +
  geom_hline(yintercept=34.5, color="red") +  
  geom_hline(yintercept=47.9, color="green") +
  theme_chart

#join
myData <- rbind(myData, landCon)

rm(landCon)

#--------------------------------------------------------------------
#		biodiversity breakdown
#--------------------------------------------------------------------

biodiversity <- read_csv("./cleanData/eco-7_biodiversityBreakdown_clean.csv") %>%
	gather(indicator, value, c(4:5)) %>%
	group_by(indicator) %>%
	mutate(value = na.approx(value, na.rm=F)) %>%
	ungroup()

ggplot(biodiversity %>% filter(indicator == "hanppGtC"), aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~indicator) +
  scale_y_continuous(limits=c(0, 30))  +
  geom_hline(yintercept=5.6, color="green") +  
  geom_hline(yintercept=11.2, color="red") +
  theme_chart

ggplot(biodiversity %>% filter(indicator == "extinction1900"), aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~indicator) +
  scale_y_continuous(limits=c(10, 120))  +
  geom_hline(yintercept=10, color="green") +  
  geom_hline(yintercept=100, color="red") +
  theme_chart

#join
myData <- rbind(myData, biodiversity)

rm(biodiversity)

#--------------------------------------------------------------------
#		ozone depletion
#--------------------------------------------------------------------

ozoneDepletion <- read_csv("./cleanData/eco-9_ozoneDepletion_clean.csv") %>%
	gather(indicator, value, totalOzone)

ggplot(ozoneDepletion, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  geom_hline(yintercept=290, color="green") +  
  geom_hline(yintercept=260, color="red") +
  theme_chart

#join
myData <- rbind(myData, ozoneDepletion)

rm(ozoneDepletion)



#--------------------------------------------------------------------
#		social indicators
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#		food
#--------------------------------------------------------------------

food <- read_csv("./cleanData/soc-1_food_clean.csv") %>%
	gather(indicator, value, c(4:5))

ggplot(food, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(50, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, food)

rm(food)

#--------------------------------------------------------------------
#		health
#--------------------------------------------------------------------

health <- read_csv("./cleanData/soc-2_health_clean.csv") %>%
	gather(indicator, value, c(4:5)) %>%
	group_by(indicator) %>%
	mutate(value = na.approx(value, na.rm=F)) %>%
	ungroup()

ggplot(health, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(80, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, health)

rm(health)

#--------------------------------------------------------------------
#		education
#--------------------------------------------------------------------

education <- read_csv("./cleanData/soc-3_education_clean.csv") %>%
  gather(indicator, value, c(4:5)) %>%
  rowwise() %>%
  mutate(value = 100 - value) %>%
  ungroup

ggplot(education, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(80, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, education)

rm(education)

#--------------------------------------------------------------------
#		income & work
#--------------------------------------------------------------------

incomeWork <- read_csv("./cleanData/soc-4_incomeWork_clean.csv") %>%
	gather(indicator, value, c(4:5)) %>%
	filter(date >= 2000) %>%
	rowwise() %>%
	mutate(value = ifelse(indicator == "societalPoverty", value*100, value)) %>%
	ungroup()

ggplot(incomeWork, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(85, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, incomeWork)

rm(incomeWork)


#--------------------------------------------------------------------
#		water
#--------------------------------------------------------------------

water <- read_csv("./cleanData/soc-5_water_clean.csv") %>%
	gather(indicator, value, c(4:5)) %>%
	mutate(value = 100 - value)

ggplot(water, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(80, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, water)

rm(water)

#--------------------------------------------------------------------
#		energy
#--------------------------------------------------------------------

energy <- read_csv("./cleanData/soc-6_energy_clean.csv") %>%
	gather(indicator, value, c(4:5)) %>%
	mutate(value = 100 - value)

ggplot(energy, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(60, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, energy)

rm(energy)

#--------------------------------------------------------------------
#		connectivity
#--------------------------------------------------------------------

connectivity <- read_csv("./cleanData/soc-7_connectivity_clean.csv") %>%
	gather(indicator, value, c(4:5)) %>%
	mutate(value = 100 - value)

ggplot(connectivity, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  geom_point() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(100, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, connectivity)

rm(connectivity)

#--------------------------------------------------------------------
#		housing
#--------------------------------------------------------------------

housing <- read_csv("./cleanData/soc-8_housing_clean.csv") %>%
	gather(indicator, value, urbanSlums) %>%
	mutate(value = na.approx(value, na.rm=F))

ggplot(housing, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(50, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, housing)

rm(housing)

#--------------------------------------------------------------------
#		equality
#--------------------------------------------------------------------

equality <- read_csv("./cleanData/soc-9_equality_clean.csv") %>%
	gather(indicator, value, c(4:5)) %>%
	group_by(indicator) %>%
	mutate(value = na.approx(value, na.rm=F)) %>%
	ungroup() %>%
	mutate(value = 100 - value)

ggplot(equality, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(50, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, equality)

rm(equality)


#--------------------------------------------------------------------
#		social cohesion
#--------------------------------------------------------------------

socialCohesion <- read_csv("./cleanData/soc-10_socialCohesion_clean.csv") %>%
	select(-avgPalma) %>%
	gather(indicator, value, c(4:5))

ggplot(socialCohesion, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(100, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, socialCohesion)

rm(socialCohesion)

#--------------------------------------------------------------------
#		political voice
#--------------------------------------------------------------------

politicalVoice <- read_csv("./cleanData/soc-11_politicalVoice_clean.csv") %>%
	gather(indicator, value, voiceAccount)

ggplot(politicalVoice, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(100, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, politicalVoice)

rm(politicalVoice)

#--------------------------------------------------------------------
#		peace and justice
#--------------------------------------------------------------------

peaceJustice <- read_csv("./cleanData/soc-12_peaceJustice_clean.csv") %>%
	gather(indicator, value, c(4:5))

ggplot(peaceJustice, aes(x=date, y=value, col=indicator)) +
  geom_line() +
  facet_wrap(~dimension) +
  scale_y_reverse(limits=c(100, 0))  +
  geom_hline(yintercept=0, color="green") +  
  #geom_hline(yintercept=261, color="red") +
  theme_chart

#join
myData <- rbind(myData, peaceJustice)

rm(peaceJustice)

#--------------------------------------------------------------------
#		WRITE LONG AND WIDE TO FILE
#--------------------------------------------------------------------
myData1 <- myData %>%
	spread(date, value)


#write_csv(myData, "./myData/1_20240614_doughnutv3-data.csv")
#write_csv(myData1, "./myData/1_20240614_doughnutv3-data_wide.csv")

rm(myData, myData1)


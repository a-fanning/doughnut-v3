# This script calculates linear rates of change needed to eliminate social shortfall and ecological overshoot
# for each indicator by 2050, based on current levels.

#read in cleaned data file
myData12 <- read_csv("./myData/5_20240614_doughnutDat-boundariesRatios.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice")))

ecoSoc3 <- read_csv("./myData/9_20240624_Doughnut-TablesData.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice")))

#-----------------------------------------------------------------------------------------------------------
#read in population data
popData <- read_csv("./cleanData/WPP2022_PopulationData_1950-2100.csv") 

wld <- popData %>%
  group_by(date) %>%
  summarise(population = sum(population, na.rm=T)) %>%
  ungroup() %>%
  add_column(variant = "estimate", country = "World", iso3c = "WLD") %>%
  relocate(variant, country, iso3c)

#write world population to file
#write_csv(wld, "./myData/10_worldPopulation_1950-2100.csv")
rm(popData)

#-----------------------------------------------------------------------------------------------------------
#Pull out 2000-01 and 2020-21 population results to discuss
wldPopStart <- wld %>% 
  filter(date %in% c(2000, 2001)) %>%
  select(place = country, placeCode = iso3c, date, populationStart = population) %>%
  group_by(place, placeCode) %>%
  summarise(populationStart = mean(populationStart)) %>%
  ungroup()

wldPopEnd<- wld %>% 
  filter(date %in% c(2020, 2021)) %>%
  select(place = country, placeCode = iso3c, date, populationEnd = population) %>%
  group_by(place, placeCode) %>%
  summarise(populationEnd = mean(populationEnd)) %>%
  ungroup()

wldPop <- full_join(wldPopStart, wldPopEnd, by=c("place", "placeCode"))

rm(wldPopStart, wldPopEnd)

ecoSoc4 <- left_join(ecoSoc3, wldPop, by=c("place", "placeCode"))

rm(ecoSoc3)
#-----------------------------------------------------------------------------------------------------
#ranges
#-----------------------------------------------------------------------------------------------------
#look at change in social shortfall range by year (bring first observation backward to remove structural breaks
#from missing vals)

socFull <- myData12 %>%
  filter(domain == "social", date <= 2021, !indicator %in% c("foodInsecurity", "racialEquality")) %>%
  mutate(ratio = na.approx(1-ratio, na.rm=F, rule=2)) 

ggplot(socFull, aes(date, ratio)) +
  geom_hline(yintercept = 0, lwd=3, col="#227443") +
  geom_boxplot(aes(group = date), outlier.shape=NA) +
  geom_jitter(width=0.2) +
  coord_cartesian(ylim=c(1,0), xlim=c(2000,2021)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Year", y="Range of shortfall across social\nindicators (0 = no shortfall)") +
  theme_bw()

#save figure to file
#ggsave("./figures/10_shortfallRange-by-year_Boxplots.png", width = 180, height = 80, units = "mm", device="png")

#get data
g <- ggplot(socFull, aes(date, ratio)) +
  geom_boxplot(aes(group = date))

socRange <- layer_data(g)
rm(g)

socOutliers <- socRange %>%
  select(outliers, x) %>%
  unnest(outliers)

socRange1 <- socRange %>%
  select(-outliers) %>%
  left_join(., socOutliers, by="x")

#write boxplot data to file
#write_csv(socRange1, "./myData/10_20240515_SocialShortfall-boxPlotData.csv")

rm(socRange, socOutliers)
#-----------------------------------------------------------------------------------------------------
#look at change in ecological overshoot range by year
ecoFull <- myData12 %>%
  filter(domain == "ecological", date <= 2021) %>%
  mutate(ratio = ratio-1)

ggplot(ecoFull, aes(date, ratio)) +
  geom_hline(yintercept = 0, lwd = 3, col="#227443") + 
  geom_boxplot(aes(group = date)) + 
  geom_jitter(width=0.2) +
  coord_cartesian(ylim = c(-1,3), xlim = c(2000,2021)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Range of overshoot across ecological\nindicators (0 = no overshoot)") +
  theme_bw()

#ggsave("./figures/10_overshootRange-by-year_Boxplots.png", width = 180, height = 80, units = "mm", device="png")

#get data
g <- ggplot(ecoFull, aes(date, ratio)) +
  geom_boxplot(aes(group = date))

ecoRange <- layer_data(g) 
rm(g)

# i don't have time to figure out how to save the listed outliers to file. 
#they are repeated values for extinction rate and chemical pollution, at 900% and 1400+% respectively

ecoRange1 <- ecoRange %>%
  select(-outliers) 


#write boxplot data to file
#write_csv(ecoRange1, "./myData/10_20240515_EcologicalOvershoot-boxPlotData.csv")

#------------------------------------------------------------------------------------------
#look at start/end periods
soc <- ecoSoc4 %>% 
  filter(domain == "social") %>%
  rowwise() %>%
  mutate(ratioStart = ifelse(indicator == "racialEquality", NA, ratioStart),
    ratioEnd = ifelse(indicator == "racialEquality", NA, ratioEnd)) %>%
  ungroup()

eco <- ecoSoc4 %>%
  filter(domain == "ecological")
#------------------------------------------------------------------------------------------
#Check number of indicators by range of shortfall 
#------------------------------------------------------------------------------------------
#Check number of indicators with more than 50%  
soc %>% filter(ratioStart >= 0.5) #10
soc %>% filter(ratioEnd >= 0.5) #5

soc %>% filter(ratioStart >= 0.25) #16
soc %>% filter(ratioEnd >= 0.25) #15

#------------------------------------------------------------------------------------------
#Historical trends 
#------------------------------------------------------------------------------------------
#Check ecological interquartile range across indicators w/ available time series (N=8) 
eco %>% 
  filter(!indicator %in% c("blueDev", "extinction1900", "interhemAOD", "omega_a"), betterWorse == "worsening") %>%
  summary() #

#Check ecological interquartile range across indicators w/ available time series
soc %>% 
  summary()

#------------------------------------------------------------------------------------------
#Comparing social indicators with significant improving trends with pathway to eliminate social shortfall by 2030 (N=15)
#------------------------------------------------------------------------------------------
soc1 <- soc %>% 
  filter(!indicator %in% c("publicTrans","racialEquality", "lifeSatisfaction", "voiceAccount"), betterWorse == "improving") %>% 
  rowwise() %>%
  mutate(ambition = slopeD / histEstimate) %>%
  ungroup() 

#check ranges
soc1 %>% filter(ambition > 9) #(N=5)
soc1 %>% filter(ambition <= 2) #(N=2)
soc1 %>% filter(ambition > 2 & ambition <= 9) #(N=9)

#check number of indicators with trends > population growth
soc1 %>% filter(histEstimate <= -0.012)

soc1 %>% summary()

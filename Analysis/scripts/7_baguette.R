#This script creates an 'unrolled' Doughnut visual in a stacked bar chart
#note if coming from previous 6_convertToJSON script, 
#it'd be a good idea to clear the global environment by restarting R (and re-run '1_setup.r')

#Read in data file if needed.
#myData12 <- read_csv("./myData/5_20240614_doughnutDat-boundariesRatios.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice")))

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#					PREP SOCIAL VARIABLES 
#-------------------------------------------------------------------------------------------------
#get social data up to 2021
socData <- myData12 %>% 
  filter(domain == "social", date <= 2021)


#get 2000-01 values for social indicators (except indicators that start later) 
socStart <- socData %>%
  filter(date %in% c(2000,2001),
    !indicator %in% c("youthNEET", "genderGapIndex", "lifeSatisfaction", "foodInsecurity")) %>%
  select(-scenario, -BAU_low, -BAU_high, -boundary, -ratio_low, -ratio_high) %>%
  group_by(place, placeCode, domain, dimension, indicator, indicatorCode) %>%
  summarise(valueStart = mean(value, na.rm=T),
    ratioStart = mean(ratio, na.rm=T)) %>%
  ungroup()

#get first observations from 2005-06 for youthNEET, genderGapIndex, and lifeSatisfaction
soc05 <- socData %>%
  filter(date %in% c(2005, 2006), 
    indicator %in% c("youthNEET", "genderGapIndex", "lifeSatisfaction")) %>%
  select(-scenario, -BAU_low, -BAU_high, -boundary, -ratio_low, -ratio_high) %>%
  group_by(place, placeCode, domain, dimension, indicator, indicatorCode) %>%
  summarise(valueStart = mean(value, na.rm=T),
    ratioStart = mean(ratio, na.rm=T)) %>%
  ungroup()

#get first observations from 2014-15 for foodInsecurity
soc14 <- socData %>%
  filter(date %in% c(2014, 2015), indicator == "foodInsecurity") %>%
  select(-scenario, -BAU_low, -BAU_high, -boundary, -ratio_low, -ratio_high) %>%
  group_by(place, placeCode, domain, dimension, indicator, indicatorCode) %>%
  summarise(valueStart = mean(value, na.rm=T),
    ratioStart = mean(ratio, na.rm=T)) %>%
  ungroup()

socStart1 <- rbind(socStart, soc05, soc14)  %>%
  arrange(dimension, indicator, indicatorCode)

rm(socStart, soc05, soc14)

#get 2020-21 values for social indicators 
socEnd <- socData %>%
  filter(date %in% c(2020,2021)) %>%
  select(-scenario, -BAU_low, -BAU_high, -boundary, -ratio_low, -ratio_high) %>%
  group_by(place, placeCode, domain, dimension, indicator, indicatorCode) %>%
  summarise(valueEnd = mean(value, na.rm=T),
    ratioEnd = mean(ratio, na.rm=T)) %>%
  ungroup()


soc <- full_join(socStart1, socEnd, by=c("place", "placeCode", "domain", "dimension", "indicator",
    "indicatorCode"))

rm(socStart1, socEnd)

soc1 <- soc %>%
  arrange(dimension, indicator, indicatorCode) %>%
  mutate(ratioStart = 1 - ratioStart, ratioEnd = 1 - ratioEnd) %>%
  rowwise() %>%
  mutate(ratioStart = ifelse(indicator == "publicTrans", ratioEnd, ratioStart),
    diff = ratioEnd - ratioStart,
    betterWorse = ifelse(diff > 0, "worsening", "improving")) %>%
  ungroup()

rm(soc)

soc2 <- soc1 %>% 
  mutate(indicator = factor(indicator, levels=c("foodInsecurity", "undernourishment", "noUniversalHealth",
      "under5death", "adultLiteracy", "secondarySchool", "societalPoverty", "youthNEET", "drinkingH2O",
      "sanitation", "energyAccess", "energyIndoor", "internet", "publicTrans", "urbanSlums", "genderGapIndex",
      "racialEquality", "lifeSatisfaction", "PalmaOver2_share", "voiceAccount", "controlCorruption",
      "homicideOver5"))) %>%
  rowwise() %>%
  mutate(weight = ifelse(dimension == "housing" || dimension == "political voice", 100, 50),
    diffBetter = ifelse(diff <= 0, abs(diff), NA),
    diffWorse = ifelse(diff <= 0, NA, diff),
    mainBetter = ifelse(diff <= 0, ratioStart - diffBetter, NA),
    mainWorse = ifelse(diff > 0, ratioEnd - diffWorse, NA)) %>%
  ungroup()

rm(soc1)
#---------------------------------------------------------------------------------------------------------
#prep visual vars
#---------------------------------------------------------------------------------------------------------
#gather data and calculate x-axis bar positions
socChart <- soc2 %>%
  select(-valueStart, -ratioStart, -valueEnd, -ratioEnd, -diff) %>%
  gather(changeBar, value, c(9:12)) %>%
  group_by(changeBar) %>%
  mutate(pos = 0.5 * (cumsum(weight) + cumsum(c(0, weight[-length(weight)])))) %>%
  ungroup()

#define colours
colours <- c("mainBetter"="#cc0243", "diffBetter" = "#febbd1", 
  "mainWorse" = "#cc0243", "diffWorse"="#920130") # #f68f98 #a51523

#define dimension labels
dimLabs <- socChart %>%
  count(dimension) %>%
  select(-n) %>%
  mutate(pos = seq(50, 1150, 100),
    dimLab.ch = c("food", "health", "education", "income\n& work", "water", "energy", "connectivity",
	"housing", "equality", "social\ncohesion", "political\nvoice", "peace &\njustice"),
    dimLabs = factor(dimLab.ch, levels=c("food", "health", "education", "income\n& work", "water", "energy", "connectivity",
	"housing", "equality", "social\ncohesion", "political\nvoice", "peace &\njustice"))) %>%
  select(-dimension, -dimLab.ch)

#define axis labels
axisLabs <- tibble(labs.ch = c("food\ninsecurity", "under-\nnourished", "lack of\nhealth\nservices",
    "under-5\nmortality", "illiteracy\nrate", "incomplete\nsecondary\nschool", "societal\npoverty", "youth\nNEET",
    "unsafe\ndrinking\nwater", "unsafe\nsanitation", "lack of\nelectricity", "lack of\nclean fuels\nindoors",
    "lack of\ninternet", "lack of\npublic\ntransport", "slums or\ninadequate\nhousing", "gender\ninequality",
    "racial\ninequality", "dissatisfied\nwith life", "income\ninequality", "lack of voice &\naccountability",
    "corruption", "homicide\nrate"),
  labs = factor(labs.ch, levels = c("food\ninsecurity", "under-\nnourished", "lack of\nhealth\nservices",
    "under-5\nmortality", "illiteracy\nrate", "incomplete\nsecondary\nschool", "societal\npoverty", "youth\nNEET",
    "unsafe\ndrinking\nwater", "unsafe\nsanitation", "lack of\nelectricity", "lack of\nclean fuels\nindoors",
    "lack of\ninternet", "lack of\npublic\ntransport", "slums or\ninadequate\nhousing", "gender\ninequality",
    "racial\ninequality", "dissatisfied\nwith life", "income\ninequality", "lack of voice &\naccountability",
    "corruption", "homicide\nrate"))) %>% select(-labs.ch)

#get top column positions
colLabs <- soc2 %>%
  select(dimension, indicator, ratioStart, ratioEnd) %>%
  rowwise() %>%
  mutate(topVal = max(ratioStart, ratioEnd)) %>%
  ungroup() %>%
  cbind(axisLabs) %>%
  select(-ratioStart, -ratioEnd) %>% tibble()

#get xpos values for labels
pos <- socChart %>%
  select(dimension, indicator, pos) %>%
  slice_head(n=22)

colLabs1 <- left_join(colLabs, pos, by=c("dimension", "indicator")) %>%
  mutate(topVal = ifelse(indicator %in% c("noUniversalHealth", "secondarySchool", "drinkingH2O", 
    "energyIndoor", "publicTrans", "urbanSlums"), topVal + 0.07,
      ifelse(indicator %in% c("internet", "racialEquality"), 0.85,
      ifelse(indicator == "controlCorruption", topVal + 0.03, topVal + 0.05))),
    constVal = 0.84,
    myCol = "black")

rm(colLabs, pos)

#---------------------------------------------------------------------------------------------------------
#plot!
#---------------------------------------------------------------------------------------------------------
socBars <- ggplot(socChart, aes(x=pos, y=value, width=weight-2, group=changeBar)) +
  geom_bar(aes(fill=changeBar), stat="identity") + 
  geom_vline(xintercept = seq(100,1100,100), colour = "#ffffff", lwd=1.5) +
  geom_vline(xintercept = seq(100,1100,100), colour = "#f0f0f0", lwd=0.5) +
  geom_hline(yintercept = 0, colour="#227443", lwd=2.5) +
  annotate("rect", xmin=851, xmax=897, ymin=0, ymax=2, alpha=0.2) +
  annotate("text", x = colLabs1$pos, y = colLabs1$constVal, label = colLabs1$labs,
    colour = colLabs1$myCol, vjust=1, size=1.58) +
  scale_fill_manual(name = "Change in\nsocial shortfall,\n2021 versus 2000", limits=c("mainBetter", "mainWorse", "diffBetter", "diffWorse"),
    labels=c("Reference1", "Reference2", "Improving from 2000", "Worsening from 2000"), values=colours) +
  coord_cartesian(ylim=c(1,0)) +
  scale_y_reverse(labels = scales::percent_format(big.mark=","), expand = c(0,0.01)) +
  scale_x_continuous(labels = dimLabs$dimLabs, breaks=dimLabs$pos, position = "top", expand=c(0,0.05)) +
  labs(x=element_blank(), y="Social shortfall") +
  theme_classic() +
  theme(
    plot.margin = margin(0.5, 0.1, 0.1, 0.1, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    axis.title = element_text(size=8),
    axis.text = element_text(size=6),
    axis.line = element_blank(),
    axis.ticks.x = element_blank())

rm(socChart, colours, axisLabs, colLabs1, dimLabs)

#-------------------------------------------------------------------------------------------------
#					PREP ECOLOGICAL VARIABLES 
#-------------------------------------------------------------------------------------------------
#get ecological data up to 2021
ecoData <- myData12 %>% 
  filter(domain == "ecological", date <= 2021)

#get 2-year average 2000-01 values for ecological indicators, except ozone
ecoStart <- ecoData %>%
  filter(date %in% c(2000,2001), !indicator %in% c("chemicalsMt", "EUshare_hzdHealth", "totalOzone")) %>%
  select(-scenario, -BAU_low, -BAU_high, -boundary, -ratio_low, -ratio_high) %>%
  group_by(place, placeCode, domain, dimension, indicator, indicatorCode) %>%
  summarise(valueStart = mean(value, na.rm=T),
    ratioStart = mean(ratio, na.rm=T)) %>%
  ungroup()

#get 5-year average 2000-2004 values for ozone
ecoStartOz <- ecoData %>%
  filter(date %in% c(2000:2004), indicator == "totalOzone") %>%
  select(-scenario, -BAU_low, -BAU_high, -boundary, -ratio_low, -ratio_high) %>%
  group_by(place, placeCode, domain, dimension, indicator, indicatorCode) %>%
  summarise(valueStart = mean(value, na.rm=T),
    ratioStart = mean(ratio, na.rm=T)) %>%
  ungroup()

ecoStart1 <- rbind(ecoStart, ecoStartOz) %>%
  arrange(domain, dimension, indicator)

rm(ecoStart, ecoStartOz)

#get 2-year avg 2020-21 values for ecological indicators, except ozone
ecoEnd <- ecoData %>%
  filter(date %in% c(2020,2021), !indicator %in% c("chemicalsMt", "EUshare_hzdHealth", "totalOzone")) %>%
  select(-scenario, -BAU_low, -BAU_high, -boundary, -ratio_low, -ratio_high) %>%
  group_by(place, placeCode, domain, dimension, indicator, indicatorCode) %>%
  summarise(valueEnd = mean(value, na.rm=T),
    ratioEnd = mean(ratio, na.rm=T)) %>%
  ungroup()

#get 5-year average 2017-2021 values for ozone
ecoEndOz <- ecoData %>%
  filter(date %in% c(2017:2021), indicator == "totalOzone") %>%
  select(-scenario, -BAU_low, -BAU_high, -boundary, -ratio_low, -ratio_high) %>%
  group_by(place, placeCode, domain, dimension, indicator, indicatorCode) %>%
  summarise(valueEnd = mean(value, na.rm=T),
    ratioEnd = mean(ratio, na.rm=T)) %>%
  ungroup()

ecoEnd1 <- rbind(ecoEnd, ecoEndOz) %>%
  arrange(domain, dimension, indicator)

rm(ecoEnd, ecoEndOz)

#join start and end average values
eco <- full_join(ecoStart1, ecoEnd1, by=c("place", "placeCode", "domain", "dimension", "indicator", "indicatorCode")) 

rm(ecoStart1, ecoEnd1)

eco1 <- eco %>%
  rowwise() %>%
  mutate(diff = ratioEnd - ratioStart,
    betterWorse = ifelse(diff > 0, "worsening", "improving")) %>%
  ungroup()

rm(ecoData, eco)

eco2 <- eco1 %>% 
  mutate(indicator = factor(indicator, levels=c("co2_ppm", "erf_wm2", "omega_a",
      "chemicalsMt_Hzd", "nitrogenMt", "phosphorusMt", "interhemAOD", "blueDev", "soilDev", "forestAreaMKM2",
      "extinction1900", "hanppGtC", "totalOzone"))) %>%
  rowwise() %>%
  mutate(weight = ifelse(dimension == "ocean acidification" || dimension == "chemical pollution" || 
	dimension == "land conversion" || dimension == "air pollution" || dimension == "ozone depletion", 100, 50),
    diffBetter = ifelse(diff <= 0, abs(diff), NA),
    diffWorse = ifelse(diff <= 0, NA, diff),
    mainBetter = ifelse(diff <= 0, ratioStart - diffBetter, NA),
    mainWorse = ifelse(diff > 0, ratioEnd - diffWorse, NA)) %>%
  ungroup()

rm(eco1)
#---------------------------------------------------------------------------------------------------------
#prep visual vars
#---------------------------------------------------------------------------------------------------------
#gather data
ecoChart <- eco2 %>%
  select(-valueStart, -ratioStart, -valueEnd, -ratioEnd, -diff) %>%
  gather(changeBar, value, c(9:12)) %>%
  group_by(changeBar) %>%
  mutate(pos = 0.5 * (cumsum(weight) + cumsum(c(0, weight[-length(weight)])))) %>%
  ungroup()


#define colours
colours <- c("mainBetter"="#cc0243", "diffBetter" = "#febbd1", 
  "mainWorse" = "#cc0243", "diffWorse"="#920130") # #f68f98 #a51523

#define dimension labels
dimLabs <- ecoChart %>%
  count(dimension) %>%
  select(-n) %>%
  mutate(pos = seq(50, 850, 100),
    dimLab.ch = c("climate\nchange", "ocean\nacidification", "chemical\npollution",
	"nutrient\npollution", "air\npollution", "freshwater\ndisruption", "land\nconversion", "biodiversity\nbreakdown",
	"ozone layer\ndepletion"),
    dimLabs = factor(dimLab.ch, levels=c("climate\nchange", "ocean\nacidification", "chemical\npollution",
	"nutrient\npollution", "air\npollution", "freshwater\ndisruption", "land\nconversion", "biodiversity\nbreakdown",
	"ozone layer\ndepletion"))) %>%
  select(-dimension, -dimLab.ch)

dimLabsY <- tibble(dimLabY = c("*", "0%", "100%", "200%", "300%"))

#define axis labels
axisLabs <- tibble(labs.ch = c("CO2\nconcentration", "radiative\nforcing", "aragonite\nsaturation",
    "hazardous\nchemicals", "nitrogen", "phosphorus", "interhemispheric\naerosols", "blue\nwater", "green\nwater",
    "forest\narea", "extinction\nrate", "hanpp", "stratospheric\nozone"),
  labs = factor(labs.ch, levels = c("CO2\nconcentration", "radiative\nforcing", "aragonite\nsaturation",
    "hazardous\nchemicals", "nitrogen", "phosphorus", "interhemispheric\naerosols", "blue\nwater", "green\nwater",
    "forest\narea", "extinction\nrate", "hanpp", "stratospheric\nozone"))) %>% select(-labs.ch)

#get top column positions
colLabs <- eco2 %>%
  select(dimension, indicator, ratioStart, ratioEnd) %>%
  rowwise() %>%
  mutate(topVal = max(ratioStart, ratioEnd)) %>%
  ungroup() %>%
  cbind(axisLabs) %>%
  select(-ratioStart, -ratioEnd) %>% tibble()

#get xpos values for labels
pos <- ecoChart %>%
  select(dimension, indicator, pos) %>%
  slice_head(n=13)

colLabs1 <- left_join(colLabs, pos, by=c("dimension", "indicator")) %>%
  mutate(topVal = ifelse(indicator %in% c("omega_a", "interhemAOD", "totalOzone"), 1.35,
      ifelse(indicator %in% c("chemicalsMt_Hzd", "extinction1900"), 4.4, topVal + 0.25)),
    constVal = 4.5,
    myCol = ifelse(indicator %in% c("chemicalsMt_Hzd", "extinction1900"), "white", "black")) 

rm(colLabs, pos)

#---------------------------------------------------------------------------------------------------------
#plot!
#---------------------------------------------------------------------------------------------------------
ecoBars <- ggplot(ecoChart, aes(x=pos, y=value, width = weight-2, group=changeBar)) +
  geom_bar(aes(fill=changeBar), stat="identity") + 
  geom_vline(xintercept = seq(100,800,100), colour = "#ffffff", lwd=1.5) +
  geom_vline(xintercept = seq(100,800,100), colour = "#f0f0f0", lwd=0.5) +
  geom_hline(yintercept = 1, colour="#227443", lwd=2.5) +
  annotate("text", x = colLabs1$pos, y = colLabs1$constVal, label = colLabs1$labs, 
    colour = colLabs1$myCol, vjust=1, size = 1.58) +
  scale_fill_manual(name = "Change in\necological overshoot,\n2021 versus 2000",
    limits=c("mainBetter", "mainWorse", "diffBetter", "diffWorse"),
    labels=c("Reference1", "Reference2", "Improving from 2000", "Worsening from 2000"), values=colours) +
  coord_cartesian(ylim=c(0,4.9)) +
  scale_y_continuous(labels = dimLabsY$dimLabY, breaks=seq(0,4,1), expand = c(0,0.01)) +
  scale_x_continuous(labels = dimLabs$dimLabs, breaks=dimLabs$pos, expand=c(0,0.05)) + #ecoChart$indicator
  labs(x=element_blank(), y="Ecological overshoot") +
  theme_classic() +
  theme(
    plot.margin = margin(0.1, 0.1, 0.5, 0.1, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    axis.title = element_text(size=8),
    axis.text = element_text(size=6),
    axis.line = element_blank(),
    axis.ticks.x = element_blank())

rm(ecoChart, colours, axisLabs, colLabs1, dimLabs, dimLabsY)


#-----------------------------------------------------------------------------------------------
# merge charts into one figure and write baguette data to file
#-----------------------------------------------------------------------------------------------

ggarrange(ecoBars, socBars, ncol=1) #, labels=c("a", "b"), font.label=list(size=8)

#ggsave("./figures/7_baguette_dark_v4.pdf", width = 183, height = 100, units = "mm", device = "pdf")

rm(ecoBars, socBars)
#-----------------------------------------------------------------------------------------------
ecoSoc <- rbind(eco2,soc2) %>%
  select(-diffBetter, -diffWorse, -mainBetter, -mainWorse, -weight) %>%
  mutate(ratioStart = ifelse(domain=="ecological", ratioStart-1, ratioStart),
    ratioEnd = ifelse(domain=="ecological", ratioEnd-1, ratioEnd))

#write_csv(ecoSoc, "./myData/7_20240614_baguetteData.csv")

rm(eco2, soc2, ecoSoc)
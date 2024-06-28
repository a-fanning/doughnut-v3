#This script estimates statistical linear trend lines from observed values

#read in data file if needed.
#myData12 <- read_csv("./myData/5_20240614_doughnutDat-boundariesRatios.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice")))

#read in non-interpolated data file to estimate statistical trends
myData1 <- read_csv("./myData/1_20240614_doughnutv3-data.csv")
#-------------------------------------------------------------------------------------------------
#add 2000-2017 hazardous chemicals data to myData1
hzdChem <- myData12 %>%
  filter(date <= 2021, indicator == "chemicalsMt_Hzd") %>% 
  select(domain, dimension, date, indicator, value) %>%
  rowwise() %>%
  mutate(value = ifelse(date > 2017, NA, value)) %>%
  ungroup()

myData2 <- rbind(myData1, hzdChem) %>%
  arrange(domain, dimension)

rm(hzdChem, myData1)
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#			join ratio values to myData1 non-interpolated data  
#-------------------------------------------------------------------------------------------------
#get ratios
ratios <- myData12 %>%
  filter(date <= 2021) %>%
  select(domain, dimension, date, indicator, indicatorCode, ratio) %>%
  rowwise() %>%
  mutate(ratio = ifelse(domain == "social", 1 - ratio, ratio)) %>%
  ungroup()


lmData <- left_join(myData2, ratios, by=c("domain", "dimension", "date", "indicator")) %>%
  relocate(value, .before=ratio)

rm(ratios, myData2)

#---------------------------------------------------------------------------------------
#Drop NAs and transform date so 0 is 2000 (for a meaningful intercept coefficient) 
lmData1 <- lmData %>%
  drop_na() %>%
  rowwise() %>%
  mutate(t = date - 2000) %>%
  ungroup()  

rm(lmData)

#---------------------------------------------------------------------------------------
#little function to fit lm models by year
mod_fit <- function(data){
  lm(ratio ~ t, data=data)
}

#estimate lm models by group and extract coefficients and fitted values
mods <- lmData1 %>%
  group_nest(domain, dimension, indicator, indicatorCode) %>%
  mutate(model = map(data, mod_fit), 
    glance = map(model, broom::glance),
    tidy = map(model, broom::tidy),
    augment = map(model, broom::augment))

#---------------------------------------------------------------------------------------
#get fitted values to visualise
mods_aug <- mods %>%
  unnest(augment) %>%
  select(domain, dimension, indicator, indicatorCode, ratio, t, .fitted)
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
#			FIGURE 3 - social line charts
#---------------------------------------------------------------------------------------
#look at social data and fitted values
socData <- mods_aug %>%
  filter(domain == "social") %>%
  rowwise() %>%
  mutate(date = t + 2000) %>%
  ungroup() %>%
  mutate(dimension = factor(dimension, levels = c("food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice")),
    indicator = factor(indicator, levels=c("foodInsecurity", "undernourishment", "noUniversalHealth", "under5death", "adultLiteracy",
      "secondarySchool", "societalPoverty", "youthNEET", "drinkingH2O", "sanitation", "energyAccess", "energyIndoor", "internet", "publicTrans",
      "urbanSlums", "genderGapIndex", "lifeSatisfaction", "PalmaOver2_share", "voiceAccount", "controlCorruption", "homicideOver5")))

#set 2-colour scheme by dimension 
cols <- c("foodInsecurity"="#262626", "undernourishment"="#7f7f7f", "noUniversalHealth"="#262626", "under5death"="#7f7f7f",
    "adultLiteracy"="#7f7f7f", "secondarySchool"="#262626", "societalPoverty"="#262626", "youthNEET"="#7f7f7f", "drinkingH2O"="#7f7f7f", 
    "sanitation"="#262626", "energyAccess"="#7f7f7f", "energyIndoor"="#262626", "internet"="#262626", "publicTrans"="#7f7f7f", 
    "urbanSlums"="#262626", "genderGapIndex"="#262626", "lifeSatisfaction"="#7f7f7f", "PalmaOver2_share"="#262626", 
    "voiceAccount"="#262626", "controlCorruption"="#262626", "homicideOver5"="#7f7f7f")


#set indicator labels for annotation
indicatorLabs <- socData %>%
  count(dimension, indicator) %>%
  select(-n) %>%
  mutate(indLabel.ch = c("food insecurity", "undernourished", "lack of health services", "under-5 mortality", "illiteracy rate",
    "incomplete secondary school", "societal poverty", "youth NEET", "unsafe drinking water", "unsafe sanitation", "lack of electricity",
    "lack of clean indoor fuels", "lack of internet", "lack of public transport", "slums or inadequate housing", "gender inequality", 
    "dissatisfied with life", "income inequality", "lack of voice & accountability", "perception of corruption", "homicide rate"))

#get regression data for annotation
eqnData <- mods %>%
  filter(domain == "social") %>%
  unnest(glance, tidy) %>%
  select(dimension, indicator, adjR2 = adj.r.squared, nobs, term, estimate) %>%
  spread(term, estimate) %>%
  rename(intercept = `(Intercept)`, slope = t) %>%
  mutate(dimension = factor(dimension, levels = c("food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice")),
    indicator = factor(indicator, levels=c("foodInsecurity", "undernourishment", "noUniversalHealth", "under5death", "adultLiteracy",
      "secondarySchool", "societalPoverty", "youthNEET", "drinkingH2O", "sanitation", "energyAccess", "energyIndoor", "internet", "publicTrans",
      "urbanSlums", "genderGapIndex", "lifeSatisfaction", "PalmaOver2_share", "voiceAccount", "controlCorruption", "homicideOver5")))

#join equation data to labels
indicatorLabs1 <- left_join(indicatorLabs, eqnData, by=c("dimension", "indicator")) %>%
  mutate(intercept = intercept*100, slope = slope*100) %>%
  rowwise() %>%
  mutate(slope = ifelse(indicator == "publicTrans", 0, slope),
    formula = sprintf("italic(y) == %.0f %+.1f * italic(x) * ',' ~~ italic(r)^2 ~ '=' ~ %.2f", 
      round(intercept, 0), round(slope, 1), round(adjR2, 2))) %>%
  ungroup()

pos <- tibble(
  x1 = c(2000, 2012, 2000, 2012, 2012, 2000, 2000, 2012, 2012, 2000, 2012, 2000, 2000, 2012, 2000, 2000, 2012, 2000, 2000, 2000, 2012), 
  x2 = c(2000, 2012, 2000, 2012, 2012, 2000, 2000, 2012, 2012, 2000, 2012, 2000, 2000, 2012, 2000, 2000, 2012, 2000, 2000, 2000, 2012), 
  y1 = c(1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05, 1.05),
  y2 = c(1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13, 1.13))

indicatorLabs2 <- cbind(indicatorLabs1, pos)
#--------------------------------------------------------------------------------------------------
#plot!

ggplot(data = socData) +
  geom_hline(yintercept = 0, col = "#227443", lwd=1.5) +
  geom_line(aes(x=date, y=.fitted, col=indicator), lwd=0.5) + 
  geom_point(aes(x=date, y=ratio, col=indicator), size=0.6) + 
  coord_cartesian(ylim=c(1.15, 0)) +
  scale_y_reverse(labels=scales::percent, breaks=seq(0,1,0.25)) + 
  scale_colour_manual(values=cols) +
  facet_wrap(~dimension, ncol=3) +
  geom_text(data = indicatorLabs2, aes(x = x1, y = y1, label = indLabel.ch, col=indicator), hjust=0, size=2) +
  geom_text(data = indicatorLabs2, aes(x = x2, y = y2, label = formula, col=indicator), parse=TRUE, hjust=0, size=2) +
  labs(x = "Year", y = "Social shortfall (0% = social foundation)") +
  theme_chart_SMALLM +
  theme(legend.position="none",
    panel.grid=element_blank())

#save Figure 3 to file
#ggsave("./figures/8_socialIndicators-dataAndTrends-greys.png", width = 180, height = 184, units = "mm", device="png")

rm(cols, indicatorLabs, eqnData, indicatorLabs1, pos, indicatorLabs2)

#---------------------------------------------------------------------------------------
#			FIGURE 4 - ecological line charts
#---------------------------------------------------------------------------------------
#Look at ecological values and fitted values
ecoData <- mods_aug %>%
  filter(domain == "ecological", !indicator %in% c("chemicalsMt", "EUshare_hzdHealth")) %>%
  rowwise() %>%
  mutate(date = t + 2000) %>%
  ungroup() %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion")),
    indicator = factor(indicator, levels = c("co2_ppm", "erf_wm2", "omega_a", "chemicalsMt_Hzd", "phosphorusMt", "nitrogenMt",
      "interhemAOD", "blueDev", "soilDev", "forestAreaMKM2", "extinction1900", "hanppGtC", "totalOzone")))
  
#set 2-colour scheme by dimension 
#cols <- c("co2_ppm"="#f6914c", "erf_wm2"="#9f1523", "omega_a"="#6eaf46", "chemicalsMt_Hzd"="#9f1523", "phosphorusMt"="#9f1523", 
 # "nitrogenMt"="#f6914c", "blueDev"="#9f1523", "soilDev"="#f6914c", "forestAreaMKM2"="#9f1523", "extinction1900"="#9f1523",
  #"hanppGtC"="#f6914c", "interhemAOD"="#6eaf46", "totalOzone"="#6eaf46")

cols <- c("co2_ppm"="#7f7f7f", "erf_wm2"="#262626", "omega_a"="#262626", "chemicalsMt_Hzd"="#262626", "phosphorusMt"="#262626", 
  "nitrogenMt"="#7f7f7f", "interhemAOD"="#262626", "blueDev"="#262626", "soilDev"="#7f7f7f", "forestAreaMKM2"="#262626",
  "extinction1900"="#262626", "hanppGtC"="#7f7f7f", "totalOzone"="#262626")


#set y-axis labels
dimLabsY <- tibble(dimLabY = c("*", "0%", "100%", "200%", "300%"))

#set indicator labels for annotation
indicatorLabs <- ecoData %>%
  count(dimension, indicator) %>%
  select(-n) %>%
  mutate(indLabel.ch = c("CO2 concentration", "radiative forcing", "aragonite saturation", "hazardous chemicals", "phosphorus",
    "nitrogen", "interhemispheric aerosols", "blue water", "green water", "forest area", "extinction rate", "hanpp",
    "stratospheric ozone"))

#get regression data for annotation
eqnData <- mods %>%
  filter(domain == "ecological") %>%
  unnest(glance, tidy) %>%
  select(dimension, indicator, adjR2 = adj.r.squared, nobs, term, estimate) %>%
  spread(term, estimate) %>%
  rename(intercept = `(Intercept)`, slope = t) %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion")),
    indicator = factor(indicator, levels = c("co2_ppm", "erf_wm2", "omega_a", "chemicalsMt_Hzd", "phosphorusMt", "nitrogenMt",
      "interhemAOD", "blueDev", "soilDev", "forestAreaMKM2", "extinction1900", "hanppGtC", "totalOzone")))

#join equation data and indicator labels
indicatorLabs1 <- left_join(indicatorLabs, eqnData, by=c("dimension", "indicator")) %>%
  mutate(intercept = (intercept-1)*100, slope = slope*100) %>%
  rowwise() %>%
  mutate(adjR2 = ifelse(indicator %in% c("extinction1900", "interhemAOD"),0, adjR2),
    formula = sprintf("italic(y) == %.1f %+.1f * italic(x) * ',' ~~ italic(r)^2 ~ '=' ~ %.2f", 
      round(intercept, 0), round(slope, 1), round(adjR2, 3))) %>%
  ungroup()

#set label positions for indicators (x1, y1) and equations (x2, y2)
pos <- tibble(
  x1 = c(2011, 2000, 2000, 2000, 2000, 2011, 2000, 2011, 2000, 2000, 2011, 2000, 2000), 
  x2 = c(2011, 2000, 2000, 2000, 2000, 2011, 2000, 2011, 2000, 2000, 2011, 2000, 2000), 
  y1 = c(4.2, 4.2, 4.2, 4.2, 4.2, 4.2, 4.2, 4.2, 4.2, 4.2, 4.2, 4.2, 4.2), 
  y2 = c(3.9, 3.9, 3.9, 3.9, 3.9, 3.9, 3.9, 3.9, 3.9, 3.9, 3.9, 3.9, 3.9))

#join coordinates to labels 
indicatorLabs2 <- cbind(indicatorLabs1, pos)

#--------------------------------------------------------------------------------------------------
#plot!

ggplot(data = ecoData) +
  geom_hline(yintercept = 1, col = "#227443", lwd=1.5) +
  geom_line(aes(x=date, y=.fitted, col=indicator), lwd=0.5) + 
  geom_point(aes(x=date, y=ratio, col=indicator), size=0.6) + 
  scale_y_continuous(labels=dimLabsY$dimLabY, breaks=seq(0,4,1)) +
  scale_colour_manual(values=cols) +
  coord_cartesian(ylim=c(0,4.2)) +
  facet_wrap(~dimension, ncol=3) +
  geom_text(data = indicatorLabs2, aes(x = x1, y = y1, label = indLabel.ch, col=indicator), hjust=0, size=2) +
  geom_text(data = indicatorLabs2, aes(x = x2, y = y2, label = formula, col=indicator), parse=TRUE, hjust=0, size=2) +
  labs(x = "Year", y = "Ecological overshoot (0% = planetary boundary)") +
  theme_chart_SMALLM +
  theme(legend.position="none",
    panel.grid=element_blank())

#save Figure 4 to file
#ggsave("./figures/8_ecologicalIndicators-dataAndTrends-greys_v1.png", width = 180, height = 140, units = "mm", device="png")

rm(cols, indicatorLabs, eqnData, indicatorLabs1, pos, indicatorLabs2, dimLabsY)
#---------------------------------------------------------------------------------------
##write time series and fitted values to file

obsFit <- rbind(ecoData, socData) %>%
  rowwise() %>%
  mutate(ratio = ifelse(domain == "ecological", ratio-1, ratio),
    .fitted = ifelse(domain == "ecological", .fitted-1, .fitted)) %>%
  ungroup() %>% 
  select(-t) %>%
  relocate(date, .before=ratio) %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "freshwater disruption", "land conversion", "biodiversity breakdown", "air pollution",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice"))) %>%
  arrange(domain, dimension)


#write_csv(obsFit, "./myData/8_20240615_ratios-observedAndFitted.csv")

rm(mods_aug, obsFit, socData, ecoData)
#---------------------------------------------------------------------------------------
#get regression results
mods_coefs <- mods %>%
  unnest(glance, tidy) %>%
  select(domain, dimension, indicator, indicatorCode, adjr2 = adj.r.squared, lmPval = p.value, term, 
    estimate, stdError = std.error, coefPval = p.value1) %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "freshwater disruption", "land conversion", "biodiversity breakdown", "air pollution",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice"))) %>%
  arrange(domain, dimension)

#write regression results to file
write_csv(mods_coefs, "./myData/8_20240615_lmModelCoefficients.csv")

rm(mods, mods_coefs, mod_fit, lmData1)



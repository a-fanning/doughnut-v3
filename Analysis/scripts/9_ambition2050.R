# This script calculates linear rates of change needed to eliminate social shortfall and ecological overshoot
# for each indicator by 2050, based on current levels.

#read in cleaned data file
myData12 <- read_csv("./myData/5_20240614_doughnutDat-boundariesRatios.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice"))) 

ecoSoc <- read_csv("./myData/7_20240614_baguetteData.csv")

lmData <- read_csv("./myData/8_20240615_lmModelCoefficients.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice"))) %>%
  arrange(domain, dimension) %>%
  filter(term == "t") %>%
  select(-lmPval, -term)

#-------------------------------------------------------------------------------------------------------------------
#get 2020-21 overshoot/shortfall values and calculate linear rates of change needed to live within the Doughnut
# by 2030 for the social foundation and 2050 for the ecological ceiling.

ecoSoc1 <- ecoSoc %>%
  add_column(endYear = 0) %>% 
  rowwise() %>%
  mutate(t = ifelse(domain == "ecological", 2050-2021, 2030-2021),
    slopeD = (endYear - ratioEnd)/t) %>%
  ungroup() %>%
  select(-endYear)

rm(ecoSoc)

#add estimated trends based on observed ratios to ecoSoc to have table data on file
ecoSoc2 <- left_join(ecoSoc1, lmData, by=c("domain", "dimension", "indicator", "indicatorCode")) %>%
  rename(histEstimate = estimate) %>%
  relocate(adjr2, .after=coefPval)

rm(ecoSoc1, lmData)

#calculate/round scale of ambition needed
ecoSoc3 <- ecoSoc2 %>%
  mutate(valueStart = round(valueStart, 3),
    valueEnd = round(valueEnd, 3),
    ratioStart = round(ratioStart, 3),
    ratioEnd = round(ratioEnd, 3),
    diff = round(diff, 3),
    slopeD = round(slopeD, 3),
    histEstimate = round(histEstimate, 3),
    adjr2 = round(adjr2, 4)) %>%
  rowwise()

rm(ecoSoc2)
#------------------------------------------------------------------------------------------
#write this data to file as input to Tables 1 and 2, and do final clean up in Excel.
write_csv(ecoSoc3, "./myData/9_20240624_Doughnut-TablesData.csv")

rm(ecoSoc3)
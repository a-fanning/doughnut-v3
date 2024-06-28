#This script outputs a near-final Supplementary Data spreadsheet.
#Tab 1: analysis data all years (with indicator units)
#Tab 2: historical trend full regression estimates
#-----------------------------------------------------------------------------------------------------------------------
#read in cleaned data file
myData12 <- read_csv("./myData/5_20240614_doughnutDat-boundariesRatios.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice")))

#read in regression data
lmData <- read_csv("./myData/8_20240615_lmModelCoefficients.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice"))) %>%
  arrange(domain, dimension)
#-----------------------------------------------------------------------------------------------------------------------
#prep 2000-2021 supplementary dataset with units

allYears <- myData12 %>%
  select(-place, -placeCode, -scenario, -indicatorCode, -BAU_low, -BAU_high, -ratio_low, -ratio_high) %>%
  filter(date <= 2021, !indicator %in% c("chemicalsMt", "EUshare_hzdHealth"))
  
#unit tibble
units <- allYears %>% 
  count(domain, dimension, indicator) %>%
  select(-n) %>%
  mutate(unit = c("ppm CO2", "Watt per m2", "omega aragonite", "million tonnes", "million tonnes", "million tonnes",
    "interhemispheric AOD", "percent", "percent", "million km2", "extinctions per million species-years", "billion tonnes C", 
    "Dobson units", "percent", "percent", "percent", "percent", "percent", "percent", "percent", "percent", "percent", 
    "percent", "percent", "percent", "percent", "percent", "percent", "0-100 scale", "Not available", "percent", "percent",
    "percent", "percent", "percent")) 

#merge units with data
allYears1 <- left_join(allYears, units, by=c("domain", "dimension", "indicator")) %>%
  relocate(unit, .before=ratio) %>%
  mutate(value = round(value,digits=2),
    ratio = round(ratio, digits=2)) %>%
  rowwise() %>%
  mutate(ratio = ifelse(indicator == "racialEquality", NA, ratio)) %>%
  ungroup()

#write to file
#write_csv(allYears1, "./myData/11_20240624_doughnutData_full.csv")

#-----------------------------------------------------------------------------------------------------------------------
#prep regression results supplementary data
regData <- lmData %>%
  select(domain, dimension, indicator, `adj-R2`=adjr2, `p-value_regression` = lmPval, term, coefficient = estimate, 
    SE = stdError, `p-value_coefficient` = coefPval) %>%
  relocate(`adj-R2`, `p-value_regression`, .after=`p-value_coefficient`) %>%
  mutate(coefficient = round(coefficient, digits=3), 
    `adj-R2` = round(`adj-R2`, digits = 3))

# write to file
#write_csv(regData, "./myData/11_20240624_doughnutData_regressions.csv")
 

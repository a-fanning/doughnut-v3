# This script adds social and planetary boundaries to the database

#read in latest dataset, and rename biodiversity loss to biodiversity breakdown
#myData3 <- read_csv("./myData/3_20240614_historicalAndBAU-ecologicalAndSocial.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice")))
#---------------------------------------------------------------------------------------
#prep myData for boundaries

#drop racial inequality missing indicator (to put back in later!)
myData4 <- myData3 %>%
  filter(indicator != "racialInequality") %>%
  relocate(date, .after = indicator)

rm(myData3)

#---------------------------------------------------------------------------------------
#calculate shares of chemicals hazardous to health
chem <- myData4 %>%
  filter(indicator %in% c("chemicalsMt", "EUshare_hzdHealth")) %>%
  select(-BAU_low, -BAU_high) %>%
  spread(indicator, value) %>%
  mutate(EUshare_hzdHealth = na.approx(EUshare_hzdHealth, na.rm=F, rule=2)) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(value = EUshare_hzdHealth/100*chemicalsMt) %>%
  ungroup() %>%
  select(-chemicalsMt, -EUshare_hzdHealth) %>%
  add_column(indicator = "chemicalsMt_Hzd", BAU_low = NA, BAU_high = NA)  %>%
  relocate(indicator, .before=date) %>%
  arrange(date)

myData6 <- rbind(myData4, chem) %>%
  arrange(domain, dimension, indicator, date)

rm(myData4, chem)
#---------------------------------------------------------------------------------------
#drop duplicate blueDev values mistakenly created earlier 
#and drop 2031 ocean acidification forecast

blueDev06up <- myData6 %>%
  filter(indicator == "blueDev", date >= 2006, scenario != "historical")

blueDev05dn <- myData6 %>%
  filter(indicator == "blueDev", date <= 2005)

blueDev <- rbind(blueDev05dn, blueDev06up)

rm(blueDev05dn, blueDev06up)

myData7 <- myData6 %>%
  filter(indicator != "blueDev") %>%
  rbind(blueDev) %>%
  arrange(dimension) %>%
  filter(date <= 2030)
   
rm(myData6)
 
#--------------------------------------------------------------------------------------------------
#create boundaries tibble
boundaries <- myData7 %>%
  count(domain, dimension, indicator) %>%
  filter(!indicator %in% c("chemicalsMt", "EUshare_hzdHealth")) %>%
  select(-n)

boundVals <- tibble(boundary = c(350, 1, 2.75, 60, 62, 6.2, 0.1, 10.2, 11.1, 47.9, 10, 5.59, 276,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

boundaries1 <- cbind(boundaries, boundVals)

#join boundaries1 to myData
myData8 <- left_join(myData7, boundaries1, by=c("domain", "dimension", "indicator"))

rm(myData7, boundaries, boundVals, boundaries1)

#--------------------------------------------------------------------------------------------------
#			WRITE TO FILE
#--------------------------------------------------------------------------------------------------

write_csv(myData8, "./myData/4_20240614_doughnutData-boundaries.csv")




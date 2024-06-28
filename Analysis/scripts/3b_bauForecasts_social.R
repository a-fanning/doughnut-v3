# This script estimates best-fit statistical forecasts for each social indicator out to 2030

#read in latest dataset
#myData1 <- read_csv("./myData/1_20240614_doughnutv3-data.csv")
#bauData <- read_csv("./myData/3_20240614_BAU-ecological.csv")
#---------------------------------------------------------------------------------------
#Drop NAs
myData_dropNA <- myData1 %>%
	drop_na()

#Convert myData to time series tibble, or tsibble
myDataTS <- myData_dropNA %>%
	as_tsibble(key = c(domain, dimension, indicator), index = date)

#Convert BAU data to tsibble
bauData <- bauData %>%
	as_tsibble(key = c(domain, dimension, indicator), index = date)

rm(myData_dropNA)

#------------------------------------------------------------------------
#		food
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "undernourishment")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Food", y = "Undernourishment (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#CHANGE INIT IN CHOOSEMODEL() TO 5
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "foodInsecurity")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Food", y = "Food insecurity(%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		health
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "under5death")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Health", y = "Population in countries with under-5 mortality > 25 per 1,000 (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "noUniversalHealth")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Health", y = "Lack of universal health coverage (0-100)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		education
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "adultLiteracy")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 10)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Education", y = "Adult illiteracy (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "secondarySchool")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Education", y = "No upper secondary completion (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		income & work
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "societalPoverty")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 11)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Income & work", y = "Population living below region-specific societal poverty lines (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "youthNEET")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Income & work", y = "Youth (15–24) not in education, employment, or training (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		water
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "drinkingH2O")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 10)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Water", y = "No access to safe drinking water (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "sanitation")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 10)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Water", y = "No access to safe sanitation (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		energy
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "energyAccess")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Energy", y = "Lack of access to energy (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "energyIndoor")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Energy", y = "Lack of access to clean fuels and technologies for cooking, heating and lighting (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#	connectivity (note lack of data series to project public transport)
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "internet")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Connectivity", y = "Non-internet users per 100 inhabitants") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		housing
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "urbanSlums")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 10)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Housing", y = "Urban population in inadequate/informal housing (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		equality (no data for racial inequality)
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "genderGapIndex")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Equality", y = "Gender gap index (0–100 scale)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		social cohesion
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "lifeSatisfaction")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Social cohesion", y = "Population living in countries with average Cantril life satisfaction score of 5 or less out of 10 (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "PalmaOver2_share")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 10)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Social cohesion", y = "Population share of countries with Palma ratio over 2 (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)


#------------------------------------------------------------------------
#		political voice
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "voiceAccount")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Political voice", y = "Population scoring 50 or less out of 100 on Voice & Acc. Index (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		peace & justice
#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "controlCorruption")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Peace & justice", y = "Population with 50 or less out of 100 on Control of Corruption Index (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#get indicator 
dat <- myDataTS %>%
	filter(indicator == "homicideOver5")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 0, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Peace & justice", y = "Population in countries with homicide rate > 5 per 100,000 (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#--------------------------------------------------------------------
#		write full BAU data to file
#--------------------------------------------------------------------

#write_csv(bauData, "./myData/3_20240614_BAU-ecologicalAndSocial.csv")

#read in bauData as normal tibble rather than ts
#bauData <- read_csv("./myData/3_20240614_BAU-ecologicalAndSocial.csv")

#--------------------------------------------------------------------
# add scenario tags, carry 'historical' values up to 2021 and join with myData
#--------------------------------------------------------------------
#add historical scenario tag
#and remove "publicTrans" indicator with only one observation (to add later!)

myData2 <- myData1 %>%
  filter(indicator != "publicTrans") %>%
  add_column(scenario = "historical") %>%
  relocate(scenario)

publicTrans <- myData1 %>%
  filter(indicator == "publicTrans")  %>%
  add_column(scenario = "historical") %>%
  relocate(scenario)

rm(myData1)

#get values after 2013 and drop nas
myData2013up <- myData2 %>%
  filter(date >= 2014) %>%
  drop_na(value)

#get values before 2013 and keep nas
myData2013down <- myData2 %>%
  filter(date <= 2013)

#add bau scenario tag
bauData1 <- bauData %>%
  rename(BAU_med = value) %>%
  add_column(scenario = "bau") %>%
  relocate(scenario)

rm(bauData)

#join historical and bau data
myData2013up1 <- full_join(myData2013up, bauData1, by=c("scenario", "domain", "dimension", "indicator", "date"))

#use bau projections for indicators lacking historical data to 2021
myData2013up2 <- myData2013up1 %>%
  rowwise() %>%
  mutate(value = ifelse(date <= 2021 && is.na(value), BAU_med, value)) %>%
  ungroup() %>% 
  filter(date <= 2021) %>%
  select(-BAU_med, -BAU_low, -BAU_high)
  
#join with pre-2014 historical values
myDataHist <- rbind(myData2013down, myData2013up2)

rm(myData2013down, myData2013up, myData2013up1, myData2013up2)

#join with post-2021 bau data
bauData2 <- bauData1 %>%
	filter(date >= 2022) %>%
	rename(value = BAU_med)

rm(bauData1, myData2)

myData3 <- full_join(myDataHist, bauData2, by = c("scenario", "domain", "dimension", "indicator", "date",
  "value")) %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice"))) %>%
  arrange(domain, dimension, indicator) %>%
  rowwise() %>%
  mutate(value = ifelse(indicator == "blueDev" && date > 2005, 18.2, value)) %>%
  ungroup()

rm(myDataHist, bauData2, myDataTS)

#add public transport series
publicTransUp <- publicTrans %>%
  filter(date >= 2020) %>%
  add_column(BAU_low = as.numeric(NA), BAU_high = as.numeric(NA))

publicTransDn <- publicTrans %>%
  filter(date <= 2019) %>%
  add_column(BAU_low = as.numeric(NA), BAU_high = as.numeric(NA))

bauTrans <- tibble(scenario = "bau", domain = "social", dimension = "connectivity", date = 2022:2030, indicator = "publicTrans",
  value = as.numeric(NA), BAU_low = as.numeric(NA), BAU_high = as.numeric(NA))

transUp <- rbind(publicTransUp, bauTrans) %>%
  mutate(value = (na.locf(value)))

publicTrans1 <- rbind(publicTransDn, transUp)

#bind public transport series to myData3

myData3a <- rbind(myData3, publicTrans1) %>% 
  arrange(domain, dimension, indicator)


#--------------------------------------------------------------------
#		write historical and BAU data to file
#--------------------------------------------------------------------

write_csv(myData3a, "./myData/3_20240614_historicalAndBAU-ecologicalAndSocial.csv")

#--------------------------------------------------------------------
rm(myData3, myData3a, publicTrans, publicTrans1, publicTransUp, publicTransDn, transUp)


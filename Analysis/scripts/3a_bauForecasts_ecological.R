# This script estimates best-fit statistical forecasts for each ecological indicator out to 2030

#read in latest dataset, if needed
#myData1 <- read_csv("./myData/1_20240614_doughnutv3-data.csv")

#---------------------------------------------------------------------------------------
#Drop NAs and convert to time series tibble, or tsibble

myData_dropNA <- myData1 %>%
	drop_na()

myDataTS <- myData_dropNA %>%
	as_tsibble(key = c(domain, dimension, indicator), index = date)

rm(myData_dropNA)

#------------------------------------------------------------------------
#		climate change
#------------------------------------------------------------------------
#CO2 ppm
dat <- myDataTS %>%
	filter(indicator == "co2_ppm")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  geom_hline(yintercept = 450, col="red", lwd = 1) +
  geom_hline(yintercept = 350, col="green", lwd=1) +
  labs(title = "Climate change", y = "Atmospheric CO2 concentration (ppm)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

rm(dat, mod, datFcast)
#------------------------------------------------------------------------
#anthro effective radiative forcing
dat <- myDataTS %>%
	filter(indicator == "erf_wm2")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  geom_hline(yintercept = 1.5, col="red", lwd = 1) +
  geom_hline(yintercept = 0, col="green", lwd=1) +
  labs(title = "Climate change", y = "Anthropogenic radiative forcing (W/m2)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat2 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(dat1, dat2)


rm(dat, dat1, dat2, mod, datFcast)
#------------------------------------------------------------------------
#		ocean acidification
#------------------------------------------------------------------------

#get indicator
dat <- myDataTS %>%
	filter(indicator == "omega_a")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 10)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  geom_hline(yintercept = 2.41, col="red", lwd = 1) +
  geom_hline(yintercept = 2.75, col="green", lwd=1) +
  labs(title = "Ocean acidification", y = "aragonite saturation state") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		chemical pollution
#------------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "chemicalsMt")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 13)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 2.41, col="red", lwd = 1) +
  #geom_hline(yintercept = 2.75, col="green", lwd=1) +
  labs(title = "Chemical pollution", y = "total chemical, Mt") +
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
	filter(indicator == "EUshare_hzdHealth")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 2.41, col="red", lwd = 1) +
  #geom_hline(yintercept = 2.75, col="green", lwd=1) +
  labs(title = "Chemical pollution", y = "EU share of chemicals hazardous to health, %") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------
#		nutrient pollution
#------------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "nitrogenMt")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  geom_hline(yintercept = 82, col="red", lwd = 1) +
  geom_hline(yintercept = 62, col="green", lwd=1) +
  labs(title = "Nutrient pollution", y = "Nitrogen, Mt") +
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
	filter(indicator == "phosphorusMt")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  geom_hline(yintercept = 11.2, col="red", lwd = 1) +
  geom_hline(yintercept = 6.2, col="green", lwd=1) +
  labs(title = "Nutrient pollution", y = "Phosphorus, Mt") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#--------------------------------------------------------------------
#		air pollution
#--------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "interhemAOD")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  geom_hline(yintercept = 0.25, col="red", lwd = 1) + #261
  geom_hline(yintercept = 0.1, col="green", lwd=1) + #275.5
  labs(title = "Air pollution", y = "Interhemispheric difference in aerosol optical depth (AOD)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)


#--------------------------------------------------------------------
#		freshwater disruption #switch to init=4 in chooseModel() for blueDev
#--------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "blueDev")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat) 

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 25)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 6000, col="red", lwd = 1) +
  #geom_hline(yintercept = 4000, col="green", lwd=1) +
  labs(title = "Freshwater disruption", y = "Blue water, deviation (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals 
dat1 <- extractVars(datFcast) %>%
  mutate(value = 18.2)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#------------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "soilDev")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 16)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  #geom_hline(yintercept = 11.2, col="red", lwd = 1) +
  #geom_hline(yintercept = 5, col="green", lwd=1) +
  #scale_y_continuous(limits=c(2,8)) +
  labs(title = "Freshwater disruption", y = "Land area with root-zone soil moisture deviations\nbeyond local bounds (%)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#--------------------------------------------------------------------
#		land conversion
#--------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "forestAreaMKM2")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 10)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  geom_hline(yintercept = 34.5, col="red", lwd = 1) +
  geom_hline(yintercept = 47.9, col="green", lwd=1) +
  labs(title = "Land conversion", y = "Forest area, million km2") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#--------------------------------------------------------------------
#		biodiversity loss (#switch to init=5 in chooseModel for HANPP)
#--------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "hanppGtC")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat) 

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 10)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  geom_hline(yintercept = 20, col="red", lwd = 1) +
  geom_hline(yintercept = 10, col="green", lwd=1) +
  labs(title = "Biodiversity loss", y = "% HANPP") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#--------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "extinction1900")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  geom_hline(yintercept = 100, col="red", lwd = 1) +
  geom_hline(yintercept = 10, col="green", lwd=1) +
  labs(title = "Biodiversity loss", y = "Extinctions per million species-years (E/MSY)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#--------------------------------------------------------------------
#		ozone depletion
#--------------------------------------------------------------------

#get indicator 
dat <- myDataTS %>%
	filter(indicator == "totalOzone")

#choose best-fitting ETS and ARIMA
mod <- chooseModel(dat)

#generate forecasts with best-fitting model
datFcast <- fcast(dat, mod, 9)

#plot basic forecasts
datFcast %>%
  autoplot(dat %>% filter(date >= 2000), lwd=1) +
  geom_hline(yintercept = 261, col="red", lwd = 1) + #261
  geom_hline(yintercept = 276, col="green", lwd=1) + #275.5
  labs(title = "Ozone depletion", y = "Global average stratospheric ozone (Dobson units)") +
  theme_chart

#extract forecast estimates with 66% confidence intervals
dat1 <- extractVars(datFcast)

#join to BAUdata
bauData <- bind_rows(bauData, dat1)

#clean up
rm(dat, dat1, mod, datFcast)

#--------------------------------------------------------------------
#		write ecological BAU to file
#--------------------------------------------------------------------

write_csv(bauData, "./myData/3_20240614_BAU-ecological.csv")


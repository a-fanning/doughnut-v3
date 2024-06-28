#This script converts the global data into JSON format for visualising

#Read in data file if needed.
#myData12 <- read_csv("./myData/5_20240614_doughnutDat-boundariesRatios.csv")

#-------------------------------------------------------------------------------------------------
#					PREP GLOBAL VARIABLES FOR NESTING
#-------------------------------------------------------------------------------------------------
placeList <- myData12 %>%
  count(place, placeCode) %>%
  select(-n)

ecoMinmax <- myData12 %>%
  filter(domain == "ecological") %>%
  group_by(place, placeCode) %>%
  summarise(min = min(ratio, na.rm=T),
	max = max(ratio, na.rm=T)) %>%
  ungroup()

socMinmax <- myData12 %>%
  filter(domain == "social") %>%
  group_by(place, placeCode) %>%
  summarise(min = min(ratio, na.rm=T),
	max = max(ratio, na.rm=T)) %>%
  ungroup()

#-------------------------------------------------------------------------------------------------
#					PREP ECOLOGICAL VARIABLES FOR NESTING
#-------------------------------------------------------------------------------------------------
#climate change
clim1Range <- rangeRatio(myData12, indicatorCode == "clim1")
clim1Ratio1 <- spreadRatio(myData12, indicatorCode == "clim1") 
clim1Ratio2 <- blankRatio() 

#climate change
clim2Range <- rangeRatio(myData12, indicatorCode == "clim2")
clim2Ratio1 <- spreadRatio(myData12, indicatorCode == "clim2") 
clim2Ratio2 <- blankRatio() 

#ocean acidification
acidRange <- rangeRatio(myData12, indicatorCode == "acid")
acidRatio1 <- blankRatio() 
acidRatio2 <- spreadRatio(myData12, indicatorCode == "acid") 

#chemical pollution
chemRange <- rangeRatio(myData12, indicatorCode == "chem")
chemRatio1 <- blankRatio()
chemRatio2 <- spreadRatio(myData12, indicatorCode == "chem")

#nutrient pollution
fertPRange <- rangeRatio(myData12, indicatorCode == "fertP")
fertPRatio1 <- spreadRatio(myData12, indicatorCode == "fertP") 
fertPRatio2 <- blankRatio()

#nutrient pollution
fertNRange <- rangeRatio(myData12, indicatorCode == "fertN")
fertNRatio1 <- spreadRatio(myData12, indicatorCode == "fertN") 
fertNRatio2 <- blankRatio()

#air pollution
airPolRange <- rangeRatio(myData12, indicatorCode == "airPol")
airPolRatio1 <- blankRatio() 
airPolRatio2 <- spreadRatio(myData12, indicatorCode == "airPol") 

#freshwater use
H2ObluRange <- rangeRatio(myData12, indicatorCode == "H2Oblu")
H2ObluRatio1 <- spreadRatio(myData12, indicatorCode == "H2Oblu") 
H2ObluRatio2 <- blankRatio()

#freshwater use
H2OgrnRange <- rangeRatio(myData12, indicatorCode == "H2Ogrn")
H2OgrnRatio1 <- spreadRatio(myData12, indicatorCode == "H2Ogrn") 
H2OgrnRatio2 <- blankRatio()

#land conversion
landRange <- rangeRatio(myData12, indicatorCode == "land")
landRatio1 <- blankRatio() 
landRatio2 <- spreadRatio(myData12, indicatorCode == "land") 

#biodiversity breakdown
bio1Range <- rangeRatio(myData12, indicatorCode == "bio1")
bio1Ratio1 <- spreadRatio(myData12, indicatorCode == "bio1") 
bio1Ratio2 <- blankRatio() 

#biodiversity breakdown
bio2Range <- rangeRatio(myData12, indicatorCode == "bio2")
bio2Ratio1 <- spreadRatio(myData12, indicatorCode == "bio2") 
bio2Ratio2 <- blankRatio() 

#ozone depletion
O3depRange <- rangeRatio(myData12, indicatorCode == "O3dep")
O3depRatio1 <- blankRatio() 
O3depRatio2 <- spreadRatio(myData12, indicatorCode == "O3dep") 

#-------------------------------------------------------------------------------------------------
#					PREP SOCIAL VARIABLES FOR NESTING
#-------------------------------------------------------------------------------------------------
#food
food1Range <- rangeRatio(myData12, indicatorCode == "food1")
food1Ratio1 <- spreadRatio(myData12, indicatorCode == "food1") 
food1Ratio2 <- blankRatio()

#food
food2Range <- rangeRatio(myData12, indicatorCode == "food2")
food2Ratio1 <- spreadRatio(myData12, indicatorCode == "food2") 
food2Ratio2 <- blankRatio()

#health
hlth1Range <- rangeRatio(myData12, indicatorCode == "hlth1")
hlth1Ratio1 <- spreadRatio(myData12, indicatorCode == "hlth1") 
hlth1Ratio2 <- blankRatio()

#health
hlth2Range <- rangeRatio(myData12, indicatorCode == "hlth2")
hlth2Ratio1 <- spreadRatio(myData12, indicatorCode == "hlth2") 
hlth2Ratio2 <- blankRatio()

#education
edu1Range <- rangeRatio(myData12, indicatorCode == "edu1")
edu1Ratio1 <- spreadRatio(myData12, indicatorCode == "edu1") 
edu1Ratio2 <- blankRatio()

#education
edu2Range <- rangeRatio(myData12, indicatorCode == "edu2")
edu2Ratio1 <- spreadRatio(myData12, indicatorCode == "edu2") 
edu2Ratio2 <- blankRatio()

#income & work
inWk1Range <- rangeRatio(myData12, indicatorCode == "inWk1")
inWk1Ratio1 <- spreadRatio(myData12, indicatorCode == "inWk1") 
inWk1Ratio2 <- blankRatio()

#income & work
inWk2Range <- rangeRatio(myData12, indicatorCode == "inWk2")
inWk2Ratio1 <- spreadRatio(myData12, indicatorCode == "inWk2") 
inWk2Ratio2 <- blankRatio()

#water
watr1Range <- rangeRatio(myData12, indicatorCode == "watr1")
watr1Ratio1 <- spreadRatio(myData12, indicatorCode == "watr1") 
watr1Ratio2 <- blankRatio()
 
#water
watr2Range <- rangeRatio(myData12, indicatorCode == "watr2")
watr2Ratio1 <- spreadRatio(myData12, indicatorCode == "watr2") 
watr2Ratio2 <- blankRatio()

#energy
enrgy1Range <- rangeRatio(myData12, indicatorCode == "enrgy1")
enrgy1Ratio1 <- spreadRatio(myData12, indicatorCode == "enrgy1") 
enrgy1Ratio2 <- blankRatio()

#energy
enrgy2Range <- rangeRatio(myData12, indicatorCode == "enrgy2")
enrgy2Ratio1 <- spreadRatio(myData12, indicatorCode == "enrgy2") 
enrgy2Ratio2 <- blankRatio()

#connectivity
con1Range <- rangeRatio(myData12, indicatorCode == "con1")
con1Ratio1 <- spreadRatio(myData12, indicatorCode == "con1") 
con1Ratio2 <- blankRatio()

#connectivity
con2Range <- rangeRatio(myData12, indicatorCode == "con2")
con2Ratio1 <- spreadRatio(myData12, indicatorCode == "con2") 
con2Ratio2 <- blankRatio()

#housing
housRange <- rangeRatio(myData12, indicatorCode == "hous")
housRatio1 <- blankRatio() 
housRatio2 <- spreadRatio(myData12, indicatorCode == "hous")

#equality
eqDi1Range <- rangeRatio(myData12, indicatorCode == "eqDi1")
eqDi1Ratio1 <- spreadRatio(myData12, indicatorCode == "eqDi1") 
eqDi1Ratio2 <- blankRatio()

#equality
eqDi2Range <- rangeRatio(myData12, indicatorCode == "eqDi2")
eqDi2Ratio1 <- spreadRatio(myData12, indicatorCode == "eqDi2") 
eqDi2Ratio2 <- blankRatio()

#social cohesion
socCo1Range <- rangeRatio(myData12, indicatorCode == "socCo1")
socCo1Ratio1 <- spreadRatio(myData12, indicatorCode == "socCo1") 
socCo1Ratio2 <- blankRatio()

#social cohesion
socCo2Range <- rangeRatio(myData12, indicatorCode == "socCo2")
socCo2Ratio1 <- spreadRatio(myData12, indicatorCode == "socCo2") 
socCo2Ratio2 <- blankRatio()

#political voice
poVoRange <- rangeRatio(myData12, indicatorCode == "poVo")
poVoRatio1 <- blankRatio() 
poVoRatio2 <- spreadRatio(myData12, indicatorCode == "poVo")

#peace & justice
peJu1Range <- rangeRatio(myData12, indicatorCode == "peJu1")
peJu1Ratio1 <- spreadRatio(myData12, indicatorCode == "peJu1") 
peJu1Ratio2 <- blankRatio()

#peace & justice
peJu2Range <- rangeRatio(myData12, indicatorCode == "peJu2")
peJu2Ratio1 <- spreadRatio(myData12, indicatorCode == "peJu2") 
peJu2Ratio2 <- blankRatio()

#----------------------------------------------------------------------------------------------------------------
doughnutNest <- list()
myList <- list()


for(i in 1:length(placeList$place)){
  doughnutNest[[i]] <- list(
	place = placeList[[1]][i],
	placeCode = placeList[[2]][i],
	ecological = list(
		min = ecoMinmax[[3]][i],
		max = ecoMinmax[[4]][i],
		range = list(
			clim1 = list(clim1Range[[3]][i], clim1Range[[4]][i]),
			clim2 = list(clim2Range[[3]][i], clim2Range[[4]][i]),
			acid1 = list(acidRange[[3]][i], acidRange[[4]][i]),
			acid2 = list(acidRange[[3]][i], acidRange[[4]][i]),
			chem1 = list(chemRange[[3]][i], chemRange[[4]][i]),
			chem2 = list(chemRange[[3]][i], chemRange[[4]][i]),
			fertN = list(fertNRange[[3]][i], fertNRange[[4]][i]),
			fertP = list(fertPRange[[3]][i], fertPRange[[4]][i]),
			airPol1 = list(airPolRange[[3]][i], airPolRange[[4]][i]),
			airPol2 = list(airPolRange[[3]][i], airPolRange[[4]][i]),
			H2Oblu = list(H2ObluRange[[3]][i], H2ObluRange[[4]][i]),
			H2Ogrn = list(H2OgrnRange[[3]][i], H2OgrnRange[[4]][i]),
			land1 = list(landRange[[3]][i], landRange[[4]][i]),
			land2 = list(landRange[[3]][i], landRange[[4]][i]),
			bio1 = list(bio1Range[[3]][i], bio1Range[[4]][i]),
			bio2 = list(bio2Range[[3]][i], bio2Range[[4]][i]),
			O3dep1 = list(O3depRange[[3]][i], O3depRange[[4]][i]),
			O3dep2 = list(O3depRange[[3]][i], O3depRange[[4]][i])
		),
		series1 = list(
			clim1 = pullVars(clim1Ratio1, i),
			clim2 = pullVars(clim2Ratio1, i),
			acid1 = pullVars(acidRatio1, i),
			acid2 = pullVars(acidRatio1, i),
			chem1 = pullVars(chemRatio1, i),
			chem2 = pullVars(chemRatio1, i),
			fertN = pullVars(fertNRatio1, i),
			fertP = pullVars(fertPRatio1, i),
			airPol1 = pullVars(airPolRatio1, i),
			airPol2 = pullVars(airPolRatio1, i),
			H2Oblu = pullVars(H2ObluRatio1, i),
			H2Ogrn = pullVars(H2OgrnRatio1, i),
			land1 = pullVars(landRatio1, i),
			land2 = pullVars(landRatio1, i),
			bio1 = pullVars(bio1Ratio1, i),
			bio2 = pullVars(bio2Ratio1, i),
			O3dep1 = pullVars(O3depRatio1, i),
			O3dep2 = pullVars(O3depRatio1, i)
		),
		series2 = list(
			clim = pullVars(clim1Ratio2, i),
			acid = pullVars(acidRatio2, i),
			chem = pullVars(chemRatio2, i),
			fert = pullVars(fertPRatio2, i),
			airPol = pullVars(airPolRatio2, i),
			H2O = pullVars(H2ObluRatio2, i),
			land = pullVars(landRatio2, i),
			bio = pullVars(bio1Ratio2, i),
			O3dep = pullVars(O3depRatio2, i)
		)
	),
	social = list(
		min = socMinmax[[3]][i],
		max = socMinmax[[4]][i],
		range = list(
			con1 = list(con1Range[[3]][i], con1Range[[4]][i]),
			con2 = list(con2Range[[3]][i], con2Range[[4]][i]),
			enrgy1 = list(enrgy1Range[[3]][i], enrgy1Range[[4]][i]),
			enrgy2 = list(enrgy2Range[[3]][i], enrgy2Range[[4]][i]),
			watr1 = list(watr1Range[[3]][i], watr1Range[[4]][i]),
			watr2 = list(watr2Range[[3]][i], watr2Range[[4]][i]),
			food1 = list(food1Range[[3]][i], food1Range[[4]][i]),
			food2 = list(food2Range[[3]][i], food2Range[[4]][i]),
			hlth1 = list(hlth1Range[[3]][i], hlth1Range[[4]][i]),
			hlth2 = list(hlth2Range[[3]][i], hlth2Range[[4]][i]),
			edu1 = list(edu1Range[[3]][i], edu1Range[[4]][i]),
			edu2 = list(edu2Range[[3]][i], edu2Range[[4]][i]),
			inWk1 = list(inWk1Range[[3]][i], inWk1Range[[4]][i]),
			inWk2 = list(inWk2Range[[3]][i], inWk2Range[[4]][i]),
			peJu1 = list(peJu1Range[[3]][i], peJu1Range[[4]][i]),
			peJu2 = list(peJu2Range[[3]][i], peJu2Range[[4]][i]),
			poVo1 = list(poVoRange[[3]][i], poVoRange[[4]][i]),
			poVo2 = list(poVoRange[[3]][i], poVoRange[[4]][i]),
			socCo1 = list(socCo1Range[[3]][i], socCo1Range[[4]][i]),
			socCo2 = list(socCo2Range[[3]][i], socCo2Range[[4]][i]),
			eqDi1 = list(eqDi1Range[[3]][i], eqDi1Range[[4]][i]),
			eqDi2 = list(eqDi2Range[[3]][i], eqDi2Range[[4]][i]),
			hous1 = list(housRange[[3]][i], housRange[[4]][i]),
			hous2 = list(housRange[[3]][i], housRange[[4]][i])
		),
		series1 = list(
			con1 = pullVars(con1Ratio1, i),
			con2 = pullVars(con2Ratio1, i),
			enrgy1 = pullVars(enrgy1Ratio1, i),
			enrgy2 = pullVars(enrgy2Ratio1, i),
			watr1 = pullVars(watr1Ratio1, i),
			watr2 = pullVars(watr2Ratio1, i),
			food1 = pullVars(food1Ratio1, i),
			food2 = pullVars(food2Ratio1, i),
			hlth1 = pullVars(hlth1Ratio1, i),
			hlth2 = pullVars(hlth2Ratio1, i),
			edu1 = pullVars(edu1Ratio1, i),
			edu2 = pullVars(edu2Ratio1, i),
			inWk1 = pullVars(inWk1Ratio1, i),
			inWk2 = pullVars(inWk2Ratio1, i),
			peJu1 = pullVars(peJu1Ratio1, i),
			peJu2 = pullVars(peJu2Ratio1, i),
			poVo1 = pullVars(poVoRatio1, i),
			poVo2 = pullVars(poVoRatio1, i),
			socCo1 = pullVars(socCo1Ratio1, i),
			socCo2 = pullVars(socCo2Ratio1, i),
			eqDi1 = pullVars(eqDi1Ratio1, i),
			eqDi2 = pullVars(eqDi2Ratio1, i),
			hous1 = pullVars(housRatio1, i),
			hous2 = pullVars(housRatio1, i)
		),
		series2 = list(
			con = pullVars(con1Ratio2, i),
			enrgy = pullVars(enrgy1Ratio2, i),
			watr = pullVars(watr1Ratio2, i),
			food = pullVars(food1Ratio2, i),
			hlth = pullVars(hlth1Ratio2, i),
			edu = pullVars(edu1Ratio2, i),
			inWk = pullVars(inWk1Ratio2, i),
			peJu = pullVars(peJu1Ratio2, i),
			poVo = pullVars(poVoRatio2, i),
			socCo = pullVars(socCo1Ratio2, i),
			eqDi = pullVars(eqDi1Ratio2, i),
			hous = pullVars(housRatio2, i)
		)
	)
  )
}

#-------------------------------------------------------------------------------------
wldJSON <- toJSON(doughnutNest, pretty = TRUE, auto_unbox=TRUE, digits=2)  #
#write(wldJSON, "./myData/6_20240614_globalDoughnut.json")
#-------------------------------------------------------------------------------------

#NOTE, USE VISUAL STUDIO CODE TO CLEAN UP JSON FILE BEFORE LOADING ON OBSERVABLE PLATFORM
# (1) REPLACE NAs with 1.00001
# (2) REPLACE racial equality (eqDi2) values from '1' to '1.00001'





#this script pulls out variability within and across country groups

#read in cleaned data file
myData6 <- read_csv("./myData/5_20250108_doughnutDat-boundariesRatios.csv") %>%
  mutate(dimension = factor(dimension, levels = c("climate change", "ocean acidification", "chemical pollution",
	"nutrient pollution", "air pollution", "freshwater disruption", "land conversion", "biodiversity breakdown",
	"ozone depletion", "food", "health", "education", "income and work", "water", "energy", "connectivity",
	"housing", "equality", "social cohesion", "political voice", "peace and justice")))
#-------------------------------------------------------------------------------------------------------
# pull out 2017 social and ecological data by country group
#-------------------------------------------------------------------------------------------------------
#social
grpSoc <- myData6 %>%
  filter(date == 2017, type == "national aggregate", domain == "social", group != "World", indCode != "EQ2")

#replace public transport 2017 NA with 2020 value
trans <- myData6 %>% 
  filter(type == "national aggregate", indicator == "publicTrans", date == 2020, group != "World") %>%
  mutate(date = 2017)

grpSoc1 <- grpSoc %>%
  filter(indicator != "publicTrans") %>%
  rbind(., trans) %>%
  mutate(ratio = 1 - ratio, 
    indCode = factor(indCode, levels = c("NU1", "NU2", "HE1", "HE2", "ED1", "ED2", 
      "IW1", "IW2", "WA1", "WA2", "EN1", "EN2", "CO1", "CO2", "HO1", "EQ1", "SC1", "SC2", 
      "PV1", "PJ1", "PJ2"))) %>%
  arrange(dimension, indCode)

rm(grpSoc, trans)

#ecological
grpEco <- myData6 %>%
  filter(date == 2017, type == "national aggregate", domain == "ecological", group != "World",
    indCode %in% c("CC3", "NP3", "NP4", "FD3", "BB3", "BB4")) %>% 
  mutate(indCode = factor(indCode, levels = c("CC3", "NP3", "NP4", "FD3", "BB3", "BB4")),
    ratio = ratio-1) %>%
  arrange(dimension, indCode)

#-----------------------------------------------------------------------------------------------------
#ranges
#-----------------------------------------------------------------------------------------------------
#look at social shortfall range across groups (N=21)

ggplot(grpSoc1, aes(x=group, y=ratio)) +
  geom_hline(yintercept = 0, lwd=3, col="#227443") +
  geom_boxplot(aes(group = group), outlier.shape=NA) +
  geom_jitter(width=0.2) +
  scale_y_reverse(limits=c(1,-0.01), labels = scales::percent) +
  scale_x_discrete(labels=c("Poorest 40%", "Middle 40%", "Richest 20%"), expand = c(0,0.4)) +
#  scale_y_continuous(labels = scales::percent) +
#  coord_cartesian(ylim=c(1,0), xlim=c(2000,2022)) +
  labs(x="Country cluster", y="Range of shortfall across social\nindicators (0 = no shortfall)") +
  theme_bw()

#save figure to file
#ggsave("./figures/11_shortfallRange-by-group_Boxplots_R2.png", width = 120, height = 80, units = "mm", device="png")

#get data
g <- ggplot(grpSoc1, aes(group, ratio)) +
  geom_boxplot(aes(group = group))

socRange <- layer_data(g)
rm(g)

#log one outlier in R2 update: Top-20 control of corruption = 0.609

socRange1 <- socRange %>%
  select(-outliers, -newx) %>%
  tibble()

#write boxplot data to file
#write_csv(socRange1, "./myData/11_20250122_socialShortfall-grpBoxPlotData.csv")

rm(socRange, socRange1)

#-----------------------------------------------------------------------------------------------------
#look at ecological overshoot range across groups

ggplot(grpEco, aes(group, ratio)) +
  geom_hline(yintercept = 0, lwd = 3, col="#227443") + 
  geom_boxplot(aes(group = group), outlier.shape=NA) + 
  geom_jitter(width=0.2) +
  scale_y_continuous(breaks=c(-1,0,2.5,5,7.5,10, 12.5), labels = c("*", "0%", "250%", "500%", "750%", "1,000%", "1,250%")) +
  scale_x_discrete(labels=c("Poorest 40%", "Middle 40%", "Richest 20%"), expand = c(0,0.4)) +
  labs(x = "Country cluster", y = "Range of overshoot across ecological\nindicators (0 = no overshoot)") +
  theme_bw()

#ggsave("./figures/11_overshootRange-by-group_Boxplots_R2.png", width = 120, height = 80, units = "mm", device="png")

#get data
g <- ggplot(grpEco, aes(group, ratio)) +
  geom_boxplot(aes(group = group))

ecoRange <- layer_data(g) 
rm(g)

#log one outlier in R2 update: Top-20 carbon footprint = 11.263

ecoRange1 <- ecoRange %>%
  select(-outliers, -newx) 


#write boxplot data to file
#write_csv(ecoRange1, "./myData/11_20250122_EcologicalOvershoot-grpBoxPlotData.csv")

#------------------------------------------------------------------------------------------
#create chart of country groups by average GNI per capita over the 2000-2022 period
myDat1 <- read_csv("./cleanData/001_nationalGNIperCap-Population_2000-2022_n193.csv")

#calculate average national values over the 2000-2022 period and tag countries into B40, M40, T20 groups
avgDat <- myDat1 %>% 
  group_by(country, iso3c) %>%
  summarise(population = mean(population, na.rm=T),
    GNIperCap = mean(GNIperCap, na.rm=T)) %>%
  ungroup() %>%
  mutate(quintile_ts = ntile(GNIperCap, 5)) %>%
  rowwise() %>%
  mutate(b40m40t20_ts = ifelse(quintile_ts == 1 || quintile_ts == 2, 1,
    ifelse(quintile_ts == 3 || quintile_ts == 4, 2, 3))) %>%
  ungroup() %>%
  arrange(-quintile_ts) %>%
  select(-quintile_ts)

#calculate group averages for labels
grpDat <- avgDat %>%
  group_by(b40m40t20_ts) %>%
  summarise(population = sum(population, na.rm=T),
    GNIperCap = mean(GNIperCap, na.rm=T)) %>%
  ungroup()  

wldDat <- avgDat %>%
  summarise(wldPop = sum(population, na.rm=T))

grpDat1 <- grpDat %>%
  add_column(wldPop = wldDat$wldPop) %>%
  rowwise() %>%
  mutate(popShare = round(population/wldPop*100, digits=2)) %>%
  ungroup()

avgDat1 <- avgDat %>%
  add_column(wldPop = wldDat$wldPop) %>%
  rowwise() %>%
  mutate(popShare = round(population/wldPop*100, digits=3),
    country = ifelse(iso3c == "MKD", "North Macedonia", country)) %>%
  ungroup() %>%
  mutate(cumPopShare = cumsum(popShare))

rm(wldDat, grpDat)
#-----------------------------------------------------------------------------------------
#plot!
#ggplot(avgDat1, aes(x=reorder(factor(country), GNIperCap, sum), y=GNIperCap)) +
  geom_col(width=0.8, fill="darkred") +
  coord_flip() +
  labs(x ="Country", y="Average GNI per capita, 2000-2022 (International-$ in 2017 prices)") +
  theme_bw() + 
  theme(axis.text = element_text(size=6, family="sans", color="#000000"),
    axis.title = element_text(size=7, family="sans", color="#000000"),
    panel.grid = element_blank())

#ggsave("./figures/11_countriesByGNIperCap.pdf", width=180, height=287, units="mm", device="pdf")

#-----------------------------------------------------------------------------------------
#write chart data to file
#write_csv(avgDat1, "./myData/11_avgCountryGNIperCap.csv")




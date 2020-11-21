# Analyses of "Mosquito bloodmeal preference 
# in disturbed vs. wild landscapes (a review)"

# Guadalupe López-Nava
# Damián Villaseñor-Amador
# César A. Sandoval-Ruiz

#R map tutorial---- 

#By Kim Gilbert
#https://www.molecularecologist.com/2012/09/making-maps-with-r/

#Packages
library(maps)    #map functions
library(mapdata) #basic world map data
library(maptools)#for shapefiles
library(scales)  #for transparency

#Canada map
#Coordinates in decimals, R won't read
#degrees, minutes and seconds
map("worldHires","Canada",
  xlim=c(-141,-53), #longitude
  ylim=c(40,85),    #latitude
  col="gray90",
  fill=T)

#superior left corner of USA+Latinamerica: -131.256851W, 51.229805N 
#inferior right corner of USA+Latinamerica: -29.305435W, -56.884485S, 

map(database="worldHires",
  xlim=c(-131,-29),  #longitude
  ylim=c(51,-56))    #latitude

#long story short: I gave up, lets use
#the full world map for now
map()

#Mosquito database
#Note: it only has one mosquito species
#column and one landscape column
mosquito <- read.csv("./Analisis/MosquitoData.txt", header=T, sep="\t")
head(mosquito)

#Plotting mosquito study locations
#and saving it as a png image
png("MosquitoMap.png", width = 1080, height = 720, units = "px")
map()
points(mosquito$Long_dec, mosquito$Lat_dec, pch="\uD83E\uDD9F", col="navyblue", cex=2)
dev.off()

#Plotting temporal trend of mosquito
#studies (as suggested by Nakagawa 
#et al., 2018; pp. 3)
plot(as.numeric(mosquito$ID)~mosquito$Year)
levels(mosquito$ID)
levels(as.factor(mosquito$Year))
length(levels(mosquito$ID))

#15 November 2020----
#superior left corner of USA+Latinamerica: -53 S, -131 W 
#inferior right corner of USA+Latinamerica: 50 N, -23 W, 

#Importing mosquito study sites
mosquito <- read.csv("./Analisis/MosquitoData.txt", header=T, sep="\t")
mosquitosymbol <- "\uD83E\uDD9F"
png("Map_WrongCoords.png", units="in", width=16, height=16, res=600)
map(database="worldHires", 
    regions = c("USA",
    "Mexico",
    "Guatemala",
    "Belize",
    "El Salvador",
    "Honduras",
    "Nicaragua",
    "Panama",
    "Costa Rica",
    "Venezuela",
    "Colombia",
    "Guyana",
    "French Guiana",
    "Suriname",
    "Brazil",
    "Ecuador(?!:Galapagos)",
    "Peru",
    "Bolivia",
    "Paraguay",
    "Uruguay",
    "Chile",
    "Argentina"),
    xlim=c(-124,-35),  #longitude (left, right)
    ylim=c(-35,49),    #latitude  (bottom, top)
    col="gray90",
    fill=T)   
#plotting mosquito study sites
points(mosquito$Long_dec, mosquito$Lat_dec, 
       pch=21, 
       col="white",
       bg="black",
       lwd=2,
       cex=5)
dev.off()

#17 November 2020----
#Temporal publication series of the 20
#articles that we have chosen for the
#review

#Specific database to plot this graph
TempSeries = read.table("./Analisis/MosquitoYearPubs.txt", header=T)
head(TempSeries)

png("Mosquito_TempSer.png", units="in", width=8, height=4, res=300)

#Plot
plot(TempSeries$Year,TempSeries$Publications,
  ylim = c(1,4),
  las=1,
  xlab = "",
  ylab = "Publications",
  type="b",
  axes=F)

#Y axis
axis(2,at=c(1:4),labels=c(1:4),las=1)

#X axis
axis(1,at=levels(as.factor(TempSeries$Year)),labels=F,las=3)

#X axis labels
text(x = levels(as.factor(TempSeries$Year)),
     y = par("usr")[3] - 0.45,
     labels = levels(as.factor(TempSeries$Year)),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 40,
     cex = 1,
     adj=0.7)

dev.off()


# Lupita's map 18-Nov-20 ----
library(tidyverse)
library(ggthemes) # for a mapping theme
# if you have a more recent version of ggplot2, it seems to clash with the ggalt package
# installing this version of the ggalt package from GitHub solves it
# You might need to also restart your RStudio session
devtools::install_github("eliocamp/ggalt@new-coord-proj") # for 
library(praise)
library(ggalt)  

library(ggrepel)  # for annotations
library(viridis)  # for nice colours
library(broom)  # for cleaning up models
devtools::install_github("wilkox/treemapify")
library(treemapify)  # for making area graphs
library(wesanderson)  # for nice colours
library(devtools)
library(ellipsis)

# Data
dir()
mosquitos <- read_csv("Mosquito_Review.csv")
view(mosquitos)
str(mosquitos)
mosquitos <- dplyr::select(mosquitos, LatN_dec, LongW_dec, author_key, MosquitoSpecies1, Year ) 
names(mosquitos) <- c("lat", "long", "author", "species", "year")
mosquitos<-mosquitos[!(mosquitos$author=="Komar2018"| mosquitos$author=="NA"),]
mosquitos$long <- as.numeric (mosquitos$long)
mosquitos$lat <- as.numeric (mosquitos$lat)
mosquitos$species <- as.factor (mosquitos$species)
str(mosquitos)
view(mosquitos)

# Get the shape of America
america <- map_data("world", region = c("USA", "Canada", "Argentina","Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador","El Salvador", "Guatemala", "Haití", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay","Peru","Republica Dominicana","Uruguay","Venezuela", "Puerto Rico", "Guayana Francesa", "San Bartolomé", "Guadalupe"))

# A very basic map
(mosquitos_map1 <- ggplot() +
    geom_map(map = america, data = america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.1) +
    # Add points for the site locations
    geom_point(data = mosquitos, 
               aes(x = long, y = lat),
               colour = "#3A5FCD") +
    theme_classic())
ggsave(mosquitos_map1, filename = "mosquitos_map.png",
       height = 5, width = 8)




# Richness barplots 20-Nov-20 ----
mosquitos <- read.csv("./Analisis/Mosquito_Review.csv",header=T)
names(mosquitos)

#New column of landscape with two levels:
#disturbed (urban, rural) and wild (wild)
mosquitos$Landscape2 = mosquitos$Landscape1
levels(mosquitos$Landscape2) <- c(levels(mosquitos$Landscape2), "disturbed")
mosquitos$Landscape2[mosquitos$Landscape2 == 'rural'] <- 'disturbed'
mosquitos$Landscape2[mosquitos$Landscape2 == 'urban'] <- 'disturbed'
mosquitos$Landscape2 = droplevels(mosquitos$Landscape2)

# #Creating host richness column
# 
# #Empty vector to fill it with values
# RichnessVector = numeric()
#
# #Loop to extrac host richness per row
# for(i in 1:length(mosquitos$BloodSource1)){
# 
#   
#   #Numeric vector of hosts per row: each host's
#   #ID is the number of letters of its name 
#   #(ex. "Bos_taurus" ID is "10")
#   HostsPerRow = as.numeric(mosquitos[i,17:72]) 
#   
#   #Host richness per row (length of hosts' IDs)
#   HostRichness = length(HostsPerRow[!is.na(HostsPerRow)]) 
#   
#   #Fill the empty vector with host richness values
#   RichnessVector = c(RichnessVector,HostRichness)
# 
# }
# 
# #Fill the "host_richness" column of the
# #mosquitos' database with the values recovered
# #in the loop
# mosquitos$host_richness = RichnessVector
#
# write.csv(mosquitos,file="./Analisis/Mosquito_Review.csv")

#Boxplot of landscape vs. host richness
plot(mosquitos$Landscape1, mosquitos$host_richness)

#Boxplot of landscape (disturbed or wild)
#vs host richness:
plot(mosquitos$Landscape2, mosquitos$host_richness)

#_____________________________________________

#Number of hosts observations in disturbed: 123
length(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"])

#Mean host observations in disturbed: 5.512195
mean(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"])

#Median host observations in disturbed: 5
median(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"])

#Standard deviation host observations in disturbed: 5.569775
sd(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"])

#Standard error host observations in disturbed: 0.5042637
sd(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"])/
  sqrt(length(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"])-1)

#Selecting subgroup of the dataset that only
#encompasses "disturbed environment" values
disturbed = mosquitos[mosquitos$Landscape2=="disturbed",]

#Naming that subgroup "disturbed frequencies" (distFreq)
distFreq = data.frame(disturbed$MosquitoSpecies1,disturbed$host_richness)

#Assigning column names to distFreq
colnames(distFreq) = c("mosquito","host")

#Dropping non-used levels of mosquito
#species in distFreq table
distFreq$mosquito = droplevels(distFreq$mosquito)

#Sorting distFreq by alphabetical order
#of mosquito species and from max to min
#host number (VERY IMPORTANT STEP!!!)
distFreq = distFreq[order(distFreq$mosquito,-distFreq$host),]

#Eliminate duplicated species rows
#If the distFreq data table hasn't been
#arranged by max to min host number, then
#you're going to LOOSE HOST RICHNESS
#so be careful about this step and
#the previous one
distMaxHost = distFreq[!(duplicated(distFreq$mosquito)),]

#Sort distMaxHost (the table only with
#the maximum amount possible of bloodmeal source
#hosts per mosquito species) from max to min
distMaxHost = distMaxHost[order(-distMaxHost$host),]

#Disturbed environment mosquito host frequencies represented in a barplot

#Save as image
png("Disturbed_Hosts.png", units="in", width=12, height=15, res=300)

#Overall plot settings
par(mai=c(1.5,3,0,1.5), cex=1)

#Mosquito species names vector
mosquitoSpecies<-gsub("_"," ",distMaxHost$mosquito)

#Barplot
barplot(sort(distMaxHost$host),horiz = T,
  xlab= "",
  xlim=c(0,35),
  ylab="",
  xaxt="n",
  names.arg = rev(mosquitoSpecies),
  las=1,
  font=3,
  cex.axis = 2,
  col = "grey90")
axis(1,at=seq(from=0,to=35,by=10),labels = seq(from=0,to=35,by=10), cex.axis=1.5)

#X axis label
text(x = 22,
     y = par("usr")[3] - 3,
     labels = "Number of bloodmeal source hosts (disturbed)",
     xpd = NA,
     srt = 0,
     cex = 2,
     adj=0.7)

#Mosquito species with 1 host
text(2,3,labels = "1", cex=1.5)

#Mosquito species with 2 hosts
text(3,9,labels = "2", cex=1.5)

#Mosquito species with 3 hosts
text(4,14,labels = "3", cex=1.5)

#Mosquito species with 4 hosts
text(5,16.5,labels = "4", cex=1.5)

#Mosquito species with 5 hosts
text(6,21,labels = "5", cex=1.5)

#Mosquito species with 6 hosts
text(7,27,labels = "6", cex=1.5)

#Mosquito species with 7 hosts
text(8,32,labels = "7", cex=1.5)

#Mosquito species with 9 hosts
text(10,34.5,labels = "9", cex=1.5)

#Mosquito species with 10 hosts
text(11,35.5,labels = "10", cex=1.5)

#Culex erraticus with 17 hosts
text(18,36.5,labels = "17", cex=1.5)

#Culex restuans with 23 hosts
text(24,38,labels = "23", cex=1.5)

#Aedes vexans with 30 hosts
text(31,39,labels = "30", cex=1.5)

#Culex quinquefasciatus with 31 hosts
text(32,40.5,labels = "31", cex=1.5)

#Culex pipiens with 32 hosts
text(33,41.7,labels = "32", cex=1.5)

dev.off()

#__________________________________________

#Number of host observations in wild: 84
length(mosquitos$host_richness[mosquitos$Landscape2=="wild"])

#Mean host observations in wild: 6.107143
mean(mosquitos$host_richness[mosquitos$Landscape2=="wild"])

#Median host observations in wild: 6
median(mosquitos$host_richness[mosquitos$Landscape2=="wild"])

#Standard deviation host observations in wild: 6.230546
sd(mosquitos$host_richness[mosquitos$Landscape2=="wild"])

#Standard deviation host observations in wild: 0.6838913
sd(mosquitos$host_richness[mosquitos$Landscape2=="wild"])/
  sqrt(length(mosquitos$host_richness[mosquitos$Landscape2=="wild"])-1)

#Selecting subgroup of the dataset that only
#encompasses "wild environment" values
wild = mosquitos[mosquitos$Landscape2=="wild",]

#Naming that subgroup "wild frequencies" (wildFreq)
wildFreq = data.frame(wild$MosquitoSpecies1,wild$host_richness)

#Assigning column names to wildFreq
colnames(wildFreq) = c("mosquito","host")

#Dropping non-used levels of mosquito
#species in wildFreq table
wildFreq$mosquito = droplevels(wildFreq$mosquito)

#Sorting wildFreq by alphabetical order
#of mosquito species and from max to min
#host number (VERY IMPORTANT STEP!!!)
wildFreq = wildFreq[order(wildFreq$mosquito,-wildFreq$host),]

# #Creating new column with only the maximum
# #observation number of hosts per mosquito
# #species
# wildFreq$maxHosts=ave(wildFreq$host,wildFreq$mosquito,FUN=max)

#Eliminate duplicated species rows
#If the wildFreq data table hasn't been
#arranged by max to min host number, then
#you're going to LOOSE HOST RICHNESS
#so be careful about this step and
#the previous
wildMaxHost = wildFreq[!(duplicated(wildFreq$mosquito)),]

#Sort wildMaxHost (the table only with
#the maximum amount possible of bloodmeal source
#hosts per mosquito species) from max to min
wildMaxHost = wildMaxHost[order(-wildMaxHost$host),]

#Wild environment mosquito host frequencies
#represented in a barplot

#Save as image
png("Wild_Hosts.png", units="in", width=12, height=15, res=300)

#Overall plot settings
par(mai=c(1.5,3,0,1.5), cex=1)

#Mosquito species names vector
mosquitoSpecies<-gsub("_"," ",wildMaxHost$mosquito)

#Barplot
barplot(sort(wildMaxHost$host),horiz = T,
  xlab= "",
  xlim=c(0,60),
  ylab="",
  xaxt="n",
  names.arg = rev(mosquitoSpecies),
  las=1,
  font=3,
  cex.axis = 2,
  col = "seagreen")
axis(1,at=seq(from=0,to=60,by=10),labels = seq(from=0,to=60,by=10), cex.axis=1.5)

#X axis label
text(x = 34,
     y = par("usr")[3] - 5,
     labels = "Number of bloodmeal source hosts (wild)",
     xpd = NA,
     srt = 0,
     cex = 2,
     adj=0.7)

#Mosquito species with 1 host
text(2,3,labels = "1", cex=1.5)

#Mosquito species with 2 hosts
text(3,9,labels = "2", cex=1.5)

#Mosquito species with 3 hosts
text(4,13,labels = "3", cex=1.5)

#Mosquito species with 4 hosts
text(5,18,labels = "4", cex=1.5)

#Mosquito species with 5 hosts
text(6,23,labels = "5", cex=1.5)

#Mosquito species with 6 hosts
text(7,27.5,labels = "6", cex=1.5)

#Mosquito species with 7 hosts
text(8,34,labels = "7", cex=1.5)

#Mosquito species with 8 hosts
text(9,45,labels = "8", cex=1.5)

#Mosquito species with 9 hosts
text(10,51.5,labels = "9", cex=1.5)

#Culex territans with 12 hosts
text(14,53.5,labels = "12", cex=1.5)

#Culex peccator with 20 hosts
text(22,54.5,labels = "20", cex=1.5)

#Culex erraticus with 56 hosts
text(58,56,labels = "56", cex=1.5)

dev.off()

#__________________________________________

#Wilcoxon test because host observations
#don't follow a normal distribution
wilcox.test(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"],mosquitos$host_richness[mosquitos$Landscape2=="wild"])
#W = 4107.5, p-value = 0.01183
#There is a difference in mosquito host richness
#amongst disturbed and wild environments

#Wilcoxon test for maximum amount
#of hosts of wild vs. disturbed
wilcox.test(wildMaxHost$host,distMaxHost$host)
#W = 924.5, p-value = 0.3387


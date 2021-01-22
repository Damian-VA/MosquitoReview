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

#Temporal trend 17-Nov-2020----
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


#Lupita's map 18-Nov-20 ----
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




#Richness barplots 20-Nov-20 ----
mosquitos <- read.csv("./Analisis/Mosquito_Review.csv",header=T)
names(mosquitos)

# #New column of landscape with two levels:
# #disturbed (urban, rural) and wild (wild)
# mosquitos$Landscape2 = mosquitos$Landscape1
# levels(mosquitos$Landscape2) <- c(levels(mosquitos$Landscape2), "disturbed")
# mosquitos$Landscape2[mosquitos$Landscape2 == 'rural'] <- 'disturbed'
# mosquitos$Landscape2[mosquitos$Landscape2 == 'urban'] <- 'disturbed'
# mosquitos$Landscape2 = droplevels(mosquitos$Landscape2)

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

#Number of hosts observations in disturbed: 118
length(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"])

#Mean host observations in disturbed: 5.466102
mean(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"])

#Median host observations in disturbed: 5
median(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"])

#Standard deviation host observations in disturbed: 5.503196
sd(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"])

#Standard error host observations in disturbed: 0.5087706
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

#Number of mosquito species in disturbed environments
length(distMaxHost$mosquito)
# 32 spp.

#Disturbed environment mosquito host frequencies represented in a barplot

#Save as image
png("Disturbed_Hosts.png", units="in", width=12, height=15, res=300)

#Overall plot settings
par(mai=c(1.5,3,0,1.5), cex=1.2)

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
text(x = 26.5,
     y = par("usr")[3] - 3,
     labels = "Number of bloodmeal source hosts (disturbed)",
     xpd = NA,
     srt = 0,
     cex = 2,
     adj=0.7)

#Mosquito species with 1 host
text(2,1.5,labels = "1", cex=1.5)

#Mosquito species with 2 hosts
text(3,7.5,labels = "2", cex=1.5)

#Mosquito species with 3 hosts
text(4,11.5,labels = "3", cex=1.5)

#Mosquito species with 4 hosts
text(5,12.7,labels = "4", cex=1.5)

#Mosquito species with 5 hosts
text(6,18.5,labels = "5", cex=1.5)

#Mosquito species with 6 hosts
text(7,25.5,labels = "6", cex=1.5)

#Mosquito species with 7 hosts
text(8,29,labels = "7", cex=1.5)

#Mosquito species with 9 hosts
text(9.5,30.7,labels = "9", cex=1.5)

#Mosquito species with 10 hosts
text(11,32,labels = "10", cex=1.5)

#Culex erraticus with 17 hosts
text(17.5,33.2,labels = "17", cex=1.5)

#Culex restuans with 23 hosts
text(24,34.2,labels = "23", cex=1.5)

#Aedes vexans with 30 hosts
text(30.5,35.5,labels = "30", cex=1.5)

#Culex quinquefasciatus with 31 hosts
text(32,36.8,labels = "31", cex=1.5)

#Culex pipiens with 32 hosts
text(33,38,labels = "32", cex=1.5)

dev.off()

#__________________________________________

#Number of host observations in wild: 88
length(mosquitos$host_richness[mosquitos$Landscape2=="wild"])

#Mean host observations in wild: 5.397727
mean(mosquitos$host_richness[mosquitos$Landscape2=="wild"])

#Median host observations in wild: 5
median(mosquitos$host_richness[mosquitos$Landscape2=="wild"])

#Standard deviation host observations in wild: 6.114877
sd(mosquitos$host_richness[mosquitos$Landscape2=="wild"])

#Standard deviation host observations in wild: 0.6555836
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

#Number of mosquito species in wild environments
length(wildMaxHost$mosquito)
# 50 spp.

#Wild environment mosquito host frequencies
#represented in a barplot

#Save as image
png("Wild_Hosts.png", units="in", width=12, height=15, res=300)

#Overall plot settings
par(mai=c(1.5,3,0,1.5), cex=1.2)

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
text(x = 40,
     y = par("usr")[3] - 5,
     labels = "Number of bloodmeal source hosts (wild)",
     xpd = NA,
     srt = 0,
     cex = 2,
     adj=0.7)

#Mosquito species with 1 host
text(2,4.5,labels = "1", cex=1.5)

#Mosquito species with 2 hosts
text(3,12,labels = "2", cex=1.5)

#Mosquito species with 3 hosts
text(4,17,labels = "3", cex=1.5)

#Mosquito species with 4 hosts
text(5,24,labels = "4", cex=1.5)

#Mosquito species with 5 hosts
text(6,29.5,labels = "5", cex=1.5)

#Mosquito species with 6 hosts
text(7,34.5,labels = "6", cex=1.5)

#Mosquito species with 7 hosts
text(8,43.5,labels = "7", cex=1.5)

#Mosquito species with 8 hosts
text(9,52,labels = "8", cex=1.5)

#Mosquito species with 9 hosts
text(10,56,labels = "9", cex=1.5)

#Culex territans with 12 hosts
text(13,57,labels = "12", cex=1.5)

#Culex peccator with 20 hosts
text(22,58.2,labels = "20", cex=1.5)

#Culex erraticus with 56 hosts
text(58,59.5,labels = "56", cex=1.5)

dev.off()

#__________________________________________

#Wilcoxon test because host observations
#don't follow a normal distribution
wilcox.test(mosquitos$host_richness[mosquitos$Landscape2=="disturbed"],mosquitos$host_richness[mosquitos$Landscape2=="wild"])
#W = 4823, p-value = 0.3802

#Wilcoxon test for maximum amount
#of hosts of wild vs. disturbed
wilcox.test(wildMaxHost$host,distMaxHost$host)
#W = 924.5, p-value = 0.3387

#_________________________________________

disturbedVector = rep("disturbed",length(distMaxHost$mosquito))
disturbedTable = cbind(distMaxHost,disturbedVector)
colnames(disturbedTable) = c("mosquito_sp","host","landscape")

wildVector = rep("wild",length(wildMaxHost$mosquito))
wildTable = cbind(wildMaxHost,wildVector)
colnames(wildTable) = c("mosquito_sp","host","landscape")

distVSwild = rbind(disturbedTable,wildTable)
distVSwild = distVSwild[order(distVSwild$mosquito_sp),]
write.csv(distVSwild, file="./Analisis/mosquito_environment.csv", row.names = F)

#Lupita's updated map 20-Nov-20----

# Packages #
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

# Data #
dir()

mosquitos1 <- read_csv("Mosquito_map.csv")
view(mosquitos1)
str(mosquitos1)
# Get the shape of America #
america <- map_data("world", region = c("USA", "Canada", "Argentina","Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador","El Salvador", "Guatemala", "Haití", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay","Peru","Republica Dominicana","Uruguay","Venezuela", "Puerto Rico", "Guayana Francesa", "San Bartolomé", "Guadalupe"))

# MAP without authors 
(mosquitos_map1 <- ggplot() +
    geom_map(map = america, data = america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    coord_proj(paste0("+proj=wintri"),
               xlim = c(-125, -30)) +
    geom_point(data = mosquitos1,
               aes(x =lon, y = lat, fill = landscape),
               alpha = 0.8, size = 6, colour =
                 "grey30", shape = 21)+
    theme_map(base_size = 20) + theme(legend.position = "left"))

ggsave(mosquitos_map1, filename = "mosquitos_map_noauthors.png",
       height = 7, width = 9)
#map with authors 

(mosquitos_map2 <- ggplot() +
    geom_map(map = america, data = america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    coord_proj(paste0("+proj=wintri"),
               xlim = c(-125, -30)) +
    geom_point(data = mosquitos1,
               aes(x =lon, y = lat, fill = landscape),
               alpha = 0.8, size = 6, colour =
                 "grey30", shape = 21)+
    theme_map(base_size = 20) + theme(legend.position = "left") +
    geom_label_repel(data = mosquitos1,
                     aes(x = lon, y = lat,
                         label = author),
                     box.padding = 1, size = 3, nudge_x= 0.5, nudge_y = 0.5))

ggsave(mosquitos_map2, filename = "mosquitos_map_new.png",
       height = 7, width = 9)

#Disturbed vs. wild barplots 21-Nov-20----
mosq <- read.csv("./Analisis/mosquito_environment.csv",header=T)

#How many mosquito species
mosqFULL = mosq[!(duplicated(mosq$mosquito)),]
length(mosqFULL$mosquito_sp)
# 66 spp.

#How many mosquito species whose blood hosts
#observations were made in both disturbed and
#wild environments
mosqBOTH = mosq[(duplicated(mosq$mosquito)),]
length(mosqBOTH$mosquito_sp)
# 16 spp.

#create new data frame with only those
#species that appear in both environments
#(16 spp. x 2 environments = 32 rows)
mosqDW = mosq[duplicated(mosq$mosquito_sp)|duplicated(mosq$mosquito_sp, fromLast = T),]

#mosqDW = mosqDW[order(mosqDW$mosquito_sp,-mosqDW$host),]

#Barplot disturbed vs. wild host number per
#mosquito species
#Save as image
png("disturbedVSwild.png", units="in", width=12, height=15, res=300)

#Overall plot settings
par(mai=c(1.5,3,0,1.5), cex=1)

#Mosquito species names vector
mosquitoSpecies<-gsub("_"," ",mosqDW$mosquito_sp)

#Barplot
barplot(mosqDW$host,
  horiz = T,
  xlab= "",
  xlim=c(0,60),
  ylab="",
  xaxt="n",
  names.arg = mosquitoSpecies,
  las=1,
  font=3,
  cex.axis = 2,
  col = c("gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen","gray90","seagreen"))

axis(1,at=seq(from=0,to=60,by=10),labels = seq(from=0,to=60,by=10), cex.axis=1.5)

#X axis label
text(x = 34,
     y = par("usr")[3] - 3,
     labels = "Number of bloodmeal source hosts",
     xpd = NA,
     srt = 0,
     cex = 2,
     adj=0.7)

dev.off()

#Corrections 23-November-20

#Make mosquito species names larger, unique
#and both breaks for disturbed and wild must
#be glued to each other
mosq <- read.csv("./Analisis/mosquito_environment.csv",header=T)
mosqDW = mosq[duplicated(mosq$mosquito_sp)|duplicated(mosq$mosquito_sp, fromLast = T),]

#Separating hosts in wild and hosts in disturbed
#for those 16 mosquito species that have hosts in
#both environments
mosqDW
wildData = mosqDW[mosqDW$landscape=="wild",]
wildData = wildData[,c(1,2)]
colnames(wildData) = c("mosquito","hostsInWild")
disturbedData = mosqDW[mosqDW$landscape=="disturbed",]
disturbedData = disturbedData[,c(1,2)]
colnames(disturbedData) = c("mosquito","hostsInDisturbed")
wildDistDataframe = merge(wildData,disturbedData, by="mosquito")

#Converting dataframe to matrix
wildDistMatrix = rbind(as.numeric(wildDistDataframe$hostsInWild),as.numeric(wildDistDataframe$hostsInDisturbed))
rownames(wildDistMatrix) = c("wild","disturbed")
colnames(wildDistMatrix) = wildDistDataframe$mosquito
wildDistMatrix=wildDistMatrix[,ncol(wildDistMatrix):1]

#Save as image
png("disturbedVSwild.png", units="in", width=15, height=15, res=300)

#Overall plot settings
par(mai=c(1.5,8,0,0), cex=2)

#Bar colors
environmentColors = c("seagreen","gray90")

#Bar names
mosquitoSpp <- gsub("_"," ",wildDistDataframe$mosquito)

barplot(wildDistMatrix,
  beside=T,
  horiz = T,
  xlim=c(0,65),
  names.arg = rev(mosquitoSpp),
  xlab="",
  ylab="",
  xaxt="n",
  las=1,
  font=3,
  cex.names = 1.5,
  col = environmentColors)

#X axis values
axis(1,at=c(0,10,20,30,40,50,60),labels = c("0","10","20","30","40","50","60"), cex.axis=1.2)

#X axis label
text(x = 39,
     y = par("usr")[3] - 4.5,
     labels = "Number of bloodmeal source hosts",
     xpd = NA,
     srt = 0,
     cex = 1.2,
     adj=0.7)

dev.off()
#Rep and Pseudorep 23-Nov-20----
mosquitos <- read.csv("./Analisis/Mosquito_Review.csv",header=T)
names(mosquitos)

#Extracting mosquito species, host richness,
#host Class and landscape (disturbed or wild)
mos = mosquitos[,c(10,11,93,73:76,78,9)]
colnames(mos) = c("sp","host","landscape","mammalia","aves","amphibia","reptilia","location","id")

#How many samples per mosquito species we have?
mosSamples = as.data.frame(table(mos$sp))
mosSamples = mosSamples[order(-mosSamples$Freq),]
colnames(mosSamples) = c("mosquito","samples")
write.csv(mosSamples,file="./Analisis/pseudoreplication.csv", row.names = F)
#Aedes scapularis has 25 samples, those
#aren't true replicates for we only have
#21 studies. That means, Aedes scapularis was
#sampled more than once in, at least, one
#study. Ergo this samples are pseudoreplicated

#Join species and study ID
mos$spID = paste(mos$sp,mos$id,sep = ",")
mosReplicates = as.data.frame(table(mos$spID))
mosReplicates = mosReplicates[order(-mosReplicates$Freq),]
colnames(mosReplicates) = c("mosquito","true_replicates")
head(mosReplicates,7)
head(mosSamples,7)
write.csv(mosReplicates,file="./Analisis/replication.csv", row.names = F)

#Dist VS wild barplot with aegypti and albopictus 25-Nov-20----

mosq <- read.csv("./Analisis/mosquito_environment.csv",header=T)
mosqDW = mosq[duplicated(mosq$mosquito_sp)|duplicated(mosq$mosquito_sp, fromLast = T),]

#Separating hosts in wild and hosts in disturbed
#for those 16 mosquito species that have hosts in
#both environments
mosqDW
wildData = mosqDW[mosqDW$landscape=="wild",]
wildData = wildData[,c(1,2)]
colnames(wildData) = c("mosquito","hostsInWild")
disturbedData = mosqDW[mosqDW$landscape=="disturbed",]
disturbedData = disturbedData[,c(1,2)]
colnames(disturbedData) = c("mosquito","hostsInDisturbed")
wildDistDataframe = merge(wildData,disturbedData, by="mosquito")

#Adding Aedes aegypti and Aedes albopictus to
#the dataframe
AeAegypti = c("Aedes_aegypti",0,mosq$host[mosq$mosquito_sp=="Aedes_aegypti"])
AeAlbopictus = c("Aedes_albopictus",0,mosq$host[mosq$mosquito_sp=="Aedes_albopictus"])
wildDistDataframe2 = rbind(wildDistDataframe,AeAegypti,AeAlbopictus)
wildDistDataframe2 = wildDistDataframe2[order(wildDistDataframe2$mosquito),]


#Converting dataframe to matrix
wildDistMatrix = rbind(as.numeric(wildDistDataframe2$hostsInWild),as.numeric(wildDistDataframe2$hostsInDisturbed))
rownames(wildDistMatrix) = c("wild","disturbed")
colnames(wildDistMatrix) = wildDistDataframe2$mosquito
wildDistMatrix=wildDistMatrix[,ncol(wildDistMatrix):1]

#Save as image
png("distVSwild_aegypti_albopictus.png", units="in", width=15, height=15, res=300)

#Overall plot settings
par(mai=c(1.5,8,0,0), cex=2)

#Bar colors
environmentColors = c("seagreen","gray90")

#Bar names
mosquitoSpp <- gsub("_"," ",wildDistDataframe2$mosquito)

barplot(wildDistMatrix,
  beside=T,
  horiz = T,
  xlim=c(0,65),
  names.arg = rev(mosquitoSpp),
  xlab="",
  ylab="",
  xaxt="n",
  las=1,
  font=3,
  cex.names = 1.4,
  col = environmentColors)

#X axis values
axis(1,at=c(0,10,20,30,40,50,60),labels = c("0","10","20","30","40","50","60"), cex.axis=1.2)

#X axis label
text(x = 39,
     y = par("usr")[3] - 4.5,
     labels = "Number of bloodmeal source hosts",
     xpd = NA,
     srt = 0,
     cex = 1.2,
     adj=0.7)

dev.off()

#Host class stacked 25-Nov-20----

#Importing database and extracting the top 8
#mosquito records with the most host richness
#reported, with their respective number of mammal,
#bird, amphibian and reptile hosts
mosquitos <- read.csv("./Analisis/Mosquito_Review.csv",header=T)
mos = mosquitos[,c(10,11,93,73:76,78,9)]
colnames(mos) = c("sp","host","landscape","mammalia","aves","amphibia","reptilia","location","id")
mos = mos[order(mos$sp,-mos$host),]
mos = mos[!(duplicated(mos$sp)),]
mos = mos[order(-mos$host),]
mos[1:8,]

#Transforming dataframe to a matrix to barplot it
mosMatrix = rbind(as.numeric(mos$aves),as.numeric(mos$mammalia),as.numeric(mos$amphibia),as.numeric(mos$reptilia))
rownames(mosMatrix) = c("Aves","Mammalia","Amphibia","Reptilia")
colnames(mosMatrix) = mos$sp
mosMatrix[,1:8]

#Save as image
png("hostClass.png", units="in", width=28, height=15, res=300)

#Overall plot settings
par(mai=c(6,2,0,0), cex=2)

#Bar colors
hostClassColors = c("#00BFC4","#F8766D","#7CAE00","#C77CFF")

#Bar names
mosquitoSpp <- gsub("_"," ",colnames(mosMatrix[,1:20]))

hostClassbp = barplot(mosMatrix[,1:20],
  horiz = F,
  ylim=c(0,70),
  names.arg = mosquitoSpp,
  xlab="",
  ylab="",
  xaxt="n",
  yaxt="n",
  las=2,
  font=3,
  col = hostClassColors)

# #X axis values if horizontal barplots is true
# axis(1,at=c(0,10,20,30,40,50,60),labels = c("0","10","20","30","40","50","60"), cex.axis=1.2)
# 
# #X axis label if horizontal barplots is true
# text(x = 39,
#      y = par("usr")[3] - 4.5,
#      labels = "Number of bloodmeal source hosts",
#      xpd = NA,
#      srt = 0,
#      cex = 1.2,
#      adj=0.7)

#Y axis values if vertical barplots is true
axis(2,at=c(0,10,20,30,40,50,60),labels = c("0","10","20","30","40","50","60"), cex.axis=1.2, las=1)

#Y axis label if vertical barplots is true
text(x = par("usr")[3] - 2.5,
     y = 35,
     labels = "Host richness",
     xpd = NA,
     srt = 90,
     cex = 1.7,
     adj=0.7)

#X axis values if vertical barplots is true
text(x = hostClassbp,
     y = par("usr")[3] - 3,
     labels = mosquitoSpp,
     xpd = NA,
     srt = 50,
     cex = 1.7,
     adj = 0.99,
     font = 3)

legend(20,65,
  legend = rownames(mosMatrix), 
  col = hostClassColors, 
  bty = "n",
  pch = 16,
  y.intersp = 1.4,
  pt.cex=3,
  cex=2)

dev.off()
#Checking Homo sapiens predominance 1-Dec-20----
#We want to know the top 20 mosquitoes
#with the biggest host richness
mosquitos <- read.csv("./Analisis/Mosquito_Review.csv",header=T)
mos = mosquitos[,c(10,11,93,73:76,78,9)]
colnames(mos) = c("sp","host","landscape","mammalia","aves","amphibia","reptilia","location","id")
mos = mos[order(mos$sp,-mos$host),]
mos = mos[!(duplicated(mos$sp)),]
mos = mos[order(-mos$host),]
head(mos,20)

#For each of the 20 mosquito species the number
#of human bloodmeals in relation to the entirety of
#mammal and total bloodmeals was manually retrieved:
CxErra = c("Culex_erraticus",31,691,1162) #Burkett-Cadena2011
CxPipi = c("Culex_pipiens",100,121,636) #Hamer2009
CxQuin = c("Culex_quinquefasciatus",0,19,96) #Greenberg2013
AeVexa = c("Aedes_vexans",11,206,213) #Greenberg2013
CxRest = c("Culex_restuans",31,37,221) #Hamer2009
CxPecc = c("Culex_peccator",5,8,158) #Burkett-Cadena2011
CxTerr = c("Culex_territans",0,0,118) #Burkett-Cadena2011
CqVene = c("Coquillettidia_venezuelensis",1,201,212) #Navia-Gine2013
AeAegy = c("Aedes_aegypti",119,135,136) #Stenn2018
AeMedi = c("Aedes_mediovittatus",107,217,218) #Barrera2012
AeScap = c("Aedes_scapularis",2,16,27) #Alencar2015
CxBast = c("Culex_bastagarius",3,26,34) #Alencar2015
CxDecl = c("Culex_declarator",2,16,28) #Alencar2015
CxPleu = c("Culex_pleuristriatus",3,13,26) #Alencar2015
AeSerr = c("Aedes_serratus",2,5,11) #Alencar2015
AnAlbi = c("Anopheles_albitarsis",3,15,32) #Alencar2015
AnEvan = c("Anopheles_evansae",3,19,44) #DosSantosSilva2012
ChFaja = c("Chagasia_fajardi",8,30,40) #DosSantosSilva2012
CxUsqu = c("Culex_usquatus",0,6,12) #Alencar2015
PsAlbi = c("Psorophora_albigenu",75,93,93) #Mucci2015

#Binding as data frame the data of said 20 mosquito
#species with the human bloodmeals, the mammal blood
#meals and the total bloodemeals:
MosBM = as.data.frame(rbind(CxErra,CxPipi,CxQuin,AeVexa,CxRest,CxPecc,CxTerr,CqVene,AeAegy,AeMedi,AeScap,CxBast,CxDecl,CxPleu,AeSerr,AnAlbi,AnEvan,ChFaja,CxUsqu,PsAlbi))
colnames(MosBM) = c("Mosquito","HumanBM","MammalBM","TotalBM")
MosBM$HumanBM=as.numeric(as.character(MosBM$HumanBM))
MosBM$MammalBM=as.numeric(as.character(MosBM$MammalBM))
MosBM$TotalBM=as.numeric(as.character(MosBM$TotalBM))

#Adding column of human bloodmeal proportion respective
#to mammal bloodmeal
MosBM$HuMam = round((MosBM$HumanBM/MosBM$MammalBM),2)

#Adding column of human bloodmeal proportion respective
#to total bloodmeal
MosBM$HuTot = round((MosBM$HumanBM/MosBM$TotalBM),2)

# #Saving the data frame as "Bloodmeal Proportions
# #of 20 mosquito species"
# write.csv(MosBM, file = "./Analisis/BloodmealProportions_20spp.csv", row.names = F)

#Table ordered so to see those mosquito species with
#50% or more mammal bloodmeals being human
MosBM[order(-MosBM$HuMam),]

#Aedes aegypti 88%
#Culex restuans 84%
#Culex pipiens 83%
#Psorophora albigenu 81%
#Culex peccator 62%
#Aedes mediovittatus 49%

#litsearchr Eliza Grames 18-Dec-20----

#More info about this section in the article:
# Grames EM, Stillman AN, Tingley MW, Elphick CS.
# An automated approach to identifying search 
# terms for systematic reviews using keyword 
# cooccurrence networks. Methods Ecol Evol. 
# 2019;10:1645–1654. 
# https ://doi.org/10.1111/2041-210X.13268

#Check this website for templates:
#https://elizagrames.github.io/litsearchr/introduction_vignette_v010.html
#https://elizagrames.github.io/litsearchr/litsearchr_vignette_v041.html

#Installation issues:
devtools::install_github("elizagrames/litsearchr")
# Error: Failed to install 'unknown package' from GitHub:
#   HTTP error 404.
#   No commit found for the ref master
# 
#   Did you spell the repo owner (`elizagrames`) and repo name (`litsearchr`) correctly?
#   - If spelling is correct, check that you have the required permissions to access the repo.

remotes::install_github("elizagrames/litsearchr", ref="main")
# Installing package into ‘C:/Users/Damián/Documents/R/win-library/3.6’
# (as ‘lib’ is unspecified)
# * installing *source* package 'litsearchr' ...
# ** using staged installation
# Error in file(file, if (append) "a" else "w") : 
#   (convertido del aviso) cannot open file 'C:/Users/Damian/Documents/R/win-library/3.6/00LOCK-litsearchr/00new/litsearchr/DESCRIPTION': No such file or directory
# ERROR: installing package DESCRIPTION failed for package 'litsearchr'
# * removing 'C:/Users/Damián/Documents/R/win-library/3.6/litsearchr'
# Error: Failed to install 'litsearchr' from GitHub:
#   (convertido del aviso) installation of package ‘C:/Users/DAMIN~1/AppData/Local/Temp/RtmpUnSQqg/file46f495a3faa/litsearchr_1.0.0.tar.gz’ had non-zero exit status

#litsearchr package still hasn't been uploaded
#to CRAN, so it isn't an R official package.
#In order to use it one will need to individually
#download the scripts where her functions have
#been written. Download Grames' scripts from:
#https://zenodo.org/record/2551701#.X9zWk9hKhaQ

source("litsearchrZENODO/R/import_and_clean_data.R") #detect_database() usable_databases() import_results() deduplicate() clean_keywords()
source("litsearchrZENODO/R/term_selection.R") #make_corpus() add_stopwords() extract_terms() make_dictionary() create_dfm() create_network() make_importance() select_ngrams() select_unigrams() find_knots() fit_splines() find_cutoff() get_keywords() reduce_graph() make_ngram_graph() condense_network() get_condensed_terms() get_similar_terms()
source("litsearchrZENODO/R/write_scrape_test_searches.R") #get_language_data() choose_languages() language_graphs() translate_search() should_stem() write_search() available_languages() write_title_search() scrape_hits() scrape_oatd() scrape_ndltd() scrape_openthesis() check_recall() search_performance()
source("litsearchrZENODO/R/pretty_plots.R") #make_wordle() plot_full_network()
source("litsearchrZENODO/R/data.R") #data examples

## About the package  
# The *litsearchr* package for R is designed to partially automate search term selection and writing search strategies for systematic reviews. This vignette demonstrates its utility through a mock, example review examining the effects of fire on black-backed woodpeckers by demonstrating how the package: (1) Identifies potential keywords through the naive search input, (2) Builds a keyword co-occurence network to assist with building a more precise search strategy, (3) Uses a cutoff function to identify important changes in keyword importance, (4) Assists with grouping terms into concepts, and (5) Writes a Boolean search as a result of completion of the four previous steps.

usable_databases()
#>          Platform                                               Database
#> 1  Web of Science                                  BIOSIS Citation Index
#> 2  Web of Science                                      Zoological Record
#> 3          Scopus                                                 Scopus
#> 4           EBSCO                                Academic Search Premier
#> 5           EBSCO                                               Agricola
#> 6           EBSCO                                              GreenFILE
#> 7           EBSCO                                      OpenDissertations
#> 8           EBSCO                                          CAB Abstracts
#> 9           EBSCO                                                MEDLINE
#> 10          EBSCO                               Science Reference Center
#> 11       ProQuest         Earth, Atmospheric & Aquatic Science Database?
#> 12       ProQuest                ProQuest Dissertations & Theses Global?
#> 13       ProQuest NTIS Database (National Technical Information Service)
#> 14          NDLTD  Networked Digital Library of Theses and Dissertations
#> 15           OATD                   Open Access Theses and Dissertations
#> 16     OpenThesis                                             OpenThesis
#> 17     CAB Direct                                        (all databases)
#> 18       WorldCat                                                OAIster
#> 19       WorldCat                                               WorldCat
#> 20    Science.gov                                            Science.gov
#> 21 IngentaConnect                                         IngentaConnect
#> 22         PubMed                                                 PubMed

## Write and conduct naive search
# In our empirical example, we begin with a naive search intended to capture a set of relevant articles. Naive search terms: (("black-backed woodpecker" OR "picoides arcticus" OR "picoides tridactylus" AND (burn\* OR fire\*)). We ran the search in Scopus and Zoological Record (Web of Science), exporting results in .ris and .txt, respectively. These exported search results are then imported to litsearchr using the *import_results* function and next deduplicated using the *remove_duplicates* function. In some cases, it is best to run the *remove_duplicates* function two or more times, for example starting with exact matches and moving on to fuzzy matching. 

# When writing a naive search, the first step is to clearly articulate the research question. This serves as the basis for identifying concept groups and naive search terms. In our case, the research question is "What processes lead to the decline in black-backed woodpecker occupancy of post-fire forest systems with time since fire?" Although the exact concept groups needed for a review will vary on a case-by-case basis, the PICO (Population Intervention Control Outcome) model used in public health and medical reviews can be transformed to work for ecology. Instead of a population, we have a study system; intervention becomes predictor variables; outcome becomes response variables. The control category doesn't translate well to ecological reviews and can generally be omitted from the search. In our case, we are interested in either the predictor (processes) or response (occupancy) variables in our system (woodpeckers in post-fire forest systems), so our search will combine the concept groups as ( (processes OR occupancy) AND fire AND woodpecker ). The "OR" operator will include all hits that have either a process term or an occupancy term. The "AND" operator will require all hits to also have a term both the fire and woodpecker category. The parentheses work just like basic order of operations; items inside parentheses are considered before items outside of parentheses. 
# 
# We truncated terms to include word forms by adding an asterisk (\*) to the end of a word stem. For example, occup\* will pick up occupancy, occupance, occupied, occupy, occupying, etc... We included alternate spellings (i.e. colonization and colonisation) when possible, though we did not truncate one letter earlier because coloni\* would also pick up colonies or colonial, which has a different meaning altogether. Because there are multiple ways to describe nest success, we represented this concept with two groups of terms separated by W/3. This operator forces a word to occur within a certain number of words to another word (in this case, 3 words). By combining the OR operator with W/3, we can get any articles that include the concept of nesting and success next to each other. For example, an article about "success of nestlings" would be captured because the terms occur within three words of each other and nest* captures nestlings. Because we want our naive search to be discrete (i.e. only capture results most relevant to our question to yield better keyword suggestions), we decided to only include birds in the tribe Dendropicini. We included both common names (woodpecker, sapsucker) and genus names to capture studies which used only latin species names. The bird terms were only searched in the full text because study systems are often not specified in the title, abstract, or keywords. Genus names were truncated to account for studies that refer to groups with the suffix "-ids".
# 
# Naive search: ( 
#(occup\* OR occur\* OR presen\* OR coloniz\* OR colonis\* OR abundan\* OR "population size"  OR "habitat suitability" OR "habitat selection" OR persist\*) OR ( (nest\* OR reproduct* OR breed\* OR fledg\*) W/3 (succe\* OR fail\* OR surviv\*) ) OR ( surviv\* OR mortalit\* OR death\* ) OR ( "food availab\*" OR forag\* OR provision\*) OR  ( emigrat\* OR immigrat\* OR dispers\*) ) 
#AND (fire\* OR burn\* OR wildfire\*) ) 
#AND (woodpecker\* OR sapsucker\* OR Veniliorn\* OR Picoid\* OR Dendropic\* OR Melanerp\* OR Sphyrapic\*)
# 
# Searches were conducted on 10/22/18 with no date restrictions. We searched two databases on Web of Science (BIOSIS Citation Index and Zoological Record) and Scopus. Number of hits were as follows: BIOSIS (212), Zoological Record (179), and Scopus (592).
# 
# Although other databases could also be used, the import functions of this package are set up to work with commonly used databases and platforms in ecology or with .bib or .ris files from other databases. Instructions on how to export files to match what litsearchr is expecting are viewable with usable_databases(). 
# 
# The original export files should not be altered at all - none of the columns need to be removed and default headers should be left alone. These are used as signatures to detect which database a file originated from. If one of your naive searches results in more than 500 hits and you need to export multiple files from BIOSIS or Zoological Record, they can be left as separate files and don't need to be manually combined -litsearchr will do this for you. However, note that if your naive search returns more than 500 hits, the search terms are likely too broad. This lack of specificity may mean that the updated search terms returned by litsearchr will not adequately capture the desired level of inference. 
# 
# Optionally, if you want to return extremely specific keywords, you can conduct a critical appraisal of your naive search results to remove articles that you know aren't relevant to your question. However, if these articles are relatively rare, their keywords should be filtered out by litsearchr as unimportant.

source("litsearchrGitHub/R/import_and_clean_data.R")

naiveimport <- import_results(directory = "litsearchrGitHub/inst/extdata/",verbose=TRUE)
table(naiveimport$database)
naiveresults <- remove_duplicates(naiveimport, field = "title", method="string_osa")

#19-Dec-20----

source("litsearchrGitHub/R/import_and_clean_data.R")#to get the remove_duplicates() function

#First import the results from different search engines databases
#that you downloaded: in this case we used our naive search of the
#19 of december 2020:
# “mosquito*” 
# AND (“landscape” OR “deforestation” OR “soil use change” OR “logging”) 
# AND (“blood*” OR “blood meal” OR “blood meal source*” OR “host” OR “blood feeding” OR “feed*” OR “forag*”)
#In the WoS (160 results - title and abstract search), Scopus (227 results - title and abstract
#search) and PubMed (128 results - all fields) = 515 records (515 rows)

mosquitoimport = import_results(directory = "Analisis/naive_19Dec20/", verbose=T) #429 rows, 86 records missing
mosquitoresults = remove_duplicates(mosquitoimport, field = "title", method="string_osa") #299 rows, 130 duplicates
write.csv(mosquitoresults,file="naiveDeduplicated_19Dec20.csv",row.names = F) #save deduplicated database


#23-Dec-20 Sorting december articles----

#All articles pooled:
mosLup = read.csv("./Analisis/FullDatabaseMosquito_21-12-20.csv")
length(mosLup$Title) #2137 articles recovered
mosLup$Database = as.factor(mosLup$Database)
length(mosLup$Database[mosLup$Database=="SCOPUS"]) #1011 scopus articles
length(mosLup$Database[mosLup$Database=="WoS"]) #1098 WoS articles
length(mosLup$Database[mosLup$Database=="SciELO"]) #28 SciELO articles

#To deduplicate the database check
#https://cran.r-project.org/web/packages/synthesisr/vignettes/synthesisr_vignette.html:
library(synthesisr)

#Use function deduplicate to eliminate duplicate articles by title.
#The method is exact, which will remove articles that have identical
# titles. This is a fairly conservative approach, so we will remove
# them without review.
exactMosLup = deduplicate(mosLup, match_by = "Title", method = "exact")
#It now says that there're 1682 unique articles. Which implies
#455 records are duplicates.

#But we still need to eliminate highly similar titles: those that only
#differ in an extra space or the use of a dash-line. For this we'll use
#string distance:
stringMosLup = find_duplicates(exactMosLup$Title, method = "string_osa", to_lower=T, rm_punctuation = T, threshold = 7)

# We can extract the line numbers from the dataset that are likely
# duplicated this lets us manually review those titles to confirm
# they are duplicates:
manual_checks <- review_duplicates(exactMosLup$Title, stringMosLup)
length(manual_checks$title) #453 duplicates

# Now we can extract unique references from our dataset: we need to
#pass it the exact method dataset (exactMosLup) and the matching 
#articles (stringMosLup)
uniqueMosLup = extract_unique_references(exactMosLup, stringMosLup)
#1455 unique articles (914 scopus, 515 WoS, 26 SciELO). 
#682 duplicates.

#______________________________________________
#Screened 177 title and abstract Lupita's articles:
data = read.csv("./Analisis/177articulos.csv")
names(data)

#Spot duplicates by string distance
ddata = find_duplicates(data$Title, method = "string_osa", to_lower=T, rm_punctuation = T, threshold = 7)
manual_checks <- review_duplicates(data$Title, ddata)
length(manual_checks$title) #12 duplicates
write.csv(manual_checks,"./Analisis/177duplicates.csv")

#Extract unique records (eliminate duplicates)
udata = extract_unique_references(data, ddata)
write.csv(udata,"./Analisis/todos2.csv")

#_____________________________________________
#To the 164 unique new records the 21 records already
#selected since september 2020 were added, for a total 
#of 185 records:
data = read.csv("./Analisis/todos3.csv")
names(data)
length(data$Title)
#Spot duplicates by string distance
ddata = find_duplicates(data$Title, method = "string_osa", to_lower=T, rm_punctuation = T, threshold = 7)
manual_checks <- review_duplicates(data$Title, ddata)
length(manual_checks$title) #12 duplicates
write.csv(manual_checks, "./Analisis/185duplicates.csv")

#Extract unique records (eliminate duplicates)
udata = extract_unique_references(data, ddata)
write.csv(udata,"./Analisis/todos3.csv")

#14-Jan-21 Disturbed vs. wild barplots ----
mosquitos <- read.csv("./Analisis/Mosquito_Review_39articles.csv",header=T)

#Number of mosquito species
length(levels(factor(mosquitos$MosquitoSpecies)))
#91 spp.

#__________________________________________
#Selecting subgroup of the dataset that only encompasses "disturbed
#environment" values
disturbed = mosquitos[mosquitos$Landscape=="disturbed",]

#Naming that subgroup "disturbed frequencies" (distFreq)
distFreq = data.frame(disturbed$MosquitoSpecies,disturbed$HostRichness,disturbed$Landscape,disturbed$AuthorKey)

#Assigning column names to distFreq
colnames(distFreq) = c("mosquito","host","landscape","ID")

#Dropping non-used levels of mosquito species in distFreq table
distFreq$mosquito = as.factor(distFreq$mosquito)

#Sorting distFreq by alphabetical order of mosquito species and
#from max to min host number (VERY IMPORTANT STEP!!!)
distFreq = distFreq[order(distFreq$mosquito,-distFreq$host),]

#Eliminate duplicated species rows. If the distFreq data table 
#hasn't been arranged by max to min host number, then you're 
#going to LOOSE HOST RICHNESS so be careful about this step and
#the previous one
distMaxHost = distFreq[!(duplicated(distFreq$mosquito)),]

#Sort distMaxHost (the table only with the maximum amount possible 
#of bloodmeal source hosts per mosquito species) from max to min
distMaxHost = distMaxHost[order(-distMaxHost$host),]

#Number of mosquito species in disturbed environments
length(distMaxHost$mosquito)
# 63 spp.
#_________________________________________

#_________________________________________
#Selecting subgroup of the dataset that only encompasses "wild 
#environment" values
wild = mosquitos[mosquitos$Landscape=="wild",]

#Naming that subgroup "wild frequencies" (wildFreq)
wildFreq = data.frame(wild$MosquitoSpecies,wild$HostRichness,wild$Landscape,wild$AuthorKey)

#Assigning column names to wildFreq
colnames(wildFreq) = c("mosquito","host","landscape","ID")

#Dropping non-used levels of mosquito species in wildFreq table
wildFreq$mosquito = as.factor(wildFreq$mosquito)

#Sorting wildFreq by alphabetical order of mosquito species and 
#from max to min host number (VERY IMPORTANT STEP!!!)
wildFreq = wildFreq[order(wildFreq$mosquito,-wildFreq$host),]

#Eliminate duplicated species rows. If the wildFreq data table 
#hasn't been arranged by max to min host number, then you're going
#to LOOSE HOST RICHNESS so be careful about this step and the 
#previous one
wildMaxHost = wildFreq[!(duplicated(wildFreq$mosquito)),]

#Sort wildMaxHost (the table only with the maximum amount possible
#of bloodmeal source hosts per mosquito species) from max to min
wildMaxHost = wildMaxHost[order(-wildMaxHost$host),]

#Number of mosquito species in wild environments
length(wildMaxHost$mosquito)
# 57 spp.
#_________________________________________

#Joining distMaxHost and wildMaxHost (the tables for both disturbed
#and wild environments with only the maximum amount possible of
#bloodemal source hosts per mosquito species)
dw = rbind(distMaxHost,wildMaxHost)

#Obtaining mosquito species in both landscape types (disturbed and wild)
dw2 = dw[duplicated(dw$mosquito)|duplicated(dw$mosquito, fromLast = T),]
dw2[order(dw2$mosquito),]
length(dw2$mosquito)
#There're 23 mosquito species that feed from bloodhosts in both landscapes

#Reformat the database so that it has 3 columns: mosquito species,
#bloodhosts in disturbed landscape and bloodhosts in wild landscapes
wildData = dw2[dw2$landscape=="wild",]
wildData = wildData[,c(1,2)]
colnames(wildData) = c("mosquito","hostWild")
disturbedData = dw2[dw2$landscape=="disturbed",]
disturbedData = disturbedData[,c(1,2)]
colnames(disturbedData) = c("mosquito","hostDist")
dw3 = merge(disturbedData,wildData, by="mosquito")

#Adding Aedes aegypti to the dataframe
AeAegypti = c("Aedes_aegypti",dw$host[dw$mosquito=="Aedes_aegypti"],0)
dw4 = rbind(dw3,AeAegypti)
dw4 = dw4[order(dw4$mosquito),]

#Converting dataframe to matrix to plot it easier
dw4.matrix = rbind(as.numeric(dw4$hostWild),as.numeric(dw4$hostDist))
rownames(dw4.matrix) = c("wild","disturbed")
colnames(dw4.matrix) = dw4$mosquito
dw4.matrix=dw4.matrix[,ncol(dw4.matrix):1]

#________________________________________
#Save as image the barplot of the landscape types
png("LandscapeBarplot_24spp.png", units="in", width=15, height=15, res=300)

#Overall plot settings
par(mai=c(1.5,8,0,0), cex=2)

#Bar colors
environmentColors = c("seagreen","gray90")

#Bar names
mosquitoSpp <- gsub("_"," ",dw4$mosquito)

barplot(dw4.matrix,
  beside=T,
  horiz = T,
  xlim=c(0,65),
  names.arg = rev(mosquitoSpp),
  xlab="",
  ylab="",
  xaxt="n",
  las=1,
  font=3,
  cex.names = 1,
  col = environmentColors)

#X axis values
axis(1,at=c(0,10,20,30,40,50,60),labels = c("0","10","20","30","40","50","60"), cex.axis=1.2)

#X axis label
text(x = 39,
     y = par("usr")[3] - 6,
     labels = "Number of bloodmeal source hosts",
     xpd = NA,
     srt = 0,
     cex = 1.2,
     adj=0.7)

#Landscape type legend
legend(42,72,
       legend = c("Wild","Disturbed"),
       pch = 22,
       col = "black",
       pt.bg = environmentColors,
       pt.cex = 1.5)

dev.off()
#_________________________________________

#Extracting mosquito species, host richness, host class and
#landscape (disturbed or wild)
mosquitos <- read.csv("./Analisis/Mosquito_Review_39articles.csv",header=T)
mos = mosquitos[,c("MosquitoSpecies","HostRichness","Landscape","Mammalia","Aves","Reptilia","Amphibia","AuthorKey")]
colnames(mos) = c("sp","host","landscape","mammalia","aves","reptilia","amphibia","id")
head(mos)

#Find true replicates: how many studies researched each of 
#the 91 mosquito species
mos$spID = paste(mos$sp,mos$id,sep = ",")
levels(factor(mos$spID))
mos2 = (sort(levels(factor(mos$spID))))
mos3 = strsplit(mos2, ",")
mos4 = unlist(lapply(mos3 , '[[', 1))
mos5 = as.data.frame(table(mos4))
colnames(mos5) = c("sp","studies")
mos5[order(-mos5$studies),]

#Checking which studies had data for both disturbed and wild landscapes
mos$id=factor(mos$id)
mos$landscape=factor(mos$landscape)
md = mos$id[mos$landscape=="disturbed"]
md = factor(md)
mdl = levels(md)
mw = mos$id[mos$landscape=="wild"]
mw = factor(mw)
mwl = levels(mw)
wdtab = data.frame(table(c(mdl,mwl)))
wdtab[order(-wdtab$Freq),]



#19-Jan-21 Host class stacked barplots----

#Importing database and extracting the top mosquito records with
#the most host richness reported, with their respective number of
#mammal, bird, amphibian and reptile hosts
mosquitos <- read.csv("./Analisis/Mosquito_Review_39articles.csv",header=T)
mos = mosquitos[,c("MosquitoSpecies","HostRichness","BloodengorgedMosquitoes","Landscape","Mammalia","Aves","Amphibia","Reptilia","AuthorKey")]
colnames(mos) = c("sp","host","engorged","landscape","mammalia","aves","amphibia","reptilia","id")
mos = mos[order(mos$sp,-mos$host),]
mos = mos[!(duplicated(mos$sp)),]
mos = mos[order(-mos$host),]
mos[1:20,]

#Transforming dataframe to a matrix to barplot it
mosMatrix = rbind(as.numeric(mos$aves),as.numeric(mos$mammalia),as.numeric(mos$amphibia),as.numeric(mos$reptilia))
rownames(mosMatrix) = c("Aves","Mammalia","Amphibia","Reptilia")
colnames(mosMatrix) = mos$sp
mosMatrix[,1:20]

#Save as image
png("hostClass17mos.png", units="in", width=28, height=15, res=300)

#Overall plot settings
par(mai=c(6,2,0,0), cex=2)

#Bar colors
hostClassColors = c("#00BFC4","#F8766D","#7CAE00","#C77CFF")

#Bar names
mosquitoSpp <- gsub("_"," ",colnames(mosMatrix[,1:20]))

hostClassbp = barplot(mosMatrix[,1:20],
  horiz = F,
  ylim=c(0,70),
  names.arg = mosquitoSpp,
  xlab="",
  ylab="",
  xaxt="n",
  yaxt="n",
  las=2,
  font=3,
  col = hostClassColors)

# #X axis values if horizontal barplots is true
# axis(1,at=c(0,10,20,30,40,50,60),labels = c("0","10","20","30","40","50","60"), cex.axis=1.2)
# 
# #X axis label if horizontal barplots is true
# text(x = 39,
#      y = par("usr")[3] - 4.5,
#      labels = "Number of bloodmeal source hosts",
#      xpd = NA,
#      srt = 0,
#      cex = 1.2,
#      adj=0.7)

#Y axis values if vertical barplots is true
axis(2,at=c(0,10,20,30,40,50,60),labels = c("0","10","20","30","40","50","60"), cex.axis=1.2, las=1)

#Y axis label if vertical barplots is true
text(x = par("usr")[3] - 2.5,
     y = 35,
     labels = "Host richness",
     xpd = NA,
     srt = 90,
     cex = 1.7,
     adj=0.7)

#X axis values if vertical barplots is true
text(x = hostClassbp,
     y = par("usr")[3] - 3,
     labels = mosquitoSpp,
     xpd = NA,
     srt = 50,
     cex = 1.7,
     adj = 0.99,
     font = 3)

legend(20,65,
  legend = rownames(mosMatrix), 
  col = hostClassColors, 
  bty = "n",
  pch = 16,
  y.intersp = 1.4,
  pt.cex=3,
  cex=2)

dev.off()

#Studying the blood hosts labeling
bloodsources = mosquitos[,14:69]
names(bloodsources)
b1=unlist(bloodsources) #transform data frame into vector by columns
b2=data.frame(table(b1)) #obtain the frequency of each blood source host label as a data frame
b3 = b2[order(-b2$Freq),]
colnames(b3) = c("bloodhost","replicates")
head(b3)
length(b3$bloodhost) 
#317 not-curated blood host labels
#1899 blood host replicates

#Extract all the blood host labels that aren't a species names
#and are probably wrongly written
b4=subset(b3, !(grepl("_", bloodhost)))
b4
sum(b4$replicates) #232 hosts labeled NOT to species level
sum(b4$replicates[c(1,5,6,10,12,17,20,27)]) #8 labels are genera names, its total replicate sum is equal to 35


#Extract all labels identified to genus level (endend in sp.)
b5=subset(b3, (grepl("_sp", bloodhost)))
b5
sum(b5$replicates[c(4,7,8)]) #7 hosts labeled only to genus level

#Extract all the blood host labels that ARE species names
b6=subset(b3, (grepl("_", bloodhost)))
b6
sum(b6$replicates) #1667 hosts labeled to species level

#22-Jan-21 correcting labels in database----
mosquitos <- read.csv("./Analisis/Mosquito_Review_39articles.csv",header=T)
mc = mosquitos

#Studying the blood hosts labeling
bloodsources = mc[,14:69]
b1=unlist(bloodsources) #transform data frame into vector by columns
b2=data.frame(table(b1)) #obtain the frequency of each blood source host label as a data frame
b3 = b2[order(b2$b1),]
colnames(b3) = c("bloodhost","replicates") #length 317

#Correcting records
mc=replace(mc, mc=="Alligator_missisippiensis", "Alligator_mississippiensis")
mc=replace(mc, mc=="Alligator_mississippiens", "Alligator_mississippiensis")
mc=replace(mc, mc=="Alligator_mississippiens", "Alligator_mississippiensis")
mc=replace(mc, mc=="Anas_platyrhynchos_domesticus", "Anas_platyrhynchos")
mc=replace(mc, mc=="Anhinga", "Anhinga_anhinga")
mc=replace(mc, mc=="Anolis_caronilensis", "Anolis_carolinensis")
mc=replace(mc, mc=="Ardea_herdias", "Ardea_herodias")
mc=replace(mc, mc=="Bare_faced_ibis", "Phimosus_infuscatus")
mc=replace(mc, mc=="Bird", "bird")
mc=replace(mc, mc=="Black_crowdned_night_heron", "Nycticorax_nycticorax")
mc=replace(mc, mc=="bos_taurus", "Bos_taurus")
mc=replace(mc, mc=="Branta_canadenis", "Branta_canadensis")
mc=replace(mc, mc=="Canis_familiaris", "Canis_lupus_familiaris")
mc=replace(mc, mc=="Cannis_lupus", "Canis_lupus_familiaris")
mc=replace(mc, mc=="Capra_aegagrus_hircus", "Capra_hircus")
mc=replace(mc, mc=="Capybara", "Hydrochoerus_hydrochaeris")
mc=replace(mc, mc=="Carolina", "Carolina_sp")
mc=replace(mc, mc=="cat", "Felis_catus")
mc=replace(mc, mc=="Chatarus_gattatus", "Catharus_guttatus")
mc=replace(mc, mc=="Chicken", "Gallus_gallus")
mc=replace(mc, mc=="Cocoi_heron", "Ardea_cocoi")
mc=replace(mc, mc=="Common_moorhen", "Gallinula_chloropus")
mc=replace(mc, mc=="Common_opossum", "Didelphis_sp")
mc=replace(mc, mc=="Corvus_brachyrhyncho", "Corvus_brachyrhynchos")
mc=replace(mc, mc=="Cow", "Bos_taurus")
mc=replace(mc, mc=="dasypus_novemcinctus", "Dasypus_novemcinctus")
mc=replace(mc, mc=="Didelphis", "Didelphis_sp")
mc=replace(mc, mc=="Dog", "Canis_lupus_familiaris")
mc=replace(mc, mc=="Equus", "Equus_caballus")
mc=replace(mc, mc=="Equus_ferus_caballus", "Equus_caballus")
mc=replace(mc, mc=="Felis_silvestris_catus", "Felis_catus")
mc=replace(mc, mc=="Frog", "Anura")
mc=replace(mc, mc=="gallus_gallus", "Gallus_gallus")
mc=replace(mc, mc=="gallu_gallus", "Gallus_gallus")
mc=replace(mc, mc=="gallus_gallus_domesticus", "Gallus_gallus")
mc=replace(mc, mc=="Gallus_gallus_domesticus", "Gallus_gallus")
mc=replace(mc, mc=="Goat", "Capra_hircus")
mc=replace(mc, mc=="Goat(sheep)", "Capra_hircus")
mc=replace(mc, mc=="Homo", "Homo_sapiens")
mc=replace(mc, mc=="homo_sapiens", "Homo_sapiens")
mc=replace(mc, mc=="Homo_sapiens_sapiens", "Homo_sapiens")
mc=replace(mc, mc=="Human", "Homo_sapiens")
mc=replace(mc, mc=="Horse", "Equus_caballus")
mc=replace(mc, mc=="Iguana", "Iguana_iguana")
mc=replace(mc, mc=="Least_bittern", "Ixobrychus_exilis")
mc=replace(mc, mc=="Limpkin", "Aramus_guarauna")
mc=replace(mc, mc=="Lithobates_sp.", "Lithobates_sp")
mc=replace(mc, mc=="Lithobates_sphenocephelus", "Lithobates_sphenocephalus")
mc=replace(mc, mc=="Melospize_melodia", "Melospiza_melodia")
mc=replace(mc, mc=="Memphitis_memphitis", "Mephitis_mephitis")
mc=replace(mc, mc=="Myprocta_pratti", "Myoprocta_prattis")
mc=replace(mc, mc=="Nerodia_erythrogaster,", "Nerodia_erythrogaster")
mc=replace(mc, mc=="Nyctanassa_violace", "Nyctanassa_violacea")
mc=replace(mc, mc=="Nycticorax_cycticorax", "Nycticorax_nycticorax")
mc=replace(mc, mc=="Nycticorax_myticorax", "Nycticorax_nycticorax")
mc=replace(mc, mc=="Nyctiocorax_nyctiocorax", "Nycticorax_nycticorax")
mc=replace(mc, mc=="Odocoileus_virginiamus", "Odocoileus_virginianus")
mc=replace(mc, mc=="Odocoileus_virginuanus", "Odocoileus_virginianus")
mc=replace(mc, mc=="Opossum", "Didelphis_sp")
mc=replace(mc, mc=="Pig", "Sus_scrofa")
mc=replace(mc, mc=="Pinneated_bittern", "Botaurus_pinnatus")
mc=replace(mc, mc=="Poecile_atricapilla", "Poecile_atricapillus")
mc=replace(mc, mc=="Poecile_atricapilla", "Poecile_atricapillus")
mc=replace(mc, mc=="primate", "Primates")
mc=replace(mc, mc=="Rattus_norvergicus", "Rattus_norvegicus")
mc=replace(mc, mc=="Rattus_norvergicus,", "Rattus_norvegicus")
mc=replace(mc, mc=="Rattus_sp.", "Rattus_sp")
mc=replace(mc, mc=="Rattus", "Rattus_sp")
mc=replace(mc, mc=="Rodent", "Rodentia")
mc=replace(mc, mc=="Sheep", "Ovis_aries")
mc=replace(mc, mc=="Straited_heron", "Butorides_striata")
mc=replace(mc, mc=="Sus_scrofa_domesticus", "Sus_scrofa")
mc=replace(mc, mc=="Sylvialagus_floridanus", "Sylvilagus_floridanus")
mc=replace(mc, mc=="Terrapene_carolina_carolina", "Terrapene_carolina")
mc=replace(mc, mc=="Tiarsis_bicolor", "Tiaris_bicolor")
mc=replace(mc, mc=="Toxostoma_curviroste", "Toxostoma_curvirostre")
mc=replace(mc, mc=="Tropidurus_sp.", "Tropidurus_sp")
mc=replace(mc, mc=="turtle", "turtle")
mc=replace(mc, mc=="Turtle", "turtle")
mc=replace(mc, mc=="Zanaida_asiatica", "Zenaida_asiatica")

#Studying the blood hosts labeling after correcting errors
b.corr = mc[,14:69]
b.corr1=unlist(b.corr)
b.corr2=data.frame(table(b.corr1))
b.corr3 = b.corr2[order(b.corr2$b.corr1),]
colnames(b.corr3) = c("bloodhost","replicates")
length(b.corr3$bloodhost) #252 bloodhost labels
sum(b.corr3$replicates) #1899 bloodhost records
b.corr3 = b.corr3[order(-b.corr3$replicates),]

#Extract all the blood host labels that aren't a species names
#and are probably wrongly written
b.corr4=subset(b.corr3, !(grepl("_", bloodhost)))
b.corr4
sum(b.corr4$replicates) #174 non-bloodhost records plus...

#Extract all labels identified to genus level (endend in sp.)
b.corr5=subset(b.corr3, (grepl("_sp", bloodhost)))
b.corr5
sum(b.corr5$replicates[c(-1,-4,-5,-7)]) #37 bloodhost genera not id to species level

# #Save as CSV the table with the 252 bloodhost labels and its replicates
# write.csv(b.corr3, file="Hosts.csv", row.names=F)

# #Save as XLSX the database with 252 bloodhosts correct labels
# write.csv(mc, file="Mosquito_Review_39articles.csv", row.names = F)

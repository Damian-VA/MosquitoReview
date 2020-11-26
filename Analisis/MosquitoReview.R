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
par(mai=c(5,2,0,0), cex=2)

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
     cex = 1.1,
     adj = 0.99,
     font = 3)

legend(22,65,
  legend = rownames(mosMatrix), 
  col = hostClassColors, 
  bty = "n",
  pch = 16,
  y.intersp = 3,
  pt.cex = 2)

dev.off()
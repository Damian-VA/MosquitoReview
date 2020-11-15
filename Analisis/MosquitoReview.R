# Analyses of "Mosquito bloodmeal preference 
# in disturbed vs. wild landscapes (a review)"

# Guadalupe López-Nava
# Damián Villaseñor-Amador
# César A. Sandoval-Ruiz

####R map tutorial####

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

#15 November 2020
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



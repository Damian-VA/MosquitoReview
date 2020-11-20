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

#17 November 2020
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

# Analyses of "Mosquito bloodmeal preference 
# in disturbed vs. wild landscapes (a review)"

# Guadalupe López-Nava
# Damián Villaseñor-Amador
# César A. Sandoval-Ruiz

####Fooling around####

#Packages
library(maps)    #map functions
library(mapdata) #basic world map data

#Canada map
#Coordinates in decimals, R won't read
#degrees, minutes and seconds
map("worldHires","Canada",
  xlim=c(-141,-53), #longitude
  ylim=c(40,85),    #latitude
  col="gray90",
  fill=T)





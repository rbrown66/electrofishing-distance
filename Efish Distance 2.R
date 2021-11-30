rm(list = ls())
setwd("C:/Users/rbrown66/OneDrive - University of Toledo/Documents/R")


library(riverdist)
library(sp)
library(rgdal)
library(sf)
library(dplyr)
library(raster)
library(ggplot2)
library(prettymapr)
library(plyr)




####Importing csv and shapefile and removing values with no coordinates. Making sure bounding box on shapefiles
####is in correct units before using line2network. Should be in meters and not decimal degrees####
UT_EFISH<-read.csv("UT_EFISH_COORDS.csv")
UT_EFISH<-UT_EFISH[!is.na(UT_EFISH$Start_lat_test),]
UT_EFISH<-UT_EFISH[!is.na(UT_EFISH$Start_long_test),]
UT_EFISH<-UT_EFISH[!is.na(UT_EFISH$End_lat_test),]
UT_EFISH<-UT_EFISH[!is.na(UT_EFISH$End_long_test),]
sandusky_polyline<-shapefile("C:/Users/rbrown66/OneDrive - University of Toledo/Documents/R/Sandusky_River_Line.shp")
portage_polyline<-shapefile("C:/Users/rbrown66/OneDrive - University of Toledo/Documents/R/Portage_River3.shp")
Maumee_polyline<-shapefile("C:/Users/rbrown66/OneDrive - University of Toledo/Documents/R/Maumee_River.shp")
Black_polyline<-shapefile()
Huron_polyline<-shapefile()




####Importing river line shapefile, outlines do not work with riverdist, it must be a single polyline for the river####
Sandusky<-line2network(layer = "Sandusky_River_Line", tolerance = 1)
plot(Sandusky)
Portage<-line2network(layer = "Portage_River3")
plot(Portage)
Maumee<-line2network(layer = "Maumee_River")
plot(Maumee)
Black<-line2network(layer = "Black_River")
plot(Black)
Huron<-line2network(layer = "Huron_River")
plot(Huron)
Toussaint<-line2network(layer = "Toussaint_River")
#Clean_Sandusky=cleanup(Sandusky)




####Converting xy data to river point locations. Use pointshp2segvert for xy point shapefile####
#Sandusky_Points=xy2segvert(UT_EFISH$Start_long_test, UT_EFISH$Start_lat_test, rivers = Sandusky)
#Sandusky_Points_2=xy2segvert(UT_EFISH$End_long_test, UT_EFISH$End_lat_test, rivers = Sandusky)
Sandusky_Start<-pointshp2segvert(path = ".", layer = "Sandusky_Start", rivers=Sandusky)
Sandusky_End<-pointshp2segvert(path = ".", layer = "Sandusky_End", rivers=Sandusky)
Portage_Start<-pointshp2segvert(path = ".", layer = "Portage_Start", rivers = Portage)
Portage_End<-pointshp2segvert(path = ".", layer = "Portage_End", rivers = Portage)
Maumee_Start<-pointshp2segvert(path = ".", layer = "Maumee_Start", rivers = Maumee)
Maumee_End<-pointshp2segvert(path = ".", layer = "Maumee_End", rivers = Maumee)
Black_Start<-pointshp2segvert(path = ".", layer = "Black_Start", rivers = Black)
Black_End<-pointshp2segvert(path = ".", layer = "Black_End", rivers = Black)
Huron_Start<-pointshp2segvert(path = ".", layer = "Huron_Start", rivers = Huron)
Huron_End<-pointshp2segvert(path = ".", layer = "Huron_End", rivers = Huron)
Toussaint_Start<-pointshp2segvert(path = ".", layer = "Toussaint_Start", rivers = Huron)
Toussaint_End<-pointshp2segvert(path = ".", layer = "Toussaint_End", rivers = Huron)



#########################################
####Placing river points onto the map####
#########################################
##Sandusky
riverpoints(Sandusky_Start$seg, Sandusky_Start$vert, rivers = Sandusky, col = "green")
riverpoints(Sandusky_End$seg, Sandusky_End$vert, rivers = Sandusky, col = "red")
title("Sandusky River Electrofishing Start and End Points")
##Portage
riverpoints(Portage_Start$seg, Portage_Start$vert, rivers = Portage, col = "green")
riverpoints(Portage_End$seg, Portage_End$vert, rivers = Portage, col = "red")
title("Portage River Electrofishing Start and End Points")
##Maumee
riverpoints(Maumee_Start$seg, Maumee_Start$vert, rivers = Maumee, col = "green")
riverpoints(Maumee_End$seg, Maumee_End$vert, rivers = Maumee, col = "red")
title("Maumee River Electrofishing Start and End Points")
##Black
riverpoints(Black_Start$seg, Black_Start$vert, rivers = Black, col = "green")
riverpoints(Black_End$seg, Black_End$vert, rivers = Black, col = "red")
title("Black River Electrofishing Start and End Points")
##Huron
riverpoints(Huron_Start$seg, Huron_Start$vert, rivers = Huron, col = "green")
riverpoints(Huron_End$seg, Huron_End$vert, rivers = Huron, col = "red")
title("Huron River Electrofishing Start and End Points")
#Toussaint
riverpoints(Toussaint_Start$seg, Toussaint_Start$vert, rivers = Huron, col = "green")
riverpoints(Toussaint_End$seg, Toussaint_End$vert, rivers = Huron, col = "red")
title("Huron River Electrofishing Start and End Points")



##########################################################################################################
####Determining river distance between two points. Sample code to ensure distances can be determined######
##########################################################################################################
riverdistance(startseg = 59, endseg = 59, startvert = 31, endvert = 18, rivers = Sandusky)
riverdistance(startseg = 113, endseg = 113, startvert = 54, endvert = 35, rivers = Portage)
riverdistance(startseg = 136, endseg = 137, startvert = 4, endvert = 34, rivers = Maumee, map = T, add = F)
riverdistance(startseg = 24, endseg = 18, startvert = 40, endvert = 4, rivers = Black)
riverdistance(startseg = 36, endseg = 95, startvert = 57, endvert = 34, rivers = Huron)




#################################################################################
####Creating a function to determine river distance for entire river dataset#####
#################################################################################
EFISH_Distance<-function(water_shapefile_name, point_shapefile_start, point_shapefile_end, plot_title)
{
  River_Name<-line2network(path= ".", layer = water_shapefile_name)
  windowsFonts(A=windowsFont("Times New Roman"))
  plot(River_Name, xlab="UTM Zone 17 (m)", ylab="UTM Zone 17 (m)", segmentnum=F, cex.axis=1, cex.lab=1.5, family="A")
  windowsFonts(B=windowsFont("Times New Roman"))
  legend("topleft", legend = c("Start", "End"), pch = c(16, 15), col = (c("Black", "grey")),
         inset = 0.05, title = "Electrofishing Sample Points")
  start_points<-pointshp2segvert(path = ".", layer = point_shapefile_start, rivers = River_Name)
  end_points<-pointshp2segvert(path = ".", layer = point_shapefile_end, rivers = River_Name)
  river_start<-riverpoints(start_points$seg, start_points$vert, rivers = River_Name, col="black", pch = 16)
  river_end<-riverpoints(end_points$seg, end_points$vert, rivers = River_Name, col = "grey", pch = 15)
  windowsFonts(C=windowsFont("Times New Roman"))
  title(plot_title, cex.main=2, family="C")
  addnortharrow(pos = "topright", scale = 0.5)
  river_coords<-data.frame(start_points$seg, end_points$seg, start_points$vert, end_points$vert)
  for (i in 1:nrow(river_coords)) 
  {
    river_distance<-riverdistance(startseg = river_coords[i,1], endseg = river_coords[i,2], 
          startvert = river_coords[i,3], endvert = river_coords[i,4], rivers = River_Name, stopiferror = F, 
          algorithm = "Dijkstra")
    river_coords[i,5]<-river_distance
    #rename(river_coords, Electrofishing_Distance=V5)
    #print(river_coords)
  }
  Electrofishing_Distances<-cbind(start_points, river_coords[,5])
  #print(Electrofishing_Distances)
  #rename(Electrofishing_Distances, Electrofishing_Distance=river_coords[,5])
  write.csv(Electrofishing_Distances, "C:/Users/rbrown66/OneDrive - University of Toledo/Documents/R/river_distance_dataset.csv", row.names = F)
}
Maumee_Distances<-EFISH_Distance(water_shapefile_name = "Maumee_River", point_shapefile_start = "Maumee_Start", point_shapefile_end = "Maumee_End", plot_title = "Maumee River Electrofishing Points")
Sandusky_Distances<-EFISH_Distance(water_shapefile_name = "Sandusky_River_Line", point_shapefile_start = "Sandusky_Start", point_shapefile_end = "Sandusky_End", plot_title = "Sandusky River Electrofishing Points")
Portage_Distances<-EFISH_Distance(water_shapefile_name = "Portage_River3", point_shapefile_start = "Portage_Start", point_shapefile_end = "Portage_End", plot_title = "Portage River Electrofishing Points")
Black_Distances<-EFISH_Distance(water_shapefile_name = "Black_River", point_shapefile_start = "Black_Start", point_shapefile_end = "Black_End", plot_title = "Black River Electrofishing Points")
Huron_Distances<-EFISH_Distance(water_shapefile_name = "Huron_River", point_shapefile_start = "Huron_Start", point_shapefile_end = "Huron_End", plot_title = "Huron River Electrofishing Points")
Cuyahoga_Distances<-EFISH_Distance(water_shapefile_name = "Cuyahoga_River", point_shapefile_start = "Cuyahoga_Start", point_shapefile_end = "Cuyahoga_End", plot_title = "Cuyahoga River Electrofishing Points")
Vermillion_Distances<-EFISH_Distance(water_shapefile_name = "Vermillion_River", point_shapefile_start = "Vermillion_Start", point_shapefile_end = "Vermillion_End", plot_title = "Vermillion River Electrofishing Points")
Grand_Distances<-EFISH_Distance(water_shapefile_name = "Grand_River", point_shapefile_start = "Grand_Start", point_shapefile_end = "Grand_End", plot_title = "Grand River Electrofishing Points")
Toussaint_Distances<-EFISH_Distance(water_shapefile_name = "Toussaint_River", point_shapefile_start = "Toussaint_Start", point_shapefile_end = "Toussaint_End", plot_title = "Toussaint River Electrofishing Points")
Plum_Distances<-EFISH_Distance(water_shapefile_name = "Plum_Creek", point_shapefile_start = "Plum_Start", point_shapefile_end = "Plum_End", plot_title = "Plum Creek Electrofishing Points")
Trenton_Channel_Distances<-EFISH_Distance(water_shapefile_name = "Trenton_Channel", point_shapefile_start = "Trenton_Channel_Start", point_shapefile_end = "Trenton_Channel_End", plot_title = "Trenton Channel Electrofishing Points")
Huron_River_MI<-EFISH_Distance(water_shapefile_name = "Huron_River_MI", point_shapefile_start = "Huron_MI_Start", point_shapefile_end = "Huron_MI_End", plot_title = "Huron River (MI) Electrofishing Points")
Flint_Distances<-EFISH_Distance(water_shapefile_name = "Flint_River", point_shapefile_start = "Flint_Start", point_shapefile_end = "Flint_End", plot_title = "Flint River Electrofishing Points")
Cass_Distances<-EFISH_Distance(water_shapefile_name = "Cass_River", point_shapefile_start = "Cass_Start", point_shapefile_end = "Cass_End", plot_title = "Cass River Electrofishing Points")




#####################################
####Creating basic stats function####
#####################################
Electrofishing_Basic_Stats<-function(river_dataset)
{
  new_river_dataset<-read.csv(river_dataset)
  new_river_dataset$River_Distance[is.na(new_river_dataset$River_Distance)]<-0
  new_river_dataset$Difference<-new_river_dataset$River_Distance-new_river_dataset$Euclidean_Distance_m
  difference_mean<-mean(new_river_dataset$Difference)
  difference_stdev<-sd(new_river_dataset$Difference)
  print(difference_mean)
  print(difference_stdev)
  lm_efish_distances<-lm(new_river_dataset$River_Distance~new_river_dataset$Euclidean_Distance_m)
  plot(new_river_dataset$River_Distance, new_river_dataset$Euclidean_Distance_m, main="Calculated River Distance vs Straight Line Distance between Electrofishing Points", xlab="River Distance (m)", ylab="Straight Line Distance (m)")
  abline(lm_efish_distances)
  Project_SLD<-ddply(new_river_dataset, .(Project_Re), summarize, mean=mean(Euclidean_Distance_m))
  Project_RD<-ddply(new_river_dataset, .(Project_Re), summarize, mean=mean(River_Distance))
  Project_Difference<-ddply(new_river_dataset, .(Project_Re), summarize, mean=mean(Difference))  
  print(Project_SLD)
  print(Project_RD)
  print(Project_Difference)
  summary(lm_efish_distances)
}
Electrofishing_Basic_Stats("Huron_OH_Distances.csv")
Electrofishing_Basic_Stats("Black_Efish_Distances.csv")
Electrofishing_Basic_Stats("targeted_sandusky_efish_distances.csv")
Electrofishing_Basic_Stats("Sandusky_Efish_Distance_Updated.csv")
Electrofishing_Basic_Stats("Maumee_Efish_Distances.csv")
Electrofishing_Basic_Stats("Portage_Efish_Distance.csv")
Electrofishing_Basic_Stats("Toussaint_Efish_Distance.csv")




###############################################################
####Checking if river distance data is normally distributed####
###############################################################
sandusky_electrofishing_data<-read.csv("Sandusky_Efish_Distance.csv")
p1<-ggplot(sandusky_electrofishing_data, aes(x=River_Distance)) +
  geom_histogram(binwidth = 25, colour="black", fill="White") 
  #stat_function(fun = dnorm, lwd=2, col="red", args = list(mean=mean(sandusky_electrofishing_data$River_Distance), sd=sd(sandusky_electrofishing_data$River_Distance)))
p1

maumee_electrofishing_data<-read.csv("Maumee_Efish_Distances.csv")
p2<-ggplot(maumee_electrofishing_data, aes(x=River_Distance)) +
  geom_histogram(binwidth = 25, colour="black", fill="White") 
p2


###################################
####Histogram of snap distances####
###################################
sample_snaps<-read.csv("Sandusky_Efish_Distance_Updated.csv")
hist(sample_snaps$snapdist, main="Snapping Distance (m)")


topologydots(rivers = Sandusky)
s_up<-cleanup_verts(rivers = Sandusky, startwith = 1)
















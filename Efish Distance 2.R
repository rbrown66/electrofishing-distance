rm(list = ls())
setwd("your working directory")


library(riverdist)
library(sp)
library(rgdal)
library(sf)
library(dplyr)
library(prettymapr)


#Steps 1:4 include code to calculate river distance for one line of data in a dataset. Calculating river distance for an entire dataset is below Step 4.  

#Step 1
#######################################################################################################################
####Importing river line shapefile, outlines do not work with riverdist, it must be a single polyline for the river####
#######################################################################################################################
River<-line2network(layer = "your_river_shapefile", tolerance = 1)

#Step 2
################################################################################################
####Converting xy data to river point locations. Use pointshp2segvert for xy point shapefile####
################################################################################################
River_Start<-pointshp2segvert(path = ".", layer = "your_river_Start", rivers=River)#path= your working directory, layer= shapefile
River_End<-pointshp2segvert(path = ".", layer = "your_river_End", rivers=River)

#Step 3
#########################################
####Placing river points onto a map####
#########################################
riverpoints(River_Start$seg, River_Start$vert, rivers = River, col = "green")
riverpoints(River_End$seg, River_End$vert, rivers = River, col = "red")
title("Sandusky River Electrofishing Start and End Points")

#Step 4
##########################################################################################################
####Determining river distance between two points. Sample code to ensure distances can be determined######
##########################################################################################################
riverdistance(startseg = 59, endseg = 59, startvert = 31, endvert = 18, rivers = River)


#Combining steps 1-4
###################################################################################################################
####Creating a function to determine river distance for entire river dataset and appending distance to dataset#####
###################################################################################################################
EFISH_Distance<-function(water_shapefile_name, point_shapefile_start, point_shapefile_end, plot_title)
{
  River_Name<-line2network(path= ".", layer = water_shapefile_name)
  River_Name2<-splitsegments(rivers = River_Name)#connecting segments that may be unconnected
  River_Name3<-addverts(rivers = River_Name2, mindist = 10)#adding vertices every 10 meters on river line
  windowsFonts(A=windowsFont("Times New Roman"))
  plot(River_Name3, xlab="UTM Zone 17 (m)", ylab="UTM Zone 17 (m)", segmentnum=F, cex.axis=1, cex.lab=1.5, family="A")
  windowsFonts(B=windowsFont("Times New Roman"))
  legend("topleft", legend = c("Start", "End"), pch = c(16, 15), col = (c("Black", "grey")),
         inset = 0.05, title = "Electrofishing Sample Points")
  start_points<-pointshp2segvert(path = ".", layer = point_shapefile_start, rivers = River_Name3)
  end_points<-pointshp2segvert(path = ".", layer = point_shapefile_end, rivers = River_Name3)
  river_start<-riverpoints(start_points$seg, start_points$vert, rivers = River_Name3, col="black", pch = 16)
  river_end<-riverpoints(end_points$seg, end_points$vert, rivers = River_Name3, col = "grey", pch = 15)
  windowsFonts(C=windowsFont("Times New Roman"))
  title(plot_title, cex.main=2, family="C")
  addnortharrow(pos = "topright", scale = 0.5)
  river_coords<-data.frame(start_points$seg, end_points$seg, start_points$vert, end_points$vert)
  for (i in 1:nrow(river_coords)) 
  {
    river_distance<-riverdistance(startseg = river_coords[i,1], endseg = river_coords[i,2], 
          startvert = river_coords[i,3], endvert = river_coords[i,4], rivers = River_Name3, stopiferror = F, 
          algorithm = "Dijkstra")#Djikstra allows for channel braiding
    river_coords[i,5]<-river_distance
  }
  Electrofishing_Distances<-cbind(start_points, river_coords[,5])
  write.csv(Electrofishing_Distances, "C:/R/river_distance_dataset.csv", row.names = F)
}
River_Distances<-EFISH_Distance(water_shapefile_name = "your_river_shapefile", point_shapefile_start = "River_Start", 
          point_shapefile_end = "River_End", plot_title = "River Electrofishing Points")


















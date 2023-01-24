install.packages("gdata")
install.packages("tidyverse")
install.packages("RCurl")
install.packages('geodist')
install.packages("mapview")
install.packages('sf')
install.packages("ggmap")
install.packages("ggrepel")
install.packages("googleway")
install.packages("maptools")
install.packages("sp")
install.packages("ggsn")
install.packages("grid")

###############################################################################

library(ggrepel)
library(gdata)
library(tidyverse)
library(RCurl)
library(geodist)
library(ggmap)
library(mapview)
library(sf)
library(googleway)
library(maptools)
library(ggsn)

###############################################################################
# function to extract and assemble data from the json data obtain from Google API
grabInfo<-function(var){
  print(paste("Variable", var, sep=" "))  
  sapply(xData, function(x) returnData(x, var)) 
}

################################################################################
#set working directory
setwd(choose.dir())

#Register Google key
register_google(key="")#this will be used
                                                              #for the static map
set_key("")  #this will be used 
                                                    # for the interactive map
key=""  # this will be used to request
                                               #travel time and distance info.

#read the data files
geoData<-read.csv("samplepoints50.csv",header = TRUE,stringsAsFactors = FALSE)

# Create a for loop to make calls to Google API for travel time and
#travel distance by car data
origin<-list()
destination<-list()

for (row in 1:nrow(geoData)){
  origin<-c(origin,geoData[row,"Source_Address"])
  destination<-c(destination, geoData[row,"Target_Address"])
  orig<-chartr(" ","+$",origin)
  destin<-chartr(" ","+$",destination)
  origin1<-as.list(orig)
  destination1<-as.list(destin)
  url.mapq<-paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial&origins=",
                   origin1,"&destinations=",destination1,"&key=",key)
  xData<-getURL(url.mapq)
}

# extracting and assembling data from Google API using the above function
fmDataDF<-data.frame(sapply(1:2, grabInfo), stringsAsFactors=FALSE)


# Extract only the relevant information from the data frame
# create a sequence
sequen<-c(1:71)

fieldNames<-as.character(sequen)

# separate the data using the sequence created above as field names

sepp<-separate(fmDataDF,X1,into=fieldNames,sep="([:}{\"])")

#Convert the data back to data frame

processedData<-data.frame(Source_add=sepp$'6', Target_add=sepp$'11',
                          Distance_miles=sepp$'28',
                          Distance_meters=sepp$'32',
                          Ttime_mins=sepp$'41',Ttime_sec=sepp$'45')

View(processedData)

#calculate Euclidean Distance using geodesic
#Obtain origin and destination from geoData and convert it into matrix

or<-matrix(c(geoData$Source_Ycoord,geoData$Source_Xcoord),ncol= 2)
colnames(or)<-c("long","lat")

dis<-matrix(c(geoData$Target_Ycoord,geoData$Target_Xcoord),ncol=2)
colnames(dis)<-c("long","lat")

#use geodesic to calculate Euclidean Distance
processedData$EUC_meters<-geodist(or,dis,
                                  paired = TRUE,
                                  sequential = FALSE,
                                  pad = FALSE,
                                  measure = "geodesic"
)
View(processedData)

###############################################################################
#scatter plot comparing Euclidean Distance vs Distance travel by car
# Using ggplot to create a scatter plot with a trend line
#Convert Euclidean Distance and Distance Traveled by Car from meters to miles

ggplot(data=processedData, mapping =aes(x=EUC_meters/1609.34,
                                        y=as.numeric(Distance_meters)/1609.34))+
  geom_point()+geom_smooth(method = lm, color="red",fill="#69b3a2",se=TRUE)+
  labs(title="Distance treveled by car vs Euclidean Distance",
       x="Euclidean Distance in miles",y="Distance traveled by car in miles")+
  xlim(1,36) + ylim(1,36)

###############################################################################
# Creating a static map

#obtain the static map from Google API
bw_map<-get_googlemap('atlanta,georgia',zoom = 11,maptype="roadmap",color="bw",
                      style = "feature:road|visibility:off&
                      style=element:labels|visibility:off&
                      style=feature:administrative|visibility:on")

#read the shapefile containing Atlanta city boundary
shapefile<-rgdal::readOGR('Atlanta.shp')
data<-fortify(shapefile)


#convert lon and lat to numeric
geoData$X<-as.numeric(geoData$Source_Xcoord)
geoData$y<-as.numeric(geoData$Source_Ycoord)
geoData$Xend<-as.numeric(geoData$Target_Xcoord)
geoData$yend<-as.numeric(geoData$Target_Ycoord)

# use ggmap to plot the static map,the shapefile,points and connections between
#the points
ggmap(bw_map,extent = "device")+
  geom_polygon(aes(x = long, y = lat, group = group), data = data,
               colour = 'gray', fill = 'lightgreen', alpha = .4, size = .3)+
  geom_leg(aes(x = y,y = X,xend = yend,yend = Xend,fill=y),
           alpha = 0.5,size=1,data = geoData)+
  geom_point(data=geoData, aes(x=y,y=X,color="Origin"),size=1.5)+
  geom_point(data=geoData, aes(x=yend,y=Xend,color="Destination"),size=1.5)+
  scale_color_manual(values=c(Origin="red",Destination="purple"))+
  labs(title = "Euclidean Distance (Origin and Destination)",size="Letters",
       color=NULL)+
  guides(color=guide_legend(override.aes = list(size=2)))
  

################################################################################
#Create interactive map using Google API
#read the data file

samplePoints<-read.csv("samplepoints.csv",header = TRUE,stringsAsFactors = FALSE)


#obtain driving directions from google API

a<-google_directions(origin = c(samplePoints$Source_Xcoord[1],
                                samplePoints$Source_Ycoord[1]),
                     destination =c(samplePoints$Target_Xcoord[1],
                                    samplePoints$Target_Ycoord[1]),
                     mode = "driving")

b<-google_directions(origin = c(samplePoints$Source_Xcoord[2],
                                samplePoints$Source_Ycoord[2]),
                     destination =c(samplePoints$Target_Xcoord[2],
                                    samplePoints$Target_Ycoord[2]),
                     mode = "driving")

c<-google_directions(origin = c(samplePoints$Source_Xcoord[3],
                                samplePoints$Source_Ycoord[3]),
                     destination =c(samplePoints$Target_Xcoord[3],
                                    samplePoints$Target_Ycoord[3]),
                     mode = "driving")
#extract the necessary route data
pl<-direction_polyline(a)
pl1<-direction_polyline(b)
pl2<-direction_polyline(c)

df<-data.frame(polyline=pl)
df1<-data.frame(polyline=pl1)
df2<-data.frame(polyline=pl2)

#add lon & lat for easy recognition by Google_map function
geoData1$lat<-paste(geoData1$Source_Xcoord)
geoData1$lon<-paste(geoData1$Source_Ycoord)
lon<-paste(geoData1$Target_Ycoord)
lat<-paste(geoData1$Target_Xcoord)

#convert it to a data frame
data1<-cbind(lon,lat)%>% 
  as.data.frame()

# create an interactive map with routes from origin to destination
google_map(key = key) %>%
  add_polylines(data = df, polyline = "polyline", stroke_weight = 5)%>%
  add_polylines(data = df1,polyline = "polyline", stroke_weight = 5)%>%
  add_polylines(data = df2,polyline = "polyline", stroke_weight = 5)%>%
  add_markers(data = geoData1)%>%
  add_markers(data=data1)

#############################################################################
# create a static map to compare euclidean distance and distance traveled by car for 
#selected coordinates

#extract lon lat from polylines above
route1<-decode_pl(pl)
route2<-decode_pl(pl1)
route3<-decode_pl(pl2)

#convert source and target coordinates to numeric

geoData1$X<-as.numeric(geoData1$Source_Xcoord)
geoData1$y<-as.numeric(geoData1$Source_Ycoord)
geoData1$Xend<-as.numeric(geoData1$Target_Xcoord)
geoData1$yend<-as.numeric(geoData1$Target_Ycoord)


#Plot Euclidean Distance vs Distance Travel by car
ggmap(bw_map,extent = "device")+
  geom_polygon(aes(x = long, y = lat, group = group), data = data,
               colour = 'gray', fill = 'white', alpha = .4, size = .3)+
  geom_path(aes(x=lon,y=lat),data=route1)+
  geom_path(aes(x=lon,y=lat),data=route2)+
  geom_path(aes(x=lon,y=lat),data=route3)+
  geom_point(aes(x=Source_Ycoord,y=Source_Xcoord,shape="Origin"), 
             data=geoData1,size=2)+
  geom_point(aes(x=Target_Ycoord,y=Target_Xcoord,shape="Destination"),
             data=geoData1,size=2)+
  geom_leg(aes(x = y,y = X,xend = yend,yend = Xend,color="EUC"),
           alpha = 0.5,size=1,data = geoData1)+
  scale_color_manual(values=c(EUC="purple"))+
  labs(title = "Euclidean Distance vs Distance Travel by car",
       size="Letters",color=NULL)+
  guides(color=guide_legend(override.aes = list(size=2)))

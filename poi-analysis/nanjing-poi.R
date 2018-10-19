library(sf)
library(openxlsx)
library(Rgctc2)
library(maptools)
library(ggplot2)
library(dplyr)
library(splancs)
#input
setwd('F:/Administrator/Documents/GitHub/Nanjing-POI')
nj.railroad<-st_read('F:/Administrator/Documents/R/Map/十个城市的轨道交通站点和线路数据/十个城市的轨道交通站点和线路数据/南京/南京_轨道线路数据(2016).shp')
nj.re<-openxlsx::read.xlsx ('F:/Administrator/Documents/R/Map/南京POI数据_262000+高德地图_火星坐标系_13个标签/住宿_5900+.xlsx')
jiangsu<-st_read('F:/Administrator/Documents/Mapproject/shinodata.shp')
nj.frame<-jiangsu[13,]

#geocoordination transformation 
nj.re$lng_wgs84<-gcj02_wgs84_lng(nj.re$lng,nj.re$lat)
nj.re$lat_wgs84<-gcj02_wgs84_lat(nj.re$lng,nj.re$lat)

#format transformation
for(i in 1:dim(nj.re)[1]){
  nj.re$geo[[i]]<-st_point(c(nj.re$lng_wgs84[i],nj.re$lat_wgs84[i]),dim = 'XY')
}
 nj.re$geo<- st_sfc(nj.re$geo,crs = 4326)

st_sf(nj.re) %>%  as('Spatial') %>% writeSpatialShape("nj_resp2.shp") 
nj.re.sf<- st_read('nj_resp2.shp')
st_crs(nj.re.sf)<- 4326

#kernal analyse
#set analysis frame rectangular
poly<- as.points( 
                 c(max(nj.re.sf$lng_wgs84),max(nj.re.sf$lng_wgs84),
                   min(nj.re.sf$lng_wgs84),min(nj.re.sf$lng_wgs84)),
                 c(min(nj.re.sf$lat_wgs84),max(nj.re.sf$lat_wgs84),
                  max(nj.re.sf$lat_wgs84),min(nj.re.sf$lat_wgs84))
                )

#set parameter
cellsize = c(0.005,0.005)
xymin = c(min(nj.re.sf$lng_wgs84),min(nj.re.sf$lat_wgs84))
xymax = c(max(nj.re.sf$lng_wgs84),max(nj.re.sf$lat_wgs84))
cdim = (xymax - xymin)/cellsize

#setup a grid
grdx <- GridTopology(xymin,cellsize=cellsize,cells.dim=cdim)
sp_point <- matrix(NA, nrow=length(nj.re.sf$lng_wgs84),ncol=2)
sp_point[,1] <- nj.re.sf$lng_wgs84
sp_point[,2] <- nj.re.sf$lat_wgs84
colnames(sp_point) <- c("X","Y")
sp_points <- SpatialPoints(coords=sp_point, proj4string=CRS("+proj=longlat +datum=WGS84"))
sp_points@coords
kernel1 <-spkernel2d(sp_points,poly=poly, h0=4, grd=grdx)%>% data.frame()
SG <- SpatialGridDataFrame(grdx, data=kernel1)
p<-spplot(SG)
t<-ggplot() + geom_sf(data = nj.re.sf) + geom_sf(data = nj.railroad,colour = 'red') +coord_sf()
p
ggplot(nj.frame)+geom_sf()

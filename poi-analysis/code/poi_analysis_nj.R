library(pacman)
p_load(sf,raster,tidyverse,spatstat,maptools)

#data input
nj_owin=readRDS('C:/Users/zhouq/Documents/GitHub/data/data_universal/geometry_of_js_highrate_jq/nj_district_owin.rds')
nj_poi=readRDS('C:/Users/zhouq/Documents/GitHub/poi_data/city/nj_entity_poi.rds') %>% st_transform(4509)
jq_poly=readRDS('C:/Users/zhouq/Documents/GitHub/data/data_universal/geometry_of_js_highrate_jq/jq_polygon.rds')  %>% st_transform(4509)
nj_sf=readRDS('~/GitHub/data/data_universal/geometry_of_js_highrate_jq/nj_district_sf.rds') %>% st_transform(4509)
x=st_intersects(nj_poi,nj_sf) 
y=lapply(x,length) %>% unlist %>% sapply(is.logical)
poi=nj_poi[y==1,]
#make ppp files
nj_owin=nj_sf %>% as('Spatial') %>% as('SpatialPolygons') %>% as('owin')
poi_coord=st_coordinates(nj_poi)
poi_ppp=ppp(x=poi_coord[,1],y=poi_coord[,2],window=nj_owin,marks = nj_poi$typecode)
rm(poi_ppp)


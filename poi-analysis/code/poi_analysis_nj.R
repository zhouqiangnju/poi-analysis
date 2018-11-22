library(pacman)
p_load(sf,raster,tidyverse,spatstat,maptools,foreach)
library(tmap)
#data input

nj_poi=readRDS('~/GitHub/data/poi_analysis/nj_entity_poi(type).rds') %>% st_transform(4509)
jq_poly=readRDS('~/GitHub/data/data_universal/geometry_of_js_highrate_jq/jq_polygon.rds')  %>% st_transform(4509)
nj_sf=readRDS('~/GitHub/data/poi_analysis/nj_district_sf.rds') %>% st_transform(4509)
x=st_intersects(nj_poi,nj_sf)%>%lapply(length) %>% unlist
poi=nj_poi[x==1,]
nj_jq=filter(jq_poly,City=='NJ')
nj_jq_ppp=foreach(i=1:nrow(nj_jq)) %do% {as(nj_jq[i,],'Spatial') %>% as('SpatialPolygons') %>% as('owin')}
nj_jq_tess=tess(tiles=nj_jq_ppp)
as(nj_jq[1,],'Spatial') %>% as('SpatialPolygons') %>% as('owin')

#make ppp files
#make owin for whole city of nanjing
nj_city_owin=nj_sf %>% as('Spatial') %>% as('SpatialPolygons') %>% as('owin')
#make tess files from  owin of every districts of nanjing 
nj_district = list()

for(i in 1:nrow(nj_sf))
{ nj_district[[i]]=
   as(nj_sf[i,],'Spatial') %>% as('SpatialPolygons') %>% as('owin')}
nj_district_owin = nj_district%>%tess(tiles=. )
#make ppp file for poi
poi_coord=st_coordinates(poi)
marks_df=dplyr::select(poi,adcode,maintype,subtletype) 
st_geometry(marks_df)=NULL
poi_ppp=ppp(x=poi_coord[,1],y=poi_coord[,2],window=nj_city_owin,marks = marks_df)

test=sample(poi$maintype,200) 
t=factor(test,levels = c('010000','020000','030000','040000','050000','060000','070000','080000','090000',
                         '100000','110000','120000','130000','140000','150000','160000','170000','180000'))
t
# new data input

poi$maintype=str_extract(poi$type1,'^\\d{2}') %>% paste0('0000')
poi$subtletype=str_extract(poi$type2,'^\\d{2}') %>% paste0('0000')
poi$subtletype=factor(poi$subtletype,levels=c('010000','020000','030000','040000','050000','060000','070000','080000','090000',
                                          '100000','110000','120000','130000','140000','150000','160000','170000','180000',NA),exclude = NULL)
poi$maintype %>% class
table(poi$subtletype)
x=poi[poi$maintype=='010000',]

jq_poly=readRDS('~/GitHub/data/data_universal/geometry_of_js_highrate_jq/jq_polygon.rds')  %>% st_transform(4509)
nj_sf=readRDS('~/GitHub/data/poi_analysis/nj_district_sf.rds') %>% st_transform(4509)
poi=readRDS('F:/Administrator/Documents/GitHub/poi-analysis/poi-analysis/poi_for_ppp.rds')
poi_ppp=readRDS('F:/Administrator/Documents/GitHub/poi-analysis/poi-analysis/nj_poi_ppp.rds')
quadratcount.ppp(poi_ppp,tess=nj_jq_tess)
poi_ppp$marks$adcode=factor(poi_ppp$marks$adcode)
summary(poi_ppp)
saveRDS(poi,'poi_for_ppp.rds')
saveRDS(poi_ppp,'nj_poi_ppp.rds')

#
unitname(poi_ppp)='meter'
summary(poi_ppp)
den=density(poi_ppp,sigma=70)
plot(den)
plot(poi_ppp,add=TRUE)
dev.off()
persp(den,theta=30)
?persp
?spatstat::persp.im()
contour(den,axes=F)
aden=adaptive.density(poi_ppp,f=0.01,nrep=10)

library(pacman)
p_load(sf,raster,tidyverse,spatstat)
gc()
rm(night)
#data input 
setwd()
nj_poi=readRDS('C:/Users/zhouq/Documents/GitHub/poi_data/city/nj_entity_poi.rds')
nj=readRDS('~/GitHub/data/data_universal/geometry_of_js_highrate_jq/nj_district_sf.rds')
nj_owin=readRDS('C:/Users/zhouq/Documents/GitHub/data/data_universal/geometry_of_js_highrate_jq/nj_district_owin.rds')

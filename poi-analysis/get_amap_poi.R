#get amap poi
library(pacman)
p_load(tidyverse,sf,rlist,httr,jsonlite)
library(Rgctc2,lib.loc='~/GitHub/R_coordination_transformation')
options(digits=11)
#1 get search bound
setwd('F:/Administrator/Documents/R/Mapproject/JSframe')

get_admin_geo= function(address){
  
  address='南京'
  key = '7c6b6c0d1b641f4aa9cdb7d2229ae728'
  url = 'http://restapi.amap.com/v3/config/district?' %>%
    paste('keywords=' , address ,
          '&key=' ,key ,
          '&subdistrict=3' ,
          '&extensions=all',
          sep = '')
  city<-GET(url)%>% content(as="text",encoding="UTF-8") %>% fromJSON(flatten = TRUE)%>%
        magrittr::extract2('districts')
  #extract information
  city_geo=dplyr::select(city,-1)
  town_info=city %>% select(districts) %>% magrittr::extract2(1) %>% magrittr::extract2(1)
            
  #tidy and transform to sf object
  city_geo$geometry=city_geo%>% magrittr::extract('polyline') %>% str_split('\\|') %>% 
            lapply(str_split,';')%>% '[['(1)%>%
            lapply(str_split,',') %>% lapply(lapply,as.numeric) %>% 
            lapply(list.rbind) %>% lapply(list) %>% st_multipolygon %>% st_sfc()
 city_geo=city_geo%>% dplyr:: %>% st_as_sf
  
  return(city_geo)
}
nj_amap_sf=get_admin_geo('南京')

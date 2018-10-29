library(pacman)
p_load(tidyverse,sf,rlist,httr,jsonlite,magrittr,foreach,rlist,tcltk)

library(Rgctc2,lib.loc='~/GitHub/R_coordination_transformation')
options(digits=11)
get_admin_geo_enhanced= function(city){

 get_raw_info=function(city) {
   key = '7c6b6c0d1b641f4aa9cdb7d2229ae728'
   url = 'http://restapi.amap.com/v3/config/district?' %>%
          paste('keywords=' , city ,
          '&key=' ,key ,
          '&subdistrict=3' ,
          '&extensions=all',
          sep = '')
    GET(url)%>% content(as="text",encoding="UTF-8") %>% 
    fromJSON(flatten = TRUE)%>%
    magrittr::extract2('districts')
 }
 
  #extract information
  admin_geo      = get_raw_info(city)%>%dplyr::select(-'districts')
  sub_admin_geo  = get_raw_info(city) %>% dplyr::select(districts) %>% 
    magrittr::extract2(1) %>% 
    magrittr::extract2(1)
  sub_admin_adcode=sub_admin_geo %>% dplyr::select(adcode) %>%t%>% as.character()
  #tidy and transform to sf object
  #make_admin_geo
  make_admin_geo=function(admin_geo){
    geometry=admin_geo%>% magrittr::extract('polyline') %>% str_split('\\|') %>% 
    lapply(str_split,';')%>% '[['(1)%>%
    lapply(str_split,',') %>% lapply(lapply,as.numeric) %>% 
    lapply(list.rbind) 
  admin_geo$geometry_wgs84=geometry%>%lapply(gcj02_wgs84_matrix_matrix)  %>% 
    lapply(list) %>% st_multipolygon %>% st_sfc(crs=4326)
  admin_geo$geometry_amap=geometry %>% lapply(list) %>% 
    st_multipolygon %>% st_sfc
  admin_geo=admin_geo%>% dplyr::select(-'polyline') %>% st_as_sf(sf_column_name='geometry_wgs84')
  return(admin_geo)
  }
  
  #sub_admin_geo
  make_sub_admin_geo=function(sub_admin_geo){
  sub_admin_district=sub_admin_geo %>% 
    magrittr::extract('districts') %>% 
    magrittr::extract2(1)%>% list.rbind
  sub_admin_geo=sub_admin_geo %>%rbind(sub_admin_district)%>% 
    dplyr::select(-'districts') 
  
  center=sub_admin_geo$center %>% str_split(',') %>% 
    list.rbind %>% apply(2,as.numeric)
  center_wgs84=center %>% 
    gcj02_wgs84_matrix_matrix
  sub_admin_geo$center_amap      = map2(center[,1],center[,2],
                                   function(x1,x2){c(x1,x2)%>%st_point}) %>% st_sfc
  sub_admin_geo$center_wgs84= map2(center_wgs84[,1],center_wgs84[,2],
                                   function(x1,x2){c(x1,x2)%>% st_point}) %>% st_sfc(crs=4326)
  sub_admin_geo=st_as_sf(sub_admin_geo,sf_column_name = 'center_wgs84')
  return(sub_admin_geo)
  }
  
  #sub_admin_poly

    sub_admin_poly=lapply(sub_admin_adcode,get_raw_info) %>% list.rbind %>%
                   dplyr::select(-'districts')
    sub_admin_poly=foreach::foreach(i=1:nrow(sub_admin_poly),.combine = rbind) %do% 
                   (sub_admin_poly[i,]%>%make_admin_geo)
   
   
  #combine data
  admin_new=list(make_admin_geo(admin_geo),make_sub_admin_geo(sub_admin_geo),sub_admin_poly)
  names(admin_new)=c('admin_geo','sub_admin_geo','sub_admin_poly')
  return(admin_new)
}

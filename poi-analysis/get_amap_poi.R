#get amap poi
library(pacman)
p_load(tidyverse,sf,rlist,httr,jsonlite,tmap,tmaptools,magrittr)
library(Rgctc2,lib.loc='~/GitHub/R_coordination_transformation')
options(digits=11)
#1 get search bound
getwd()

get_admin_geo= function(address){
  key = '7c6b6c0d1b641f4aa9cdb7d2229ae728'
  url = 'http://restapi.amap.com/v3/config/district?' %>%
        paste('keywords=' , address ,
          '&key=' ,key ,
          '&subdistrict=3' ,
          '&extensions=all',
          sep = '')
  admin=GET(url)%>% content(as="text",encoding="UTF-8") %>% 
        fromJSON(flatten = TRUE)%>%
        magrittr::extract2('districts')
  #extract information
  admin_geo      = dplyr::select(admin,-'districts')
  sub_admin_geo = admin %>% select(districts) %>% 
                  magrittr::extract2(1) %>% 
                  magrittr::extract2(1)
            
  #tidy and transform to sf object
  #admin geo
    geometry=admin_geo%>% magrittr::extract('polyline') %>% str_split('\\|') %>% 
            lapply(str_split,';')%>% '[['(1)%>%
            lapply(str_split,',') %>% lapply(lapply,as.numeric) %>% 
            lapply(list.rbind) 
    admin_geo$geometry_wgs84=geometry%>%lapply(gcj02_wgs84_matrix_matrix)  %>% 
                         lapply(list) %>% st_multipolygon %>% st_sfc(crs=4326)
    admin_geo$geometry_amap=geometry %>% lapply(list) %>% 
                           st_multipolygon %>% st_sfc
    admin_geo=admin_geo%>% dplyr::select(-'polyline') %>% st_as_sf(sf_column_name='geometry_wgs84')
 #sub_admin_geo
    sub_admin_district=sub_admin_geo %>% 
                       magrittr::extract('districts') %>% 
                       magrittr::extract2(1)%>% list.rbind
    sub_admin_geo=sub_admin_geo %>%rbind(sub_admin_district)%>% 
                  dplyr::select(-'districts') 
    
      center=sub_admin_geo$center %>% str_split(',') %>% 
        list.rbind %>% apply(2,as.numeric)
      center_wgs84=center %>% gcj02_wgs84_matrix_matrix
      
      sub_admin_geo$center      = map2(center[,1],center[,2],
                                       function(x1,x2){c(x1,x2)%>%st_point}) %>% st_sfc
      sub_admin_geo$center_wgs84= map2(center_wgs84[,1],center_wgs84[,2],
                                      function(x1,x2){c(x1,x2)%>% st_point}) %>% st_sfc(crs=4326)
      sub_admin_geo=st_as_sf(sub_admin_geo,sf_column_name = 'center_wgs84')
    admin_new=list(admin_geo,sub_admin_geo)
    names(admin_new)=c('admin_geo','sub_admin_geo')
    return(admin_new)
}
nj=get_admin_geo('南京')

chn=get_admin_geo('中国')
chn_sf=chn[[1]] 
chn_sf$geometry_wgs84 %>% plot
province=chn[[2]]
adcode=province$adcode
province=get_admin_geo(adcode[1]) %>% magrittr::extract2(1)
for( i in 2:length(adcode)){
  
 
  new_province=get_admin_geo(adcode[i]) %>% magrittr::extract2(1)

  province=rbind(province,new_province)
}

saveRDS(province,'Province.rds')

#make grid
#1 get grid within city region
nj_sf=nj$admin_geo
st_geometry(nj_sf)
grid_intersects_admin =function(admin_sf){
  #the number of grid is 100 by default
 
  admin_grid=st_make_grid(admin_sf) %>% st_sf(id=1:100,geometry=.) 
  admin_grid_within=admin_grid%>% 
                    st_intersects(admin_sf)%>% sapply(length)%>% 
                    as.logical%>% magrittr::extract(admin_grid,.,)
}
nj_grid=grid_intersects_admin(nj_sf)
#2 get dialog corner point coordinate of a grid 

get_grid_diag_coord=function(grid){
  grid_coord=grid %>%st_coordinates() %>% 
  magrittr::extract(,c(1,2))
  paste0(grid_coord[4,1],',',grid_coord[4,2],'|',grid_coord[2,1],',',grid_coord[2,2])
  
}
grid_coord_matrix=sapply(nj_grid$geometry,grid_diag_coord)
#function to get poi information
get_poi=function(polygon,type,npage){
  #
  key1='7a10a7d758402ca05b54c6badc1f0f18'
  key0='7c6b6c0d1b641f4aa9cdb7d2229ae728'
  #
  grid_diag_coord=get_grid_diag_coord(polygon)#polygon must be a rectangular sf object
  
  url='https://restapi.amap.com/v3/place/polygon?' %>%
       paste0(
         'key=',key1,
         '&polygon=',grid_diag_coord,
         '&keywords=',
         '&types=',type,
         '&offset=20',
         '&page=',npage,
         '&output=json',
         '&extensions=all'
       )
  poi=GET(url)%>% content(as="text",encoding="UTF-8") %>% fromJSON(flatten = TRUE)  %>%
      magrittr::extract('pois') %>% '[['(1)%>% 
      dplyr::select(1,3,5,6,7,8,9,14,16,17,18,19,25)

  #poi information extract and uniform
  return(poi)
}

#derterminants for if a grid's poi number of a certain type exceeds 20 page volume
grid_poi=sapply(nj_grid$geometry,get_poi,'050000',20)
         
  
poi_n_page=function(grid,type,npage){
  
  tryCatch({get_poi(grid,type,npage) %>% nrow
  },error=function(e){
             paste0('error')
           }
           )
}
x=sapply(nj_grid$geometry,poi_n_page,'050000',20)
#
get_grid_poi=function(grid,type){
  
   grid_poi=get_poi(grid,type,1)
   i=2
   while(poi_n_page(grid,type,i)!='error'){
   grid_poi_new=get_poi(grid,type,i)
   grid_poi=rbind(grid_poi,grid_poi_new)
   i=i+1
   }
   return(grid_poi)
}
x=get_grid_poi(nj_grid[4,],'050000')
nj_grid$p20=x
nj_poi1=get_poi(nj_grid[1,],'050000',3) %>% nrow

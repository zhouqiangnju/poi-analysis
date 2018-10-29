#get amap poi

get_city_poi=function(city,type=1,...){
#set up work environment  
library(pacman)
p_load(tidyverse,sf,rlist,httr,jsonlite,magrittr,foreach,rlist,tcltk)

library(Rgctc2,lib.loc='~/GitHub/R_coordination_transformation')
options(digits=11)

path='~/GitHub/poi-analysis'
setwd(path)
if(city %in% list.files()) {
  setwd(paste0(path,'\\',city))}else {
    dir.create(paste0(path,'\\',city))
      setwd(paste0(path,'\\',city))
  }
#

# choose one of 3 types
type_name=c('entity','automobile','info_facilities')
type=type_name[type]
if(!(type %in% list.files())){
  dir.create(paste0(getwd(),'\\',type))
  
}
paste0(getwd(),'\\',type) %>% setwd()

entity=c('050000','060000','070000','080000','090000','100000','110000','120000','130000','140000','150000','160000','170000')
automobile=c('010000','020000','030000','040000')
info_facilities=c('180000','190000','200000','220000','970000','990000')
type=switch(type,
       'entity'=entity,
       'automobile'=automobile,
       'info_facilities'=info_facilities)

#define functions
#1 get admin spatial information (boundry,center,sub_admin information)
get_admin_geo= function(city){

  key = '7c6b6c0d1b641f4aa9cdb7d2229ae728'
  url = 'http://restapi.amap.com/v3/config/district?' %>%
        paste('keywords=' , city ,
          '&key=' ,key ,
          '&subdistrict=3' ,
          '&extensions=all',
          sep = '')
  admin=GET(url)%>% content(as="text",encoding="UTF-8") %>% 
        fromJSON(flatten = TRUE)%>%
        magrittr::extract2('districts')
  #extract information
  admin_geo      = dplyr::select(admin,-'districts')
  sub_admin_geo = admin %>% dplyr::select(districts) %>% 
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
      center_wgs84=center %>% 
                   gcj02_wgs84_matrix_matrix
      sub_admin_geo$center      = map2(center[,1],center[,2],
                                       function(x1,x2){c(x1,x2)%>%st_point}) %>% st_sfc
      sub_admin_geo$center_wgs84= map2(center_wgs84[,1],center_wgs84[,2],
                                      function(x1,x2){c(x1,x2)%>% st_point}) %>% st_sfc(crs=4326)
      sub_admin_geo=st_as_sf(sub_admin_geo,sf_column_name = 'center_wgs84')
    admin_new=list(admin_geo,sub_admin_geo)
    names(admin_new)=c('admin_geo','sub_admin_geo')
    return(admin_new)
}


#2 grid_spatial related functions
#2.1 get grid within city region

grid_intersects_admin =function(admin_sf){
  #the number of grid is 100 by default
 
  admin_grid=st_make_grid(admin_sf) %>% st_sf(id=1:100,geometry=.) 
  admin_grid_within=admin_grid%>% 
                    st_intersects(admin_sf)%>% sapply(length)%>% 
                    as.logical%>% magrittr::extract(admin_grid,.,)
}

#2.2 get dialog corner point coordinate of a grid 

get_grid_diag_coord=function(grid){
  grid_coord=grid %>%st_coordinates() %>% 
  magrittr::extract(,c(1,2))
  paste0(grid_coord[4,1],',',grid_coord[4,2],'|',grid_coord[2,1],',',grid_coord[2,2])
  
}

#3 functions to get poi information
#3.1
get_poi=function(polygon,type,npage){
  #
  key1='7a10a7d758402ca05b54c6badc1f0f18'
  key0='7c6b6c0d1b641f4aa9cdb7d2229ae728'
  #
  grid_diag_coord=get_grid_diag_coord(polygon)#polygon must be a rectangular sf  object
  
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
  #make it sf
  location=poi$location %>% str_split(',') %>% 
    list.rbind %>% apply(2,as.numeric)
  location_wgs84=location %>% 
    gcj02_wgs84_matrix_matrix
    poi$geometry      = map2(location[,1],location[,2],
                                   function(x1,x2){c(x1,x2)%>%st_point}) %>% st_sfc
    poi$geometry_wgs84= map2(location_wgs84[,1],location_wgs84[,2],
                                   function(x1,x2){c(x1,x2)%>% st_point}) %>% st_sfc(crs=4326)
    poi=st_as_sf(poi,sf_column_name = 'geometry_wgs84')

  #poi information extract and uniform
  return(poi)
}
#3.2 
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

#3.3
get_poi_debug=function(polygon,type,npage){
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
  poi=GET(url)%>% content(as="text",encoding="UTF-8") %>% fromJSON(flatten = TRUE) 
  return(poi)
}

#derterminants for if a grid's poi number of a certain type exceeds 20 page volume

#
poi_n_page=function(grid,type,npage){
  
    tryCatch({get_poi(grid,type,npage) %>% nrow
    },error=function(e){
             paste0('error')
           }
           )
}

# divide grid into 4 piece and get poi for each piece
make_sub_grid=function(grid,type){
  sub_grid=grid%>%st_make_grid(n=c(2,2))%>% st_sf(id=paste0(grid$id,1:4),geometry=.)
  sub_grid$n_p1=sapply(sub_grid$geometry,poi_n_page,type,1)
  sub_grid$n_p20=sapply(sub_grid$geometry,poi_n_page,type,20)
  
  return(sub_grid)
  
}

get_sub_grid=function(grid,type){
 
  sub_grid=make_sub_grid(grid,type)
  sub_grid_new=sub_grid[0,]
  
  while(length(grep(20,sub_grid$n_p20))!=0){
    for(i in grep(20,sub_grid$n_p20)){
      sub_grid_new=sub_grid[i,]%>%make_sub_grid(type) %>% rbind(sub_grid_new,.)
      }
    #sub_grid=sub_grid[-grep(20,sub_grid$n_p20),]%>%rbind(sub_grid_new)
    sub_grid=sub_grid[-grep(20,sub_grid$n_p20),]
    sub_grid=rbind(sub_grid,sub_grid_new)
    sub_grid_new=sub_grid[0,]
  }
  return(sub_grid)
                        
}
#
get_valid_grid=function(admin_sf_amap,type){

  admin_grid=grid_intersects_admin(admin_sf_amap) #the coordinate system of admin_sf must be GCJ02
  admin_grid$n_p1=sapply(admin_grid$geometry,poi_n_page,type,1) #the poi number of page1
  admin_grid$n_p20=sapply(admin_grid$geometry,poi_n_page,type,20) #the poi number of page20
  admin_grid_sub=filter(admin_grid,n_p20==20)
  if (nrow(admin_grid_sub)>0){
  admin_grid_sub_valid=foreach(i=1:nrow(admin_grid_sub),.combine = rbind) %do%  get_sub_grid(admin_grid_sub[i,],type)
  
  admin_grid_valid=filter(admin_grid,n_p20!=20)%>%
                   rbind(admin_grid_sub_valid) %>% 
                   filter(n_p1!='error')
  }else admin_grid_valid=admin_grid %>%filter(n_p1!='error') 
  
  admin_grid_valid= admin_grid_valid %>% st_intersects(admin_sf_amap) %>% 
                    sapply(length) %>% as.logical %>% magrittr::extract(admin_grid_valid,.,)
  return(admin_grid_valid)
}

#
  get_type_poi=function(admin_grid_valid,admin_sf_amap,type){
  
  poi_type= map(admin_grid_valid$geometry,get_grid_poi,type) %>%
            list.rbind
  poi_type= st_as_sf(poi_type,geometry=geometry)
  poi_type= st_intersects(poi_type,admin_sf_amap) %>% 
            sapply(length)%>% 
            as.logical%>% magrittr::extract(poi_type,.,)
}

   
    admin_sf=get_admin_geo(city)
    admin_sf=admin_sf$admin_geo %>% st_as_sf(geometry=geometry_amap)
    
    d=(list.files() %>% length())+1
    #此为一简易判断函数，仅在以type为名的文件夹中没有文件或仅包含由
    #本程序此前下载的数据时方为正确，因为本函数只以当前文件夹中的文件个数
    #作为其后程序运行的起点，并不判断当前文件夹中的文件的性质。如文件夹中包含无关文件，也将被视作有效文件。
 
      
    pb <- tkProgressBar("进度","已完成 %", 0, 100)
    for(j in d:length(type)){
    info<- sprintf("已完成 %d%%", round(j*100/length(type)))
    setTkProgressBar(pb, j*100/length(type), sprintf("进度 (%s)", info),info)
  city_grid_valid=get_valid_grid(admin_sf,type[j])
  city_poi_type=get_type_poi(city_grid_valid,admin_sf,type[j])
  saveRDS(city_poi_type,paste0('poi_',type[j],'.rds'))
  Sys.sleep(10)}
  close(pb)
}
get_city_poi('宿迁')

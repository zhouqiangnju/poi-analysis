library(pacman)
library(fpc)
p_load(sf,tidyverse,tmap)

s=filter(nj_entity_poi,grepl('狮子桥',name)) 
s =s %>%st_transform(crs=4509)

nj_sf=nj_district_sf %>% st_transform(crs=4509)
gl=filter(nj_sf,name=='鼓楼区')
  tm_shape(gl)+tm_polygons()
    tm_shape(s)+tm_dots()
co=st_coordinates(s) %>% as.data.frame()
#add id to trace back to original sf point
co$id_o=1:nrow(co)
x=duplicated(co[,1:2])
co=co[!x,]
#dbscan
db=dbscan(co[,1:2],eps=100)
co_db=co[db$isseed,]
co_db_sf=st_as_sf(co_db,coords = c('X','Y'),crs=4509)
#minimum Convex Hull
p=co_db[which.min(co_db$Y),] #initial p as the start point

#1 order point set by its angular (from min to max)
co_db$cos.theta=(co_db$X-p$X)/sqrt((co_db$X-p$X)^2+(co_db$Y-p$Y)^2)
co_db_ordered=co_db[order(co_db$cos.theta,decreasing = T),]
p=filter(co_db_ordered,cos.theta=='NaN')
p[2:3,]=co_db_ordered[1:2,]
cross_vector=function(p0,p1,co){#即进行判断的点总是有两个来自于P点集，一个来自于co点集
   return (co$X-p0$X)*(p1$Y-p0$Y)-(p1$X-p0$X)(co$Y-p0$Y)
}

for (i in 3:(nrow(co_db)-1)){
  if(cross_vector(p[nrow(p)-1,],p[nrow(p),],co[i,])<0)
    {p[nrow(p)+1,]=co[i,]}else
    {p[nrow(p),]=co[i,]}
}
p[nrow(p)+1,]=p[1,]
sfc=p[,1:2]%>%as.matrix%>%list%>% st_polygon()%>% st_sfc(crs=4509)
minimum_convex_hull=st_sf(name='siziqiao',geometry=sfc)
tm_shape(minimum_convex_hull)+tm_borders()+
tm_shape(s)+tm_dots()+tm_shape(co_db_sf)+tm_dots(col='red',size=0.3)
x=rbind(p[1,],p[2,],co_db[3,])
x[,1:2] %>% plot
cross_vector(p[1,],p[2,],co_db[3,])
rank(co_db$cos.theta)
rank(sort(co_db$cos.theta,decreasing = T))
?rank
top_n(co_db$cos.theta,2)
p=data.frame()
p=p0
p[1,]
p
p0=co_c[which.min(co_c$Y),]
p1=co_c[1,]
?crosspr

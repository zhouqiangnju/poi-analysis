library(pacman)
library(fpc)
gc()
p_load(sf,tidyverse,tmap)

s=filter(nj_entity_poi,grepl('狮子桥',name)) 

s =s %>%st_transform(crs=4509)

co=st_coordinates(s) %>% as.data.frame()
#add id to trace back to original sf point
co$id_o=1:nrow(co)
co=co[!duplicated(co[,1:2]),]
#dbscan
db=dbscan(co[,1:2],eps=100)
co_db=co[db$isseed,]
co_db_sf=st_as_sf(co_db,coords = c('X','Y'),crs=4509)
#minimum Convex Hull
p=co_db[which.min(co_db$Y),] #initial p as the start point

#1 order point set by its angular (from min to max)
co_db$cos.theta=(co_db$X-p$X)/sqrt((co_db$X-p$X)^2+(co_db$Y-p$Y)^2)
co_db_ordered=co_db[order(co_db$cos.theta,decreasing = T),]

co_db_ordered$theta.order=1:nrow(co_db_ordered)

p=filter(co_db_ordered,cos.theta=='NaN')
co_db_ordered=filter(co_db_ordered,cos.theta!='NaN')
p[2:3,]=co_db_ordered[1:2,]

top=nrow(p)
cross_vector=function(p0,p1,c){#即进行判断的点总是有两个来自于P点集，一个来自于co_db_ordered点集

  (c$X-p0$X)*(p1$Y-p0$Y)-(p1$X-p0$X)*(c$Y-p0$Y)
}

 for (i in 3:(nrow(co_db_ordered))){
   while(cross_vector(p[top-1,],p[top,],co_db_ordered[i,])>0){
     p=p[-top,]
     top=top-1
   }
   p[top+1,]=co_db_ordered[i,]
   top=top+1
 }

p_sf=st_as_sf(p,coords=c('X','Y'),crs=4509)
tm_shape(co_db_sf)+tm_dots(alpha=1)+
  tm_shape(p_sf)+tm_dots(col='red',size=0.2)+tm_text('theta.order',auto.placement  = T)

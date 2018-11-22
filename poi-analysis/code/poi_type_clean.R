#poi type clea
library('doParallel')
library(tidyverse)
nj_entity_poi=readRDS('~/GitHub/data/poi_analysis/nj_entity_poi(type).rds') 
type=nj_entity_poi$typecode 
prefix_og=type %>% str_extract('^\\d{2}')
type_list=type  %>% str_split('\\|')
which(type_number==3)
type_number=lapply(type_list,length) %>% unlist

type_extract=function(typecode){
   type_number = typecode %>% str_split('\\|') %>%unlist%>% length  
   type_list   = typecode %>% str_split('\\|')  %>%unlist
   if(type_number>1){
    prefix_table=type_list %>% str_extract('^\\d{2}') %>% table %>% as.data.frame
    
     if(prefix_table[1,2]>1){
     type1=prefix_table[1,1] %>% paste0('^',.)   %>% str_subset(type_list,.) %>% '['(1)
     type2=prefix_table[2,1] %>% paste0('^',.)   %>% str_subset(type_list,.) %>% '['(1)
     }else{
      type1=type_list[1]
      type2=type_list[type_list!=type1][1]
    }
  }   else{
     type1=type_list
     type2=NA
   }
   type=cbind(type1,type2) 
}
system.time(x<-foreach(i=1:nrow(nj_entity_poi),.combine = rbind,.packages = 'tidyverse') %do% 
  type_extract(nj_entity_poi$typecode[i]))

system.time( x <- mean(1000,10000))
?system.time
system.time(type_add<-map(nj_entity_poi$typecode,type_extract))
system.time()
type_add=lapply(nj_entity_poi$typecode,type_extract)
type_add=rlist::list.rbind(type_add)
type_add_sf=as.data.frame(type_add)
poi_add=cbind(poi,type_add_sf)
poi$type2=type_add_sf$type2
which(type_add_sf$type1=='010000')
table(type_add_sf$type1)
poi$typecode[159334]
x=type_extract(poi$typecode[7605])
foreach(i=1:nrow(type_number)) %dopar% 
?foreach
vignette("nested") 




cl<-makeCluster(4)  
registerDoParallel(cl)  
stopImplicitCluster()
stopCluster()
getCluster()

library(parallel)
cl.cores <- detectCores()
cl=makeCluster(4)
system.time(results=parLapply(cl,nj_entity_poi$typecode,type_extract))
?parLapply

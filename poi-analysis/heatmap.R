library(sf)
library(ggplot2)
library(dbplyr)
library(plyr)
#input data and map
js_base<-st_read('F:/Administrator/Documents/Mapproject/shinodata.shp')
visitor<-read.csv('F:/Administrator/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis/JVTnew.csv',stringsAsFactors = FALSE)
visitor$Visitor<-as.numeric(visitor$Visitor)
cityorder<-c('NJ','WX','XZ','CZ','SZ','NT','LYG','HA','YC','YZ','ZJ','TZ','SQ')
cityorder<-factor(cityorder,levels = cityorder)
visitor$chengshicfactor(visitor$chengshi,levels = cityorder)
#aggregate

sep<-visitor[which(visitor$Month==9&visitor$Year==2017),]
sep_sum<-aggregate(Visitor~chengshi,data=sep,sum)
names(sep-sum)[2]
sep_mean<-aggregate(Visitor~chengshi,data=sep,mean)
names(js_base)[1]<-'chengshi'
js_base$chengshi<-factor(js_base$chengshi,levels = cityorder)
sep_sta<-join(sep_sum,sep_mean)

js_base<-st_sf(merge(sep_mean,js_base))



plot(js_base)
p<-ggplot(js_base)+geom_sf(aes(fill=Visitor),colour='grey40')+coord_sf()+
  scale_fill_gradient(low = 'white',high = 'red')+
  theme(
    panel.grid=element_blank(),
    panel.background=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    legend.position=c(0.2,0.3)
  )
p

City<-
  data.frame(Residential_City=
               c("相模原市","横浜市","藤沢市","横須賀市","茅ヶ崎市","三浦市","綾瀬市",
                 "大和市","厚木市","鎌倉市","平塚市","小田原市","湯河原町","真鶴町",
                 "愛川町","座間市","伊勢原市","開成町","海老名市","寒川町","南足柄市",
                 "大井町","秦野市","箱根町","葉山町","逗子市","山北町","大磯町",
                 "二宮町","中井町","清川村","松田町","川崎市麻生区","川崎市宮前区",
                 "川崎市川崎区","川崎市高津区","川崎市幸区","川崎市中原区","川崎市多摩区"))

jinko<-read.csv("jinko.csv",fileEncoding = "UTF-8")
# jinko<-data.frame(jinko)

shp<- read_sf("N03-190101_14_GML/N03-19_14_190101_2.shp",options = "ENCODING=sJIS") %>%
  mutate(ID=paste0("Z",1:n()))
#     distinct(N03_007,.keep_all = T)

MXY <-  st_bbox(shp$geometry)

shp2 <-read_sf("N03-190101_14_GML/N03-19_14_190101.shp",options = "ENCODING=CP932") %>%
  filter(N03_003=="横浜市") %>%
  mutate(ID=paste0("Z",1:n()))
#     distinct(N03_007,.keep_all = T)

MXY2 <-  st_bbox(shp2$geometry)

tetu<-
  read_sf("N02-19_GML/N02-19_Station.shp",options = "ENCODING=CP932") %>%
  mutate(ct=st_centroid(geometry)) %>%
  mutate(ctx=st_coordinates(ct)[,1]) %>%
  mutate(cty=st_coordinates(ct)[,2]) %>%
  mutate(mxy=ctx>=MXY[1]&ctx<=MXY[3]&
           cty>=MXY[2]&cty<=MXY[4]) %>%
  filter(mxy) %>%
  ungroup()

tcol <-
  tetu %>%
  data.frame() %>%
  count(N02_001,N02_002,N02_003,N02_004) %>%
  group_by(N02_004) %>%
  mutate(nn=n()) %>%
  ungroup() %>%
  mutate(ln=paste(N02_004,N02_003)) %>%
  mutate(ln=ifelse(nn<=5,N02_004,ln)) %>%
  mutate(ln=ifelse(n<5,N02_004,ln))%>%
  arrange(ln)

tetudo <-
  tetu %>%
  left_join(tcol) %>%
  arrange(ln)

rosen<-
  read_sf("N02-19_GML/N02-19_RailroadSection.shp",options = "ENCODING=CP932") %>%
  mutate(ct=st_centroid(geometry)) %>%
  mutate(ctx=st_coordinates(ct)[,1]) %>%
  mutate(cty=st_coordinates(ct)[,2]) %>%
  mutate(mxy=ctx>=MXY[1]&ctx<=MXY[3]&
           cty>=MXY[2]&cty<=MXY[4]) %>%
  filter(mxy) %>%
  ungroup()

pal  <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(1)*50), reverse=F)
pal2 <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(7)*50), reverse=F)
pal3 <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(1)*8), reverse=F)
pal4 <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(7)*8), reverse=F)
pal5 <-colorFactor(topo.colors(length(unique(tcol$ln))),domain = tcol$ln)

if (!require(dplyr)) {
    install.packages("dplyr")
}
library(dplyr)
if (!require(sf)) {
    install.packages("sf")
}
library(sf)
if (!require(stringr)) {
    install.packages("stringr")
}
library(stringr)
if(!require(lubridate)){
    install.packages("lubridate")
}
library(lubridate)
if(!require(tidyr)){
    install.packages("tidyr")
}
library(tidyr)
if(!require(htmltools)){
    install.packages("htmltools")
}
library(htmltools)
if(!require(sp)){
    install.packages("sp")
}
library(sp)
if(!require(data.table)){
    install.packages("data.tabl")
}
library(data.table)
shinyServer(function(input, output, session) {
    data7<-
        fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/coviddata.csv",encoding = "UTF-8")%>%
        mutate(Fixed_Date=as.Date(Fixed_Date))%>%
        filter(!is.na(X))
    date<-
        data7%>%
        data.frame()%>%
        arrange(desc(Fixed_Date))%>%
        distinct(Fixed_Date)
    output$date<-
        renderUI({
            dateInput("x",
                      label = "日付を入力してください",
                      min = "2020-04-21",
                      max = date[1,1],
                      value = date[1,1])
        })
    output$update<-
        renderUI({
            h5(paste0("2020-04-21記者発表資料から",date[1,1],"記者発表資料掲載分まで集計しています。"))
        })
    City<-
        data.frame(Residential_City=c("相模原市","横浜市","藤沢市","横須賀市","茅ヶ崎市","三浦市","綾瀬市",
          "大和市","厚木市","鎌倉市","平塚市","小田原市","湯河原町","真鶴町",
          "愛川町","座間市","伊勢原市","開成町","海老名市","寒川町","南足柄市",
          "大井町","秦野市","箱根町","葉山町","逗子市","山北町","大磯町",
          "二宮町","中井町","清川村","松田町","川崎市麻生区","川崎市宮前区",
          "川崎市川崎区","川崎市高津区","川崎市幸区","川崎市中原区",
          "川崎市多摩区"))
    jinko<-read.csv("jinko.csv",fileEncoding = "UTF-8")
    jinko<-data.frame(jinko)


    shp<-
        read_sf("N03-190101_14_GML/N03-19_14_190101_2.shp",options = "ENCODING=sJIS")
    head(shp%>%
             filter(N03_003 %in% c("川崎市")))

    head(shp)
    yoko<-
        read.csv("https://square.umin.ac.jp/kenkono/csv/ward-new.csv",
                 encoding = "UTF-8",
                 header = F)

    yoko2<-
        yoko%>%
        filter(V1!="",V1!="区名")%>%
        tidyr::pivot_longer(-V1,
                            names_to = "V",
                            values_to="count")%>%
        rename("name"="V1")
    yoko3<-
        yoko%>%
        filter(V1=="")%>%
        tidyr::pivot_longer(-V1,
                            names_to="V",
                            values_to="year")%>%
        select(-V1)
    yoko4<-
        yoko%>%
        filter(V1=="区名")%>%
        tidyr::pivot_longer(-V1,
                            names_to="V",
                            values_to="date")%>%
        select(-V1)
    data<-
        left_join(yoko3,yoko4)%>%
        left_join(yoko2)%>%
        filter(!name%in%c("日本","横浜市","市外","調査中","神奈川県"))%>%
        rename("N03_004"="name")%>%
        mutate(count=as.numeric(count))%>%
        #filter(date=="4/16~4/22")%>%
        mutate(N03_003="横浜市")%>%
        mutate(start=str_replace(date,"~.+",""),
               end=str_replace(date,".*~",""))%>%
        mutate(end=str_replace(end," .+",""))%>%
        mutate(year2=str_replace(year,"年",""))%>%
        mutate(start=str_replace(start,"/","-"),
               end=str_replace(end,"/","-"),
               start=paste0(year2,"-",start),
               end=paste0(year2,"-",end),
               start=lubridate::ymd(start),
               end=lubridate::ymd(end))
    day1<-
        reactive({
            data%>%
                filter(year==input$year1)%>%
                distinct(date)
        })

    output$date2<-
        renderUI({
            selectInput("d2","日付を選択してください。",
                        choices=day1()
            )
        })
    shp2 <-read_sf("N03-190101_14_GML/N03-19_14_190101.shp",options = "ENCODING=CP932")

    tetudo<-
        read_sf("N02-19_GML/N02-19_Station2.shp",options = "ENCODING=CP932")

    output$covid_map <- renderLeaflet({
        x<-input$x
        y<-input$y
        if(is.null(x)){
            x<-date[1,1]
        }

                     date1<-lubridate::ymd(x)-as.numeric(y)+1
                    data7.1<-
                        data7%>%
                        dplyr::filter(Fixed_Date>=date1,
                                      Fixed_Date<=lubridate::ymd(x))%>%
                        # dplyr::filter(Fixed_Date>="2021-04-24",
                        #               Fixed_Date<="2021-04-24")%>%
                        dplyr::group_by(Residential_City,X,Y)%>%
                        summarise(count=n())%>%
                        full_join(City)%>%
                        mutate(count=ifelse(is.na(count),0,count))%>%
                        mutate(N03_004=Residential_City)%>%
                        ungroup()%>%
                        select(-X,-Y)%>%
                        #dplyr::filter(X>0,Y>0)%>%
                        dplyr::filter(is.numeric(count))%>%
                        ungroup()
                    s<-
                        data7.1%>%mutate(f=count>=10)%>%
                        ungroup()%>%
                        summarise(sum=sum(f))
                    data7.1<-
                        data7.1%>%
                        mutate(sum=s)
                    data7.2<-
                        sp::merge(shp, data7.1,
                                  by="N03_004", all=F,duplicateGeoms = TRUE)

                    pal <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(y)*50), reverse=F)


                    pal2<-
                        data7.2%>%
                        dplyr::mutate(col=pal(count),
                               col2=ifelse(count>300*as.numeric(y),"red",col),
                               col2=ifelse(N03_004=="横浜市","gray",col2))
                    pal3<-colorFactor(topo.colors(81),domain = tetudo$N02_003)
                    if(input$onoff){
                        leaflet(data7.2) %>%
                            fitBounds(lng1=139.752206, lat1=35.153839, lng2=138.9488, lat2=35.645042)%>%
                            #setView(lng=139.424343, lat=35.417843,zoom=10)%>%
                            addProviderTiles(providers$CartoDB.Positron)%>%
                            addPolygons(
                                fillOpacity = 1,
                                weight=1,
                                color = "#666",
                                fillColor = ~pal2$col2,
                                label = paste0(data7.2$N03_004,data7.2$count,"人"),
                                labelOptions = labelOptions(textsize = "15px")
                            )%>%
                            addLegend(data=pal2%>%
                                          #distinct(flag,.keep_all = T)%>%
                                          distinct(col2,.keep_all = T)%>%
                                          filter(N03_004!="横浜市")%>%
                                          arrange(count),
                                      pal=pal,
                                      values = c(0,as.numeric(y)*50),
                                      position="bottomright",#color=~col2,labels=~count,
                                      opacity = 1,
                                      #labFormat = labelFormat(transform = function(x)x*x)
                            )%>%
                            addControl(tags$div(HTML(paste(date1,lubridate::ymd(x),sep = "~")))  , position = "topright")%>%
                            addMarkers(139.274823,35.365831, label = "東海大学湘南キャンパス")%>%
                            addMarkers(139.313644,35.407144, label = "東海大学伊勢原キャンパス")%>%
                            addPolylines(data=tetudo,
                                         color = ~pal3(N02_003),
                                         label = paste(tetudo$N02_004,tetudo$N02_003),
                                         labelOptions = labelOptions(textsize = "15px"),
                                         group = tetudo$N02_003)%>%
                            addLayersControl(overlayGroups =tetudo$N02_003,
                                             position = "bottomleft")
                    }else{
                        leaflet(data7.2) %>%
                        fitBounds(lng1=139.752206, lat1=35.153839, lng2=138.9488, lat2=35.645042)%>%

                        #setView(lng=139.424343, lat=35.417843,zoom=10)%>%
                        addProviderTiles(providers$CartoDB.Positron)%>%
                        addPolygons(
                                    fillOpacity = 1,
                                    weight=1,
                                    color = "#666",
                                    fillColor = ~pal2$col2,
                                    label = paste0(data7.2$N03_004,data7.2$count,"人"),
                                    labelOptions = labelOptions(textsize = "15px")
                                    )%>%
                        addLegend(data=pal2%>%
                                      #distinct(flag,.keep_all = T)%>%
                                      distinct(col2,.keep_all = T)%>%
                                      filter(N03_004!="横浜市")%>%
                                      arrange(count),
                            pal=pal,
                            values = c(0,as.numeric(y)*50),
                                  position="bottomright",#color=~col2,labels=~count,
                                  opacity = 1,
                                  #labFormat = labelFormat(transform = function(x)x*x)
                        )%>%
                        addControl(tags$div(HTML(paste(date1,lubridate::ymd(x),sep = "~")))  , position = "topright")%>%
                        addMarkers(139.274823,35.365831, label = "東海大学湘南キャンパス")%>%
                        addMarkers(139.313644,35.407144, label = "東海大学伊勢原キャンパス")
                    }


    }
    )
    output$yoko_map<-renderLeaflet({
        x<-input$x
        if(is.null(x)){
            x<-date[1,1]
        }
        data1<-data%>%
            dplyr::filter(end<=lubridate::ymd(x),
                          start<=lubridate::ymd(x)-6)%>%
            group_by(N03_004)%>%
            mutate(rank=dense_rank(desc(end)))%>%
            filter(rank==1)%>%
            ungroup()

        yoko_shp<-
            sp::merge(shp2, data1,
                          #filter(year==input$year1,date%in%input$d2)
                      by=c("N03_004","N03_003"), all=F,duplicateGeoms = TRUE)

        pal <- colorNumeric(palette=c("white","red"),domain=c(0,350), reverse=F)
        pal3<-colorFactor(topo.colors(81),domain = tetudo$N02_003)
        if(input$onoff){
            yoko_shp%>%
            leaflet() %>%
            fitBounds(lng1=139.692206, lat1=35.328117, lng2=139.474443, lat2=35.573221)%>%
            #fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>%
            #setView(lng=139.604343, lat=35.4547843,zoom=10)%>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(fillOpacity = 1,
                        weight=1,
                        color = "#666",
                        fillColor = ~pal(yoko_shp$count),
                        label = paste0(yoko_shp$N03_004,yoko_shp$count),
                        labelOptions = labelOptions(textsize = "15px")
            )%>%
            addLegend(pal=pal,
                      values = c(0,350),
                      position="bottomright",
                      opacity = 1)%>%
            addControl(tags$div(HTML(unique(yoko_shp$date)))  , position = "topright")%>%
            addPolylines(data=tetudo,
                         color = ~pal3(N02_003),
                         label = paste(tetudo$N02_004,tetudo$N02_003),
                         labelOptions = labelOptions(textsize = "15px"),
                         group = tetudo$N02_003)%>%
                addLayersControl(overlayGroups =tetudo$N02_003,
                                 position = "bottomleft")
        }else{
            yoko_shp%>%
                leaflet() %>%
                fitBounds(lng1=139.692206, lat1=35.328117, lng2=139.474443, lat2=35.553221)%>%
                #fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>%
                #setView(lng=139.604343, lat=35.4547843,zoom=10)%>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(fillOpacity = 1,
                            weight=1,
                            color = "#666",
                            fillColor = ~pal(yoko_shp$count),
                            label = paste0(yoko_shp$N03_004,yoko_shp$count),
                            labelOptions = labelOptions(textsize = "15px")
                            )%>%
                addLegend(pal=pal,
                          values = c(0,350),
                          position="bottomright",
                          opacity = 1)%>%
                addControl(tags$div(HTML(unique(yoko_shp$date)))  , position = "topright")
        }

    })
    output$covid_map2 <- renderLeaflet({
        x<-input$x
        y<-input$y
        if(is.null(x)){
            x<-date[1,1]
        }
        date1<-lubridate::ymd(x)-as.numeric(y)+1
        #集計
        data7.1<-data7%>%
            filter(Fixed_Date>=date1,Fixed_Date<=lubridate::ymd(x))%>%
            group_by(Residential_City,X,Y)%>%
            summarise(count=n())%>%
            full_join(City)%>%
            mutate(count=ifelse(is.na(count),0,count))%>%
            dplyr::filter(is.numeric(count))%>%
            #filter(X>0,Y>0)
            select(-X,-Y)
        jinko2<-left_join(data7.1,jinko,by=c("Residential_City"="City"))
        jinko3<-jinko2%>%
            mutate(count_j=count/jinko*100000)%>%
            dplyr::filter(is.numeric(count_j))%>%
            filter(!is.na(count_j))%>%
            mutate(N03_004=Residential_City)
        s<-
            jinko3%>%mutate(f=count_j>=10)%>%
            ungroup()%>%
            summarise(sum=sum(f))
        jinko3<-
            jinko3%>%
            mutate(sum=s$sum)
        data7.2<-
            sp::merge(shp, jinko3,
                      by="N03_004", all=F,duplicateGeoms = TRUE)
        # #色設定
        pal <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(y)*8), reverse=F)
        pal2<-
            data7.2%>%
            mutate(col=pal(count_j),
                   col2=ifelse(count_j>as.numeric(y)*8,"red",col))
        pal3<-colorFactor(topo.colors(81),domain = tetudo$N02_003)
        if(input$onoff){
            leaflet(data7.2) %>%
            fitBounds(lng1=139.752206, lat1=35.153839, lng2=138.9488, lat2=35.645042)%>%
            #fitBounds(lng1=139.224343, lat1=35.217843, lng2=139.552899, lat2=35.565052)%>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(fillOpacity = 1,
                        weight=1,
                        color = "#666",
                        #labelOptions = labelOptions(noHide = T, textOnly = TRUE),
                        fillColor = ~pal2$col2,
                        label = paste0(data7.2$N03_004,round(data7.2$count_j,2)),
                        labelOptions = labelOptions(textsize = "15px")
            )%>%
            addLegend(data=pal2%>%
                          distinct(col2,.keep_all = T)%>%
                          arrange(count_j),
                      position="bottomright",
                      pal=pal,
                      values = c(0,as.numeric(y)*8),
                      #color=~col2,labels=~flag,
                      opacity = 1,
                      #labFormat = labelFormat(transform = function(x)x*x)
            )%>%
            addControl(tags$div(HTML(paste(date1,lubridate::ymd(x),sep = "~")))  , position = "topright")%>%
            addMarkers(139.274823,35.365831, label = "東海大学湘南キャンパス")%>%
            addMarkers(139.313644,35.407144, label = "東海大学伊勢原キャンパス")%>%
            addPolylines(data=tetudo,
                         color = ~pal3(N02_003),
                         label = paste(tetudo$N02_004,tetudo$N02_003),
                         labelOptions = labelOptions(textsize = "15px"),
                         group = tetudo$N02_003)%>%
                addLayersControl(overlayGroups =tetudo$N02_003,
                                 position = "bottomleft")
        }else{
            leaflet(data7.2) %>%
                fitBounds(lng1=139.752206, lat1=35.153839, lng2=138.9488, lat2=35.645042)%>%
                #fitBounds(lng1=139.224343, lat1=35.217843, lng2=139.552899, lat2=35.565052)%>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(fillOpacity = 1,
                            weight=1,
                            color = "#666",
                            #labelOptions = labelOptions(noHide = T, textOnly = TRUE),
                            fillColor = ~pal2$col2,
                            label = paste0(data7.2$N03_004,round(data7.2$count_j,2)),
                            labelOptions = labelOptions(textsize = "15px")
                )%>%
                addLegend(data=pal2%>%
                              distinct(col2,.keep_all = T)%>%
                              arrange(count_j),
                          position="bottomright",
                          pal=pal,
                          values = c(0,as.numeric(y)*8),
                          #color=~col2,labels=~flag,
                          opacity = 1,
                          #labFormat = labelFormat(transform = function(x)x*x)
                )%>%
                addControl(tags$div(HTML(paste(date1,lubridate::ymd(x),sep = "~")))  , position = "topright")%>%
                addMarkers(139.274823,35.365831, label = "東海大学湘南キャンパス")%>%
                addMarkers(139.313644,35.407144, label = "東海大学伊勢原キャンパス")
        }


    })
    output$yoko_map2<-renderLeaflet({
        x<-input$x
        if(is.null(x)){
            x<-date[1,1]
        }
        data1<-data%>%
            dplyr::filter(end<=lubridate::ymd(x),
                          start<=lubridate::ymd(x)-6,
                          lubridate::ymd(x)-6<=end)
        data2<-
            left_join(data1,#%>%
                          #filter(year==input$year1,date%in%input$d2),
                      jinko,by=c("N03_004"="City"))%>%
            mutate(count_j=round(count/jinko*100000,2))
        yoko_shp2<-
            sp::merge(shp2, data2,
                      by=c("N03_004","N03_003"), all=F,duplicateGeoms = TRUE)
        pal <- colorNumeric(palette=c("white","red"),domain=c(0,56), reverse=F)
        pal3<-colorFactor(topo.colors(81),domain = tetudo$N02_003)
        if(input$onoff){
            yoko_shp2%>%
            leaflet() %>%
            fitBounds(lng1=139.692206, lat1=35.328117, lng2=139.474443, lat2=35.573221)%>%
            #fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>%
            #setView(lng=139.604343, lat=35.457843,zoom=10)%>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(fillOpacity = 1,
                        weight=1,
                        color = "#666",
                        fillColor = ~pal(yoko_shp2$count_j),
                        label = paste0(yoko_shp2$N03_004,yoko_shp2$count_j),
                        labelOptions = labelOptions(textsize = "15px")
            )%>%
            addLegend(pal=pal,
                      values = c(0,56),
                      position="bottomright",
                      opacity = 1)%>%
            addControl(tags$div(HTML(unique(yoko_shp2$date))), position = "topright")%>%
            addPolylines(data=tetudo,
                         color = ~pal3(N02_003),
                         label = paste(tetudo$N02_004,tetudo$N02_003),
                         labelOptions = labelOptions(textsize = "15px"),
                         group = tetudo$N02_003)%>%
                addLayersControl(overlayGroups =tetudo$N02_003,
                                 position = "bottomleft")
        }else{
            yoko_shp2%>%
                leaflet() %>%
                fitBounds(lng1=139.692206, lat1=35.328117, lng2=139.474443, lat2=35.573221)%>%
                #fitBounds(lng1=139.124343, lat1=35.117843, lng2=139.652899, lat2=35.665052)%>%
                #setView(lng=139.604343, lat=35.457843,zoom=10)%>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(fillOpacity = 1,
                            weight=1,
                            color = "#666",
                            fillColor = ~pal(yoko_shp2$count_j),
                            label = paste0(yoko_shp2$N03_004,yoko_shp2$count_j),
                            labelOptions = labelOptions(textsize = "15px")
                )%>%
                addLegend(pal=pal,
                          values = c(0,56),
                          position="bottomright",
                          opacity = 1)%>%
                addControl(tags$div(HTML(unique(yoko_shp2$date))), position = "topright")
        }


    })


    })

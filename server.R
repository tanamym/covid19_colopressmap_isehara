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
    install.packages("data.table")
}
library(data.table)

load("Dataset.RData")

shinyServer(function(input, output, session) {
    data7<-
        # read.csv("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/coviddata.csv",fileEncoding = "sJIS")%>%
        fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/coviddata.csv",encoding = "UTF-8")%>%
        mutate(Fixed_Date=as.Date(Fixed_Date))%>%
        filter(!is.na(X))
    date<-
        data7%>%
        data.frame()%>%
        arrange(desc(Fixed_Date))%>%
        distinct(Fixed_Date)
    
    yoko<-
        # read.csv("https://square.umin.ac.jp/kenkono/csv/ward-new.csv", encoding = "UTF-8", header = F)
        fread("https://square.umin.ac.jp/kenkono/csv/ward-new.csv", encoding = "UTF-8",header = F)
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
        # mutate(start=as.Date(paste0(year2,"/",start))) %>%
        # mutate(end=as.Date(paste0(year2,"/",end))) %>%
        mutate(start=str_replace(start,"/","-"),
               end=str_replace(end,"/","-"),
               start=paste0(year2,"-",start),
               end=paste0(year2,"-",end),
               start=lubridate::ymd(start),
               end=lubridate::ymd(end)) 
        # mutate(date=as.character(date)) %>%
        # mutate(count=as.numeric(as.character(count)))
    
    output$update<-
        renderUI({
            h5(paste0("2020-04-21記者発表資料から",date[1,1],"記者発表資料掲載分まで集計しています。"))
        })
    output$date<-
        renderUI({
            dateInput("x",
                      label = "日付を入力してください",
                      min = "2020-04-21",
                      max = date[1,1],
                      value = date[1,1])
        })
    
    output$covid_map <- renderLeaflet({
        leaflet() %>%
            fitBounds(lng1=MXY[[1]], lat1=MXY[[2]], lng2=MXY[[3]], lat2=MXY[[4]]) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            #addTiles() %>%
            addMarkers(139.274823,35.365831, label = "東海大学湘南キャンパス") %>%
            addMarkers(139.313644,35.407144, label = "東海大学伊勢原キャンパス") %>%
            addPolygons(data=shp, layerId = ~ID, fillOpacity = 1, weight = 1, color = "#666", fillColor = "white")#,
        # highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE))
    })
    
    output$yoko_map<-renderLeaflet({
        leaflet() %>%
            fitBounds(lng1=MXY2[[1]], lat1=MXY2[[2]], lng2=MXY2[[3]], lat2=MXY2[[4]]) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(data=shp2, layerId = ~ID, fillOpacity = 1, weight = 1, color = "#666", fillColor = "white")
    })
    
    output$covid_map2 <- renderLeaflet({
        leaflet() %>%
            fitBounds(lng1=MXY[[1]], lat1=MXY[[2]], lng2=MXY[[3]], lat2=MXY[[4]]) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addMarkers(139.274823,35.365831, label = "東海大学湘南キャンパス") %>%
            addMarkers(139.313644,35.407144, label = "東海大学伊勢原キャンパス") %>%
            addPolygons(data=shp, layerId = ~ID, fillOpacity = 1, weight = 1, color = "#666", fillColor = "white")
        # %>%
        #     addControl(tags$div(HTML(paste("更新ボタンを再度押してください")))  , position = "topright")
    })
    
    output$yoko_map2<-renderLeaflet({
        leaflet() %>%
            fitBounds(lng1=MXY2[[1]], lat1=MXY2[[2]], lng2=MXY2[[3]], lat2=MXY2[[4]]) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(data=shp2, layerId = ~ID, fillOpacity = 1, weight = 1, color = "#666", fillColor = "white")
    })
    
    LD <- eventReactive(input$button1,ignoreNULL = FALSE, ignoreInit = FALSE,{
        x=input$x
        if(is.null(x)){
            x=date$Fixed_Date[1]
        }
        
        print(x)
        date2=ymd(x)
        date1=date2-7+1
        
        data7.1<-
            data7%>%
            dplyr::filter(Fixed_Date>=date1,
                          Fixed_Date<=date2)%>%
            dplyr::group_by(Residential_City,X,Y)%>%
            summarise(count1=sum(Fixed_Date==date2),count7=n())%>%
            ungroup() %>%
            full_join(City)%>%
            mutate(count1=ifelse(is.na(count1),0,count1))%>%
            mutate(count7=ifelse(is.na(count7),0,count7))%>%
            dplyr::mutate(col1=pal(count1),
                          col12=ifelse(count1>300*as.numeric(1),"red",col1),
                          col12=ifelse(Residential_City=="横浜市","gray",col12))%>%
            dplyr::mutate(col7=pal2(count7),
                          col72=ifelse(count7>300*as.numeric(7),"red",col7),
                          col72=ifelse(Residential_City=="横浜市","gray",col72)) %>%
            left_join(jinko,by=c("Residential_City"="City")) %>%
            mutate(count_j1=count1/jinko*100000)%>%
            mutate(count_j7=count7/jinko*100000)%>%
            filter(!is.na(count_j1))%>%
            dplyr::mutate(col_j1=pal3(count_j1),
                          col_j12=ifelse(count_j1>8*as.numeric(1),"red",col_j1),
                          col_j12=ifelse(Residential_City=="横浜市","gray",col_j12))%>%
            dplyr::mutate(col_j7=pal4(count_j7),
                          col_j72=ifelse(count_j1>8*as.numeric(7),"red",col_j7),
                          col_j72=ifelse(Residential_City=="横浜市","gray",col_j72)) %>%
            rename(N03_004=Residential_City)
        
        data7.2<-
            sp::merge(shp, data7.1,
                      by="N03_004", all=F,duplicateGeoms = TRUE) %>%
            mutate(date1) %>%
            mutate(date2)
        return(data7.2)
    })
    
    observe({
        y=input$y
        data7.2=LD()
        date1=unique(data7.2$date1)
        date2=unique(data7.2$date2)
        if(y==1){
            leafletProxy("covid_map",data=data7.2) %>%
                clearControls() %>%
                # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                removeShape(layerId=paste0("P",1:nrow(data7.2))) %>%
                addPolygons(layerId=paste0("P",1:nrow(data7.2)),
                            label = paste0(data7.2$N03_004," ",data7.2$count1,"人"),
                            labelOptions = labelOptions(textsize = "15px"),
                            opacity = 0,
                            fillOpacity = 0) %>%
                setShapeStyle(layerId = ~ID,
                              fillColor = ~col12) %>%
                addLegend(pal=pal,
                          values = c(0,as.numeric(1)*50),
                          position="bottomright",#color=~col2,labels=~count,
                          opacity = 1) %>%
                addControl(tags$div(HTML(paste(date2,date2,sep = "~")))  , position = "topright")
            
            leafletProxy("covid_map2",data=data7.2) %>%
                clearControls() %>%
                # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                addPolygons(label = paste0(data7.2$N03_004," ",round(data7.2$count_j1,2),"人"),
                            labelOptions = labelOptions(textsize = "15px"),
                            opacity = 0,
                            fillOpacity = 0) %>%
                setShapeStyle(layerId = ~ID,
                              fillColor = ~col_j12) %>%
                addLegend(pal=pal3,
                          values = c(0,as.numeric(1)*8),
                          position="bottomright",#color=~col2,labels=~count,
                          opacity = 1) %>%
                addControl(tags$div(HTML(paste(date2,date2,sep = "~")))  , position = "topright")
        }
        if(y==7){
            leafletProxy("covid_map",data=data7.2) %>%
                clearControls() %>%
                # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                removeShape(layerId=paste0("P",1:nrow(data7.2))) %>%
                addPolygons(layerId=paste0("P",1:nrow(data7.2)),
                            label = paste0(data7.2$N03_004," ",data7.2$count7,"人"),
                            labelOptions = labelOptions(textsize = "15px"),
                            opacity = 0,
                            fillOpacity = 0) %>%
                # addLabelOnlyMarkers(layerId = ~ID,
                #                     label = paste0(data7.2$N03_004," ",data7.2$count7,"人"),
                #                                 labelOptions = labelOptions(textsize = "15px")) %>%
                setShapeStyle(layerId = ~ID,
                              fillColor = ~col72) %>%
                addLegend(pal=pal2,
                          values = c(0,as.numeric(7)*50),
                          position="bottomright",#color=~col2,labels=~count,
                          opacity = 1) %>%
                addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")
            
            leafletProxy("covid_map2",data=data7.2) %>%
                clearControls() %>%
                # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                addPolygons(label = paste0(data7.2$N03_004," ",round(data7.2$count_j7,2),"人"),
                            labelOptions = labelOptions(textsize = "15px"),
                            opacity = 0,
                            fillOpacity = 0) %>%
                setShapeStyle(layerId = ~ID,
                              fillColor = ~col_j72) %>%
                addLegend(pal=pal4,
                          values = c(0,as.numeric(7)*8),
                          position="bottomright",#color=~col2,labels=~count,
                          opacity = 1) %>%
                addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")
        }
    })
    
    LD_yoko <- eventReactive(input$button1,ignoreNULL = FALSE, ignoreInit = FALSE,{
        x=input$x
        if(is.null(x)){
            x=date$Fixed_Date[1]
        }
        
        print(x)
        date2=ymd(x)
        date1=date2-7+1
        
        data0<-data %>%
            dplyr::filter(end<=date2,
                          end>=date1)%>%
            left_join(jinko,by=c("N03_004"="City")) %>%
            mutate(count_j=count/jinko*100000)
        
        data1 <-
            sp::merge(shp2,data0,
                      by=c("N03_003","N03_004"), all=F,duplicateGeoms = TRUE) %>%
            mutate(date1) %>%
            mutate(date2)
        return(data1)
    })
    
    observe({
        data1=LD_yoko()
        date1=unique(data1$start)
        date2=unique(data1$end)
        
        leafletProxy("yoko_map",data=data1) %>%
            clearControls() %>%
            # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
            # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
            # removeShape(layerId=paste0("P",1:nrow(data7.2))) %>%
            addPolygons(layerId=paste0("P",1:nrow(data1)),
                        label = paste0(data1$N03_004," ",data1$count,"人"),
                        labelOptions = labelOptions(textsize = "15px"),
                        opacity = 0,
                        fillOpacity = 0) %>%
            setShapeStyle(layerId = ~ID,
                          fillColor = ~pal2(count)) %>%
            addLegend(pal=pal2,
                      values = c(0,as.numeric(7)*50),
                      position="bottomright",#color=~col2,labels=~count,
                      opacity = 1) %>%
            addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")
        
        leafletProxy("yoko_map2",data=data1) %>%
            clearControls() %>%
            # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
            # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
            # removeShape(layerId=paste0("P",1:nrow(data7.2))) %>%
            addPolygons(layerId=paste0("P",1:nrow(data1)),
                        label = paste0(data1$N03_004," ",round(data1$count_j,2),"人"),
                        labelOptions = labelOptions(textsize = "15px"),
                        opacity = 0,
                        fillOpacity = 0) %>%
            setShapeStyle(layerId = ~ID,
                          fillColor = ~pal4(count_j)) %>%
            addLegend(pal=pal4,
                      values = c(0,as.numeric(7)*8),
                      position="bottomright",#color=~col2,labels=~count,
                      opacity = 1) %>%
            addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")
    })
    
    observe({
        # input$button1
        # input$y
        if(input$onoff){
            leafletProxy("covid_map") %>%
                addPolylines(data=rosen,color = "black",weight = 1,
                             layerId=paste0("X",1:nrow(rosen))) %>%
                addPolylines(data=tetudo,
                             color = ~pal5(ln),
                             opacity = 1,
                             label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                             labelOptions = labelOptions(textsize = "15px"),
                             layerId=paste0("Y",1:nrow(tetudo)),
                )
            leafletProxy("covid_map2") %>%
                addPolylines(data=rosen,color = "black",weight = 1,
                             layerId=paste0("X",1:nrow(rosen))) %>%
                addPolylines(data=tetudo,
                             color = ~pal5(ln),
                             opacity = 1,
                             label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                             labelOptions = labelOptions(textsize = "15px"),
                             layerId=paste0("Y",1:nrow(tetudo)),
                )
            leafletProxy("yoko_map") %>%
                addPolylines(data=rosen,color = "black",weight = 1,
                             layerId=paste0("X",1:nrow(rosen))) %>%
                addPolylines(data=tetudo,
                             color = ~pal5(ln),
                             opacity = 1,
                             label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                             labelOptions = labelOptions(textsize = "15px"),
                             layerId=paste0("Y",1:nrow(tetudo)),
                )
            leafletProxy("yoko_map2") %>%
                addPolylines(data=rosen,color = "black",weight = 1,
                             layerId=paste0("X",1:nrow(rosen))) %>%
                addPolylines(data=tetudo,
                             color = ~pal5(ln),
                             opacity = 1,
                             label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                             labelOptions = labelOptions(textsize = "15px"),
                             layerId=paste0("Y",1:nrow(tetudo)),
                )
        }
        else{
            leafletProxy("covid_map") %>%
                removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                removeShape(layerId=paste0("Y",1:nrow(tetudo)))
            leafletProxy("covid_map2") %>%
                removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                removeShape(layerId=paste0("Y",1:nrow(tetudo)))
            leafletProxy("yoko_map") %>%
                removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                removeShape(layerId=paste0("Y",1:nrow(tetudo)))
            leafletProxy("yoko_map2") %>%
                removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                removeShape(layerId=paste0("Y",1:nrow(tetudo)))
        }
    })
    
    vals <- reactiveValues(counter = 0)
    observe({
        print(vals$counter)
        if(isolate(vals$counter)==0&input$tabset=="tab2"){
            vals$counter=1
            y=input$y
            data7.2=LD()
            date1=unique(data7.2$date1)
            date2=unique(data7.2$date2)
            if(y==1){
                leafletProxy("covid_map2",data=data7.2) %>%
                    clearControls() %>%
                    # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                    # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                    addPolygons(label = paste0(data7.2$N03_004," ",round(data7.2$count_j1,2),"人"),
                                labelOptions = labelOptions(textsize = "15px"),
                                opacity = 0,
                                fillOpacity = 0) %>%
                    setShapeStyle(layerId = ~ID,
                                  fillColor = ~col_j12) %>%
                    addLegend(pal=pal3,
                              values = c(0,as.numeric(1)*8),
                              position="bottomright",#color=~col2,labels=~count,
                              opacity = 1) %>%
                    addControl(tags$div(HTML(paste(date2,date2,sep = "~")))  , position = "topright")
            }
            if(y==7){
                leafletProxy("covid_map2",data=data7.2) %>%
                    clearControls() %>%
                    # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                    # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                    addPolygons(label = paste0(data7.2$N03_004," ",round(data7.2$count_j7,2),"人"),
                                labelOptions = labelOptions(textsize = "15px"),
                                opacity = 0,
                                fillOpacity = 0) %>%
                    setShapeStyle(layerId = ~ID,
                                  fillColor = ~col_j72) %>%
                    addLegend(pal=pal4,
                              values = c(0,as.numeric(7)*8),
                              position="bottomright",#color=~col2,labels=~count,
                              opacity = 1) %>%
                    addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")
            }
            
            data1=LD_yoko()
            date1=unique(data1$start)
            date2=unique(data1$end)
            
            leafletProxy("yoko_map2",data=data1) %>%
                clearControls() %>%
                # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                # removeShape(layerId=paste0("P",1:nrow(data7.2))) %>%
                addPolygons(layerId=paste0("P",1:nrow(data1)),
                            label = paste0(data1$N03_004," ",round(data1$count_j,2),"人"),
                            labelOptions = labelOptions(textsize = "15px"),
                            opacity = 0,
                            fillOpacity = 0) %>%
                setShapeStyle(layerId = ~ID,
                              fillColor = ~pal4(count_j)) %>%
                addLegend(pal=pal4,
                          values = c(0,as.numeric(7)*8),
                          position="bottomright",#color=~col2,labels=~count,
                          opacity = 1) %>%
                addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")
            
            if(input$onoff){
                leafletProxy("covid_map2") %>%
                    addPolylines(data=rosen,color = "black",weight = 1,
                                 layerId=paste0("X",1:nrow(rosen))) %>%
                    addPolylines(data=tetudo,
                                 color = ~pal5(ln),
                                 opacity = 1,
                                 label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                                 labelOptions = labelOptions(textsize = "15px"),
                                 layerId=paste0("Y",1:nrow(tetudo)))
                leafletProxy("yoko_map2") %>%
                    addPolylines(data=rosen,color = "black",weight = 1,
                                 layerId=paste0("X",1:nrow(rosen))) %>%
                    addPolylines(data=tetudo,
                                 color = ~pal5(ln),
                                 opacity = 1,
                                 label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                                 labelOptions = labelOptions(textsize = "15px"),
                                 layerId=paste0("Y",1:nrow(tetudo)))
            }
        }
    })
})





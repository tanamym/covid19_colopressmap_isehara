library(dplyr)
library(pdftools)
library(data.table)
library(stringr)
library(stringi)
library(readxl)

k1<-
  as.numeric(Sys.Date()-as.Date("2021-3-1"))
k2<-
  as.numeric(Sys.Date()-as.Date("2021-3-29"))
for (k in k1:k2) {#1月
  if(k==k1){
    #データフレームを初期化
    res5 <-data.frame()
  }
  #日付
  day <-
    (Sys.Date() -k)%>%
    as.character() %>%
    str_replace_all("-","")
  #URLの指定
  # #12月
  # path <-
  #   paste0("https://www.pref.kanagawa.jp/documents/69299/",day,"_kanagawa.pdf")
  # if(k==(as.numeric(Sys.Date()-as.Date("2020-12-6")))){
  #   path <-
  #     paste0("https://www.pref.kanagawa.jp/documents/69299/",day,"_kangawa.pdf")
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2020-12-5")))){
  #   path <-
  #     paste0("https://www.pref.kanagawa.jp/documents/69299/",day,"_knagawa.pdf")
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2020-12-30")))){
  #   path <-
  #     paste0("https://www.pref.kanagawa.jp/documents/69299/",day,"_kanagwa.pdf")
  # }
  #1月
  # path <-
  #   paste0("https://www.pref.kanagawa.jp/documents/70141/",day,"_kanagawa.pdf")
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-1-3")))){
  #   path <-
  #     paste0("https://www.pref.kanagawa.jp/documents/70141/","20200103_kanagawa.pdf")
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-1-25")))){
  #   path <-
  #     paste0("https://www.pref.kanagawa.jp/documents/70141/","kanagawa.pdf")
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-1-28")))){
  #   path <-
  #     paste0("https://www.pref.kanagawa.jp/documents/70141/","0128kanagawa.pdf")
  # }
  #2月
  # path <-
  #   paste0("https://www.pref.kanagawa.jp/documents/71309/",day,"_kanagawa.pdf")
  # #200218_kanagawa.pdf
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-18")))){
  #   path <-
  #     paste0("https://www.pref.kanagawa.jp/documents/71309/","200218_kanagawa.pdf")
  # }
  #3月
  path <-
    paste0("https://www.pref.kanagawa.jp/documents/72483/",day,"_kanagawa.pdf")
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-20")))){
    path <-
      paste0("https://www.pref.kanagawa.jp/documents/72483/2021_0320_kanagawa.pdf")
  }
  #pdfを文字列として読み込み
  res <-
    pdf_text(path)
  res2 <-
    strsplit(res,"\r\n")
  #データフレームを初期化
  res3 <-data.frame()
  
  for (i in 1:length(res2)) {
    res4 <-
      res2[[i]] %>%
      data.frame() %>%
      rename("chr" = ".") %>%
      filter(str_length(chr)>10) %>%
      mutate(s = str_replace(chr,"^ +","")) %>%
      mutate(s = ifelse(str_detect(s,"―"), str_replace(s,"      "," 0-10 代 "),s)) %>%
      mutate(判明日 = str_replace(s," 日.*?$","日")) %>%
      mutate(判明日 = str_replace(判明日," 月 ","月")) %>%
      mutate(判明日 = ifelse(str_detect(判明日,"11月"),
                          str_replace(判明日,"^.+?11月","11月"),
                          ifelse(str_detect(判明日,"12月"),
                                 str_replace(判明日,"^.+?12月","12月"),
                                 str_sub(判明日,start = -5,end = -1))))%>%
      mutate(s = str_replace_all(s," +","_")) %>%
      mutate(s = ifelse(str_detect(s,"市"),
                        str_replace(s,"市.+","市"),
                        str_replace(s,"町.+","町"))) %>%
      tidyr::separate(s,into = c("n","年代","代","性別","保健所",
                                 "居住地"),sep ="_") %>%
      filter(str_length(n)<=2,!is.na(判明日),str_detect(判明日,"月"),代=="代")
    res3<-
      rbind(res3,res4) 
  }
  
  res5 <-
    rbind(res5,res3)
  print(k)
}
# kanagawa202012<-
#   res5
# kanagawa202101<-
#   res5
# kanagawa202102<-
#   res5
kanagawa202103<-
  res5
kanagawa<-
  rbind(kanagawa202012%>%
          mutate(判明日 = paste0("2020-",str_replace(判明日,"月","-"))
          )%>% 
          mutate(判明日 = str_replace(判明日,"日",""))%>%
          select(年代,性別,保健所,居住地,判明日),
        kanagawa202101 %>%
          mutate(判明日 = ifelse(str_detect(判明日,"12月"),
                              paste0("2020-",str_replace(判明日,"月","-")),
                              paste0("2021-",str_replace(判明日,"月","-"))
          ) )%>%
          mutate(判明日 = str_replace(判明日,"日",""))%>%
          select(年代,性別,保健所,居住地,判明日),
        kanagawa202102 %>%
          mutate(判明日 = ifelse(str_detect(判明日,"12月"),
                              paste0("2020-",str_replace(判明日,"月","-")),
                              paste0("2021-",str_replace(判明日,"月","-"))
          ) )%>%
          mutate(判明日 = str_replace(判明日,"日",""))%>%
          select(年代,性別,保健所,居住地,判明日),
        kanagawa202103 %>%
          mutate(判明日 = ifelse(str_detect(判明日,"12月"),
                              paste0("2020-",str_replace(判明日,"月","-")),
                              paste0("2021-",str_replace(判明日,"月","-"))
          ) )%>%
          mutate(判明日 = str_replace(判明日,"日",""))%>%
          select(年代,性別,保健所,居住地,判明日)
        
  ) %>%
  select(-年代,-性別) %>%
  rename("Fixed_Date"="判明日","note"="保健所","Residential_City"="居住地") 
write.csv(kanagawa,"kanagawa2.csv")

for (k in k1:k2) {#1月
  if(k==k1){
    #データフレームを初期化
    kawa3 <-data.frame()
  }
  #日付
  day <-
    (Sys.Date() -k)%>%
    as.character() %>%
    str_replace_all("-","")
  
  # path<-
  #   paste0("https://www.pref.kanagawa.jp/documents/69299/",day,"_kawasaki.pdf")
  # path<-
  #   paste0("https://www.pref.kanagawa.jp/documents/70141/",day,"_kawasaki.pdf")
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-1-16")))){
  #   path<-
  #     paste0("https://www.pref.kanagawa.jp/documents/70141/",day,"_kawasaki_1.pdf")
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-1-3")))){
  #   path<-
  #     "https://www.pref.kanagawa.jp/documents/70141/20200103_kawasaki.pdf"
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-1-26")))){
  #   path<-
  #     "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/0126.pdf"
  # }
  # path<-
  #   paste0("https://www.pref.kanagawa.jp/documents/71309/",day,"_kawasaki.pdf")
  #3月
  path<-
    paste0("https://www.pref.kanagawa.jp/documents/72483/",day,"_kawasaki.pdf")
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-1")))){
    next}
  
  kawa2<-data.frame()
  for (i in 1:length(pdf_text(path))) {
    kawa<-
      pdf_text(path)[i]%>%
      data.frame() %>%
      filter(str_detect(.,"＜内訳＞"))
    if(nrow(kawa)>0){
      kawa<-
        kawa%>%
        str_split("\r\n") %>%
        data.frame() %>%
        data.table::setnames("chr") %>%
        filter(!str_detect(chr,"＜内訳＞")) %>%
        filter(str_detect(chr,"例目")) %>%
        filter(str_detect(chr,"川崎市"))%>%
        mutate(s=str_replace(chr,"区.+","区")) %>%
        mutate(確定日=str_sub(chr,start = -14,end = -4))%>%
        mutate(確定日=str_replace(確定日,"日.+","日")) %>%
        #mutate(確定日=ifelse(str_detect(確定日,""),str_replace(確定日,".+1","1")))%>%
        mutate(s = str_replace_all(s," +","_")) %>%
        tidyr::separate(s,into = c("番号","番号2","年代","性別","居住地"),sep ="_")%>%
        mutate(年代=ifelse(str_detect(年代,"10歳未満"),"0-10",str_replace(年代,"代","")))%>%
        mutate(居住市区町村=ifelse(str_detect(居住地,"市外"),"市外","川崎市"))%>%
        mutate(居住都道府県=ifelse(str_detect(居住地,"都内"),"東京都",
                             ifelse(str_detect(居住地,"県内|川崎市"),"神奈川県","県外")))%>%
        mutate(受診都道府県="神奈川県")%>%
        mutate(備考=ifelse(str_detect(居住地,"区"),str_replace(居住地,"川崎市",""),""))%>%
        mutate(確定日=ifelse(str_detect(確定日,"11月"),
                          str_replace(確定日,".+11月","11月"),
                          ifelse(str_detect(確定日,".+12月"),
                                 str_replace(確定日,".+12月","12月"),
                                 確定日
                          )))%>%
        mutate(確定日=ifelse(str_detect(確定日,"/"),
                          str_replace(確定日,"^[0-9]",""),
                          確定日))%>%
        mutate(確定日=ifelse(str_detect(確定日,"/"),
                          str_replace_all(確定日,"[^0-9/]",""),
                          確定日))%>%
        mutate(確定日=ifelse(str_detect(確定日,"11月"),
                          paste0("2020-",str_replace(確定日,"月","-")),
                          ifelse(str_detect(確定日,"12月"),
                                 paste0("2020-",str_replace(確定日,"月","-")),
                                 paste0("2021-",str_replace(確定日,"月","-")))),
                  確定日=str_replace(確定日,"日",""))%>%
        mutate(確定日=ifelse(str_detect(確定日,"/"),
                          str_replace(確定日,"/","-"),
                          確定日
        )
        )%>%
        # mutate(確定日=ifelse(str_detect(確定日,"/"),
        #                   str_replace_all(確定日,"[^0-9/-]",""),確定日))%>%
        # mutate(確定日=ifelse(str_detect(確定日,"/"),
        #                   str_replace_all(確定日,"/","-"),確定日))%>%
        select(-chr)
      kawa2<-
        rbind(kawa,kawa2)
      # print(i)
    }
  }
  kawa3<-
    rbind(kawa2,kawa3)
  print(k)
}

# kawa202012<-
#   kawa3
# kawa202101<-
#   kawa3
# kawa202102<-
#   kawa3
kawa202103<-
  kawa3
kawasaki<-
  rbind(kawa202012,kawa202101,kawa202102,kawa202103)%>%
  rename("list"="居住地","Fixed_Date"="確定日",
         "Residential_City"="居住市区町村",
         "Residential_Pref"="居住都道府県",
         "Hospital_Pref"="受診都道府県","note"="備考")%>%
  select(-番号,-年代,-性別,-番号2)
write.csv(kawasaki,"kawasaki.csv")
for (k in k1:k2) {#1月
  if(k==k1){
    #データフレームを初期化
    chi5 <-data.frame()
  }
  #日付
  
  day <-
    (Sys.Date() -k)%>%
    as.character() %>%
    str_replace_all("-","")
  
  # path<-
  #   paste0("https://www.pref.kanagawa.jp/documents/71309/",day,"_chigasaki.pdf")
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-6")))){
  #   next
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-7")))){
  #   next
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-11")))){
  #   next
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-13")))){
  #   next
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-14")))){
  #   next
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-19")))){
  #   next
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-20")))){
  #   next
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-21")))){
  #   next
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-23")))){
  #   next
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-25")))){
  #   path<-
  #   "https://www.pref.kanagawa.jp/documents/71309/20210225_chigasai.pdf"
  # }
  # if(k==(as.numeric(Sys.Date()-as.Date("2021-2-27")))){
  #   next
  # }
  path<-
    paste0("https://www.pref.kanagawa.jp/documents/72483/",day,"_chigasaki.pdf")
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-2")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-7")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-6")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-11")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-12")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-13")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-14")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-18")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-20")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-21")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-27")))){
    next
  }
  if(k==(as.numeric(Sys.Date()-as.Date("2021-3-28")))){
    next
  }
  chi4<-data.frame()
  #管内[0-9][0-9][0-9]例目|
  for (i in 1:length(pdf_text(path))) {
    if(nrow(pdf_text(path)[i]%>%
            str_split("\r\n") %>%
            data.frame() %>%
            data.table::setnames("chr") %>%
            filter(str_detect(chr,"感染患者の詳細")))>0){
      chi<-
        pdf_text(path)[i]%>%
        str_split("\r\n") %>%
        data.frame() %>%
        data.table::setnames("chr") %>%
        filter(str_detect(chr,"例目")) %>%
        filter(str_detect(chr,"市|町")) %>%
        mutate(chr=str_replace(chr,"^ +",""),
               chr=str_replace_all(chr," +","_"),
               chr=str_replace(chr,"代",""))%>%
        tidyr::separate(chr,into = c("X1","例目","年代","性別","居住地",
                                     "X6","X7","X8","陽性確定日","X10"),sep ="_")%>%
        dplyr::select(例目,年代,性別,居住地,陽性確定日)%>%
        mutate(年代=stringi::stri_trans_nfkc(年代))
      chi4<-
        rbind(chi4,chi)
      # print(i)
    }
  }
  chi5<-
    rbind(chi5,chi4)
  print(k)
}
# chi202102<-
#   chi5
chi202103<-
  chi5
chigasaki<-
  rbind(chi202012%>%
          mutate(陽性確定日 = paste0("2020-",str_replace(陽性確定日,"月","-"))
          )%>% 
          mutate(陽性確定日 = str_replace(陽性確定日,"日",""))%>%
          select(年代,性別,居住地,陽性確定日),
        chi202101 %>%
          mutate(陽性確定日 = ifelse(str_detect(陽性確定日,"12月"),
                                paste0("2020-",str_replace(陽性確定日,"月","-")),
                                paste0("2021-",str_replace(陽性確定日,"月","-"))
          ) )%>%
          mutate(陽性確定日 = str_replace(陽性確定日,"日",""))%>%
          select(年代,性別,居住地,陽性確定日),
        chi202102 %>%
          mutate(陽性確定日 = ifelse(str_detect(陽性確定日,"12月"),
                                paste0("2020-",str_replace(陽性確定日,"月","-")),
                                paste0("2021-",str_replace(陽性確定日,"月","-"))
          ) )%>%
          mutate(陽性確定日 = str_replace(陽性確定日,"日",""))%>%
          select(年代,性別,居住地,陽性確定日),
        chi202103 %>%
          mutate(陽性確定日 = ifelse(str_detect(陽性確定日,"12月"),
                                paste0("2020-",str_replace(陽性確定日,"月","-")),
                                paste0("2021-",str_replace(陽性確定日,"月","-"))
          ) )%>%
          mutate(陽性確定日 = str_replace(陽性確定日,"日",""))%>%
          select(年代,性別,居住地,陽性確定日)) %>%
  select(-年代,-性別) %>%
  rename("Fixed_Date"="陽性確定日","Residential_City"="居住地")%>%
  filter(!str_detect(Fixed_Date,"NULL"))

write.csv(chigasaki,"chigasaki.csv",row.names = F)

data<-
  fread("kanagawa.csv", encoding="UTF-8") %>%
  mutate(確定日= as.Date(確定日,format = "%m/%d/%Y"))%>%
  select(確定日,受診都道府県,居住都道府県,居住市区町村,備考,X,Y)%>%
  filter(居住都道府県=="神奈川県")%>%
  filter(居住市区町村=="川崎市"|居住市区町村=="")%>%
  mutate(Residential_City=paste0(居住市区町村,備考))%>%
  select(-居住市区町村,-備考,-X,-Y)%>%
  rename("Fixed_Date"="確定日",
         "Hospital_Pref"="受診都道府県",
         "Residential_Pref"="居住都道府県")%>%
  filter(Residential_City!="")
data2<-fread("kanagawa.csv", encoding="UTF-8") %>%
  mutate(確定日= as.Date(確定日,format = "%m/%d/%Y"))%>%
  select(確定日,受診都道府県,居住都道府県,居住市区町村,備考,X,Y)%>%
  filter(居住都道府県=="神奈川県")%>%
  filter(居住市区町村!="川崎市",居住市区町村!="")%>%
  rename("Residential_City"="居住市区町村")%>%
  select(-備考)%>%
  rename("Fixed_Date"="確定日",
         "Hospital_Pref"="受診都道府県",
         "Residential_Pref"="居住都道府県")

patient<-
  read.csv("https://www.pref.kanagawa.jp/osirase/1369/data/csv/patient.csv") %>%
  filter(!str_detect(居住地,"管内")) %>%
  filter(発表日>="2020-12-01") %>%
  rename("Fixed_Date"="発表日","Residential_City"="居住地") %>%
  select(-年代,-性別)%>%
  mutate(Residential_City = str_replace(Residential_City,"神奈川県",""))%>%
  mutate(Fixed_Date=as.Date(Fixed_Date))

kanagawa<-read.csv("kanagawa2.csv") %>%
  select(-X,-note)%>%
  mutate(Fixed_Date=as.Date(Fixed_Date))


kanagawa2<-rbind(kanagawa,patient) %>%
  mutate(Hospital_Pref ="神奈川県",
         Residential_Pref="神奈川県"
  )

xy<-read.csv("xy.csv") %>%
  select(-X.1)%>%
  rename("Residential_City"="居住市区町村")
chigasaki<-
  read.csv("chigasaki.csv")%>%
  mutate(Hospital_Pref ="神奈川県",
         Residential_Pref="神奈川県"
  )%>%
  mutate(Fixed_Date=as.Date(Fixed_Date))%>%
  left_join(xy,by="Residential_City")%>%
  filter(!is.na(X))

list1<-read.csv("list.csv")
data3<-
  data%>%
  left_join(list1,by=c("Residential_City"="list"))%>%
  select(-Residential_City)%>%
  rename("Residential_City"="管内")%>%
  filter(!is.na(X))

kanagawa2<-
  left_join(kanagawa2,xy,by="Residential_City") %>%
  mutate(Fixed_Date=as.Date(Fixed_Date))%>%
  filter(!is.na(X))


kawasaki<-
  read.csv("kawasaki.csv") %>%
  select(-X)%>%
  mutate(Fixed_Date=as.Date(Fixed_Date))%>%
  left_join(list1)%>%
  select(-note,-管内,-Residential_City)%>%
  rename("Residential_City"="list")

data<-bind_rows(data2,data3,kanagawa2,kawasaki,chigasaki)
write.csv(data,"coviddata.csv",row.names=F)

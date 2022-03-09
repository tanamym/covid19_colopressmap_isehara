library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(lubridate)
library(pdftools)
library(data.table)
library(RCurl)
#url<-"https://www.pref.kanagawa.jp/docs/ga4/covid19/occurrence_23.html"#urlの指定
#url<-"https://www.pref.kanagawa.jp/docs/ga4/covid19/occurrence_24.html"
#rhtml<-rvest::read_html(url,encoding="UTF-8")
# pdf<-
#   rhtml%>%
#   html_nodes("a")%>%
#   html_attr("href")%>%#urlの抽出
#   as.data.frame()%>%
#   filter(str_detect(.,"pdf"))%>%
#   rename("pdf"=".")
# 
# #課題：誤字はどうやってみつけるのか
# pref<-
#   rhtml%>%
#   html_nodes("a")%>%
#   html_text()%>%
#   as.data.frame()%>%
#   filter(str_detect(.,"PDF"))%>%
#   rename("pref"=".")
# pdf_pref<-
#   cbind(pdf,pref)%>%
#   mutate(pdf_url=paste0("https://www.pref.kanagawa.jp",pdf))
# 
# today()-1
# month( "2021-03-31")
# month(today()-1)==3
#実行
#time2<-
#  Sys.time()
<<<<<<< HEAD
repeat{
while(format(Sys.time(),"%H")=="13"){
=======
while(TRUE){
>>>>>>> origin/main

  time<-
    Sys.time()
  url_top<-
    "https://www.pref.kanagawa.jp/docs/ga4/covid19/occurrence_list.html"
  rhtml_top<-rvest::read_html(url_top,encoding="UTF-8")
  html_top<-
    rhtml_top%>%
    html_nodes("a")%>%
    html_attr("href")%>%#urlの抽出
    as.data.frame()%>%
    filter(str_detect(.,"/docs/ga4/covid19/occurrence_"))%>%
    filter(!str_detect(.,"list"))%>%
    rename("html"=".")%>%
    mutate(html=paste0("https://www.pref.kanagawa.jp",html))
  for (hn  in 1:nrow(html_top)) {
    if(hn==1){
      flag<-
        data.frame()
    }
    url<-html_top[hn,]
<<<<<<< HEAD
    #url<-html_top[2,]
    #url<-html_top[3,]
=======
>>>>>>> origin/main
    rhtml<-rvest::read_html(url,encoding="UTF-8")
    #ファイルの該当する月
    month<-
      rhtml%>%
      html_nodes("p")%>%
      html_text()%>%
      as.data.frame()%>%
      filter(str_detect(.,"月"))%>%
      rename("month"=".")%>%
      mutate(month=str_replace_all(month,"月.+","月"))%>%
      filter(str_length(month)<=3)%>%
      mutate(month=str_replace_all(month,"月",""))%>%
      summarise(max=max(month))
    flag[hn,1]<-month$max
<<<<<<< HEAD
    #flag[2,1]<-month$max
=======
>>>>>>> origin/main
    #ファイルの該当する日
    day<-
      rhtml%>%
      html_nodes("p")%>%
      html_text()%>%
      as.data.frame()%>%
      filter(str_detect(.,"月"))%>%
      filter(str_detect(.,"日"))%>%
      rename("day"=".")%>%
      mutate(day=str_replace_all(day,"日.+","日"))
    flag[hn,2]<-day[1,1]
<<<<<<< HEAD
    #flag[2,2]<-day[1,1]
=======
    
>>>>>>> origin/main
    #年度？
    pdf<-
      rhtml%>%
      html_nodes("a")%>%
      html_attr("href")%>%#urlの抽出
      as.data.frame()%>%
      filter(str_detect(.,"pdf"))%>%
      rename("pdf"=".")
    t=1
    NEXT<-T
    while(NEXT){
      path<-
      paste0("https://www.pref.kanagawa.jp",pdf[t,1])
    pdf_t<-
      pdf_text(path)%>%
      strsplit("\r\n")
    year<-
      pdf_t[[1]]%>%
      data.frame()%>%
      filter(str_detect(.,"令和"))%>%
      str_replace(.,"年.+","年")%>%
      str_replace_all(" ","")
    if(str_detect(year,"令和")){
      flag[hn,3]<-year
      NEXT<-F
    }
    t=t+1
    }
    flag<-
      flag%>%
      mutate(V3=stri_trans_nfkc(V3))%>%
      mutate(seireki=ifelse(V3=="令和2年",2020,
                            ifelse(V3=="令和3年",2021,
                                   ifelse(V3=="令和4年",2022,NA))))

    print(hn)
    #年月が一致しなければ飛ばす
    if((month(today()-1)!=as.numeric(flag[hn,1]))|(year(today())!=flag[hn,4])){
      next
    }
    print("成功")
    pdf<-
      rhtml%>%
      html_nodes("a")%>%
      html_attr("href")%>%#urlの抽出
      as.data.frame()%>%
      filter(str_detect(.,"pdf"))%>%
      rename("pdf"=".")
    pref<-
      rhtml%>%
      html_nodes("a")%>%
      html_text()%>%
      as.data.frame()%>%
      filter(str_detect(.,"PDF"))%>%
      rename("pref"=".")
    pdf_pref<-
      cbind(pdf,pref)%>%
      mutate(pdf_url=paste0("https://www.pref.kanagawa.jp",pdf))
    #神奈川県のデータを取得
    kana_pref<-
      pdf_pref%>%
      filter(str_detect(pref,"神奈川県"))
    k1=1
    k2=nrow(kana_pref)
    for (k in k1:k2) {
      if(k==k1){
        #データフレームを初期化
        res5 <-data.frame()
        }
      #日付
      # day <-
      #   (Sys.Date() -k)%>%
      #   as.character() %>%
      #   str_replace_all("-","")
      #URLの指定

      #3月
      # path <-
      #   paste0("https://www.pref.kanagawa.jp/documents/72483/",day,"_kanagawa.pdf")
      # if(k==(as.numeric(Sys.Date()-as.Date("2021-3-20")))){
      #   path <-
      #     paste0("https://www.pref.kanagawa.jp/documents/72483/2021_0320_kanagawa.pdf")
      # }
      path<-
        kana_pref[k,3]
      if(pdf_text(path)[1]=="")print(paste("ファイル名:",path,"を取得出来ません"))
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
          #mutate(s = ifelse(str_detect(s,"―"), str_replace(s,"      "," 0-10 代 "),s)) %>%
          mutate(s = ifelse(str_detect(s,"10 歳未満"), str_replace(s,"10 歳未満","0-10 代"),s)) %>%
          #mutate(判明日 = str_replace_all(s," ","")) %>%
          mutate(判明日 = str_replace_all(s," 日.+","日")) %>%
          mutate(判明日 = str_replace_all(判明日," 日","日")) %>%
          mutate(判明日 = str_replace_all(判明日,"日.+","日")) %>%
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
    }
    (month(today())!=as.numeric(flag[hn,1]))|(year(today())!=flag[hn,4])
    if(month(today()-1)==4&year(today())==2021){
      kanagawa3<-
        data.frame()
    }
    #前回使用したURLに一致しなかった場合に実行
    if(html_top2!=html_top[1,]){
      kanagawa3<-
<<<<<<< HEAD
        rbind(kanagawa5,kanagawa3)
    }
    
    kanagawa5<-
=======
        rbind(kanagawa2,kanagawa3)
    }
    
    kanagawa2<-
>>>>>>> origin/main
      res5%>%
      mutate(判明日 = ifelse(str_detect(判明日,"12月"),
                          paste0("2020-",str_replace(判明日,"月","-")),
                          paste0("2021-",str_replace(判明日,"月","-"))
      ))%>%
      mutate(判明日 = str_replace(判明日,"日",""))%>%
      select(保健所,居住地,判明日)%>%
      rename("Fixed_Date"="判明日","note"="保健所","Residential_City"="居住地")
    
    kanagawa1<-
      rbind(kanagawa202012%>%
              mutate(判明日 = paste0("2020-",str_replace(判明日,"月","-"))
                        )%>% 
              mutate(判明日 = str_replace(判明日,"日",""))%>%
              select(年代,性別,保健所,居住地,判明日),
            kanagawa202101 %>%
              mutate(判明日 = ifelse(str_detect(判明日,"12月"),
                                  paste0("2020-",str_replace(判明日,"月","-")),
                                  paste0("2021-",str_replace(判明日,"月","-"))
                                  ))%>%
              mutate(判明日 = str_replace(判明日,"日",""))%>%
              select(年代,性別,保健所,居住地,判明日),
            kanagawa202102 %>%
              mutate(判明日 = ifelse(str_detect(判明日,"12月"),
                                  paste0("2020-",str_replace(判明日,"月","-")),
                                  paste0("2021-",str_replace(判明日,"月","-"))
                                  ))%>%
              mutate(判明日 = str_replace(判明日,"日",""))%>%
              select(年代,性別,保健所,居住地,判明日),
            kanagawa202103 %>%
              mutate(判明日 = ifelse(str_detect(判明日,"12月"),
                                  paste0("2020-",str_replace(判明日,"月","-")),
                                  paste0("2021-",str_replace(判明日,"月","-"))
                                  ))%>%
              mutate(判明日 = str_replace(判明日,"日",""))%>%
              select(年代,性別,保健所,居住地,判明日)
            )%>%
      select(-年代,-性別) %>%
      rename("Fixed_Date"="判明日","note"="保健所","Residential_City"="居住地") 
    
    #これまでのデータと作成したデータを結合
    kanagawa4<-
<<<<<<< HEAD
      rbind(kanagawa1,kanagawa5,kanagawa3)
=======
      rbind(kanagawa1,kanagawa2,kanagawa3)
>>>>>>> origin/main
    
    write.csv(kanagawa4,"kanagawa2.csv")
    print("神奈川県のデータを出力しました")


    #kawasaki
    kawa_pref<-
      pdf_pref%>%
      filter(str_detect(pref,"川崎"))
    k1=1
    k2=nrow(kawa_pref)
    for (k in k1:k2) {#1月
      if(k==k1){
        #データフレームを初期化
        kawa3 <-data.frame()
      }
     
      path<-
        kawa_pref[k,3]
      
      
      if(path=="https://www.pref.kanagawa.jp/documents/74417/20210402_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210402.pdf"
      }
      if(path=="https://www.pref.kanagawa.jp/documents/74417/20210405_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210405.pdf"
      }
      if(path=="https://www.pref.kanagawa.jp/documents/74417/20210408_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210408.pdf"
      }
      if(path=="https://www.pref.kanagawa.jp/documents/74417/20210411_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210411.pdf"
      }
      if(path=="https://www.pref.kanagawa.jp/documents/74417/0415_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210415.pdf"
      }
      if(path=="https://www.pref.kanagawa.jp/documents/74417/20210422_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210422.pdf"
      }
      if(path=="https://www.pref.kanagawa.jp/documents/74417/20210426_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210426.pdf"
      }
      if(path=="https://www.pref.kanagawa.jp/documents/75248/20210505_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210505.pdf"
      }
<<<<<<< HEAD
      if(path=="https://www.pref.kanagawa.jp/documents/75248/20210517_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210517.pdf"
      }
      if(path=="https://www.pref.kanagawa.jp/documents/75248/20210527_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210527.pdf"
      }
      if(path=="https://www.pref.kanagawa.jp/documents/76094/20210617_kawasaki.pdf"){
        path<-
          "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210617.pdf"
      }
=======
>>>>>>> origin/main
      if(pdf_text(path)[1]=="")print(paste("ファイル名:",path,"を取得出来ません"))
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
    }
    if(month(today())==4&year(today())==2021){
      kawasaki3<-
        data.frame()
    }
 #前回使用したURLに一致しなかった場合に実行   
    if(html_top2!=html_top[1,]){
      kawasaki3<-
        rbind(kawasaki2,kawasaki3)
    }
          
    kawasaki2<-
      kawa3%>%
      rename("list"="居住地","Fixed_Date"="確定日",
             "Residential_City"="居住市区町村",
             "Residential_Pref"="居住都道府県",
             "Hospital_Pref"="受診都道府県","note"="備考")%>%
      select(-番号,-年代,-性別,-番号2)
    
    kawasaki1<-
      rbind(kawa202012,kawa202101,kawa202102,kawa202103)%>%
      rename("list"="居住地","Fixed_Date"="確定日",
             "Residential_City"="居住市区町村",
             "Residential_Pref"="居住都道府県",
             "Hospital_Pref"="受診都道府県","note"="備考")%>%
      select(-番号,-年代,-性別,-番号2)
 
    kawasaki4<-
      rbind(kawasaki1,kawasaki2,kawasaki3)
    write.csv(kawasaki4,"kawasaki.csv")
    print("川崎市のデータを出力しました")

   

    #chigasaki
    chi_pref<-
      pdf_pref%>%
      filter(str_detect(pref,"茅ヶ崎"))
    k1=1
    k2=nrow(chi_pref)
    for (k in k1:k2) {#1月
      if(k==k1){
        #データフレームを初期化
        chi5 <-data.frame()
      }
      
      #4月
      path<-
        chi_pref[k,3]
      
      if(path=="https://www.pref.kanagawa.jp/documents/74417/20210405_chigasaki.pdf"){
        path<-
          "https://www.city.chigasaki.kanagawa.jp/_res/projects/default_project/_page_/001/042/848/20210405co.pdf"
        
      }
      if(path=="https://www.pref.kanagawa.jp/documents/74417/20210419_chigasaki.pdf"){
        path<-
          "https://www.city.chigasaki.kanagawa.jp/_res/projects/default_project/_page_/001/042/973/20210419.pdf"
        
      }
      if(path=="https://www.pref.kanagawa.jp/documents/74417/20210421_chigasaki.pdf"){
        path<-
          "https://www.city.chigasaki.kanagawa.jp/_res/projects/default_project/_page_/001/043/006/20210421co.pdf"
        
      }
      if(path=="https://www.pref.kanagawa.jp/documents/74417/20210426_chigasaki.pdf"){
        path<-
          "https://www.city.chigasaki.kanagawa.jp/_res/projects/default_project/_page_/001/043/064/20210426co.pdf"
        
      }
<<<<<<< HEAD
      if(path=="https://www.pref.kanagawa.jp/documents/75248/20210520_chigasaki.pdf"){
        path<-
          "https://www.city.chigasaki.kanagawa.jp/_res/projects/default_project/_page_/001/043/451/20210520co.pdf"
        
      }
=======
>>>>>>> origin/main
      if(pdf_text(path)[1]=="")print(paste("ファイル名:",path,"を取得出来ません"))
      chi4<-data.frame()
      #管内[0-9][0-9][0-9]例目|
      for (i in 1:length(pdf_text(path))) {
        if(nrow(pdf_text(path)[i]%>%
                str_split("\r\n") %>%
                data.frame() %>%
                data.table::setnames("chr") %>%
                filter(str_detect(chr,"No")))>0){
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

    }
    if(month(today())==4&year(today())==2021){
      chigasaki3<-
        data.frame()
    }
    #前回使用したURLに一致しなかった場合に実行
    if(html_top2!=html_top[1,]){
      chigasaki3<-
        rbind(chigasaki2,chigasaki3)
    }  
    chigasaki2<-
      chi5%>%
      mutate(陽性確定日 = ifelse(str_detect(陽性確定日,"12月"),
                            paste0("2020-",str_replace(陽性確定日,"月","-")),
                            paste0("2021-",str_replace(陽性確定日,"月","-"))
      ) )%>%
      mutate(陽性確定日 = str_replace(陽性確定日,"日",""))%>%
      select(年代,性別,居住地,陽性確定日)%>%
      select(-年代,-性別) %>%
      rename("Fixed_Date"="陽性確定日","Residential_City"="居住地")%>%
      filter(!str_detect(Fixed_Date,"NULL"))
    chigasaki1<-
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
  
    chigasaki4<-
      rbind(chigasaki1,chigasaki2,chigasaki3)
    write.csv(chigasaki4,"chigasaki.csv",row.names = F)
    print("茅ヶ崎市のデータを出力しました")


    print(paste("取得年月:",flag[hn,4],flag[hn,2],",today:",today()))
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
<<<<<<< HEAD
      mutate(Fixed_Date=as.Date(Fixed_Date))%>%
      filter(Residential_City%in%c("横浜市","相模原市","藤沢市","横須賀市"))
=======
      mutate(Fixed_Date=as.Date(Fixed_Date))
>>>>>>> origin/main
    
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
<<<<<<< HEAD
    write.csv(data,"coviddata.csv",row.names=F,fileEncoding="UTF-8")
=======
    write.csv(data,"coviddata.csv",row.names=F)
>>>>>>> origin/main
    print("coviddata.csvを出力しました")
  }
  
  
  
  # url<-"https://www.pref.kanagawa.jp/docs/ga4/covid19/occurrence_23.html"#urlの指定
  # rhtml<-rvest::read_html(url,encoding="UTF-8")
  #実行時間
  print(paste0(Sys.time()-time))
  print(Sys.time())
  #実行開始時間を保存
  time2<-time
  #urlを保存
  html_top2<-html_top[1,]
  #6時間経過後最初に戻る
<<<<<<< HEAD
  Sys.sleep(1800)
=======
  Sys.sleep(21600)
>>>>>>> origin/main
  # while (time+60*60*12>Sys.time()) {
  #   
  # }
}

<<<<<<< HEAD
}
=======

>>>>>>> origin/main

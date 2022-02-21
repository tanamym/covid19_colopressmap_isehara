url<-html_top[2,]
rhtml<-rvest::read_html(url,encoding="UTF-8")
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



kanagawa5<-
  res5%>%
  mutate(判明日 = ifelse(str_detect(判明日,"12月"),
                      paste0("2020-",str_replace(判明日,"月","-")),
                      paste0("2021-",str_replace(判明日,"月","-"))
  ))%>%
  mutate(判明日 = str_replace(判明日,"日",""))%>%
  select(保健所,居住地,判明日)%>%
  rename("Fixed_Date"="判明日","note"="保健所","Residential_City"="居住地")
#前回使用したURLに一致しなかった場合に実行

  kanagawa3<-
    rbind(kanagawa5,kanagawa3)
  
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
    if(path=="https://www.pref.kanagawa.jp/documents/75248/20210517_kawasaki.pdf"){
      path<-
        "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210517.pdf"
    }
    if(path=="https://www.pref.kanagawa.jp/documents/75248/20210527_kawasaki.pdf"){
      path<-
        "https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/20210527.pdf"
    }
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

  kawasaki2<-
    kawa3%>%
    rename("list"="居住地","Fixed_Date"="確定日",
           "Residential_City"="居住市区町村",
           "Residential_Pref"="居住都道府県",
           "Hospital_Pref"="受診都道府県","note"="備考")%>%
    select(-番号,-年代,-性別,-番号2)
  #前回使用したURLに一致しなかった場合に実行   

    kawasaki3<-
      rbind(kawasaki2,kawasaki3)
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
      if(path=="https://www.pref.kanagawa.jp/documents/75248/20210520_chigasaki.pdf"){
        path<-
          "https://www.city.chigasaki.kanagawa.jp/_res/projects/default_project/_page_/001/043/451/20210520co.pdf"
        
      }
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
    
    #前回使用したURLに一致しなかった場合に実行

      chigasaki3<-
        rbind(chigasaki2,chigasaki3)
      html_top2<-html_top[1,]
      
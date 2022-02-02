library(data.table)
library(dplyr)
library(pdftools)
library(rvest)
library(stringi)
library(stringr)
D<-"令和4年1月1日"
RtoS<-function(date=D){
  lo<-str_locate(date,"令和.+年")
  s=lo[1,1]
  e=lo[1,2]
  S<-as.numeric(str_sub(date,s+2,e-1))+2018
  seireki<-paste0(S,str_sub(date,e,-1))
  return(seireki)
}
#2021年8月20日作成
repeat{
  while(format(Sys.time(),"%H")%in%c("17","18","19","20")){
  
    time<-
      Sys.time()
    url_top<-
      "https://www.pref.kanagawa.jp/docs/ga4/covid19/occurrence_list.html"
    while(TRUE){
      rhtml_top<-try(rvest::read_html(url_top,encoding="UTF-8"))
      if(class(rhtml_top) != "try-error")break
    }
    

    html_top<-
      rhtml_top%>%
      html_nodes("a")%>%
      html_attr("href")%>%#urlの抽出
      as.data.frame()%>%
      filter(str_detect(.,"/docs/ga4/covid19/occurrence_"))%>%
      filter(!str_detect(.,"list"))%>%
      rename("html"=".")%>%
      mutate(html=paste0("https://www.pref.kanagawa.jp",html))
    # hn=1hn=2hn=3hn=4hn=5hn=6
  
    for (hn  in 1:nrow(html_top)) {
      if(hn==1){
        flag<-
          data.frame()
      }

      url<-html_top[hn,]

      while(TRUE){
        rhtml<-try(rvest::read_html(url,encoding="UTF-8"))
        if(class(rhtml) != "try-error")break
      }
      
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
        #変更####
        year<-
          pdf_t[[1]]%>%
          data.frame()%>%
          filter(str_detect(.,"令和"))%>%
          str_replace_all(.,"\n","")%>%
          str_replace_all(.,"年.+","年")%>%
          str_replace_all(" ","")%>%
          str_replace_all(.,".+?令和","令和")
        print(year)
        ####
        if(str_detect(year,"令和")){
          flag[hn,3]<-year
          NEXT<-F
        }
        t=t+1
      }
      if(flag[1,2]=="10月18日"){
        flag[1,3]<-"令和3年"
      }
      flag<-
        flag%>%
        mutate(V3=stri_trans_nfkc(V3))%>%
        mutate(seireki=ifelse(V3=="令和2年",2020,
                              ifelse(V3=="令和3年",2021,
                                     ifelse(V3=="令和4年",2022,NA))))
      
      print(hn)
     
      #年月が一致しなければ飛ばす
      if((month(Sys.Date()-1)!=as.numeric(flag[hn,1]))|(year(Sys.Date())!=flag[hn,4])){
        next
      }
      print("成功")
      pdf<-
        rhtml%>%
        html_nodes("a")%>%
        html_attr("href")%>%#urlの抽出
        as.data.frame()%>%
        filter(str_detect(.,"pdf"))%>%
        rename("pdf"=".")%>%
        distinct()
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
      #神奈川県のデータを取得####
      kana_pref<-
        pdf_pref%>%
        filter(str_detect(pref,"神奈川県"))
      k1=1
      k2=nrow(kana_pref)
      for (k in k1:k2) {
        if(k==k1){
          #データフレームを初期化
          res3 <-data.frame()
        }
        
        path<-
          kana_pref[k,3]
        if(pdf_text(path)[1]=="")print(paste("ファイル名:",path,"を取得出来ません"))
        #pdfを文字列として読み込み
        res <-
          pdf_text(path)
        date_kana<-
          res[[1]] %>%
          strsplit("\n")%>%
          data.frame() %>%
          data.table::setnames("chr")%>%
          filter(str_detect(chr,"日"))%>%
          mutate(pdate=str_replace_all(chr,"日.+","日"))%>%
          mutate(pdate=str_replace_all(pdate," ",""))%>%
          mutate(pdate=stri_trans_nfkc(pdate))
        
        for (i in 1:length(res)) {
          res2<-res[[i]]%>%
            str_split("\n")%>%
            data.frame() %>%
            mutate(発表日=date_kana[1,2])
          colnames(res2)[1]<-"chr"
          res3<-
            rbind(res2,res3)
          
        }
        
        
      }
      sr=which(grepl("^ ??[[:digit:]]+ ",res3$chr))

      n=1
      n=2
      res5 <- data.frame(発表日="",n=1:length(sr),年代="",性別="",保健所="",居住地="",判明日="")
      for (n in 1:length(sr)) {
        tds <- res3$chr[sr[n]]
        tds2 <- res3$chr[sr[n]-1]
        res5$発表日[n]=res3$発表日[sr[n]]
        re<-regexpr("^ ??[[:digit:]]+ ",tds)
        at<-attr(re,"match.length")
        res5$n[n]=as.numeric(substring(tds,re,re+at-1))
        
        re<-regexpr("[[:digit:]]{2,3} ??(代|歳未満|歳以上)",tds)
        at<-attr(re,"match.length")
        res5$年代[n]=substring(tds,re,re+at-1)
        if(res5$年代[n]==""){
          re<-regexpr("100 ??歳以??上??",tds2)
          at<-attr(re,"match.length")
          res5$年代[n]=substring(tds2,re,re+at-1)
        }
        re<-regexpr("男性|女性",tds)
        at<-attr(re,"match.length")
        res5$性別[n]=substring(tds,re,re+at-1)
        
        re<-regexpr("[^ ]{2,3}[市区町村都道府県]",tds)
        at<-attr(re,"match.length")
        res5$居住地[n]=substring(tds,re,re+at-1)
        
        re<-regexpr("厚木|小田原|平塚|鎌倉",tds)
        at<-attr(re,"match.length")
        res5$保健所[n]=substring(tds,re,re+at-1)
        
        re<-regexpr("[[:digit:]]{1,2} ??月 ??[[:digit:]]{1,2} ??日",tds)
        at<-attr(re,"match.length")
        res5$判明日[n]=substring(tds,re,re+at-1)

      }
      res5<-
        res5%>%
        mutate(年代=str_replace_all(年代,"10 ??歳未満","0-10"))%>%
        mutate(年代=str_remove_all(年代,"歳以上| "))%>%
        mutate(年代=str_remove_all(年代,"代|歳未満|歳以| "))%>%
        mutate(判明日=str_remove_all(判明日," "))%>%
        filter(年代!="")
      
      # res5<-
      #   res3%>%
      #   filter(str_length(chr)>10) %>%
      #   mutate(s = str_replace(chr,"^ +","")) %>%
      #   #mutate(s = ifelse(str_detect(s,"―"), str_replace(s,"      "," 0-10 代 "),s)) %>%
      #   mutate(s=str_replace_all(s,"代未満","歳未満")) %>%
      #   mutate(s = ifelse(str_detect(s,"10 歳未満"), str_replace(s,"10 歳未満","0-10 代"),s)) %>%
      #   #mutate(判明日 = str_replace_all(s," ","")) %>%
      #   mutate(判明日 = str_replace_all(s," 日.+","日")) %>%
      #   mutate(判明日 = str_replace_all(判明日," 日","日")) %>%
      #   mutate(判明日 = str_replace_all(判明日,"日.+","日")) %>%
      #   mutate(判明日 = str_replace(判明日," 月 ","月")) %>%
      #   mutate(判明日 = ifelse(str_detect(判明日,"11月"),
      #                       str_replace(判明日,"^.+?11月","11月"),
      #                       ifelse(str_detect(判明日,"12月"),
      #                              str_replace(判明日,"^.+?12月","12月"),
      #                              str_sub(判明日,start = -6,end = -1))))%>%
      #   mutate(s = str_replace_all(s," +","_")) %>%
      #   mutate(s = ifelse(str_detect(s,"市"),
      #                     str_replace(s,"市.+","市"),
      #                     str_replace(s,"町.+","町"))) %>%
      #   tidyr::separate(s,into = c("n","年代","代","性別","保健所",
      #                              "居住地"),sep ="_") %>%
      #   filter(str_length(n)<=3,!is.na(判明日),str_detect(判明日,"月"),代=="代")
      
   
      #前回使用したURLに一致しなかった場合に実行
      if(html_top2!=html_top[1,]){
        kanagawa3<-
          rbind(kanagawa5,kanagawa3)%>%
          filter(Fixed_Date<Sys.Date()-1)

        kana_hozon<-kanagawa3
        write.csv(kana_hozon,"kanagawa_202201.csv")
        #write.csv(kana_hozon,"kanagawa_202111.csv")
        #write.csv(kana_hozon,"kanagawa_202110.csv")
        #kanagawa3<-read.csv("kanagawa_202110.csv")[,-1]
        #kanagawa3<-read.csv("kanagawa_202111.csv")[,-1]
        #kanagawa3<-read.csv("kanagawa_202112.csv")[,-1]
        print("上書きしました")
      }
     
      kanagawa5<-
        res5%>%
        mutate(発表日 = RtoS(date = 発表日))%>%
        mutate(判明日 = ifelse(str_detect(判明日,"12月"),
                            paste0("2021-",str_replace(判明日,"月","-")),
                            paste0("2022-",str_replace(判明日,"月","-"))
        ))%>%
        mutate(判明日 = str_replace(判明日,"日",""))%>%
        mutate(判明日 = str_replace_all(判明日," ",""))%>%
        #追加####
      mutate(発表日=str_replace(発表日,"年","-"))%>%
        mutate(発表日=str_replace(発表日,"月","-"))%>%
        mutate(発表日=str_replace(発表日,"日",""))%>%
        mutate(発表日=str_replace_all(発表日," ",""))%>%
        ####
        #変更####
      select(保健所,居住地,判明日,発表日,年代,性別)%>%
        rename("Fixed_Date2"="判明日","note"="保健所","Residential_City"="居住地",
               "Age"="年代","Sex"="性別","PR_Date"="発表日")%>%
        mutate(Fixed_Date=PR_Date)
      ####
      
      kanagawa1<-
        read.csv("kanagawa202012.csv")
      
      
      #これまでのデータと作成したデータを結合
      kanagawa4<-
        rbind(kanagawa1,kanagawa5,kanagawa3)
      
      write.csv(kanagawa4,"kanagawa2.csv")
      print("神奈川県のデータを出力しました")
      #追加####
      #kawasaki####
      url_top2<-
        "https://www.city.kawasaki.jp/350/page/0000115886.html"
      while (TRUE) {
        rhtml_top2<-try(rvest::read_html(url_top2,encoding="UTF-8"))
        if(class(rhtml_top2) != "try-error")break
      }
      
      html_top3<-
        rhtml_top2%>%
        html_nodes("a")%>%
        html_attr("href")%>%#urlの抽出
        as.data.frame()%>%
        filter(str_detect(.,"contents"))%>%
        filter(!str_detect(.,"kyouiku")&!str_detect(.,"zyouge")&!str_detect(.,"kodomo")&
                 !str_detect(.,"miyamae")&!str_detect(.,"byouin")&!str_detect(.,"kotsu"))%>%
        filter(str_length(.)<=45) %>%
        rename("html"=".")%>%
        mutate(html=paste0("https://www.city.kawasaki.jp/350/",html))
      D2<-format(Sys.Date(),"%y%m")
      html_top3<- 
        html_top3%>%
        filter(str_detect(html,D2))

      if(D2=="2201"){
        html_top3[nrow(html_top3)+1,1]<-"https://www.city.kawasaki.jp/350/cmsfiles/contents/0000115/115886/040115.pdf"
      }
      k1=1
      k2=nrow(html_top3)
      
      for (k in k1:k2) {#1月
        if(k==k1){
          #データフレームを初期化
          kawa3 <-data.frame()
        }
        
        path<-
          html_top3[k,1]
        
        
        
        if(pdf_text(path)[1]=="")print(paste("ファイル名:",path,"を取得出来ません"))
        
        kawa2<-pdf_text(path)
        #追加####
        date_kawa<-
          kawa2[[1]]%>%
          strsplit("\n")%>%
          data.frame() %>%
          data.table::setnames("chr")%>%
          filter(str_detect(chr,"日"))%>%
          mutate(pdate=str_replace_all(chr,"日.+","日"))%>%
          mutate(pdate=str_replace_all(pdate," ",""))%>%
          mutate(pdate=stri_trans_nfkc(pdate))%>%
          mutate(pdate=ifelse(str_detect(pdate,"令和3年"),str_replace(pdate,"令和3年","2021年"),
                              ifelse(str_detect(pdate,"令和2年"),str_replace(pdate,"令和2年","2020年"),
                                     ifelse(str_detect(pdate,"令和4年"),str_replace(pdate,"令和4年","2022年"),pdate))))%>%
          mutate(pdate=str_replace_all(pdate,"日",""),
                 pdate=str_replace_all(pdate,"月","-"),
                 pdate=str_replace_all(pdate,"年","-"))
        
        if(as.Date(date_kawa[1,2])<as.Date("2021-07-01")){
          next
        }
        for (i in 1:length(pdf_text(path))) {
          kawa<-
            kawa2[[i]]%>%
            str_split("\n")%>%
            data.frame() %>%
            mutate(発表日=date_kawa[1,2])
          colnames(kawa)[1]<-"chr"
          kawa3<-
            rbind(kawa,kawa3)
          
        }
      }
      
      kawa4<-
        kawa3%>%
        filter(str_detect(chr,"例目")) %>%
        filter(str_detect(chr,"川崎市|県内|都内|県外|市外"))%>%
        mutate(s=str_replace(chr,"区.+","区")) %>%
        mutate(確定日=ifelse(str_detect(chr,"月"),str_sub(chr,start = -15,end = -4),str_sub(chr,start = -14,end = -4)))%>%
        mutate(確定日=str_replace(確定日,"日.+","日")) %>%
        mutate(s = str_replace_all(s," +","_")) %>%
        tidyr::separate(s,into = c("番号","番号2","年代","性別","居住地"),sep ="_")%>%
        mutate(年代=ifelse(str_detect(年代,"10歳未満"),"0-10",str_replace(年代,"代","")))%>%
        mutate(居住市区町村=ifelse(str_detect(居住地,"市外"),"市外","川崎市"))%>%
        mutate(居住都道府県=ifelse(str_detect(居住地,"都内"),"東京都",
                             ifelse(str_detect(居住地,"県内|川崎市"),"神奈川県","県外")))%>%
        mutate(受診都道府県="神奈川県")%>%
        mutate(備考=ifelse(str_detect(居住地,"区"),str_replace(居住地,"川崎市",""),居住地))%>%
        mutate(確定日=ifelse(str_detect(確定日,"月"),
                          str_replace_all(確定日,"[^(0-9月日)]",""),
                          確定日
        ))%>%
        mutate(確定日=ifelse(str_detect(確定日,"/"),
                          str_replace_all(確定日,"[^(0-9/)]",""),
                          確定日))%>%
        mutate(確定日=ifelse(str_detect(確定日,"12"),paste0("2021-",str_replace(確定日,"月","-")),
                          paste0("2022-",str_replace(確定日,"月","-"))))%>%
        mutate(確定日=ifelse(str_detect(確定日,"調査中"),NA,確定日),
                  確定日=str_replace(確定日,"日",""))%>%
        mutate(確定日=ifelse(str_detect(確定日,"/"),
                          str_replace(確定日,"/","-"),
                          確定日
        )
        )%>%
        select(-chr)
      kawa4<-
        kawa4%>%
        mutate(番号=str_remove(番号,"例目"))%>%
        mutate(番号=as.numeric(番号))%>%
        filter(!is.na(番号))
      if(format(Sys.Date(),"%m")=="09"){
        kawa4<-
          rbind(kawa4,data.frame(発表日="2021-9-4",番号=36717,番号2=" (394)",年代="20",性別="女性",
                                    居住地="川崎市宮前区",確定日=NA,居住市区町村="川崎市",居住都道府県="神奈川県",
                                    受診都道府県="神奈川県",備考="宮前区"))
      }
      #write.csv(kawa4,"kawasaki202108.csv",row.names = F)
      #write.csv(kawa4,"kawasaki202109.csv",row.names = F)
      #write.csv(kawa4,"kawasaki202110.csv",row.names = F)
      #write.csv(kawa4,"kawasaki202111.csv",row.names = F)
      write.csv(kawa4,paste0("kawasaki",D2,".csv"),row.names = F)
      kawasaki2<-
        kawa4%>%
        rbind(read.csv("kawasaki202107.csv"),
              read.csv("kawasaki202108.csv"),
              read.csv("kawasaki202109.csv"),
              read.csv("kawasaki202110.csv"),
              read.csv("kawasaki202111.csv"),
              read.csv("kawasaki2112.csv"),
              read.csv("kawasaki2201.csv"))%>%
        rename("list"="居住地","Fixed_Date2"="確定日",
               "Residential_City"="居住市区町村",
               "Residential_Pref"="居住都道府県",
               "Hospital_Pref"="受診都道府県","note"="備考",
               "Age"="年代","Sex"="性別","PR_Date"="発表日")%>%
        select(-番号,-番号2)%>%
        rbind(read.csv("kawasaki202106.csv")%>%rename("Fixed_Date2"="Fixed_Date"))%>%
        mutate(Fixed_Date=PR_Date)
      
      
      kawasaki1<-read.csv("kawasaki202012.csv")
      
      kawasaki4<-
        rbind(kawasaki1,kawasaki2)
      write.csv(kawasaki4,"kawasaki.csv")
      print("川崎市のデータを出力しました")
      
      #茅ヶ崎市####
      
      url_top3<-
        "https://www.city.chigasaki.kanagawa.jp/koho/1030702/1038773/index.html"
      while(TRUE){
        rhtml_top3<-try(rvest::read_html(url_top3,encoding="UTF-8"))
        if(class(rhtml_top3) != "try-error")break
      }
      
      
      html_top4<-
        rhtml_top3%>%
        html_nodes("a")%>%
        html_attr("href")%>%#urlの抽出
        as.data.frame()%>%
        filter(str_detect(.,"html"))%>%
        filter(str_detect(.,"1038773"))%>%
        rename("html"=".")%>%
        mutate(html=str_remove(html,"../../../"))%>%
        mutate(html=paste0("https://www.city.chigasaki.kanagawa.jp/",html))
      while (TRUE) {
        rhtml4<-try(rvest::read_html(html_top4[1,1],encoding="UTF-8")%>%
                      html_nodes("a")%>%
                      html_attr("href")%>%
                      as.data.frame()%>%
                      filter(str_detect(.,"default_project"))%>%
                      rename("html"=".")%>%
                      mutate(html=str_remove(html,"../../../"))%>%
                      mutate(html=paste0("https://www.city.chigasaki.kanagawa.jp/",html)))
        if(class(rhtml4) != "try-error")break
      }
      
      chigasaki2<-data.frame()
      D<-Sys.Date()%>%
        str_remove_all("-")%>%
        str_remove_all("2021|2022")
      chi_pref<-
        pdf_pref%>%
        filter(str_detect(pref,"茅ヶ崎"))
      chi4<-data.frame()
      if(str_detect(rhtml4[1,1],D)&nrow(chi_pref)==0){
        path2<-rhtml4[1,1]
        chi2<-pdf_text(path2)
        date_chi2<-
          chi2[1]%>%
          strsplit("\n")%>%
          data.frame() %>%
          data.table::setnames("chr")%>%
          filter(str_detect(chr,"日")&str_detect(chr,"年"))%>%
          mutate(pdate=str_replace_all(chr,"日.+","日"))%>%
          mutate(pdate=str_replace_all(pdate," ",""))%>%
          mutate(pdate=stri_trans_nfkc(pdate))%>%
          mutate(pdate=str_replace_all(pdate,"日",""),
                 pdate=str_replace_all(pdate,"月","-"),
                 pdate=str_replace_all(pdate,"年","-"))
        for (i in 1:length(chi2)) {
          chi3<-chi2[[i]]%>%
            str_split("\n")%>%
            data.frame() %>%
            mutate(発表日=date_chi2[1,2])
          colnames(chi3)[1]<-"chr"
          chi4<-
            rbind(chi4,chi3)
          }
        chi5<-
          chi4%>%
          filter(str_detect(chr,"例目")) %>%
          filter(str_detect(chr,"市|町|県内|県外")) %>%
          mutate(chr=str_replace(chr,"^ +",""),
                 chr=str_replace_all(chr," +","_"),
                 chr=str_replace(chr,"代",""))%>%
          tidyr::separate(chr,into = c("X1","例目","年代","性別","居住地",
                                       "X6","X7","X8","陽性確定日","X10"),sep ="_")%>%
          dplyr::select(例目,年代,性別,居住地,陽性確定日,発表日)%>%
          filter(!is.na(例目))%>%
          mutate(年代=stringi::stri_trans_nfkc(年代))
        chigasaki2<-
          chi5%>%
          mutate(陽性確定日 = ifelse(str_detect(陽性確定日,"12月"),
                                paste0("2021-",str_replace(陽性確定日,"月","-")),
                                paste0("2022-",str_replace(陽性確定日,"月","-"))
          ) )%>%
          mutate(陽性確定日 = str_replace(陽性確定日,"日",""))%>%
          select(年代,性別,居住地,陽性確定日,発表日)%>%
          rename("Fixed_Date2"="陽性確定日","Residential_City"="居住地",
                 "Age"="年代","Sex"="性別","PR_Date"="発表日")%>%
          mutate(Fixed_Date=PR_Date)%>%
          filter(!str_detect(Fixed_Date,"NULL"))
        }
      #chigasaki####
      chi_pref<-
        pdf_pref%>%
        filter(str_detect(pref,"茅ヶ崎"))
      if(nrow(chi_pref)!=0){
        k1=1
        k2=nrow(chi_pref)
        for (k in k1:k2) {#1月
          
          if(k==k1){
            #データフレームを初期化
            chi4 <-data.frame()
            }
          #4月
          path<-
            chi_pref[k,3]
      
          chi<-pdf_text(path)
        
          if(pdf_text(path)[1]=="")print(paste("ファイル名:",path,"を取得出来ません"))
          #追加####
          date_chi<-
            chi[1]%>%
            strsplit("\n")%>%
            data.frame() %>%
            data.table::setnames("chr")%>%
            filter(str_detect(chr,"日")&str_detect(chr,"年"))%>%
            mutate(pdate=str_replace_all(chr,"日.+","日"))%>%
            mutate(pdate=str_replace_all(pdate," ",""))%>%
            mutate(pdate=stri_trans_nfkc(pdate))%>%
            mutate(pdate=str_replace_all(pdate,"日",""),
                 pdate=str_replace_all(pdate,"月","-"),
                 pdate=str_replace_all(pdate,"年","-"))
          if(k==1&date_chi[1,2]<Sys.Date()&str_detect(rhtml4[1,1],D)){
            path2<-rhtml4[1,1]
            chi2<-pdf_text(path2)
            date_chi2<-
              chi2[1]%>%
              strsplit("\n")%>%
              data.frame() %>%
              data.table::setnames("chr")%>%
              filter(str_detect(chr,"日")&str_detect(chr,"年"))%>%
              mutate(pdate=str_replace_all(chr,"日.+","日"))%>%
              mutate(pdate=str_replace_all(pdate," ",""))%>%
              mutate(pdate=stri_trans_nfkc(pdate))%>%
              mutate(pdate=str_replace_all(pdate,"日",""),
                   pdate=str_replace_all(pdate,"月","-"),
                   pdate=str_replace_all(pdate,"年","-"))
            for (i in 1:length(chi2)) {
              chi3<-chi2[[i]]%>%
                str_split("\n")%>%
                data.frame() %>%
                mutate(発表日=date_chi2[1,2])
              colnames(chi3)[1]<-"chr"
              chi4<-
                rbind(chi4,chi3)
            }
            }
          for (i in 1:length(chi)) {
            chi3<-chi[[i]]%>%
              str_split("\n")%>%
              data.frame() %>%
              mutate(発表日=date_chi[1,2])
            colnames(chi3)[1]<-"chr"
            chi4<-
              rbind(chi4,chi3)
          }
          }
        chi5<-
          chi4%>%
          filter(str_detect(chr,"例目")) %>%
          filter(str_detect(chr,"市|町|県内|県外")) %>%
          mutate(chr=str_replace(chr,"^ +",""),
               chr=str_replace_all(chr," +","_"),
               chr=str_replace(chr,"代",""))%>%
          tidyr::separate(chr,into = c("X1","例目","年代","性別","居住地",
                                     "X6","X7","X8","陽性確定日","X10"),sep ="_")%>%
          dplyr::select(例目,年代,性別,居住地,陽性確定日,発表日)%>%
          filter(!is.na(例目))%>%
          mutate(年代=stringi::stri_trans_nfkc(年代))%>%
          distinct(例目,.keep_all = T)
        chigasaki2<-
          chi5%>%
          mutate(陽性確定日 = ifelse(str_detect(陽性確定日,"12月"),
                              paste0("2021-",str_replace(陽性確定日,"月","-")),
                              paste0("2022-",str_replace(陽性確定日,"月","-"))
                              ))%>%
          mutate(陽性確定日 = str_replace(陽性確定日,"日",""))%>%
          select(年代,性別,居住地,陽性確定日,発表日)%>%
          rename("Fixed_Date2"="陽性確定日","Residential_City"="居住地",
               "Age"="年代","Sex"="性別","PR_Date"="発表日")%>%
          mutate(Fixed_Date=PR_Date)%>%
          filter(!str_detect(Fixed_Date,"NULL"))
        }
      
      
      #前回使用したURLに一致しなかった場合に実行
      if(html_top2!=html_top[1,]){
        # chigasaki3<-chigasaki3%>%
        #   filter(Fixed_Date!="2021-9-1")
        chigasaki3<-
          rbind(chigasaki2,chigasaki3)
        chigasaki3<-
          chigasaki3%>%
          filter(Fixed_Date<Sys.Date()-1)
        chi_hozon<-chigasaki3
        write.csv(chi_hozon,"chigasaki_202201.csv")
        #write.csv(chi_hozon,"chigasaki_202111.csv")
        #chigasaki3<-read.csv("chigasaki_202109.csv")[,-1]
        print("上書きしました")
      }  
      
      
      chigasaki1<-
        read.csv("chigasaki202012.csv")
      
      chigasaki4<-
        rbind(chigasaki1,chigasaki2,chigasaki3)
      write.csv(chigasaki4,"chigasaki.csv",row.names = F)
      print("茅ヶ崎市のデータを出力しました")
      
      #横須賀市####
      while(TRUE){
        HTML <- try(read_html("https://www.city.yokosuka.kanagawa.jp/3130/hasseijoukyou.html"))
        if(class(HTML) != "try-error")break
      }
      #HTML <- try(read_html("https://www.city.yokosuka.kanagawa.jp/3130/hasseijoukyou_202201.html"))
      #HTML <- read_html("https://www.city.yokosuka.kanagawa.jp/3130/hasseijoukyou_202110.html")
      #HTML<-read_html("https://www.city.yokosuka.kanagawa.jp/3130/hasseijoukyou_202109.html")
      #HTML <- read_html("https://www.city.yokosuka.kanagawa.jp/3130/hasseijoukyou_202108_2.html")
      #HTML
      D1<-format(Sys.Date(),"%Y%m")
      Ahref <-
        HTML %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        data.frame() %>%
        rename(html=".") %>%
        filter(grepl(paste0("nagekomi/",D1),html))%>%
        distinct()
      D2<-format(Sys.Date(),"%Y%m%d")
      file<-paste0("/3130/nagekomi/",D2,".html")
      HT<-read_html(paste0("https://www.city.yokosuka.kanagawa.jp",
                       file))%>%
        html_nodes("div") %>%
        html_text() %>%
        data.frame() %>%
        rename(Text=".") %>%
        filter(grepl("^\n+新型コロナウイルス感染症による市内の患者確認",Text))
      if(length(HT)!=0){
         if(nrow(Ahref)==0){
        Ahref<-data.frame(html=file)
      }else if(Ahref[1,1]!=file){
        Ahref<-rbind(file,Ahref)
      }
      }
     
      Ahref2<-cbind(HTML %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        data.frame() %>%
        rename(html="."),
      HTML %>%
        html_nodes("a")%>%
        html_text()%>%
        data.frame()%>%
        rename("Title"="."))%>%
        filter(grepl("documents",html))%>%
        filter(grepl("発表分の一覧",Title))%>%
        mutate(pdf=paste0("https://www.city.yokosuka.kanagawa.jp",html))
      if(nrow(Ahref)!=0){
        TD <- data.frame()
        #p="https://www.city.yokosuka.kanagawa.jp/3130/documents/itiran0119.pdf"
        for (p in Ahref2$pdf) {
        
          pdf<-pdf_text(p)
          if(pdf[1]=="")print(paste("ファイル名:",p,"を取得出来ません"))
          re<-regexpr("202.年.+月.+日\n",pdf[[1]])
          at<-attr(re,"match.length")
          d=stri_trans_nfkc(substring(pdf[[1]],re,re+13))
          re<-regexpr("202.年.+月.+日",d)
          at<-attr(re,"match.length")
          d=stri_trans_nfkc(substring(d,re,at))


          
          for (l in 1:length(pdf)) {
            td <-
              pdf[[l]] %>%
              strsplit("\n") %>%
              data.frame() %>%
              mutate(Date=d) %>%
              mutate(Date=gsub("令和4年","2022年",Date)) %>%
              mutate(Date=gsub("令和3年","2021年",Date)) %>%
              mutate(Date=gsub("令和2年","2020年",Date)) %>%
              mutate(Date=gsub("日.+","日",Date))
            
            colnames(td)[1] <- "Text"
            
            TD <-
              TD %>%
              rbind(td)
            
         
          }
        
        }
        TD <-
          TD %>%
          mutate(Text2=stri_trans_nfkc(Text))
        TD2<-TD%>%
          mutate(s = str_replace_all(Text2,"^ +","")) %>%
          mutate(s = str_replace_all(s," +","_")) %>%
          tidyr::separate(s,into = c("No","性別","年代",
                                     "職業等","患者確定日"),sep ="_") %>%
          filter(!is.na(No))%>%
          filter(str_detect(性別,"男|女"))%>%
          select(Date,No,性別,年代,職業等,患者確定日)%>%
          mutate(No=as.numeric(No))%>%
          arrange(Date,No)
        #TD2$No<-seq(7095,7095+nrow(TD2)-1)
        TD2<-TD2%>%
          mutate(No=ifelse(Date=="2022年1月21日",seq(7369,7490),No))
        Table <-
        HTML %>%
        html_table() %>%
        data.frame() %>%
        select(-starts_with("No..")) %>%
        rename(No="No.") %>%
        mutate(No=as.numeric(No))
      TD <- data.frame()

      for (i in 1:nrow(Ahref)) {
        
        while(TRUE){
          yh <- try(read_html(paste0("https://www.city.yokosuka.kanagawa.jp",Ahref$html[i])))
          if(class(yh) != "try-error")break
        }
        
      
        yht <-
          yh %>%
          html_nodes("div") %>%
          html_text() %>%
          data.frame() %>%
          rename(Text=".") %>%
          filter(grepl("^\n+新型コロナウイルス感染症による市内の患者確認",Text))
        
        td <-
          yht$Text[1] %>%
          strsplit("\n") %>%
          data.frame() %>%
          mutate(Date=substring(Ahref$html[i],16,23))
        
        colnames(td)[1] <- "Text"
        
        TD <-
          TD %>%
          rbind(td)
   
      }
      
      TD <-
        TD %>%
        mutate(Text2=stri_trans_nfkc(Text)) %>%
        mutate(Text2=gsub(" ","",Text2))
      
      sr=which(grepl("^[[:digit:]]+(例|例目)$",TD$Text2))
      lr=c(sr[-1]-1,nrow(TD))
      
      te <- TD[sr,]
      
      n=1
      TDS <- data.frame(Date="",No=1:length(sr),Age="",Sex="",Hos="",City="")
      for (n in 1:length(sr)) {
        tds <- paste0(TD$Text2[sr[n]:lr[n]],collapse = " ") #:min(sr[n+1]-1,sr[length(sr)],na.rm = T)
        
        TDS$Date[n]=TD$Date[sr[n]]
        t<-attr(regexpr("(例|例目)",tds),"match.length")+1
        re<-regexpr("[[:digit:]]+(例|例目)",tds)
        at<-attr(re,"match.length")
        TDS$No[n]=as.numeric(substring(tds,re,re+at-t))
        
        re<-regexpr("[[:digit:]]{2,3}[代歳]|.学生|..学児",tds)
        at<-attr(re,"match.length")
        TDS$Age[n]=substring(tds,re,re+at-1)
        
        re<-regexpr("男性|女性|男児|女児",tds)
        at<-attr(re,"match.length")
        TDS$Sex[n]=substring(tds,re,re+at-1)
        
        re<-regexpr("居所:[^ ]+",tds)
        at<-attr(re,"match.length")
        TDS$City[n]=substring(tds,re+3,re+at-1)
        
        re<-regexpr("同居人:[^ ]+",tds)
        at<-attr(re,"match.length")
        TDS$同居人[n]=substring(tds,re+4,re+at-1)
        
        re<-regexpr("感染経路:[^ ]+",tds)
        at<-attr(re,"match.length")
        TDS$感染経路[n]=substring(tds,re+5,re+at-1)
        
        re<-regexpr("現在の症状:[^ ]+",tds)
        at<-attr(re,"match.length")
        TDS$現在の症状[n]=substring(tds,re+6,re+at-1)
        
     
      }
      # TDS<-
      #   rbind(TDS,data.frame(Date="20220126",No=8275,Sex="男性",Age="10歳未満",City="横須賀市",同居人="調査中",
      #            感染経路="調査中",現在の症状="不明",Hos=""))
      
      te <-
        TDS %>%
        count(City)
      
      TDS2 <-
        TDS %>%
        mutate(Hos="横須賀") %>%
        mutate(Date=as.Date(Date,format="%Y%m%d")) %>%
        arrange(desc(Date),desc(No))

      
      
      TDS3 <-
        Table %>%
        select(No,患者確定日,年代,性別,職業等)%>%
        rbind(TD2%>%select(-Date))%>%
        full_join(TDS2)%>%
        filter(!is.na(Age))%>%
        filter(Date>=as.Date(paste0(D1,"01"),"%Y%m%d"))
      
       #write.csv(TDS3,"yokosuka_202107.csv",row.names = F)
      #write.csv(TDS3,"yokosuka_202108.csv",row.names = F)
      #write.csv(TDS3,"yokosuka_202109.csv",row.names = F)
      #write.csv(TDS3,"yokosuka_202110.csv",row.names = F)
      #write.csv(TDS3,"yokosuka_202111.csv",row.names = F)
      write.csv(TDS3,paste0("yokosuka_",D1,".csv"),row.names = F)
      yokosuka<-rbind(
        read.csv("yokosuka_202106.csv"),
        read.csv("yokosuka_202105.csv"),
        read.csv("yokosuka_202107.csv"),
        read.csv("yokosuka_202108.csv"),
        read.csv("yokosuka_20210815.csv"),
        read.csv("yokosuka_202109.csv"),
        read.csv("yokosuka_202110.csv"),
        read.csv("yokosuka_202111.csv"),
        read.csv("yokosuka_202112.csv"),
        read.csv("yokosuka_202201.csv"),
        read.csv("yokosuka_202202.csv")
      )%>%
        filter(!is.na(No))%>%
        rename("PR_Date"="Date","Fixed_Date2"="患者確定日")%>%
        mutate(Fixed_Date=PR_Date)%>%
        mutate(Fixed_Date2=paste("2022-",str_replace(Fixed_Date2,"月","-")))%>%
        mutate(Fixed_Date2=str_remove(Fixed_Date2,"日"),
               Fixed_Date2=as.Date(Fixed_Date2))%>%
        arrange(desc(Fixed_Date))
      }

      if(nrow(Ahref)==0){
        yokosuka<-rbind(
          read.csv("yokosuka_202106.csv"),
          read.csv("yokosuka_202105.csv"),
          read.csv("yokosuka_202107.csv"),
          read.csv("yokosuka_202108.csv"),
          read.csv("yokosuka_20210815.csv"),
          read.csv("yokosuka_202109.csv"),
          read.csv("yokosuka_202110.csv"),
          read.csv("yokosuka_202111.csv"),
          read.csv("yokosuka_202112.csv")
        )%>%
          filter(!is.na(No))%>%
          rename("PR_Date"="Date","Fixed_Date2"="患者確定日")%>%
          mutate(Fixed_Date=PR_Date)%>%
          mutate(Fixed_Date2=paste("2022-",str_replace(Fixed_Date2,"月","-")))%>%
          mutate(Fixed_Date2=str_remove(Fixed_Date2,"日"),
                 Fixed_Date2=as.Date(Fixed_Date2))%>%
          arrange(desc(Fixed_Date))
      }
      
      write.csv(yokosuka,"yokosuka.csv")
      print("横須賀市を出力しました。")
      
      #横浜市####
      

      
      Yokohama <-
        pdf_pref%>%
        filter(grepl("yokohama.pdf$",pdf))
      TD <- data.frame()
      TD2 <- data.frame()
      for (p in Yokohama$pdf) {
        path2<-paste0("https://www.pref.kanagawa.jp",p)
        if(p=="/documents/77029/20210702_yokohama.pdf"){
          path2<-"https://www.city.yokohama.lg.jp/city-info/koho-kocho/press/kenko/2021/0702covid-19.files/0001_20210702.pdf"
        }
        if(p=="/documents/78017/202108071_yokohama.pdf"){
          path2<-"https://www.city.yokohama.lg.jp/city-info/koho-kocho/press/kenko/2021/0807covid-19.files/0807covid-19.pdf"
        }
        pdf<-pdf_text(path2)
        if(pdf[1]=="")print(paste("ファイル名:",path2,"を取得出来ません"))
        re<-regexpr("令和.+日\n",pdf[[1]])
        at<-attr(re,"match.length")
        d=stri_trans_nfkc(substring(pdf[[1]],re,re+at-2))
        td<-
          pdf[[1]] %>%
          strsplit("\n") %>%
          data.frame() %>%
          mutate(Date=d) %>%
          mutate(Date=gsub("令和4年","2022年",Date)) %>%
          mutate(Date=gsub("令和3年","2021年",Date)) %>%
          mutate(Date=gsub("令和2年","2020年",Date)) %>%
          mutate(Date=gsub("日.+","日",Date))
        colnames(td)[1] <- "Text"
        TD2 <-
          TD2%>%
          rbind(td)
          
        for (l in 2:length(pdf)) {
          td <-
            pdf[[l]] %>%
            strsplit("\n") %>%
            data.frame() %>%
            mutate(Date=d) %>%
            mutate(Date=gsub("令和4年","2022年",Date)) %>%
            mutate(Date=gsub("令和3年","2021年",Date)) %>%
            mutate(Date=gsub("令和2年","2020年",Date)) %>%
            mutate(Date=gsub("日.+","日",Date))
          
          colnames(td)[1] <- "Text"
          
          TD <-
            TD %>%
            rbind(td)
     
        }
       
      }
      TD <-
        TD %>%
        mutate(Text2=stri_trans_nfkc(Text))%>%
        mutate(Text2=gsub("\u2ec4","西",Text2))%>%
        mutate(Text2=gsub("\u6236塚","戸塚",Text2))%>%
        mutate(Text2=gsub("\u2ed8葉","青葉",Text2))
      TD2 <-
        TD2 %>%
        mutate(Text2=stri_trans_nfkc(Text))
      sr2=which(grepl("市外.県内",TD2$Text2))
      TDS2<-
        data.frame()
      for (n in 1:length(sr2)) {
    
        tds2 <- paste0(TD2$Text2[sr2[n]+1])%>%
          data.frame()%>%
          mutate(Date="")
        colnames(tds2)[1]<-"chr"
        TDS2<-
          rbind(TDS2,tds2)
        TDS2$Date[n]=TD2$Date[sr2[n]]
      }
      TDS3<-
        TDS2%>%
        mutate(chr=str_replace_all(chr," +","_"),
               chr=str_remove(chr,"^_"))%>%
        tidyr::separate(chr,into =c("横浜市","市外(県内)","市外(県外)","市外(都内)","計"),sep ="_")%>%
        tidyr::pivot_longer(cols=-Date,names_to="City",
                            values_to = "count")%>%
        filter(City!="計")
      if(str_detect(Sys.Date(),"09")){
        TDS3<-
          rbind(TDS3,data.frame(Date=c("2021年9月24日","2021年9月24日","2021年9月24日","2021年9月24日"),
                                City=c("横浜市","市外(県内)","市外(県外)","市外(都内)"),
                                count=c(93,3,0,1)))
      }
      #write.csv(TDS3,"yoko_covid2108.csv",row.names = F)
      #write.csv(TDS3,"yoko_covid2109.csv",row.names = F)
      #write.csv(TDS3,"yoko_covid2110.csv",row.names = F)
      write.csv(TDS3,paste0("yoko_covid",format(Sys.Date()-1,"%y%m"),".csv"),row.names = F)
      yoko_covid<-
        rbind(read.csv("yoko_covid2108.csv"),
            read.csv("yoko_covid2109.csv"),
            read.csv("yoko_covid2110.csv"),
            read.csv("yoko_covid2111.csv"),
            read.csv("yoko_covid2112.csv"),
            read.csv("yoko_covid2201.csv"),
            read.csv("yoko_covid2202.csv")
            )%>%
        mutate(Date=as.Date(Date,format="%Y年%m月%d日"))%>%
        mutate(hos="yokohama")%>%
        filter(Date<"2021-12-08")
      write.csv(yoko_covid,"yoko_covid.csv",row.names = F,fileEncoding="UTF-8")
      sr=which(grepl("(男|女)|調査中",TD$Text2))
      n=1
      TDS <- data.frame(Date="",No=1:length(sr),Age="",Gender="",Hos="",City="")
      for (n in 1:length(sr)) {
        tds <- paste0(TD$Text2[sr[n]])
        
        TDS$Date[n]=TD$Date[sr[n]]
        
        re<-regexpr("^[[:digit:]]+ ",tds)
        at<-attr(re,"match.length")
        TDS$No[n]=as.numeric(substring(tds,re,re+at-1))
        
        re<-regexpr("([[:digit:]]{2,3}[代歳])|調査中",tds)
        at<-attr(re,"match.length")
        TDS$Age[n]=substring(tds,re,re+at-1)
        
        re<-regexpr("(男|女)|調査中",tds)
        at<-attr(re,"match.length")
        TDS$Gender[n]=substring(tds,re,re+at-1)
        
        re<-regexpr("[^ ]+[市区町村]|[^ ]+[内外])|(鶴見|神奈川|西| 中|南|港南|保土ケ谷|旭|磯子|金沢|港北|緑|青葉|都筑|戸塚|栄|泉|瀬谷)",tds)
        at<-attr(re,"match.length")
        TDS$City[n]=substring(tds,re,re+at-1)
        if(as.Date(TDS$Date[n],"%Y年%m月%d日")<"2021-12-08") TDS$City[n]=""
        
    
      }
      
      te <-
        TDS %>%
        count(Date)
      
      TDS <-
        TDS %>%
        mutate(Date=as.Date(Date,format="%Y年%m月%d日")) %>%
        mutate(Hos="横浜") %>%
        mutate(City=str_remove(City," ")) %>%
        arrange(desc(Date),desc(No))
  
      if(format(Sys.Date(),"%m")=="08"){
        TDS<-TDS%>%
          rbind(read.csv("yokohama0819covid-19.csv")%>%
                  rename("Age"="年代","Gender"="性別","City"="居住地")%>%
                  select(No,Age,Gender,City)%>%
                  mutate(Date=as.Date("2021-08-19"))%>%
                  mutate(Hos="横浜")%>%
                  filter(No!="No")%>%
                  filter(No!=""))%>%
          arrange(desc(Date))
      }
      #write.csv(TDS,"yokohama202108.csv",row.names = F)
      #write.csv(TDS,"yokohama202109.csv",row.names = F)
      #write.csv(TDS,"yokohama202110.csv",row.names = F)
      write.csv(TDS,paste0("yokohama",format(Sys.Date()-1,"%Y%m"),".csv"),row.names = F)
      #横浜市today####
      Date<-format(Sys.Date(),"%m%d")
      while(TRUE){
         yoko_html1<-try(read_html("https://www.city.yokohama.lg.jp/city-info/koho-kocho/press/kenko/")%>%
                           html_nodes("a")%>%
                           html_attr("href")%>%
                           data.frame()%>%
                           filter(str_detect(.,"covid"))%>%
                           #filter(str_detect(.,"covod"))%>%
                           mutate(flag=str_detect(.,Date)))
         if(class(yoko_html1) != "try-error")break
        
      }
     
      if(Date=="0107"){
        yoko_html1[1,1]<-
          "/city-info/koho-kocho/press/kenko/2021/0107covi--19.html"
        yoko_html1[1,2]<-TRUE
      }
      if(Date=="0131"){
        yoko_html1[1,1]<-
          "/city-info/koho-kocho/press/kenko/2021/0131covid-19.html"
        yoko_html1[1,2]<-TRUE
      }
      
      if(yoko_html1[1,2]==TRUE){
        while(TRUE){
          yoko_pdf<-try(read_html(paste0("https://www.city.yokohama.lg.jp",yoko_html1[1,1]))%>%
                          html_nodes("a")%>%
                          html_attr("href")%>%
                          data.frame()%>%
                          filter(str_detect(.,"pdf"))%>%
                          mutate(pdf=paste0("https://www.city.yokohama.lg.jp/city-info/koho-kocho/press/kenko/2021/",.)))
          if(class(yoko_pdf) != "try-error")break
        }
        
          
        
        TD <- data.frame()
        TD2 <- data.frame()
        if(yoko_pdf[1,2]=="https://www.city.yokohama.lg.jp/city-info/koho-kocho/press/kenko/2021/030913covid-19.files/030913covid-19.pdf"){
          yoko_pdf[1,2]="https://www.city.yokohama.lg.jp/city-info/koho-kocho/press/kenko/2020/030913covid-19.files/030913covid-19.pdf"
        }
        pdf<-pdf_text(yoko_pdf[1,2])
        if(pdf[1]=="")print(paste("ファイル名:",yoko_pdf[1,2],"を取得出来ません"))
        re<-regexpr("令和.+日\n",pdf[[1]])
        at<-attr(re,"match.length")
        d=stri_trans_nfkc(substring(pdf[[1]],re,re+at-2))
        
        TD2<-
          pdf[[1]] %>%
          strsplit("\n") %>%
          data.frame() %>%
          mutate(Date=d) %>%
          mutate(Date=gsub("令和4年","2022年",Date)) %>%
          mutate(Date=gsub("令和3年","2021年",Date)) %>%
          mutate(Date=gsub("令和2年","2020年",Date)) %>%
          mutate(Date=gsub("日.+","日",Date))
        colnames(TD2)[1] <- "Text"

        for (l in 2:length(pdf)) {
          td <-
            pdf[[l]] %>%
            strsplit("\n") %>%
            data.frame() %>%
            mutate(Date=d) %>%
            mutate(Date=gsub("令和4年","2022年",Date)) %>%
            mutate(Date=gsub("令和3年","2021年",Date)) %>%
            mutate(Date=gsub("令和2年","2020年",Date)) %>%
            mutate(Date=gsub("日.+","日",Date))
          
          colnames(td)[1] <- "Text"
          
          TD <-
            TD %>%
            rbind(td)
          
        }
        
        TD <-
          TD %>%
          mutate(Text2=stri_trans_nfkc(Text))%>%
          mutate(Text2=gsub("\u2ec4","西",Text2))%>%
          mutate(Text2=gsub("\u6236塚","戸塚",Text2))%>%
          mutate(Text2=gsub("\u2ed8葉","青葉",Text2))
        TD2 <-
          TD2 %>%
          mutate(Text2=stri_trans_nfkc(Text))
        sr2=which(grepl("市外.県内",TD2$Text2))
        TDS2<-
          data.frame()
        for (n in 1:length(sr2)) {
          tds2 <- paste0(TD2$Text2[sr2[n]+1])%>%
            data.frame()%>%
            mutate(Date="")
          colnames(tds2)[1]<-"chr"
          TDS2<-
            rbind(TDS2,tds2)
          TDS2$Date[n]=TD2$Date[sr2[n]]
        }
        TDS3<-
          TDS2%>%
          mutate(chr=str_replace_all(chr," +","_"),
                 chr=str_remove(chr,"^_"))%>%
          tidyr::separate(chr,into =c("横浜市","市外(県内)","市外(県外)","市外(都内)","計"),sep ="_")%>%
          tidyr::pivot_longer(cols=-Date,names_to="City",
                              values_to = "count")%>%
          filter(City!="計")
        write.csv(TDS3,"yoko_covid_today.csv",row.names = F)
        yoko_covid<-
          rbind(read.csv("yoko_covid2108.csv"),
                read.csv("yoko_covid2109.csv"),
                read.csv("yoko_covid2110.csv"),
                read.csv("yoko_covid2111.csv"),
                read.csv("yoko_covid2112.csv"),
                read.csv("yoko_covid2201.csv"),
                read.csv("yoko_covid2202.csv"),
                read.csv("yoko_covid_today.csv"))%>%
          mutate(Date=as.Date(Date,format="%Y年%m月%d日"))%>%
          filter(Date<"2021-12-08")
        write.csv(yoko_covid,"yoko_covid.csv",row.names = F,fileEncoding="UTF-8")
        sr=which(grepl("(男|女)|調査中",TD$Text2))
        n=1
        TDS <- data.frame(Date="",No=1:length(sr),Age="",Gender="",Hos="",City="")
        for (n in 1:length(sr)) {
          tds <- paste0(TD$Text2[sr[n]])
          
          TDS$Date[n]=TD$Date[sr[n]]
          
          re<-regexpr("^[[:digit:]]+ ",tds)
          at<-attr(re,"match.length")
          TDS$No[n]=as.numeric(substring(tds,re,re+at-1))
          
          re<-regexpr("([[:digit:]]{2,3}[代歳])|調査中",tds)
          at<-attr(re,"match.length")
          TDS$Age[n]=substring(tds,re,re+at-1)
          
          re<-regexpr("(男|女)|調査中",tds)
          at<-attr(re,"match.length")
          TDS$Gender[n]=substring(tds,re,re+at-1)
          
          re<-regexpr("[^ ]+[市区町村]|[^ ]+[内外])|(鶴見|神奈川|西| 中|南|港南|保土ケ谷|旭|磯子|金沢|港北|緑|青葉|都筑|戸塚|栄|泉|瀬谷)",tds)
          at<-attr(re,"match.length")
          TDS$City[n]=substring(tds,re,re+at-1)
         
        }
        TDS <-
          TDS %>%
          mutate(Date=as.Date(Date,format="%Y年%m月%d日")) %>%
          mutate(Hos="横浜") %>%
          mutate(City=str_remove(City," ")) %>%
          arrange(desc(Date),desc(No))
        
        yokohamatoday<-TDS
        if(Date=="0819"){
          yokohamatoday<-read.csv("yokohama0819covid-19.csv")%>%
            rename("Age"="年代","Gender"="性別","City"="居住地")%>%
            select(No,Age,Gender,City)%>%
            mutate(Date=as.Date("2021-08-19"))%>%
            mutate(Hos="横浜")%>%
            filter(No!="No")%>%
            filter(No!="")
        }
      
        write.csv( yokohamatoday,"yokohamatoday.csv",row.names = F)
      }else{
        
      }

      if(yoko_html1[1,2]==TRUE){
        yokohama<-
          rbind(
            read.csv("yokohama202202.csv"),
            read.csv("yokohama202201.csv"),
            read.csv("yokohama202112.csv"),
            read.csv("yokohama202111.csv"),
            read.csv("yokohama202110.csv"),
            read.csv("yokohama202109.csv"),
            read.csv("yokohama202108.csv"),
            read.csv("yokohama202107.csv"),
            read.csv("yokohama202106.csv"),
            read.csv("yokohamatoday.csv")
          )%>%
          rename("Sex"="Gender","PR_Date"="Date","Residential_City"="City")%>%
          mutate(Fixed_Date=PR_Date,
                 Fixed_Date2=NA)%>%
          arrange(desc(Fixed_Date))%>%
          mutate(Age=str_remove(Age,"代"))%>%
          mutate(Age=str_replace(Age,"10歳","10歳未満"))
      }else{
        yokohama<-
          rbind(
            read.csv("yokohama202202.csv"),
            read.csv("yokohama202201.csv"),
            read.csv("yokohama202112.csv"),
            read.csv("yokohama202111.csv"),
            read.csv("yokohama202110.csv"),
            read.csv("yokohama202109.csv"),
            read.csv("yokohama202108.csv"),
            read.csv("yokohama202107.csv"),
            read.csv("yokohama202106.csv")
          )%>%
          rename("Sex"="Gender","PR_Date"="Date","Residential_City"="City")%>%
          mutate(Fixed_Date=PR_Date,
                 Fixed_Date2=NA)%>%
          arrange(desc(Fixed_Date))%>%
          mutate(Age=str_remove(Age,"代"))%>%
          mutate(Age=str_replace(Age,"10歳","10歳未満"))
      }

      yokohama<-
        yokohama%>%
        mutate(note=Residential_City)%>%
        mutate(Residential_City=ifelse(str_detect(Residential_City,"市外|非公表"),Residential_City,"横浜市"))
      # yokohama<-rbind(yokohama,data.frame(PR_Date=c("2022-01-19","2022-01-19"),
      #            No=c(73801,73806),
      #            Age=c("調査中","調査中"),
      #            Sex=c("調査中","調査中"),
      #            Hos=c("横浜","横浜"),
      #            Residential_City=c("横浜市","横浜市"),
      #            Fixed_Date=c("2022-01-19","2022-01-19"),
      #            Fixed_Date2=c(NA,NA),
      #            note=c("鶴見","鶴見")
      # ))
      write.csv(yokohama,"yokohama.csv",row.names = F)
      print("横浜市を出力しました")
      #相模原市####
      

      TD <- data.frame()
      Sagamihara <-
        #Ahref %>%
        pdf_pref%>%
        filter(grepl("sagamihara",pdf))
      if(nrow(Sagamihara)!=0){
        for (p in Sagamihara$pdf) {
        
        path2<-paste0("https://www.pref.kanagawa.jp",p)

        pdf<-pdf_text(path2)
        if(pdf[1]=="")print(paste("ファイル名:",path2,"を取得出来ません"))
        re<-regexpr("令和.{1,10}日\n",pdf[[1]])
        at<-attr(re,"match.length")
        d=gsub(" ","",stri_trans_nfkc(substring(pdf[[1]],re,re+at-2)))
        
        for (l in 1:length(pdf)) {
          td <-
            pdf[[l]] %>%
            strsplit("\n") %>%
            data.frame() %>%
            mutate(Date=d) %>%
            mutate(Date=gsub("令和4年","2022年",Date)) %>%
            mutate(Date=gsub("令和3年","2021年",Date)) %>%
            mutate(Date=gsub("令和2年","2020年",Date))
          
          colnames(td)[1] <- "Text"
          
          TD <-
            TD %>%
            rbind(td)
          
   
        }
  
      }
      TD <-
        TD %>%
        mutate(Text2=stri_trans_nfkc(Text))
      sr=which(grepl("^ *[[:digit:]]+ ",TD$Text2))
      #sr=which(grepl("男性|女性|男児|女児",TD$Text2))
      hoken=c("平塚","鎌倉","小田原","厚木")
      n=1
      TDS <- data.frame(Date="",No=1:length(sr),Age="",Gender="",Hos="",City="")
      for (n in 1:length(sr)) {
        tds <- paste0(TD$Text2[sr[n]],collapse = " ") #:min(sr[n+1]-1,sr[length(sr)],na.rm = T)
        
        TDS$Date[n]=TD$Date[sr[n]]
        
        re<-regexpr("^ *[[:digit:]]+ ",tds)
        at<-attr(re,"match.length")
        TDS$No[n]=as.numeric(substring(tds,re,re+at-1))
        
        if(regexpr("代|歳",tds)==-1){ tds <- paste0(TD$Text2[sr[n]-1],collapse = " ")}
        
        re<-regexpr("[[:digit:]]{2,3}[代歳]|.学生|..学児",tds)
        at<-attr(re,"match.length")
        TDS$Age[n]=substring(tds,re,re+at-1)
        
        tds <- paste0(TD$Text2[sr[n]],collapse = " ")
        re<-regexpr("男性|女性|男児|女児",tds)
        at<-attr(re,"match.length")
        TDS$Gender[n]=substring(tds,re,re+at-1)
        
        re<-regexpr("[^ ]+[市区町村都県郡] |[^ ]+[内外]) |県外 ",tds)
        if(re==-1){
          re<-regexpr("調査中 ",tds)
        }
        at<-attr(re,"match.length")
        TDS$City[n]=substring(tds,re,re+at-2)
        

      }
      
      te <-
        TDS %>%
        count(No)

      TDS2 <-
        TDS %>%
        filter(!is.na(No)) %>%
        filter(City!="") %>%
        mutate(Hos="相模原") %>%
        mutate(Date=as.Date(Date,format="%Y年%m月%d日")) %>%
        arrange(desc(Date),desc(No))
      if(str_detect(Sys.Date(),"08")){
        TDS2<-rbind(TDS2,data.frame(Date=c("2021-08-12","2021-08-12","2021-08-13","2021-08-13"),
                                    No=c(7006,7019,7117,7248),
                                    Age=c("20代","50代","40代","50代"),
                                    Gender=c("女性","女性","女性","女性"),
                                    Hos=c("相模原","相模原","相模原","相模原"),
                                    City=c("相模原市","相模原市","相模原市","相模原市")))
      }
      #write.csv(TDS2,"sagamihara202108.csv",row.names = F)
      #write.csv(TDS2,"sagamihara202109.csv",row.names = F)
      #write.csv(TDS2,"sagamihara202110.csv",row.names = F)
      write.csv(TDS2,paste0("sagamihara",format(Sys.Date()-1,"%Y%m"),".csv"),row.names = F)
      }
      
      #sagamihara_today####
      Date<-format(Sys.Date(),"%m%d")
      while (TRUE) {
        saga_html<-try(read_html("https://www.city.sagamihara.kanagawa.jp/shisei/koho/1019191.html")%>%
                         html_nodes("a")%>%
                         html_attr("href")%>%
                         data.frame()%>%
                         filter(str_detect(.,"pdf"))%>%
                         mutate(flag=str_detect(.,Date))%>%
                         mutate(pdf=str_remove(.,"^../..")))
        if(class(saga_html) != "try-error")break
      }
      
      if(nrow(saga_html)!=0){
        TD <- data.frame()
      if(saga_html[1,1]=="../../_res/projects/default_project/_page_/001/019/191/08/00.pdf"){
        saga_html[1,2]=TRUE
      }
      if(saga_html[1,1]=="../../_res/projects/default_project/_page_/001/019/191/09/00.pdf"){
        saga_html[1,2]=TRUE
      }
      if(saga_html[1,2]==TRUE){
        path2<-paste0("https://www.city.sagamihara.kanagawa.jp",saga_html[1,3])
        if(path2=="https://www.city.sagamihara.kanagawa.jp/_res/projects/default_project/_page_/001/019/191/07/0731_00.pdf"){
          path2<-"https://www.city.sagamihara.kanagawa.jp/_res/projects/default_project/_page_/001/019/191/07/0731_01.pdf"
        }
        pdf<-pdf_text(path2)
        if(pdf[1]=="")print(paste("ファイル名:",path2,"を取得出来ません"))
        re<-regexpr("令和.{1,10}日\n",pdf[[1]])
        at<-attr(re,"match.length")
        d=gsub(" ","",stri_trans_nfkc(substring(pdf[[1]],re,re+at-2)))
        
         for (l in 1:length(pdf)) {
          td <-
            pdf[[l]] %>%
            strsplit("\n") %>%
            data.frame() %>%
            mutate(Date=d) %>%
            mutate(Date=gsub("令和4年","2022年",Date)) %>%
            mutate(Date=gsub("令和3年","2021年",Date)) %>%
            mutate(Date=gsub("令和2年","2020年",Date))
          
          colnames(td)[1] <- "Text"
          
          TD <-
            TD %>%
            rbind(td)
          
        }
        
        TD <-
          TD %>%
          mutate(Text2=stri_trans_nfkc(Text))
        
         #sr=which(grepl("男性|女性|男児|女児",TD$Text2))
         sr=which(grepl("^ *[[:digit:]]+ ",TD$Text2))
       # hoken=c("平塚","鎌倉","小田原","厚木")
        n=1
        TDS <- data.frame(Date="",No=1:length(sr),Age="",Gender="",Hos="",City="")
        for (n in 1:length(sr)) {
          tds <- paste0(TD$Text2[sr[n]],collapse = " ") #:min(sr[n+1]-1,sr[length(sr)],na.rm = T)
          
          TDS$Date[n]=TD$Date[sr[n]]
          
          re<-regexpr("^ *[[:digit:]]+ ",tds)
          at<-attr(re,"match.length")
          TDS$No[n]=as.numeric(substring(tds,re,re+at-1))
          if(regexpr("代|歳",tds)==-1){ tds <- paste0(TD$Text2[sr[n]-1],collapse = " ")}
          
          re<-regexpr("[[:digit:]]{2,3}[代歳]|.学生|..学児",tds)
          at<-attr(re,"match.length")
          TDS$Age[n]=substring(tds,re,re+at-1)
          
          tds <- paste0(TD$Text2[sr[n]],collapse = " ") 
          re<-regexpr("男性|女性|男児|女児",tds)
          at<-attr(re,"match.length")
          TDS$Gender[n]=substring(tds,re,re+at-1)
          
          re<-regexpr("[^ ]+[市区町村都県郡] |[^ ]+[内外]) |県外 ",tds)
          if(re==-1){
            re<-regexpr("調査中 ",tds)
          }
          print(TDS$No[n])
          print(tds)
          print(n)
          at<-attr(re,"match.length")
          TDS$City[n]=substring(tds,re,re+at-2)
          
  
        }
        
         TDS2 <-
          TDS %>%
          filter(!is.na(No)) %>%
          filter(City!="") %>%
          mutate(Hos="相模原") %>%
          mutate(Date=as.Date(Date,format="%Y年%m月%d日")) %>%
          arrange(desc(Date),desc(No))
         

        }
        saga_today<-TDS2
        write.csv(saga_today,"sagamiharatoday.csv",row.names = F)
         }else{
        #saga_today<-data.frame()
         }
        
      #}
      
        
     
      # sagamihara<-
      #   rbind(
      #     read.csv("sagamihara202107.csv"),
      #     read.csv("sagamiharatoday.csv")
      #   )
      # write.csv(sagamihara,"sagamihara202107.csv")
      if(nrow(Sagamihara)!=0){
        if(nrow(saga_html)!=0){
          if(saga_html[1,2]==TRUE){
            sagamihara<-
              rbind(
                read.csv("sagamihara20201219.csv"),
                read.csv("sagamihara20210107.csv"),
                read.csv("sagamihara20210108.csv"),
                read.csv("sagamihara202102-06.csv"),
                read.csv("sagamihara202107.csv")[,-1],
                read.csv("sagamihara202108.csv"),
                read.csv("sagamihara202109.csv"),
                read.csv("sagamihara202110.csv"),
                read.csv("sagamihara202111.csv"),
                read.csv("sagamihara202112.csv"),
                read.csv("sagamihara202201.csv"),
                read.csv("sagamihara202202.csv"),
                read.csv("sagamiharatoday.csv")
                )%>%
              arrange(Date)%>%
              distinct(No,.keep_all = T)%>%
              rename("Sex"="Gender","PR_Date"="Date","Residential_City"="City")%>%
              mutate(Fixed_Date=PR_Date,
                 Fixed_Date2=NA)%>%
              arrange(desc(Fixed_Date))
          }else{
            sagamihara<-
              rbind(
                read.csv("sagamihara20201219.csv"),
                read.csv("sagamihara20210107.csv"),
                read.csv("sagamihara20210108.csv"),
                read.csv("sagamihara202102-06.csv"),
                read.csv("sagamihara202107.csv")[,-1],
                read.csv("sagamihara202108.csv"),
                read.csv("sagamihara202109.csv"),
                read.csv("sagamihara202110.csv"),
                read.csv("sagamihara202111.csv"),
                read.csv("sagamihara202112.csv"),
                read.csv("sagamihara202201.csv"),
                read.csv("sagamihara202202.csv")
              )%>%
              arrange(Date)%>%
              distinct(No,.keep_all = T)%>%
              rename("Sex"="Gender","PR_Date"="Date","Residential_City"="City")%>%
              mutate(Fixed_Date=PR_Date,
                     Fixed_Date2=NA)%>%
              arrange(desc(Fixed_Date))
          }
          }else{
            sagamihara<-
              rbind(
                read.csv("sagamihara20201219.csv"),
                read.csv("sagamihara20210107.csv"),
                read.csv("sagamihara20210108.csv"),
                read.csv("sagamihara202102-06.csv"),
                read.csv("sagamihara202107.csv")[,-1],
                read.csv("sagamihara202108.csv"),
                read.csv("sagamihara202109.csv"),
                read.csv("sagamihara202110.csv"),
                read.csv("sagamihara202111.csv"),
                read.csv("sagamihara202112.csv"),
                read.csv("sagamihara202201.csv"),
                read.csv("sagamihara202202.csv")
                )%>%
              arrange(Date)%>%
              distinct(No,.keep_all = T)%>%
              rename("Sex"="Gender","PR_Date"="Date","Residential_City"="City")%>%
              mutate(Fixed_Date=PR_Date,
                 Fixed_Date2=NA)%>%
              arrange(desc(Fixed_Date))
          }
        }
      
      if(nrow(Sagamihara)==0){
        
        #神奈川県のページにさがみはらがない場合
        if(nrow(saga_html)!=0){
          if(saga_html[1,2]==TRUE){
            sagamihara<-
              rbind(
                read.csv("sagamihara20201219.csv"),
                read.csv("sagamihara20210107.csv"),
                read.csv("sagamihara20210108.csv"),
                read.csv("sagamihara202102-06.csv"),
                read.csv("sagamihara202107.csv")[,-1],
                read.csv("sagamihara202108.csv"),
                read.csv("sagamihara202109.csv"),
                read.csv("sagamihara202110.csv"),
                read.csv("sagamihara202111.csv"),
                read.csv("sagamihara202112.csv"),
                read.csv("sagamihara202201.csv"),
                read.csv("sagamiharatoday.csv")
              )%>%
              arrange(Date)%>%
              distinct(No,.keep_all = T)%>%
              rename("Sex"="Gender","PR_Date"="Date","Residential_City"="City")%>%
              mutate(Fixed_Date=PR_Date,
                   Fixed_Date2=NA)%>%
              arrange(desc(Fixed_Date))
          }
          }else{
            sagamihara<-
              rbind(
                read.csv("sagamihara20201219.csv"),
                read.csv("sagamihara20210107.csv"),
                read.csv("sagamihara20210108.csv"),
                read.csv("sagamihara202102-06.csv"),
                read.csv("sagamihara202107.csv")[,-1],
                read.csv("sagamihara202108.csv"),
                read.csv("sagamihara202109.csv"),
                read.csv("sagamihara202110.csv"),
                read.csv("sagamihara202111.csv"),
                read.csv("sagamihara202112.csv"),
                read.csv("sagamihara202201.csv"),
                )%>%
              arrange(Date)%>%
              distinct(No,.keep_all = T)%>%
              rename("Sex"="Gender","PR_Date"="Date","Residential_City"="City")%>%
              mutate(Fixed_Date=PR_Date,
                   Fixed_Date2=NA)%>%
              arrange(desc(Fixed_Date))
          }
        }
      
      write.csv(sagamihara,"sagamihara.csv",row.names = F)
      print("相模原市を出力しました")
      #藤沢市####
      while(TRUE){
         HTML <- try(read_html("https://www.city.fujisawa.kanagawa.jp/hoken-j/corona_doukou_data.html"))
         if(class(HTML) != "try-error")break
      }
     
      #HTML
      
      Ahref <-
        HTML %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        data.frame()
      
      colnames(Ahref) <- "pdf"
      
      Fujisawa <-
        Ahref %>%
        filter(grepl("fujisawa[[:digit:]]+.+pdf$",pdf))
      
      p=Fujisawa$pdf[1]
   
      TD <- data.frame()

      pdf<-pdf_text(paste0("https://www.city.fujisawa.kanagawa.jp",p))

      for (l in 1:length(pdf)) {
        td <-
          pdf[[l]] %>%
          strsplit("\n") %>%
          data.frame()

        colnames(td)[1] <- "Text"
        
        TD <-
          TD %>%
          rbind(td)

      }

      
      TD <-
        TD %>%
        mutate(Text2=stri_trans_nfkc(Text))
      #空白までの文字を拾う
      sr=which(grepl("^ {0,1}[[:digit:]]+ ",TD$Text2))
      n=1
      TDS <- data.frame(Date="",No=1:length(sr),Age="",Gender="",Hos="",City="",Jobs="",no="")
      for (n in 1:length(sr)) {
        # for (n in 413:430) {
        (tds <- paste0(TD$Text2[sr[n]],collapse = " "))
        
        re<-regexpr("  +",tds)
        at<-attr(re,"match.length")
        TDS$No[n]=as.numeric(substring(tds,1,re-1))
        tds <- substring(tds,re+at,nchar(tds))
        
        #日付がある行
        if(at<7){
          re<-regexpr(" +",tds)
          at<-attr(re,"match.length")
          TDS$Date[n]=(substring(tds,1,re-1))
          tds <- substring(tds,re+at,nchar(tds))
        }
        
        re<-regexpr(" +",tds)
        at<-attr(re,"match.length")
        TDS$no[n]=as.numeric(substring(tds,1,re-1))
        tds <- substring(tds,re+at,nchar(tds))
        
        re<-regexpr(" +",tds)
        at<-attr(re,"match.length")
        TDS$Age[n]=(substring(tds,1,re-1))
        tds <- substring(tds,re+at,nchar(tds))
        
        re<-regexpr(" +",tds)
        at<-attr(re,"match.length")
        TDS$Gender[n]=(substring(tds,1,re-1))
        tds <- substring(tds,re+at,nchar(tds))
        
        re<-regexpr(" +",tds)
        at<-attr(re,"match.length")
        TDS$City[n]=(substring(tds,1,re-1))
        tds <- substring(tds,re+at,nchar(tds))
        
        re<-regexpr(" +",tds)
        at<-attr(re,"match.length")
        TDS$Jobs[n]=(substring(tds,1,re-1))
        if(re<0)
          TDS$Jobs[n]=tds
        tds <- substring(tds,re+at,nchar(tds))
        
        print(c(n,TDS$No[n],TDS$Date[n]))
      }
      TDS <-
        TDS %>%
        mutate(no=as.numeric(no))
      
      #日番号(no)がNAのところに次の値-1を入れる　NAがなくなるまで
      kb=which(is.na(TDS$no))
      while(length(kb)>0){
        for (n in kb) {
          TDS$no[n]=TDS$no[n+1]-1
        }
        kb=which(is.na(TDS$no))
      }
      day=which(grepl("202.年.+月.+日",TD$Text2))
      re<-regexpr("202.年.+月.+日",TD$Text2[day])
      at<-attr(re,"match.length")
      dayl= unique(substring(TD$Text2[day],re,re+at-1))
      n=1
      d=1
      TDS$Date[n]=dayl[d]
      #基本的に日番号=1のときに日付ずらす
      #既に入っている日付とずれたときはずらす
      for (n in 2:length(sr)) {
        dn=TDS$no[n]
        dt=TDS$Date[n]
        
        if(dn==1|(dt!=""&dt!=dayl[d])){ 
          d=d+1
        }
        TDS$Date[n]=dayl[d]
        print(c(n,TDS$No[n],TDS$Date[n]))
      }
      
      # sr=which(grepl("男性|女性|男児|女児",TD$Text2))
      # #sr=which(grepl("^[[:digit:]]+ ",TD$Text2))
      # day=which(grepl("202.年.+月.+日",TD$Text2))
      # 
      # day<-day[-43]
      # 
      # n=1
      # TDS <- data.frame(Date="",No=1:length(sr),Age="",Gender="",Hos="",City="")
      # d=1
      # 
      # for (n in 1:length(sr)) {
      #   tds <- paste0(TD$Text2[sr[n]],collapse = " ")
      #   
      #   re<-regexpr("^[[:digit:]]+ |^ [[:digit:]] ",tds)
      #   at<-attr(re,"match.length")
      #   TDS$No[n]=as.numeric(substring(tds,re,re+at-1))
      #   
      #   re<-regexpr("[[:digit:]]{2,3}[代歳]|.学生|..学児",tds)
      #   at<-attr(re,"match.length")
      #   TDS$Age[n]=substring(tds,re,re+at-1)
      #   
      #   TDS$no[n] <- as.numeric(substring(tds,re-7,re-1))
      #   #4481欠番no2332
      #   #6571欠番
      #   #7056欠番
      #   #7071欠番
      #   #7066欠番
      #   #7067欠番
      #   #8060欠番
      #   #8125欠番
      #   # re<-regexpr("欠番",tds)
      #   # if(re!=-1)TDS$no[n] <- "欠番"
      #   # if(n>1)
      #   #   if(TDS$no[n]==1|sr[n]-sr[n-1]>5|TDS$No[n]==7162) #1番または改ページ
      #   if(n>1)
      #     if(TDS$no[n]==1|sr[n]-sr[n-1]>5|TDS$No[n]==4482|TDS$No[n]==6572|TDS$No[n]==7057|
      #        TDS$No[n]==7072|TDS$No[n]==7067|TDS$No[n]==7068|TDS$No[n]==7162#|
      #        #TDS$No[n]==8061|TDS$No[n]==8125
      #        ) #1番または改ページ
      #       d=d+1
      #   re<-regexpr("202.年.+月.+日",TD$Text2[day[d]])
      #   at<-attr(re,"match.length")
      #   TDS$Date[n]=substring(TD$Text2[day[d]],re,re+at-1)
      #   # if(!is.na(TDS$Date[n])){
      #   #   a<-substring(TD$Text2[day[d]],re,re+at-1)
      #   # }else{
      #   #   TDS$Date[n]=a
      #   # }
      # 
      #   re<-regexpr("男性|女性|男児|女児",tds)
      #   at<-attr(re,"match.length")
      #   TDS$Gender[n]=substring(tds,re,re+at-1)
      #   
      #   re<-regexpr("[^ ]+[市区町村都]|[^ ]+[内外]",tds)
      #   at<-attr(re,"match.length")
      #   TDS$City[n]=substring(tds,re,re+at-1)
      #   
      # }
      
      
      TDS2 <-
        TDS %>%
        mutate(Hos="藤沢") %>%
        mutate(Date=as.Date(Date,format="%Y年%m月%d日")) %>%
        arrange(desc(No),desc(Date))%>%
        select(Date,No,Age,Gender,Hos,City,no)
      
      
      #write.csv(TDS2,"fujisawa202104.csv",row.names = F)
      write.csv(TDS2,"fujisawa202110.csv",row.names = F)
      fujisawa<-rbind(read.csv("fujisawa202103.csv"),
                      read.csv("fujisawa202104.csv"),
                      read.csv("fujisawa202110.csv"))%>%
        rename("PR_Date"="Date","Sex"="Gender","Residential_City"="City")%>%
        mutate(Fixed_Date=PR_Date,
               Fixed_Date2=NA)%>%
        mutate(Age=str_remove(Age,"代"))%>%
        arrange(desc(Fixed_Date))%>%
        mutate(Age=str_replace(Age,"10歳","10歳未満"))
      write.csv(fujisawa,"fujisawa.csv",row.names = F)
      print("藤沢市を出力しました")
      print(paste("取得年月:",flag[hn,4],flag[hn,2],",today:",Sys.Date()))
      #データ作成####
      data<-
        fread("kanagawa.csv", encoding="UTF-8") %>%
        mutate(確定日= as.Date(確定日,format = "%m/%d/%Y"))%>%
        select(確定日,受診都道府県,居住都道府県,居住市区町村,備考,X,Y,年代,性別,発表)%>%
        filter(居住都道府県=="神奈川県")%>%
        filter(居住市区町村=="川崎市"|居住市区町村=="")%>%
        mutate(Residential_City=paste0(居住市区町村,備考))%>%
        select(-居住市区町村,-備考,-X,-Y)%>%
        rename("Fixed_Date2"="確定日",
               "Hospital_Pref"="受診都道府県",
               "Residential_Pref"="居住都道府県",
               "Sex"="性別",
               "Age"="年代")%>%
        filter(Residential_City!="")%>%
        mutate(PR_Date=NA,
               Fixed_Date=Fixed_Date2)%>%
        mutate(Hos=発表)%>%
        select(-発表)
      data2<-fread("kanagawa.csv", encoding="UTF-8") %>%
        mutate(確定日= as.Date(確定日,format = "%m/%d/%Y"))%>%
        select(確定日,受診都道府県,居住都道府県,居住市区町村,備考,X,Y,年代,性別,発表)%>%
        filter(居住都道府県=="神奈川県")%>%
        filter(居住市区町村!="川崎市",居住市区町村!="",居住市区町村!="藤沢市")%>%
        rename("Residential_City"="居住市区町村",
               "Sex"="性別",
               "Age"="年代")%>%
        select(-備考)%>%
        rename("Fixed_Date2"="確定日",
               "Hospital_Pref"="受診都道府県",
               "Residential_Pref"="居住都道府県")%>%
        mutate(PR_Date=NA,
               Fixed_Date=Fixed_Date2)%>%
        mutate(Hos=発表)%>%
        select(-発表)
      while (TRUE) {
        patient<-try(read.csv("https://www.pref.kanagawa.jp/osirase/1369/data/csv/patient.csv") %>%
                       filter(!str_detect(居住地,"管内")) %>%
                       rename("PR_Date"="発表日","Residential_City"="居住地",
                              "Sex"="性別",
                              "Age"="年代") %>%
                       #select(-年代,-性別)%>%
                       mutate(Residential_City = str_replace(Residential_City,"神奈川県",""))%>%
                       mutate(PR_Date=as.Date(PR_Date))%>%
                       mutate(Fixed_Date2=NA,
                              Fixed_Date=PR_Date)%>%
                       mutate(Hos=NA))
        if(class(patient) != "try-error")break
        
      }
      
        
      # %>%
      #   filter(Fixed_Date>="2020-12-01") %>%
      #   filter(Residential_City%in%c("横浜市","相模原市","藤沢市","横須賀市"))
      #横浜市
      # yokohama<-read.csv("yokohama.csv")%>%
      #   select(-No)%>%
      #   mutate(Fixed_Date=as.Date(Fixed_Date),
      #          Fixed_Date2=as.Date(Fixed_Date2),
      #          PR_Date=as.Date(PR_Date))%>%
      #   rbind(patient%>%
      #           filter(Fixed_Date>="2020-12-01",Fixed_Date<"2021-01-01") %>%
      #           filter(Residential_City=="横浜市"))%>%
      #   mutate(note=NA)%>%
      #   mutate(hos="yokohama")
      yokohama<-read.csv("yokohama.csv")%>%
        select(-No)%>%
        mutate(Fixed_Date=as.Date(Fixed_Date),
               Fixed_Date2=as.Date(Fixed_Date2),
               PR_Date=as.Date(PR_Date))%>%
        select(PR_Date,Residential_City,Age,Sex,Fixed_Date2,Fixed_Date,Hos,note)%>%
        rbind(patient%>%
                filter(Fixed_Date>="2020-12-01",Fixed_Date<"2021-01-01") %>%
                filter(Residential_City=="横浜市")%>%
                mutate(note=NA))%>%
        mutate(hos="yokohama")
      #横須賀市
      yokosuka<-read.csv("yokosuka.csv")%>%
        select(Fixed_Date,PR_Date,Sex,Age,City,Hos,Fixed_Date2)%>%
        mutate(Fixed_Date=as.Date(Fixed_Date),
               Fixed_Date2=as.Date(Fixed_Date2),
               PR_Date=as.Date(PR_Date))%>%
        rename("Residential_City"="City")%>%
        rbind(patient%>%
                filter(Fixed_Date>="2020-12-01",Fixed_Date<"2021-05-01") %>%
                filter(Residential_City=="横須賀市"))%>%
        mutate(Residential_City=str_remove(Residential_City,".+郡"))%>%
        mutate(note=NA)%>%
        mutate(hos="yokosuka")
      #相模原市
      sagamihara<-read.csv("sagamihara.csv")%>%
        select(-No)%>%
        rbind(patient%>%
                filter(Fixed_Date>="2020-12-01",Fixed_Date<"2020-12-19") %>%
                filter(Residential_City=="相模原市"))%>%
        mutate(note=NA)%>%
        mutate(hos="sagamihara")
      #藤沢市
      fujisawa<-read.csv("fujisawa.csv")%>%
        select(-no,-No)%>%
        mutate(note=NA)%>%
        mutate(hos="fujisawa")
      
      #今日の神奈川県####
      while (TRUE) {
        HTML<-try(read_html("https://www.pref.kanagawa.jp/prs/list-2021-1-1.html"))
        if(class(HTML) != "try-error")break
      }
      
      URL<-HTML%>%
        html_nodes("a")%>%
        html_attr("href")
      TEXT<-HTML%>%
        html_nodes("a")%>%
        html_text()
      Date<-Sys.Date()
      Date<-str_remove(Date,"2021-|2022-")%>%
        str_replace("-","月")%>%
        str_replace_all("01","1")%>%
        str_replace_all("02","2")%>%
        str_replace_all("月0","月")
      Date<-paste0("\\(",Date)
      HTML2<-cbind(TEXT,URL)%>%
        data.frame()%>%
        mutate(TEXT=stri_trans_nfkc(TEXT))%>%
        filter(str_detect(TEXT,"新型コロナウイルス感染症による患者確認について"))%>%
        filter(str_detect(TEXT,Date))
      if(!is.na(HTML2[1,2])){
        path<-paste0("https://www.pref.kanagawa.jp",HTML2[1,2])
        while (TRUE) {
          CSV<-try(read_html(path)%>%
                     html_nodes("a")%>%
                     html_attr("href")%>%
                     data.frame()%>%
                     filter(str_detect(.,".csv"))%>%
                     rename("csv"="."))
          if(class(CSV) != "try-error")break
        }
        
        
        kanagawa_today<-data.frame()
        if(!is.na(CSV[1,1])){
          while(TRUE){
            kanagawa_today<-try(read.csv(paste0("https://www.pref.kanagawa.jp",CSV[1,1]))%>%
                                  rename("Residential_City"="居住地",
                                         "Fixed_Date2"="陽性判明日",
                                         "Sex"="性別",
                                         "Age"="年代",
                                         "note"="発生届を受理した保健福祉事務所")%>%
                                  filter(!is.na(患者概要))%>%
                                  filter(Residential_City!="")%>%
                                  select(Residential_City,Fixed_Date2,Sex,Age,note)%>%
                                  mutate(Fixed_Date=Sys.Date(),
                                         PR_Date=Sys.Date(),
                                         Hos="神奈川県",
                                         hos="kanagawa")%>%
                                  mutate(Fixed_Date2=str_remove(Fixed_Date2,"日"),
                                         Fixed_Date2=paste0("2022-",str_replace(Fixed_Date2,"月","-"))))
            if(class(kanagawa_today) != "try-error")break
          }
           
          
        }
       
      }else{
        kanagawa_today<-data.frame()
      }

      if(Sys.Date()=="2021-11-10"){
        kanagawa_today<-kanagawa_today%>%
          mutate(Fixed_Date2=str_replace(Fixed_Date2,"202.年",""))
      }
      kanagawa<-read.csv("kanagawa2.csv") %>%
        select(-X)%>%
        mutate(PR_Date=as.Date(PR_Date))%>%
        mutate(Fixed_Date=as.Date(Fixed_Date),
               Fixed_Date2=as.Date(Fixed_Date2))%>%
        mutate(Hos="神奈川県")%>%
        mutate(hos="kanagawa")%>%
        rbind(kanagawa_today)
      
      
      kanagawa2<-rbind(kanagawa,yokohama,sagamihara,fujisawa,yokosuka) %>%
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
        mutate(PR_Date=as.Date(PR_Date))%>%
        mutate(Fixed_Date=as.Date(Fixed_Date),
               Fixed_Date2=as.Date(Fixed_Date2))%>%
        left_join(xy,by="Residential_City")%>%
        mutate(Hos="茅ヶ崎市",
               hos="chigasaki")%>%
        mutate(note=NA)%>%
        arrange(desc(Fixed_Date))
      
      list1<-read.csv("list.csv")
      data3<-
        data%>%
        left_join(list1,by=c("Residential_City"="list"))%>%
        select(-Residential_City)%>%
        rename("Residential_City"="管内")
      
      kanagawa2<-
        left_join(kanagawa2,xy,by="Residential_City") %>%
        mutate(Fixed_Date=as.Date(Fixed_Date))
      
      
      kawasaki<-
        read.csv("kawasaki.csv") %>%
        select(-X)%>%
        mutate(Fixed_Date=as.Date(Fixed_Date),
               PR_Date=as.Date(PR_Date),
               Fixed_Date2=as.Date(Fixed_Date2))%>%
        left_join(list1)%>%
        select(-管内,-Residential_City)%>%
        rename("Residential_City"="list")%>%
        mutate(Hos="川崎市",
               hos="kawasaki")%>%
        arrange(desc(Fixed_Date))
      
      data<-bind_rows(data2,data3,kanagawa2,kawasaki,chigasaki)%>%
        select(Fixed_Date,Hospital_Pref,Residential_City,Age,
               Sex,X,Y,PR_Date,Fixed_Date2,Hos,note,hos)%>%
        filter(Fixed_Date<Sys.Date())%>%
        mutate(Age=str_remove(Age,"代"),
               Age=ifelse(str_detect(Age,"100"),"100-",Age),
               Age=ifelse(str_detect(Age,"歳"),"0-10",Age),
               Age=str_replace(Age,"－","-"),
               Age=str_remove_all(Age," "))%>%
        arrange(desc(Fixed_Date),Hospital_Pref,Hos,Residential_City)
      # data2021<-
      #   data%>%
      #   filter(Fixed_Date>as.Date("2021-09-30"))
      data2022<-
        data%>%
        filter(Fixed_Date>=as.Date("2022-02-01"))
      if(format(Sys.time(),"%H")%in%c("17","18","19","20","21")){
        data<-bind_rows(data2,data3,kanagawa2,kawasaki,chigasaki)%>%
          select(Fixed_Date,Hospital_Pref,Residential_City,Age,
                 Sex,X,Y,PR_Date,Fixed_Date2,Hos,note,hos)%>%
          mutate(Age=str_remove(Age,"代"),
                 Age=ifelse(str_detect(Age,"歳"),"0-10",Age),
                 Age=str_replace(Age,"－","-"),
                 Age=str_remove_all(Age," "))%>%
          arrange(desc(Fixed_Date),Hospital_Pref,Hos,Residential_City)
        # data2021<-
        #   data%>%
        #   filter(Fixed_Date>as.Date("2021-09-30"))
        data2022<-
          data%>%
          filter(Fixed_Date>=as.Date("2022-02-01"))
        # data202201<-
        #   data%>%
        #   filter(Fixed_Date>as.Date("2021-09-30"))%>%
        #   filter(Fixed_Date<=as.Date("2022-01-31"))
      }
      write.csv(data2022,"data2022.csv",row.names=F,fileEncoding="UTF-8")
      #write.csv(data202201,"data202201.csv",row.names=F,fileEncoding="UTF-8")
      #write.csv(data2021,"data2021.csv",row.names=F,fileEncoding="UTF-8")
      #write.csv(data%>%filter(Fixed_Date>as.Date("2021-06-30"),Fixed_Date<=as.Date("2021-09-30")),"data202109.csv",row.names=F,fileEncoding="UTF-8")
      print("coviddata.csvを出力しました")
      # data202106<-
      #   data%>%
      #   filter(Fixed_Date<=as.Date("2021-06-30"))%>%
      #   filter(Fixed_Date>as.Date("2020-12-31"))
      # write.csv(data202106,"data202106.csv",row.names=F,fileEncoding="UTF-8")
      # data2020<-
      #   data%>%
      #   filter(Fixed_Date<=as.Date("2020-12-31"))
      # write.csv(data2020,"data2020.csv",row.names=F,fileEncoding="UTF-8")

    }
    
    
    

    #実行時間
    print(paste0(Sys.time()-time))
    print(Sys.time())
    #実行開始時間を保存
    time2<-time
    #urlを保存
    html_top2<-html_top[1,]
    #30分経過後最初に戻る
    Sys.sleep(300)

  }
  
}

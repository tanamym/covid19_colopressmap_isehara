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
try(
repeat{
  while(format(Sys.time(),"%H")%in%c("17","18","19","20") & minute(Sys.time())%%10 %in% c(0,6)){
    
    time<-
      Sys.time()
    Date0 <- Sys.Date()
    
   
 
      #kawasaki####
      url_top2<-
        "https://www.city.kawasaki.jp/350/page/0000115886.html"
      while (TRUE) {
        rhtml_top2<-try(rvest::read_html(url_top2,encoding="UTF-8"))
        Sys.sleep(10)
        if(sum(class(rhtml_top2) != "try-error"))break
      }
      
      html_top3<-
        rhtml_top2%>%
        html_nodes("a")%>%
        html_attr("href")%>%#urlの抽出
        as.data.frame()%>%
        filter(str_detect(.,"pdf"))%>%
        filter(!str_detect(.,"kyouiku")&!str_detect(.,"zyouge")&!str_detect(.,"kodomo")&
                 !str_detect(.,"miyamae")&!str_detect(.,"byouin")&!str_detect(.,"kotsu"))%>%
        rename("html"=".")%>%
        mutate(html=paste0("https://www.city.kawasaki.jp/350/",html))
      D2<-format(Date0,"%y%m%d")

      html_top3<- 
        html_top3%>%
        filter(str_detect(html,D2))

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
        mutate(確定日=NA) %>%
        # mutate(確定日=ifelse(str_detect(chr,"月"),str_sub(chr,start = -15,end = -4),str_sub(chr,start = -14,end = -4)))%>%
        # mutate(確定日=str_replace(確定日,"日.+","日")) %>%
        mutate(s = str_replace_all(s," +","_")) %>%
        tidyr::separate(s,into = c("番号","番号2","年代","性別","居住地"),sep ="_")%>%
        mutate(年代=ifelse(str_detect(年代,"10歳未満"),"0-10",str_replace(年代,"代","")))%>%
        mutate(居住市区町村=ifelse(str_detect(居住地,"市外"),"市外","川崎市"))%>%
        mutate(居住都道府県=ifelse(str_detect(居住地,"都内"),"東京都",
                             ifelse(str_detect(居住地,"県内|川崎市"),"神奈川県","県外")))%>%
        mutate(受診都道府県="神奈川県")%>%
        mutate(備考=ifelse(str_detect(居住地,"区"),str_replace(居住地,"川崎市",""),居住地))%>%
        # mutate(確定日=ifelse(str_detect(確定日,"月"),
        #                   str_replace_all(確定日,"[^(0-9月日)]",""),
        #                   確定日
        # ))%>%
        # mutate(確定日=ifelse(str_detect(確定日,"/"),
        #                   str_replace_all(確定日,"[^(0-9/)]",""),
        #                   確定日))%>%
        # mutate(確定日=ifelse(str_detect(確定日,"12"),paste0("2021-",str_replace(確定日,"月","-")),
        #                   paste0("2022-",str_replace(確定日,"月","-"))))%>%
        # mutate(確定日=ifelse(str_detect(確定日,"調査中"),NA,確定日),
        #           確定日=str_replace(確定日,"日",""))%>%
        # mutate(確定日=ifelse(str_detect(確定日,"/"),
        #                   str_replace(確定日,"/","-"),
        #                   確定日
        # )) %>%
      ungroup() %>%
        select(-chr)
      kawa4<-
        kawa4%>%
        mutate(番号=str_remove(番号,"例目"))%>%
        mutate(番号=as.numeric(番号))%>%
        filter(!is.na(番号))

      YM<-format(Date0,"%y%m")
      if(!dir.exists(YM)){
        dir.create(YM)
      }
      write.csv(kawa4,paste0(YM,"/kawasaki",D2,".csv"),row.names = F)
      LF<-list.files(path = YM,
                 pattern="kawasaki",full.names = T)
      kawasaki<-do.call(rbind,lapply(LF,read.csv))
      kawasaki2<-
        kawasaki%>%

        rename("list"="居住地","Fixed_Date2"="確定日",
               "Residential_City"="居住市区町村",
               "Residential_Pref"="居住都道府県",
               "Hospital_Pref"="受診都道府県","note"="備考",
               "Age"="年代","Sex"="性別","PR_Date"="発表日")%>%
        select(-番号,-番号2)%>%
        mutate(Fixed_Date=PR_Date)

      write.csv(kawasaki2,paste0("YM/kawasaki",YM,".csv"))
      LF<-list.files(path = "YM",
                     pattern="kawasaki",full.names = T)
      kawasaki<-do.call(rbind,lapply(LF,read.csv))
      write.csv(kawasaki,"kawasaki.csv")
      print("川崎市のデータを出力しました")
      
      #茅ヶ崎市####

 
      while (TRUE) {
        rhtml4<-try(rvest::read_html("https://www.city.chigasaki.kanagawa.jp/kenko/1022933/1038284.html",encoding="UTF-8")%>%
                      html_nodes("a")%>%
                      html_attr("href")%>%
                      as.data.frame()%>%
                      filter(str_detect(.,"default_project"))%>%
                      rename("html"=".")%>%
                      mutate(html=str_remove(html,"../../"))%>%
                      mutate(html=paste0("https://www.city.chigasaki.kanagawa.jp/",html)))
        Sys.sleep(10)
        if(class(rhtml4) != "try-error")break
        break
      }
      
      chigasaki2<-data.frame()
      D<-Date0%>%
        str_remove_all("-")%>%
        str_remove_all("2021|2022")
      if(!str_detect(rhtml4[1,1],D)){
        url_top3<-
          "https://www.city.chigasaki.kanagawa.jp/koho/1030702/1038773/index.html"
        while(TRUE){
          rhtml_top3<-try(rvest::read_html(url_top3,encoding="UTF-8"))
          if(class(rhtml_top3) != "try-error")break
          break
        }
        html_top4<-
          rhtml_top3%>%
          html_nodes("a")%>%
          html_attr("href")%>%#urlの抽出
          as.data.frame()%>%
          rename("html"=".")%>%
          cbind(rhtml_top3%>%
          html_nodes("a")%>%
          html_text()%>%
          as.data.frame())%>%
          rename("name"=".")%>%
          filter(str_detect(html,"html"))%>%
          mutate(html=str_remove(html,"../../../"))%>%
          mutate(html=paste0("https://www.city.chigasaki.kanagawa.jp/",html))%>%
          filter(str_detect(name,"新型コロナウイルス感染症による新たな管内の患者確認"))
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
          break
        }
      }
      chi4<-data.frame()
      if(str_detect(rhtml4[1,1],D)){
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
        D2<-format(Date0,"%y%m%d")
        YM<-format(Date0,"%y%m")
        if(!dir.exists(YM)){
          dir.create(YM)
          }
      
      write.csv(chigasaki2,paste0(YM,"/chigasaki",D2,".csv"),row.names = F)
      }
 
   
      LF<-list.files(path = YM,
                     pattern="chigasaki",full.names = T)
      chigasaki<-do.call(rbind,lapply(LF,read.csv))
      if(!is.null(chigasaki))
      write.csv(chigasaki,paste0("YM/chigasaki",YM,".csv"))
      LF<-list.files(path = "YM",
                     pattern="chigasaki",full.names = T)
      chigasaki<-do.call(rbind,lapply(LF,read.csv))
      write.csv(chigasaki,"chigasaki.csv")
      print("茅ヶ崎市のデータを出力しました")
      
      #横須賀市####
      while(TRUE){
        HTML <- try(read_html("https://www.city.yokosuka.kanagawa.jp/3170/hasseijoukyou.html"))
        Sys.sleep(10)
        if(sum(class(HTML) != "try-error"))break
      }
     
      D1<-format(Date0,"%Y%m")
      D2<-format(Date0,"%Y%m%d")
      Ahref <-
        HTML %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        data.frame() %>%
        rename(html=".") %>%
        filter(grepl(paste0("nagekomi/",D2),html))%>%
        distinct()
      
      file<-paste0("/3170/nagekomi/",D2,".html")
      while(TRUE){
        HT<-try(read_html(paste0("https://www.city.yokosuka.kanagawa.jp",file)))
        Sys.sleep(10)
        if(sum(class(HT) != "try-error")){
          HT<-
            HT%>%
            html_nodes("div") %>%
            html_text() %>%
            data.frame() %>%
            rename(Text=".") %>%
            filter(grepl("^\n+新型コロナウイルス感染症による市内の患者確認",Text))
        }else{HT<-0}
        break
      }
      
      
      
      if(length(HT)!=0){
        if(nrow(Ahref)==0){
          Ahref<-data.frame(html=file)
        }
        # else if(Ahref[1,1]!=file){
        #   Ahref<-rbind(file,Ahref)
        # }
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
        filter(grepl(D,html))%>%
        mutate(pdf=paste0("https://www.city.yokosuka.kanagawa.jp",html))
     if(nrow(Ahref)!=0){
       if(nrow(Ahref2)!=0){
         TD <- data.frame()
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
       }
        
       
        # Table <-
        #   HTML %>%
        #   html_table() %>%
        #   data.frame() %>%
        #   select(-starts_with("No..")) %>%
        #   rename(No="No.") %>%
        #   mutate(No=as.numeric(No))
        TD <- data.frame()
        
        for (i in 1:nrow(Ahref)) {

          while(TRUE){
            yh <- try(read_html(paste0("https://www.city.yokosuka.kanagawa.jp",Ahref$html[i])))
            Sys.sleep(10)
            if(sum(class(yh) != "try-error"))break
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
      

        te <-
          TDS %>%
          count(City)

        TDS2 <-
          TDS %>%
          mutate(Hos="横須賀") %>%
          mutate(Date=as.Date(Date,format="%Y%m%d")) %>%
          arrange(desc(Date),desc(No))
        
        
        if(nrow(Ahref2)!=0){
        TDS3 <-
          # Table %>%
          # select(No,患者確定日,年代,性別,職業等)%>%
          #rbind(TD2%>%select(-Date))%>%
          TD2%>%
          # select(-Date)%>%
          select(-Date,-No) %>%
          cbind(TDS2 %>% arrange(No)) %>%
          # full_join(TDS2)%>%
          filter(!is.na(Age))%>%
          #filter(Date>=as.Date(paste0(D1,"01"),"%Y%m%d"))%>%
          filter(No>1000)%>%
          distinct()
        D2<-format(Date0,"%y%m%d")
        YM<-format(Date0,"%y%m")
        if(!dir.exists(YM)){
          dir.create(YM)
        }
        write.csv(TDS3,paste0(YM,"/yokosuka",D2,".csv"),row.names = F)
     }
        LF<-list.files(path = YM,
                       pattern="yokosuka",full.names = T)
        yokosuka2<-do.call(rbind,lapply(LF,read.csv))

        yokosuka<-yokosuka2%>%
          filter(!is.na(No))%>%
          rename("PR_Date"="Date","Fixed_Date2"="患者確定日")%>%
          mutate(Fixed_Date=PR_Date)%>%
          mutate(Fixed_Date2=paste("2022-",str_replace(Fixed_Date2,"月","-")))%>%
          mutate(Fixed_Date2=str_remove(Fixed_Date2,"日"),
                 Fixed_Date2=as.Date(Fixed_Date2))%>%
          arrange(desc(Fixed_Date))
        write.csv(yokosuka,paste0("YM/yokosuka",YM,".csv"))
        LF<-list.files(path = "YM",
                       pattern="yokosuka",full.names = T)
        yokosuka2<-do.call(rbind,lapply(LF,read.csv))
        write.csv(yokosuka2,"yokosuka.csv")
    
      print("横須賀市を出力しました。")
     }
      #横浜市####
      #Date<-"0220"
      Date<-format(Date0,"%m%d")
      #Date<-format(Date0-1,"%m%d")
      while(TRUE){
        yoko_html1<-try(read_html("https://www.city.yokohama.lg.jp/city-info/koho-kocho/press/kenko/")%>%
                          html_nodes("a")%>%
                          html_attr("href")%>%
                          data.frame()%>%
                          filter(str_detect(.,"covid"))%>%
                          #filter(str_detect(.,"covod"))%>%
                          mutate(flag=str_detect(.,Date))%>%
                          filter(flag==TRUE))
        Sys.sleep(10)
        if(class(yoko_html1) != "try-error")break
        
      }
      
     if(nrow(yoko_html1)!=0){
        if(yoko_html1[1,2]==TRUE){
        while(TRUE){
          yoko_pdf<-try(read_html(paste0("https://www.city.yokohama.lg.jp",yoko_html1[1,1]))%>%
                          html_nodes("a")%>%
                          html_attr("href")%>%
                          data.frame()%>%
                          filter(str_detect(.,"pdf"))%>%
                          mutate(pdf=paste0("https://www.city.yokohama.lg.jp/city-info/koho-kocho/press/kenko/2022/",.)))
          Sys.sleep(10)
          if(class(yoko_pdf) != "try-error")break
        }
        
        
        
        TD <- data.frame()

        pdf<-pdf_text(yoko_pdf[1,2])
        if(pdf[1]=="")print(paste("ファイル名:",yoko_pdf[1,2],"を取得出来ません"))
        re<-regexpr("令和.+日\n",pdf[[1]])
        at<-attr(re,"match.length")
        d=stri_trans_nfkc(substring(pdf[[1]],re,re+at-2))
        

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
        
        #D2<-"220220"
        D2<-format(Date0,"%y%m%d")
        #D2<-format(Date0-1,"%y%m%d")
        #YM<-"2202"
        YM<-format(Date0,"%y%m")
        if(!dir.exists(YM)){
          dir.create(YM)
        }
        write.csv(TDS,paste0(YM,"/yokohama",D2,".csv"),row.names = F)
        LF<-list.files(path = YM,
                       pattern="yokohama",full.names = T)
        yokohama<-do.call(rbind,lapply(LF,read.csv))%>%
          rename("Sex"="Gender","PR_Date"="Date","Residential_City"="City")%>%
          mutate(Fixed_Date=PR_Date,
                 Fixed_Date2=NA)%>%
          arrange(desc(Fixed_Date))%>%
          mutate(Age=str_remove(Age,"代"))%>%
          mutate(Age=str_replace(Age,"10歳","10歳未満"))%>%
          mutate(note=Residential_City)%>%
          mutate(Residential_City=ifelse(str_detect(Residential_City,"市外|非公表"),Residential_City,"横浜市"))
        write.csv(yokohama,paste0("YM/yokohama",YM,".csv"))
        LF<-list.files(path = "YM",
                       pattern="yokohama",full.names = T)
        yokohama<-do.call(rbind,lapply(LF,read.csv))
        write.csv(yokohama,"yokohama.csv",row.names = F)
       
      }
     }
     
      
    
      print("横浜市を出力しました")
      #相模原市####
      
      
      Date<-format(Date0,"%m%d")
      while (TRUE) {
        saga_html<-try(read_html("https://www.city.sagamihara.kanagawa.jp/shisei/koho/1019191.html")%>%
                         html_nodes("a")%>%
                         html_attr("href")%>%
                         data.frame()%>%
                         filter(str_detect(.,"pdf"))%>%
                         mutate(flag=str_detect(.,Date))%>%
                         mutate(pdf=str_remove(.,"^../..")) %>%
                         filter(flag))
        Sys.sleep(10)
        if(class(saga_html) != "try-error")break
      }
      
      if(nrow(saga_html)!=0){
        TD <- data.frame()
       
        if(saga_html[1,2]==TRUE){
          path2<-paste0("https://www.city.sagamihara.kanagawa.jp",saga_html[1,3])

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
            mutate(Text2=stri_trans_nfkc(Text)) %>%
            mutate(Text2=gsub("茅ヶ崎 ","茅ヶ崎市 ",Text2))

          sr=which(grepl("^ *[[:digit:]]+ ",TD$Text2))
          
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
            
            re<-regexpr("[^ ]+[市区町村都府県郡] |[^ ]+[内外]) |県外 ",tds)
            if(re==-1){
              re<-regexpr("調査中 ",tds)
            }
            # print(TDS$No[n])
            # print(tds)
            # print(n)
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
          D2<-format(Date0,"%y%m%d")
          YM<-format(Date0,"%y%m")
          if(!dir.exists(YM)){
            dir.create(YM)
            }
          write.csv(TDS2,paste0(YM,"/sagamihara",D2,".csv"),row.names = F)
          
          
        }
        
        LF<-list.files(path = YM,
                       pattern="sagamihara",full.names = T)
        sagamihara<-do.call(rbind,lapply(LF,read.csv))%>%
          arrange(Date)%>%
          distinct(No,Date,.keep_all = T)%>%
          rename("Sex"="Gender","PR_Date"="Date","Residential_City"="City")%>%
          mutate(Fixed_Date=PR_Date,
                 Fixed_Date2=NA)%>%
          arrange(desc(Fixed_Date))
        write.csv(sagamihara,paste0("YM/sagamihara",YM,".csv"),row.names = F)
        LF<-list.files(path = "YM",
                       pattern="sagamihara",full.names = T)
        sagamihara<-do.call(rbind,lapply(LF,read.csv))
      
        write.csv(sagamihara,"sagamihara.csv",row.names = F)
      }
     
      print("相模原市を出力しました")
      #藤沢市####
      while(TRUE){
        HTML <- try(read_html("https://www.city.fujisawa.kanagawa.jp/hoken-j/corona_doukou_data.html"))
        # Sys.sleep(10)
        # print(class(HTML))
        if(sum(class(HTML) == "try-error")) next
      
      #HTML
      if(sum(class(HTML) != "try-error")){
         Ahref <-
           HTML %>%
           html_nodes("a") %>%
           html_attr("href") %>%
           data.frame()
         colnames(Ahref) <- "pdf"
         Fujisawa <-
           Ahref %>%
           filter(grepl("[[:digit:]]+_*pr.+pdf$",pdf))
         p=Fujisawa$pdf[1]
         TD <- data.frame()
         pdf<-try(pdf_text(paste0("https://www.city.fujisawa.kanagawa.jp",p)))
        
         if(class(pdf) != "try-error") break
      }
     
      
      
      
  
      
     
     
      }
      
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
        mutate(Text2=stri_trans_nfkc(Text))%>%
        mutate(Text2=str_remove(Text2,"^ "))
      #空白までの文字を拾う
      sr=which(grepl("^ {0,1}[[:digit:]]+ ",TD$Text2))
      n=1
      TDS <- data.frame(Date="",No=1:length(sr),Age="",Gender="",Hos="",City="",Jobs="",no="")%>%
        mutate(Date=Date0)
      for (n in 1:length(sr)) {
        # for (n in 413:430) {
        (tds <- paste0(TD$Text2[sr[n]],collapse = " "))
        
        re<-regexpr(" +",tds)
        at<-attr(re,"match.length")
        TDS$no[n]=as.numeric(substring(tds,1,re-1))
        tds <- substring(tds,re+at,nchar(tds))
        
        re<-regexpr(" +",tds)
        at<-attr(re,"match.length")
        TDS$No[n]=as.numeric(substring(tds,1,re-3))
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
        # #日付がある行
        # if(at<7){
        #   re<-regexpr(" +",tds)
        #   at<-attr(re,"match.length")
        #   TDS$Date[n]=Date0
        #   tds <- substring(tds,re+at,nchar(tds))
        # }
        tds <- substring(tds,re+at,nchar(tds))
        
        # print(c(n,TDS$No[n],TDS$Date[n]))
      }
      TDS <-
        TDS %>%
        mutate(no=as.numeric(no))
      
      # #日番号(no)がNAのところに次の値-1を入れる　NAがなくなるまで
      # kb=which(is.na(TDS$no))
      # while(length(kb)>0){
      #   for (n in kb) {
      #     TDS$no[n]=TDS$no[n+1]-1
      #   }
      #   kb=which(is.na(TDS$no))
      # }
      # day=which(grepl("202.年..??月..??日",TD$Text2))
      # re<-regexpr("202.年..??月..??日",TD$Text2[day])
      # at<-attr(re,"match.length")
      # dayl= unique(substring(TD$Text2[day],re,re+at-1))
      # n=1
      # d=1
      # TDS$Date[n]=dayl[d]
      # #基本的に日番号=1のときに日付ずらす
      # #既に入っている日付とずれたときはずらす
      # for (n in 2:length(sr)) {
      #   if(TDS$No[n]==18522&TDS$Date[n]=="2022年1月24日"){
      #     TDS$Date[n]<-"2022年2月24日"
      #   }
      #   dn=TDS$no[n]
      #   dt=TDS$Date[n]
      #   # if(dayl[d]=="2022年2月24日"&TDS$no[n]==324){
      #   #   stop()
      #   # }
      #   if(dn==1|(dt!=""&dt!=dayl[d])){ 
      #     d=d+1
      #   }
      # 
      #   TDS$Date[n]=dayl[d]
      #   print(c(n,TDS$No[n],TDS$Date[n]))
      # }
      # 
      # 
      
      TDS2 <-
        TDS %>%
        filter(Age!="")%>%
        mutate(Hos="藤沢") %>%
        mutate(Date=as.Date(Date,format="%Y年%m月%d日")) %>%
        arrange(desc(no),desc(Date))%>%
        select(Date,No,Age,Gender,Hos,City,no)
      D2<-format(Date0,"%y%m%d")
      YM<-format(Date0,"%y%m")
      if(!dir.exists(YM)){
        dir.create(YM)
      }
      write.csv(TDS2,paste0(YM,"/fujisawa",D2,".csv"),row.names = F)
      
      LF<-list.files(path = YM,
                     pattern="fujisawa",full.names = T)
      #write.csv(TDS2,"fujisawa202110.csv",row.names = F)
      fujisawa<-
        # rbind(read.csv("fujisawa202103.csv"),
        #               read.csv("fujisawa202104.csv"),
        #               read.csv("fujisawa202110.csv"))%>%
        do.call(rbind,lapply(LF,read.csv))%>%
        rename("PR_Date"="Date","Sex"="Gender","Residential_City"="City")%>%
        mutate(Fixed_Date=PR_Date,
               Fixed_Date2=NA)%>%
        mutate(Age=str_remove(Age,"代"))%>%
        arrange(desc(Fixed_Date))%>%
        mutate(Age=str_replace(Age,"10歳$","10歳未満"))
      write.csv(fujisawa,paste0("YM/fujisawa",YM,".csv"),row.names = F)
      LF<-list.files(path = "YM",
                     pattern="fujisawa",full.names = T)
      fujisawa<-do.call(rbind,lapply(LF,read.csv))

      write.csv(fujisawa,"fujisawa.csv",row.names = F)
      print("藤沢市を出力しました")
     #データ作成####
      yokohama<-read.csv("yokohama.csv")%>%
        select(-No)%>%
        mutate(Fixed_Date=as.Date(Fixed_Date),
               Fixed_Date2=as.Date(Fixed_Date2),
               PR_Date=as.Date(PR_Date))%>%
        select(PR_Date,Residential_City,Age,Sex,Fixed_Date2,Fixed_Date,Hos,note)%>%

        mutate(hos="yokohama")
      #横須賀市
      yokosuka<-read.csv("yokosuka.csv")%>%
        select(Fixed_Date,PR_Date,Sex,Age,City,Hos,Fixed_Date2)%>%
        mutate(Fixed_Date=as.Date(Fixed_Date),
               Fixed_Date2=as.Date(Fixed_Date2),
               PR_Date=as.Date(PR_Date))%>%
        rename("Residential_City"="City")%>%
        mutate(Residential_City=str_remove(Residential_City,".+郡"))%>%
        mutate(note=NA)%>%
        mutate(hos="yokosuka")
      #相模原市
      sagamihara<-read.csv("sagamihara.csv")%>%
        select(-No)%>%
        mutate(note=NA)%>%
        mutate(hos="sagamihara")
      #藤沢市
      fujisawa<-read.csv("fujisawa.csv")%>%
        select(-no,-No)%>%
        mutate(note=NA)%>%
        mutate(hos="fujisawa")

      #今日の神奈川県####
      while (TRUE) {
        HTML<-try(read_html("https://www.pref.kanagawa.jp/prs/list-2022-1-1.html"))
        Sys.sleep(10)
        if(class(HTML) != "try-error")break
      }
      Date1<-format(Date0,"%d日")%>%
        str_remove("^0")
      Date2<-format(Date0,"%m月")%>%
        str_remove("^0")
      URL<-HTML%>%
        html_nodes("a")%>%
        html_attr("href")
      TEXT<-HTML%>%
        html_nodes("a")%>%
        html_text()
     
      HTML2<-cbind(TEXT,URL)%>%
        data.frame()%>%
        mutate(TEXT=stri_trans_nfkc(TEXT))%>%
        filter(str_detect(TEXT,"新型コロナウイルス感染症による患者確認について"))%>%
        filter(str_detect(TEXT,Date1))%>%
        filter(str_detect(TEXT,Date2))
      if(!is.na(HTML2[1,2])){
        path<-paste0("https://www.pref.kanagawa.jp",HTML2[1,2])
        while (TRUE) {
          CSV<-try(read_html(path)%>%
                     html_nodes("a")%>%
                     html_attr("href")%>%
                     data.frame()%>%
                     filter(str_detect(.,".csv"))%>%
                     rename("csv"="."))
          Sys.sleep(10)
          if(class(CSV) != "try-error")break
        }
        
        
        kanagawa_today<-data.frame()
        if(!is.na(CSV[1,1])){
          if(Date0=="2022-02-20"){
            kanagawa_today<-read.csv("breakdown_20220220.csv")%>%
              rename("Residential_City"="居住地",
                     "Fixed_Date2"="陽性判明日",
                     "Sex"="性別",
                     "Age"="年代",
                     "note"="発生届を受理した保健福祉事務所")%>%
              filter(!is.na(患者概要))%>%
              filter(Residential_City!="")%>%
              select(Residential_City,Fixed_Date2,Sex,Age,note)%>%
              mutate(Fixed_Date=Date0,
                     PR_Date=Date0,
                     Hos="神奈川県",
                     hos="kanagawa")%>%
              mutate(Fixed_Date2=str_remove(Fixed_Date2,"日"),
                     Fixed_Date2=paste0("2022-",str_replace(Fixed_Date2,"月","-")))
          }else{
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
                                    mutate(Fixed_Date=Date0,
                                           PR_Date=Date0,
                                           Hos="神奈川県",
                                           hos="kanagawa")%>%
                                    mutate(Fixed_Date2=str_remove(Fixed_Date2,"日"),
                                           Fixed_Date2=paste0("2022-",str_replace(Fixed_Date2,"月","-"))))
              
              if(class(kanagawa_today) != "try-error")break
            }
            
          }
          
          
        }
        
      }else{
        kanagawa_today<-data.frame()
      }
      D2<-format(Date0,"%y%m%d")
      YM<-format(Date0,"%y%m")
      if(nrow(kanagawa_today)!=0){
        write.csv(kanagawa_today,paste0(YM,"/kanagawa",D2,".csv"),row.names = F)
        print("神奈川県を出力しました")
      }
      
      LF<-list.files(path = YM,
                     pattern="kanagawa",full.names = T)
      kanagawa<-do.call(rbind,lapply(LF,read.csv))
      
      LF<-list.files(path = "2203",
                     pattern="kanagawa",full.names = T)
      kanagawa3<-do.call(rbind,lapply(LF,read.csv))
      
      LF<-list.files(path = "2204",
                     pattern="kanagawa",full.names = T)
      kanagawa3<-
        kanagawa3 %>%
        rbind(do.call(rbind,lapply(LF,read.csv)))
      
      
      kanagawa2<-
        rbind(yokohama,sagamihara,fujisawa,
              yokosuka,kanagawa,kanagawa3) %>%
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
               Fixed_Date2=NA)%>%
        left_join(xy,by="Residential_City")%>%
        mutate(Hos="茅ヶ崎市",
               hos="chigasaki")%>%
        mutate(note=NA)%>%
        arrange(desc(Fixed_Date))
      
      list1<-read.csv("list.csv")

      kanagawa2<-
        left_join(kanagawa2,xy,by="Residential_City") %>%
        mutate(Fixed_Date=as.Date(Fixed_Date))
      
      
      kawasaki<-
        read.csv("kawasaki.csv") %>%
        select(-X)%>%
        mutate(Fixed_Date=as.Date(Fixed_Date)) %>%
        mutate(PR_Date=as.Date(PR_Date)) %>%
        mutate(Fixed_Date2=as.Date(Fixed_Date2))%>%
        left_join(list1)%>%
        select(-管内,-Residential_City)%>%
        rename("Residential_City"="list")%>%
        mutate(Hos="川崎市",
               hos="kawasaki")%>%
        arrange(desc(Fixed_Date))
 
      if(nrow(yokohama%>%
              filter(PR_Date==Date0))==0){
        
        while (TRUE) {
          yo<-try(fread("https://www.city.yokohama.lg.jp/kurashi/kenko-iryo/yobosesshu/kansensho/coronavirus/corona-data.files/141003_yokohama_covid19_patients.csv",
                        encoding="UTF-8")%>%
                    data.frame()%>%
                    mutate(記者発表日=as.Date(記者発表日))%>%
                    filter(記者発表日==Date0))
          break
          if(class(yo) != "try-error")break
        }
        if(class(yo) != "try-error"){
          if(nrow(yo)!=0){
          yo<-yo%>%
            rename("Residential_City"="市町村名",
                   "Fixed_Date"="記者発表日",
                   "Age"="年代",
                   "Sex"="性別",
                   "note"="住所地",
                   "Residential_Pref"="都道府県名",
            )%>%
            mutate(PR_Date=Fixed_Date,
                   Hos="横浜",
                   hos="yokohama",
                   X="",
                   Y="",
                   Hospital_Pref="神奈川県",
                   Fixed_Date2="")%>%
            select(Residential_Pref,Residential_City,Fixed_Date,Age,Sex,note,
                   PR_Date,Fixed_Date2,Hos,hos,Hospital_Pref,X,Y)%>%
            mutate(note=str_remove(note,"区"))
          write.csv(yo,paste0("yoko/yokohama",Date0,".csv"),row.names = F)
          print("横浜市のデータを出力")
        }
        }
        
      }
      yo_list<-list.files("C:/Users/Covid_Data/Desktop/covid/yoko",full.names = T)
      yo2<-do.call(rbind,lapply(yo_list,read.csv))%>%
        mutate(Fixed_Date2=as.Date(Fixed_Date2),
               Hospital_Pref=as.character(Hospital_Pref),
               Residential_Pref=as.character(Residential_Pref),
               Residential_City=as.character(Residential_City),
               Age=as.character(Age),
               Sex=as.character(Sex),
               PR_Date=as.Date(PR_Date),
               Fixed_Date=as.Date(Fixed_Date),
               Hos=as.character(Hos),
               hos=as.character(hos),
               note=as.character(note))%>%
        filter(Fixed_Date>="2022-03-01")
      data2<-fread("data2022_2.csv",encoding = "UTF-8")%>%
        mutate(Fixed_Date2=as.Date(Fixed_Date2),
               Hospital_Pref=as.character(Hospital_Pref),
               Residential_City=as.character(Residential_City),
               Age=as.character(Age),
               Sex=as.character(Sex),
               PR_Date=as.Date(PR_Date),
               Fixed_Date=as.Date(Fixed_Date),
               Hos=as.character(Hos),
               hos=as.character(hos),
               note=as.character(note))
      # fujisawa2<-fread("data2022.csv",encoding = "UTF-8")%>%
      #   filter(Hos=="藤沢")%>%
      #   mutate(Fixed_Date2=as.Date(Fixed_Date2),
      #          Hospital_Pref=as.character(Hospital_Pref),
      #          Residential_City=as.character(Residential_City),
      #          Age=as.character(Age),
      #          Sex=as.character(Sex),
      #          PR_Date=as.Date(PR_Date),
      #          Fixed_Date=as.Date(Fixed_Date),
      #          Hos=as.character(Hos),
      #          hos=as.character(hos),
      #          note=as.character(note))%>%
      #   filter(Fixed_Date<="2022-03-07")
      fujisawa2<-fread("C:/Users/Covid_Data/Documents/covid_sy/fujisawa_data/fujisawa_2021.csv")%>%
        rename("PR_Date"="Date","Sex"="Gender","Residential_City"="City")%>%
        mutate(Fixed_Date=PR_Date,
               Fixed_Date2=NA,
               Hospital_Pref="神奈川県")%>%
        mutate(Age=str_remove(Age,"代"))%>%
        arrange(desc(Fixed_Date))%>%
        mutate(Age=str_replace(Age,"10歳$","10歳未満")) %>%
        select(-no,-No,-Jobs)%>%
        mutate(note=NA)%>%
        mutate(hos="fujisawa") %>%
        filter(Fixed_Date<="2022-03-07") %>%
        filter(Fixed_Date>="2022-03-01")
      
      data<-bind_rows(data2,kanagawa2,kawasaki,chigasaki,yo2,fujisawa2)%>%
        select(Fixed_Date,Hospital_Pref,Residential_City,Age,
               Sex,X,Y,PR_Date,Fixed_Date2,Hos,note,hos)%>%
        filter(Fixed_Date<Date0)%>%
        mutate(Age=str_remove(Age,"代"),
               Age=ifelse(str_detect(Age,"100"),"100-",Age),
               Age=ifelse(str_detect(Age,"歳"),"0-10",Age),
               Age=str_replace(Age,"－","-"),
               Age=str_remove_all(Age," "))%>%
        arrange(desc(Fixed_Date),Hospital_Pref,Hos,Residential_City)

      data2022<-
        data%>%
        filter(Fixed_Date>=as.Date("2022-03-01"))
      
      # data202203<-
      #   data%>%
      #   filter(Fixed_Date>=as.Date("2022-03-01"))%>%
      #   filter(Fixed_Date<=as.Date("2022-03-31"))
      # write.csv(data202203,"data202203.csv",row.names=F,fileEncoding="UTF-8")
      
      if(format(Sys.time(),"%H")%in%c("17","18","19","20","21")){
        data<-bind_rows(data2,kanagawa2,kawasaki,chigasaki,yo2,fujisawa2)%>%
          select(Fixed_Date,Hospital_Pref,Residential_City,Age,
                 Sex,X,Y,PR_Date,Fixed_Date2,Hos,note,hos)%>%
          mutate(Age=str_remove(Age,"代"),
                 Age=ifelse(str_detect(Age,"歳"),"0-10",Age),
                 Age=str_replace(Age,"－","-"),
                 Age=str_remove_all(Age," "))%>%
          arrange(desc(Fixed_Date),Hospital_Pref,hos,Residential_City,note)

        data2022<-
          data%>%
          filter(Fixed_Date>=as.Date("2022-04-01"))

      }

      write.csv(data2022,"data2022.csv",row.names=F,fileEncoding="UTF-8")
      print("coviddata.csvを出力しました")

    
    
    
    
    #実行時間
    print(paste0(Sys.time()-time))
    print(Sys.time())
    #実行開始時間を保存
    time2<-time
    #30分経過後最初に戻る
    # Sys.sleep(300)
    
  }
  
}
)
#エラーが起きた場合？
library(devtools)
library(slackr)


#Token & チャネル名 & Username の設定
OAuth_token <- "xoxb-3163549173589-3179082546129-vKfvL0c3JD5Iry3V9RX4RIuf"
WebURL <- "https://hooks.slack.com/services/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"
Channel <- "#check"
#チャネル名は、"Channel ID"でも可 
Username <- "rslack_create_data"
#Usernameは任意でOK

create_config_file(token=OAuth_token,
                   incoming_webhook_url = WebURL,
                   username = Username,
                   channel = Channel)

slackr_setup()
slackr_msg("エラーにより、データ収集のプログラムが終了しました。")
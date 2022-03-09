library(data.table)
library(dplyr)
library(pdftools)
library(Hmisc)
library(rvest)
library(stringi)
library(stringr)

HTML <- read_html("https://www.city.fujisawa.kanagawa.jp/hoken-j/corona_doukou_data.html")
HTML

Ahref <-
  HTML %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  data.frame()

colnames(Ahref) <- "pdf"

Fujisawa <-
  Ahref %>%
  filter(grepl("fujisawa[[:digit:]]+.pdf$",pdf))

p=Fujisawa$pdf[1]
l=1
TD <- data.frame()
for (p in Fujisawa$pdf) {
  pdf<-pdf_text(paste0("https://www.city.fujisawa.kanagawa.jp",p))
  
  # re<-regexpr("令和.{1,10}日\n",pdf[[1]])
  # at<-attr(re,"match.length")
  # d=gsub(" ","",stri_trans_nfkc(substring(pdf[[1]],re,re+at-2)))
  
  for (l in 1:length(pdf)) {
    td <-
      # gsub("  +","　",pdf[[l]]) %>%
      pdf[[l]] %>%
      strsplit("\n") %>%
      data.frame()
    # %>%
    #   mutate(Date=d) %>%
    #   mutate(Date=gsub("令和3年","2021年",Date)) %>%
    #   mutate(Date=gsub("令和2年","2020年",Date))
    
    colnames(td)[1] <- "Text"
    
    TD <-
      TD %>%
      rbind(td)
    
    print(l)
  }
  print(p)
}

TD <-
  TD %>%
  # mutate(Text2=gsub(" ","",Text)) %>%
  mutate(Text2=stri_trans_nfkc(Text))

sr=which(grepl("男性|女性|男児|女児",TD$Text2))
day=which(grepl("202.年.+月.+日",TD$Text2))[-408]
# hoken=c("平塚","鎌倉","小田原","厚木")
n=1
TDS <- data.frame(Date="",No=1:length(sr),Age="",Gender="",Hos="",City="")
d=1
for (n in 1:length(sr)) {
# for (n in 413:430) {
  tds <- paste0(TD$Text2[sr[n]],collapse = " ")
  tds
  re<-regexpr("^[[:digit:]]+ |^ [[:digit:]] ",tds)
  at<-attr(re,"match.length")
  TDS$No[n]=as.numeric(substring(tds,re,re+at-1))
  
  re<-regexpr("[[:digit:]]{2,3}[代歳]|.学生|..学児",tds)
  at<-attr(re,"match.length")
  TDS$Age[n]=substring(tds,re,re+at-1)
  #re<-regexpr(" [[:digit:]]",tds)
  TDS$no[n] <- as.numeric(substring(tds,re-5,re-1)%>%str_remove("[^0-9]"))
  #TDS$no[n] <- as.numeric(substring(tds,re-4,re-1)%>%str_remove("[^0-9]"))
  # if(n>=1092&n<1241){
  # TDS$no[n] <- as.numeric(substring(tds,re-6,re-1)%>%str_remove("[^0-9]"))
  # }
  #if((n>=38&n<1092)|n==1241|n==1246){
    #TDS$no[n] <- as.numeric(substring(tds,re-4,re-1)%>%str_remove("[^0-9]"))
  #}
  if(n>1)
    if(TDS$no[n]==1|sr[n]-sr[n-1]>5) #1番または改ページ
    d=d+1
  re<-regexpr("202.年.+月.+日",TD$Text2[day[d]])
  at<-attr(re,"match.length")
  TDS$Date[n]=substring(TD$Text2[day[d]],re,re+at-1)
  
  re<-regexpr("男性|女性|男児|女児",tds)
  at<-attr(re,"match.length")
  TDS$Gender[n]=substring(tds,re,re+at-1)
  
  re<-regexpr("[^ ]+[市区町村都]|[^ ]+[内外]",tds)
  at<-attr(re,"match.length")
  TDS$City[n]=substring(tds,re,re+at-1)
  
  print(n)
  print(TDS$No[n])
}

te <-
  TDS %>%
  count(Date)
te <-
  TD[sr,] %>%
  mutate(n=1:n())
te2 <-
  TD[day,] %>%
  mutate(n=1:n())

TDS2 <-
  TDS %>%
  # filter(!is.na(No)) %>%
  mutate(Hos="藤沢") %>%
  mutate(Date=as.Date(Date,format="%Y年%m月%d日")) %>%
  arrange(desc(No),desc(Date))

write.csv(TDS2,"fujisawa.csv",row.names = F)





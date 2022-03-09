library(data.table)
library(dplyr)
library(pdftools)
library(Hmisc)
library(rvest)
library(stringi)

HTML <- read_html("https://www.pref.kanagawa.jp/docs/ga4/covid19/occurrence_26.html")
HTML

Ahref <-
  HTML %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  data.frame()

colnames(Ahref) <- "pdf"

Chigasaki <-
  Ahref %>%
  filter(grepl("chigasaki.pdf$",pdf))

p=Chigasaki$pdf[1]
l=1
TD <- data.frame()
for (p in Chigasaki$pdf) {
  pdf<-pdf_text(paste0("https://www.pref.kanagawa.jp",p))
  
  re<-regexpr("202.+日\n",pdf[[1]])
  at<-attr(re,"match.length")
  d=gsub(" ","",stri_trans_nfkc(substring(pdf[[1]],re,re+at-2)))
  
  for (l in 1:length(pdf)) {
    td <-
      # gsub("  +","　",pdf[[l]]) %>%
      pdf[[l]] %>%
      strsplit("\n") %>%
      data.frame() %>%
      mutate(Date=d) %>%
      mutate(Date=gsub("令和3年","2021年",Date)) %>%
      mutate(Date=gsub("令和2年","2020年",Date))
    
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
hoken=c("平塚","鎌倉","小田原","厚木")
n=1
TDS <- data.frame(Date="",No=1:length(sr),Age="",Gender="",Hos="",City="")
for (n in 1:length(sr)) {
  tds <- paste0(TD$Text2[sr[n]],collapse = " ") #:min(sr[n+1]-1,sr[length(sr)],na.rm = T)
  
  TDS$Date[n]=TD$Date[sr[n]]
  
  re<-regexpr("[[:digit:]]+例目",tds)
  at<-attr(re,"match.length")
  TDS$No[n]=as.numeric(substring(tds,re,re+at-3))
  
  re<-regexpr("[[:digit:]]{2,3}[代歳]|.学生|..学児",tds)
  at<-attr(re,"match.length")
  TDS$Age[n]=substring(tds,re,re+at-1)
  
  re<-regexpr("男性|女性|男児|女児",tds)
  at<-attr(re,"match.length")
  TDS$Gender[n]=substring(tds,re,re+at-1)
  
  re<-regexpr("[^ ]+[市区町村都] |[^ ]+[内外]) ",tds)
  at<-attr(re,"match.length")
  TDS$City[n]=substring(tds,re,re+at-2)
  
  print(n)
}

te <-
  TDS %>%
  count(No)

TDS <-
  TDS %>%
  # filter(!is.na(No)) %>%
  mutate(Hos="茅ヶ崎") %>%
  mutate(Date=as.Date(Date,format="%Y年%m月%d日")) %>%
  arrange(desc(Date),desc(No))

write.csv(TDS,"chigasaki.csv",row.names = F)





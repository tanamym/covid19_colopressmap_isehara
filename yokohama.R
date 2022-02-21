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

Yokohama <-
  Ahref %>%
  filter(grepl("yokohama.pdf$",pdf))

p=Yokohama$pdf[1]
l=1
TD <- data.frame()
for (p in Yokohama$pdf) {
  pdf<-pdf_text(paste0("https://www.pref.kanagawa.jp",p))
  
  re<-regexpr("—ß˜a.+“ú\n",pdf[[1]])
  at<-attr(re,"match.length")
  d=stri_trans_nfkc(substring(pdf[[1]],re,re+at-2))
  
  for (l in 2:length(pdf)) {
    td <-
      pdf[[l]] %>%
      strsplit("\n") %>%
      data.frame() %>%
      mutate(Date=d) %>%
      mutate(Date=gsub("—ß˜a3”N","2021”N",Date)) %>%
      mutate(Date=gsub("—ß˜a2”N","2020”N",Date))
    
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

sr=which(grepl("’j«|—«|’jŽ™|—Ž™",TD$Text2))
n=1
TDS <- data.frame(Date="",No=1:length(sr),Age="",Gender="",Hos="",City="")
for (n in 1:length(sr)) {
  tds <- paste0(TD$Text2[sr[n]])
  
  TDS$Date[n]=TD$Date[sr[n]]
  
  re<-regexpr("^[[:digit:]]+ ",tds)
  at<-attr(re,"match.length")
  TDS$No[n]=as.numeric(substring(tds,re,re+at-1))
  
  re<-regexpr("[[:digit:]]{2,3}[‘ãÎ]",tds)
  at<-attr(re,"match.length")
  TDS$Age[n]=substring(tds,re,re+at-1)
  
  re<-regexpr("’j«|—«|’jŽ™|—Ž™",tds)
  at<-attr(re,"match.length")
  TDS$Gender[n]=substring(tds,re,re+at-1)
  
  re<-regexpr("[^ ]+[Žs‹æ’¬‘º]|[^ ]+[“àŠO])",tds)
  at<-attr(re,"match.length")
  TDS$City[n]=substring(tds,re,re+at-1)
  
  print(n)
}

te <-
  TDS %>%
  count(Date)

TDS <-
  TDS %>%
  mutate(Date=as.Date(Date,format="%Y”N%mŒŽ%d“ú")) %>%
  mutate(Hos="‰¡•l") %>%
  arrange(desc(Date),desc(No))

write.csv(TDS,"yokohama.csv",row.names = F)




library(data.table)
library(dplyr)
library(rvest)
library(stringi)

HTML <- read_html("https://www.city.yokosuka.kanagawa.jp/3130/hasseijoukyou_202105.html")
HTML

Ahref <-
  HTML %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  data.frame() %>%
  rename(html=".") %>%
  filter(grepl("nagekomi",html))

Ahref <-
  Ahref %>%
  rbind("/3130/nagekomi/20210501.html")

Table <-
  HTML %>%
  # html_nodes("table") %>%
  html_table() %>%
  data.frame() %>%
  select(-starts_with("No..")) %>%
  rename(No="No.") %>%
  mutate(No=as.numeric(No))

TD <- data.frame()
for (i in 1:nrow(Ahref)) {
  yh <- read_html(paste0("https://www.city.yokosuka.kanagawa.jp",Ahref$html[i]))
  
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
  
  print(Ahref$html[i])
}

TD <-
  TD %>%
  mutate(Text2=stri_trans_nfkc(Text)) %>%
  mutate(Text2=gsub(" ","",Text2))

sr=which(grepl("^[[:digit:]]+例目$",TD$Text2))
lr=c(sr[-1]-1,nrow(TD))

te <- TD[sr,]

n=1
TDS <- data.frame(Date="",No=1:length(sr),Age="",Sex="",Hos="",City="")
for (n in 1:length(sr)) {
  tds <- paste0(TD$Text2[sr[n]:lr[n]],collapse = " ") #:min(sr[n+1]-1,sr[length(sr)],na.rm = T)
  
  TDS$Date[n]=TD$Date[sr[n]]
  
  re<-regexpr("[[:digit:]]+例目",tds)
  at<-attr(re,"match.length")
  TDS$No[n]=as.numeric(substring(tds,re,re+at-3))
  
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
  
  print(c(n,TDS$No[n]))
}

te <-
  TDS %>%
  count(City)

TDS2 <-
  TDS %>%
  # filter(!is.na(No)) %>%
  mutate(Hos="横須賀") %>%
  mutate(Date=as.Date(Date,format="%Y%m%d")) %>%
  arrange(desc(Date),desc(No))

TDS3 <-
  Table %>%
  full_join(TDS2)

write.csv(TDS3,"yokosuka_202105.csv",row.names = F)





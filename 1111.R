data2020 <-
  fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data2020.csv",encoding="UTF-8")

data202106 <-
  fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data202106.csv",encoding="UTF-8")
data202109 <-
  fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data202109.csv",encoding="UTF-8")

data2021 <-
  fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data2021.csv",encoding="UTF-8")
str(kanagawa3)
data7 <-
  rbind(data2020,data202106,data202109,data2021)%>%
  filter(Hos=="神奈川県")%>%
  mutate(Fixed_Date=as.Date(Fixed_Date))%>%
  select(Fixed_Date,note,Residential_City, Fixed_Date2,PR_Date,Age,Sex)%>%
  filter(Fixed_Date<"2021-11-01")%>%
  filter(Fixed_Date>"2020-12-31")
kanagawa3<-data7

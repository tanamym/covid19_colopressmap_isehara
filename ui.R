
library(shiny)
library(leaflet)
shinyUI(fluidPage(
    # Application title
    titlePanel("COVID-MAP"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #ffffff;
               background-color: #3399ff;
               z-index: 105;
             }
          ")),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading...",id="loadmessage")),
            h5("最終アップデート:2021-05-04"),
            h3("Input"),
            h4("設定"),
            h4("神奈川県全体の状況"),
            h5("川崎市のみ区単位での表示となっています。"),
            p("注意：清川村、三浦市など人口が少ない市町村では10万人当たりの感染者数の色が濃くなることがあります。"),
            uiOutput("date"),
            #numericInput("y",label = h5("累積日数"),value="14"),
            radioButtons("y",label = "累積日数を設定してください",
                         c("1日"="1",
                           "7日"="7")
                         ),
            h4("横浜市の状況"),
            p("注意：以下の横浜市は一週間ごとの集計です。"),
            # selectInput("year1","年を選択してください。",
            #             c("2021年"="2021年","2020年"="2020年")),
            # uiOutput("date2"),
            p("＊1 横浜市が前週の新規感染者数の訂正をしたため、鶴見区と泉区の4月18日から4月27日の新規感染者数がマイナスになっています。

            ＊2 横浜市が7月18日から7月24日のデータと7月25日から7月31日のデータをまとめて公表したため、その2週間のデータを半分にしたものを1週間のデータとしています。

            ＊3 9月5日から9月11日の区ごとの新規感染者数には、8月29日から9月4日に調査中だった39人が含まれます。
            （横浜市全体の新規感染者数には含まれません。）

            ＊4 横浜市が12月4日に12月10日までのデータを公表したため、11月28日から12月10日は他の週よりも1日短くなっています。

            ＊5 横浜市が12月26日から12月31日のデータと1月1日から1月7日のデータをまとめて公表したため、その2週間のデータを半分にしたものを1週間のデータとしています。

            市外とは、横浜市外に住み、横浜市内で検査して陽性となった人の合計です。

            ")
            # sliderInput("color",
            #             label = "Setting of color",
            #             min = 10,
            #             max = 2000,
            #             value = 1600,)
                     
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("累積感染者数",
                                       h4("神奈川県全体の状況"),
                                       leafletOutput("covid_map",height = "800px"),
                                       h4("横浜市の状況"),
                                       leafletOutput("yoko_map",height = "300px"),
),
                              tabPanel("10万人当たりの累積感染者数",
                                       h4("神奈川県全体の状況"),
                                       leafletOutput("covid_map2",height = "800px"),
                                       h4("横浜市の状況"),
                                       leafletOutput("yoko_map2",height = "300px"),
                                       ),
                              tabPanel("謝辞および参考文献",
                                       h4("謝辞"),
                                       h5("この研究は、2021年度東海大学連合後援会助成金交付により研究が遂行されたものです。また日々、新型コロナウイルス感染症の対応を行っている医療関係者や保健所の皆様、データの提供元のおかげで本研究を進めることができました。また、東海大学分子生命科学の今西規先生と指導教員である東海大学理学部数学科の山本義郎先生には有益な助言をいただきました。この場を借りて深く御礼申し上げます。"),
                                       h4("参考文献、その他"),
                                       p("このサイトは、神奈川県や川崎市、茅ヶ崎市の行政のサイトで公開されている新型コロナウイルス感染症の感染者の情報を使用しています。"),
                                       tags$a(href="https://www.pref.kanagawa.jp/docs/ga4/covid19/occurrence_list.html", "新型コロナウイルスに感染した患者の発生状況一覧(神奈川県)"),
                                       p(""),
                                       tags$a(href="https://www.city.kawasaki.jp/350/page/0000115886.html","【緊急情報】川崎市内の新型コロナウイルスに感染した患者等の発生状況"),
                                       p(""),
                                       tags$a(href="http://xn--city-kw4cy81x2nbwx7m.chigasaki.kanagawa.jp/koho/1030702/1038773/index.html","新型コロナウイルス感染症による管内の患者確認について(茅ヶ崎市)"),
                                       p(""),
                                       p("横浜市の区別データは以下のサイトのデータを参照しています。"),
                                       tags$a(href="https://square.umin.ac.jp/kenkono/","横浜市区別コロナデータ"),
                                       p(""),
                                       p("本サイトでは、横浜市、横須賀市、相模原市、藤沢市は発表日を他の市町村は陽性判明日を元に感染者の集計を行っています。"),
                                       p("東海大学大学院理学研究科　棚橋真弓")
                              # textOutput("text"),
                              # textOutput("text2")
                              )
                              )
                  )
    )
    
    
)
)

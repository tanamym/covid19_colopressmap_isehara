
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
            h5("最終アップデート:2021-04-25"),
            h3("Input"),
            h4("設定"),
            h5("川崎市以外は市町村単位での表示になっています。また、川崎市のみ区単位での表示となっています。"),
            uiOutput("date"),
            #numericInput("y",label = h5("累積日数"),value="14"),
            radioButtons("y",label = "累積日数を設定してください",
                         c("1日"="1",
                           "7日"="7")
                         ),
            # sliderInput("color",
            #             label = "Setting of color",
            #             min = 10,
            #             max = 2000,
            #             value = 1600,)
                     
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("累積感染者数",
                                       leafletOutput("covid_map",height = "500px"),
                                       h5("横浜市の状況"),
                                       p("注意：以下の横浜市は一週間ごとの集計です。"),
                                       leafletOutput("yoko_map",height = "500px")),
                              tabPanel("10万人当たりの累積感染者数",
                                       leafletOutput("covid_map2",height = "500px"),
                                       p("注意：清川村、三浦市など人口が少ない市町村では10万人当たりの感染者数の色が濃くなることがあります。"),
                                       h5("横浜市の状況"),
                                       p("注意：以下の横浜市は一週間ごとの集計です。"),
                                       leafletOutput("yoko_map2",height = "500px")),
                              tabPanel("謝辞および参考文献",
                                       h5("この研究は、2021年度東海大学の助成金交付により研究が遂行されたものです。また、東海大学分子生命科学の今西規先生と指導教員である東海大学理学部数学科の山本義郎先生には有益な助言をいただきました。この場を借りて深く御礼申し上げます。"),
                                       p("このサイトは、神奈川県や川崎市、茅ヶ崎市の行政のサイトで公開されている新型コロナウイルス感染症の感染者の情報を使用しています。"),
                                       tags$a(href="https://www.pref.kanagawa.jp/docs/ga4/covid19/occurrence_list.html", "新型コロナウイルスに感染した患者の発生状況一覧(神奈川県)"),
                                       p(""),
                                       tags$a(href="https://www.city.kawasaki.jp/350/page/0000115886.html","【緊急情報】川崎市内の新型コロナウイルスに感染した患者等の発生状況"),
                                       p(""),
                                       tags$a(href="http://xn--city-kw4cy81x2nbwx7m.chigasaki.kanagawa.jp/koho/1030702/1038773/index.html","新型コロナウイルス感染症による管内の患者確認について(茅ヶ崎市)"),
                                       p(""),
                                       p("東海大学大学院理学研究科　棚橋真弓")
                              # textOutput("text"),
                              # textOutput("text2")
                              )
                              )
                  )
    )
    
    
)
)

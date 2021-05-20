if (!require(shiny)) {
    install.packages("shiny")
}
library(shiny)
if (!require(leaflet)) {
    install.packages("leaflet")
}
library(leaflet)
if(!require(shinyWidgets)){
    install.packages("shinyWidgets")
}
library(shinyWidgets)
shinyUI(fluidPage(
    # Application title
    titlePanel("神奈川県内の新型コロナ感染動向　市区町村別のコロプレスマップ"),

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
            uiOutput("update"),
            h4("市区町村別の感染者数"),
            h5("横浜市のみ、区単位の結果を右の図で表示しています。このときの結果は一週間ごとの集計となっています。"),
            uiOutput("date"),
            #numericInput("y",label = h5("累積日数"),value="14"),
            radioButtons("y",label = "累積日数を設定してください",
                         c("1日"="1",
                           "7日"="7")
                         ),
            switchInput("onoff",
                        label="駅の表示"),
            actionButton("action",
                         label="更新"),
            width=3
            
                     
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("累積感染者数",
                                       fluidRow(
                                       tags$style(type = "text/css", "#covid_map {height: calc(70vh - 15px) !important;}",
                                                  "#yoko_map {height: calc(70vh - 15px) !important;}"),
                                       column(8,
                                              h4(strong("神奈川県全体の状況")),
                                              leafletOutput("covid_map")),
                                       column(4,
                                              h4(strong("横浜市の状況")),
                                              leafletOutput("yoko_map")
                                              )
                                       )),
                              
                              tabPanel("10万人当たりの累積感染者数",
                                       fluidRow(
                                       tags$style(type = "text/css", "#covid_map2 {height: calc(70vh - 15px) !important;}",
                                                  "#yoko_map2 {height: calc(70vh - 15px) !important;}"),
                                       column(8,
                                              h4(strong("神奈川県全体の状況")),
                                              leafletOutput("covid_map2"),
                                              p("注意：清川村、三浦市など人口が少ない市町村では10万人当たりの感染者数の色が濃くなることがあります。"),),
                                       
                                       column(4,
                                              h4(strong("横浜市の状況")),
                                              leafletOutput("yoko_map2"),
                                              )
                                       )),
                              tabPanel("謝辞および参考文献",
                                       h4(strong("謝辞")),
                                       h5("この研究は、2021年度東海大学連合後援会助成金交付により研究が遂行されたものです。また日々、新型コロナウイルス感染症の対応を行っている医療関係者や保健所の皆様、データの提供元のおかげで本研究を進めることができました。また、東海大学分子生命科学の今西規先生と指導教員である東海大学理学部数学科の山本義郎先生には有益な助言をいただきました。この場を借りて深く御礼申し上げます。"),
                                       h4(strong("参考文献、その他")),
                                       p("このサイトは、神奈川県や川崎市、茅ヶ崎市の行政のサイトで公開されている新型コロナウイルス感染症の感染者の情報を使用しています。"),
                                       tags$a(href="https://www.pref.kanagawa.jp/docs/ga4/covid19/occurrence_list.html", "新型コロナウイルスに感染した患者の発生状況一覧(神奈川県)"),
                                       p(""),
                                       tags$a(href="https://www.city.kawasaki.jp/350/page/0000115886.html","【緊急情報】川崎市内の新型コロナウイルスに感染した患者等の発生状況"),
                                       p(""),
                                       tags$a(href="http://xn--city-kw4cy81x2nbwx7m.chigasaki.kanagawa.jp/koho/1030702/1038773/index.html","新型コロナウイルス感染症による管内の患者確認について(茅ヶ崎市)"),
                                       p(""),
                                       p("横浜市の区別データは以下のサイトのデータを参照しています。"),
                                       tags$a(href="https://square.umin.ac.jp/kenkono/","横浜市区別コロナデータ"),
                                       p("＊1 横浜市が前週の新規感染者数の訂正をしたため、鶴見区と泉区の4月18日から4月27日の新規感染者数がマイナスになっています。"),
                                       p("＊2 横浜市が7月18日から7月24日のデータと7月25日から7月31日のデータをまとめて公表したため、その2週間のデータを半分にしたものを1週間のデータとしています。"),
                                       p("＊3 9月5日から9月11日の区ごとの新規感染者数には、8月29日から9月4日に調査中だった39人が含まれます。（横浜市全体の新規感染者数には含まれません。）"),
                                       p("＊4 横浜市が12月4日に12月10日までのデータを公表したため、11月28日から12月10日は他の週よりも1日短くなっています。"),
                                       p("＊5 横浜市が12月26日から12月31日のデータと1月1日から1月7日のデータをまとめて公表したため、その2週間のデータを半分にしたものを1週間のデータとしています。市外とは、横浜市外に住み、横浜市内で検査して陽性となった人の合計です。"),
                                       
                                       p(""),
                                       p("本サイトでは、横浜市、横須賀市、相模原市、藤沢市は発表日を他の市町村は陽性判明日を元に感染者の集計を行っています。"),
                                       p("東海大学大学院理学研究科　棚橋真弓")),
                                tabPanel("ヘルプ",
                                         h4(strong("PC推奨環境")),#strong 文字を太くする
                                       h5("<OS>"),
                                       p("Windows10"),
                                       h5("<PC推奨ブラウザ>"),
                                       p("Microsoft Edge,Google Chrome"),
                                       p("当サイトでは、レイアウトの倍率を100％での利用を推奨しています。また、解像度は1024×768以上を推奨しています。"),
                                       h4(strong("使い方")),
                                       p("・2021-04-29の感染者数を見たい場合"),
                                       p("日付を2021-04-29に設定し、累積日数を1日に設定する。"),
                                       p("・2021-05-01から2021-05-07までの一週間(7日間)の累積感染者数を見たい場合"),
                                       p("日付を2021-05-07に設定し、累積日数を7日に設定する。"),
                                       p("・10万人当たりの感染者数や10万人当たりの一週間の累積感染者数を見たい場合"),
                                       p("累積感染者数の右隣のタブ(10万人当たりの累積感染者数)を左クリックしてください。"),
                                       p("・地図を拡大縮小したい場合"),
                                       p("地図の左上にある＋－ボタンを押すか、マウスでスクロールしてください。"),
                                       p("・地図の中心を動かしたい場合(特定の市区町村を中心に見たい場合)"),
                                       p("左クリックをしながら、マウスを動かすことで、地図を動かすことができます。"),
                                       p("・駅(路線図)を表示したい場合"),
                                       p("設定にある駅の表示を  クリックし、スイッチをオンしてください。"),
                                       )
                              ),
                  width=9
                  )
        )
    
    
)
)

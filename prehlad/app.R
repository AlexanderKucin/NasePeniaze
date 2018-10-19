library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(magrittr)
library(readr)

NAZOV<- "Naše Peniaze"
font<- "Arial"

TOPx<- 25

options(encoding = "UTF-8")

#data-----------
#dataset 1
dataset<- as.data.frame(read_delim("rezort.txt", delim=";", col_types = cols("i","i","D","c","n","n","n")))
min<- as.Date(min(dataset$ym, na.rm=T))
max<- as.Date(max(dataset$ym, na.rm=T))

#dataset2
dataset2<- as.data.frame(read_delim("ko.txt", delim=";", col_types = cols("D","c","c","n","n","n","c")))
min2<- as.Date(min(dataset2$ym, na.rm=T))
max2<- as.Date(max(dataset2$ym, na.rm=T))

#dataset3
dataset3<- as.data.frame(read_delim("kom.txt", delim=";", col_types = cols("i","c","c","c","n","n","n","n","c")))
min3<- min(dataset3$year, na.rm=T)
max3<- max(dataset3$year, na.rm=T)

#dataset4
dataset4<- as.data.frame(read_delim("do.txt", delim=";", col_types = cols("i","c","c","c","n","n","n","c")),quote='"')
min4<- min(dataset4$year, na.rm=T)
max4<- max(dataset4$year, na.rm=T)

#dataset5
dataset5<- as.data.frame(read_delim("top.txt", delim=";", col_types = cols("i","c","n","n","n","c","c","c","c","c","c")))
min5<- min(dataset5$year, na.rm=T)
max5<- max(dataset5$year, na.rm=T)

#dataset6
dataset6<- as.data.frame(read_delim("typ.txt", delim=";", col_types = cols("i","c","c","n","n","n")))
min6<- min(dataset6$year, na.rm=T)
max6<- max(dataset6$year, na.rm=T)

#dashboard data
daily<- as.data.frame(read_delim("daily.txt", delim=";", col_types = cols("D","n","i","n")))
daily$pSum<- paste(prettyNum(daily$suma,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
daily$pPoc<- paste(prettyNum(daily$pocet,digits=0,big.mark = " ", scientific=F)," zmlúv",sep=" ")
daily$pAvg<- paste(prettyNum(daily$avg,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
title1<- format(daily$datum_zverejnene[2], format="%d.%m")
sub1<- paste0("Zmena oproti ",format(daily$datum_zverejnene[1], format="%d.%m"),": ",round((daily$suma[2]/daily$suma[1])-1,2),"%")
sub2<- paste0("Zmena oproti ",format(daily$datum_zverejnene[1], format="%d.%m"),": ",round((daily$pocet[2]/daily$pocet[1])-1,2),"%")
sub3<- paste0("Zmena oproti ",format(daily$datum_zverejnene[1], format="%d.%m"),": ",round((daily$avg[2]/daily$avg[1])-1,2),"%")

monthly<- as.data.frame(read_delim("monthly.txt", delim=";", col_types = cols("D","n","i","n")))
monthly$pSum<- paste(prettyNum(monthly$suma,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
monthly$pPoc<- paste(prettyNum(monthly$pocet,digits=0,big.mark = " ", scientific=F)," zmlúv",sep=" ")
monthly$pAvg<- paste(prettyNum(monthly$avg,digits=2,big.mark = " ", scientific=F),"€",sep=" ")

title2<- format(monthly$ym[2],"%m/%Y")
sub4<- paste0("Zmena oproti ",format(monthly$ym[1],"%m/%Y"),": ",round((monthly$suma[2]/monthly$suma[1])-1,2),"%")
sub5<- paste0("Zmena oproti ",format(monthly$ym[1],"%m/%Y"),": ",round((monthly$pocet[2]/monthly$pocet[1])-1,2),"%")
sub6<- paste0("Zmena oproti ",format(monthly$ym[1],"%m/%Y"),": ",round((monthly$avg[2]/monthly$avg[1])-1,2),"%")

top10d<- as.data.frame(read_delim("topD.txt", delim=";", col_types = cols("c","c","c","c","D","D","n","c")))
top10d$pSum<- paste(prettyNum(top10d$suma_spolu,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
top10d$predmet <- paste0("<a href='",top10d$link,"' target='_blank' onclick='yaCounter42956899.reachGoal(\"TOP_CONTRACT_DAILY_CLICK\"); return true;'>",top10d$predmet,"</a>")
top10d<- top10d[,-c(7,8)]
names(top10d)<- c("Názov","Objednávateľ","Dodávateľ","Rezort","Dátum účinnosti","Dátum platnosti do", "Celková suma")

top50m<- as.data.frame(read_delim("topM.txt", delim=";", col_types = cols("c","c","c","c","D","D","n","c")))
top50m$pSum<- paste(prettyNum(top50m$suma_spolu,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
top50m$predmet <- paste0("<a href='",top50m$link,"' target='_blank' onclick='yaCounter42956899.reachGoal(\"TOP_CONTRACT_MONTHLY_CLICK\"); return true;'>",top50m$predmet,"</a>")
top50m<- top50m[,-c(7,8)]
names(top50m)<- c("Názov","Objednávateľ","Dodávateľ","Rezort","Dátum účinnosti","Dátum platnosti do", "Celková suma")

#UI START----------------
ui <- dashboardPage(title=NAZOV,
                    dashboardHeader(title=NAZOV),
                    #SIDEBAR------------
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Prehľad za deň a mesiac", tabName = "dashboard", icon = icon("dashboard"), selected = T) %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('MENU_DASHBOARD_CLICK'); return true;"),
                        menuItem(paste0("TOP ",TOPx), tabName = "top", icon = icon("bar-chart"), selected = F) %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('MENU_TOP_CLICK'); return true;"), 
                        menuItem("Rezorty", tabName = "rezorty", icon = icon("line-chart"), selected = F) %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('MENU_REZORTY_CLICK'); return true;"),
                        menuItem("Kraje a okresy", tabName = "kraj", icon = icon("bar-chart"), selected = F) %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('MENU_KRAJ_CLICK'); return true;"),
                        menuItem("Mestá", tabName = "mesto", icon = icon("bar-chart"), selected = F) %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('MENU_MESTO_CLICK'); return true;"),
                        menuItem("Objednávatelia a Dodávatelia", tabName = "zs", icon = icon("line-chart"), selected = F) %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('MENU_ZS_CLICK'); return true;"),
                        menuItem("Typy zmlúv", tabName = "typ",icon = icon("bar-chart"), selected = F) %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('MENU_TYPE_CLICK'); return true;"),
                        menuItem("O projekte", tabName = "about", icon = icon("home"), selected = F) %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('MENU_ABOUT_CLICK'); return true;"),
                        menuItem("Ako pracovať s grafmi", tabName = "faq", icon = icon("line-chart"), selected = F) %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('MENU_FAQ_CLICK'); return true;") #,
                      )
                    ),
                    
                    dashboardBody(
                      
                      #metrika counter-----------
                      tags$head(
                        tags$style(HTML(paste0('
                                        h2 {
                                        font-family: ',font,'
                                        }
                                        * {
                                        font-family: ',font,'
                                        }
                                        '))),
                        tags$link(rel = "shortcut icon", href = "favicon.ico"),
                        HTML(
                          '<!-- Yandex.Metrika counter -->
                          <script type="text/javascript">
                          (function (d, w, c) {
                          (w[c] = w[c] || []).push(function() {
                          try {
                          w.yaCounter42956899 = new Ya.Metrika({
                          id:42956899,
                          clickmap:true,
                          trackLinks:true,
                          accurateTrackBounce:true,
                          webvisor:true,
                          trackHash:true
                          });
                          } catch(e) { }
                          });
                          
                          var n = d.getElementsByTagName("script")[0],
                          s = d.createElement("script"),
                          f = function () { n.parentNode.insertBefore(s, n); };
                          s.type = "text/javascript";
                          s.async = true;
                          s.src = "https://mc.yandex.ru/metrika/watch.js";
                          
                          if (w.opera == "[object Opera]") {
                          d.addEventListener("DOMContentLoaded", f, false);
                          } else { f(); }
                          })(document, window, "yandex_metrika_callbacks");
                          </script>
                          <noscript><div><img src="https://mc.yandex.ru/watch/42956899" style="position:absolute; left:-9999px;" alt="" /></div></noscript>
                          <!-- /Yandex.Metrika counter -->'
                        )
                        ),
                      #metrika END---------
                      HTML("
                           <script type='text/javascript'>
                           window.onload = function() {
                           yaCounter42956899.reachGoal('LOADED')
                           }
                           </script>
                           "),
                      
                      tabItems(
                        #ABOUT----------
                        tabItem(tabName ="about",
                                    titlePanel("O projekte"),
                                      column(6,
                                      withTags({
                                        div(class="header", 
                                            p("Cieľom tohto projektu je dať širokej verejnosti možnosť v jednoduchej 
                                              a prehľadnej forme kontrolovať použitie verejných financií, alebo
                                              inými slovami, našich peňazí."),
                                            
                                            p("Mojim cieľom nebolo zameniť, ale doplniť Centrálny register zmlúv o akúsi 
                                              nadstavbu, ktorá umožní pozrieť sa na využívané prostriedky z nadhľadu, na úrovni
                                              krajov, okresov, miest, rezortov a v neposlednom rade aj konkrétnych zmluvných strán.
                                              Každý deň je možné pozrieť sa na TOP 25 najdrahších zmlúv, ktoré boli zverejnené v prienehu predchádzajúceho 
                                              dňa alebo mesiaca. V prípade záujmu je možné prejsť priamo na stránku zmluvy v Centrálnom 
                                              registri zmlúv."),
                                            
                                            p("Som presvedčený, že využívanie verejných zdrojov má byť transparentné a kontrolovateľné. 
                                              Dúfam, že tento projekt uľahčí všetkým túto úlohu."),
                                            p("Všetky dáta použité v tomto projekte sú verejne dostupné v", 
                                              a(target="_blank", href="http://www.crz.gov.sk", "Centrálnom registri zmlúv.")),
                                            p("Vaše komentáre, pripomienky, chyby, bugy a čokoľvek iné posielajte na adresu peniazenase@gmail.com."),
                                            br(),
                                            h4("Technické podrobnosti"),
                                            p("Projekt je napísaný v jazyku R, s použitím frameworku Shiny. Grafy sú generované 
                                              pomocou knižnice Plotly. Ak budete mať záujem o detaily, budem rád, ak napíšete na peniazenase@gmail.com."),
                                            br(),
                                           p("Alexander Kučin, autor projektu Naše Peniaze")
                                        )
                                      })
                                      )
                                  
                        ),
                        #FAQ----------
                        tabItem(tabName ="faq",
                                titlePanel("Ako pracovať s grafmi"),
                                fluidRow(column(8,
                                       withTags({
                                         div(class="header", 
                                             h4(b("1. Ako stiahnem graf?")),
                                             p("Kliknite na 'Download as PNG' v pravej časti grafu"),
                                             h4(""),
                                             img(src="graf_png.png"),
                                             h4(""),
                                             h4(b("2. Ako môžem uvidieť všetky dáta grafu?")),
                                             p("Kliknite na 'Compare data on Hover' v pravej časti grafu"),
                                             h4(""),
                                             img(src="compare_hover.png"),
                                             h4(""),
                                             img(src="compare_show.png"),
                                             h4(""),
                                             h4(b("3. Vyhľadávanie v menu s výberom")),
                                             p("Jednoducho začnite písať v menu s výberom"),
                                             img(src="search_selector.png"),
                                             h4(""),
                                             h4(b("4. Ako odstrániť položku menu?")),
                                             p("Kliknite na položku a vymažte kliknutím na 'Backspace'"),
                                             img(src="select_chosen.png"),
                                             h4(""),
                                             h4(b("5. Ako priblížiť a oddialiť graf?")),
                                             p("Ľavým tlačidlom myší vyznačte oblasť, ktorú chcete priblížiť"),
                                             img(src="zoom_in.png"),
                                             h4(""),
                                             p("Ak sa chcete vrátiť k pôvodnej veľkosti grafu kliknite na 'Autoscale' v pravej časti grafu"),
                                             img(src="zoom_out.png")
                                         )
                                       })
                                       )
                                )
                                ),
                        #DASHBOARD----------
                        tabItem(tabName ="dashboard",
                                titlePanel("Prehľad za deň a mesiac"),
                                
                                fluidRow(
                                  
                                  column(4,offset=1,
                                         tags$div(h4(strong(paste0("Deň: ",title1))), align="center"),
                                          if(round((daily$suma[2]/daily$suma[1])-1,0)>=0)
                                          {
                                            infoBox(value=daily$pSum[2],title="Celková Suma",subtitle=sub1,color='green',icon=icon("arrow-up"),width=NULL)
                                          }else{
                                            infoBox(value=daily$pSum[2],title="Celková Suma",subtitle=sub1,color='red',icon=icon("arrow-down"),width=NULL)
                                          }
                                        
                                  ),
                                  
                                  column(4,offset=2,
                                         tags$div(h4(strong(paste0("Mesiac: ",title2))), align="center"),
                                         if(round((monthly$suma[2]/monthly$suma[1])-1,0)>=0)
                                         {
                                           infoBox(value=monthly$pSum[2],title="Celková Suma",subtitle=sub4,color='teal',icon=icon("arrow-up"),width=NULL)
                                         }else{
                                           infoBox(value=monthly$pSum[2],title="Celková Suma",subtitle=sub4,color='maroon',icon=icon("arrow-down"),width=NULL)
                                         }
                                  )  
                                  
                                ),
                                fluidRow(
                                  column(4,offset=1,
                                         
                                         if(round((daily$pocet[2]/daily$pocet[1])-1,0)>=0)
                                         {
                                           infoBox(value=daily$pPoc[2],title="Počet",subtitle=sub2,color='green',icon=icon("arrow-up"),width=NULL)
                                         }else{
                                           infoBox(value=daily$pPoc[2],title="Počet",subtitle=sub2,color='red',icon=icon("arrow-down"),width=NULL)
                                         }
                                  ),
                                  column(4,offset=2,
                                         
                                         if(round((monthly$pocet[2]/monthly$pocet[1])-1,0)>=0)
                                         {
                                           infoBox(value=monthly$pPoc[2],title="Počet",subtitle=sub5,color='teal',icon=icon("arrow-up"),width=NULL)
                                         }else{
                                           infoBox(value=monthly$pPoc[2],title="Počet",subtitle=sub5,color='maroon',icon=icon("arrow-down"),width=NULL)
                                         }
                                  )  
                                  
                                ),
                                fluidRow(
                                  column(4,offset=1,
                                         
                                         if(round((daily$avg[2]/daily$avg[1])-1,0)>=0)
                                         {
                                           infoBox(value=daily$pAvg[2],title="Priemerná zmluvná čiastka",subtitle=sub3,color='green',icon=icon("arrow-up"),width=NULL)
                                         }else{
                                           infoBox(value=daily$pAvg[2],title="Priemerná zmluvná čiastka",subtitle=sub3,color='red',icon=icon("arrow-down"),width=NULL)
                                         }
                                  ),
                                  column(4,offset=2,
                                         
                                         if(round((monthly$avg[2]/monthly$avg[1])-1,0)>=0)
                                         {
                                           infoBox(value=monthly$pAvg[2],title="Priemerná zmluvná čiastka",subtitle=sub6,color='teal',icon=icon("arrow-up"),width=NULL)
                                         }else{
                                           infoBox(value=monthly$pAvg[2],title="Priemerná zmluvná čiastka",subtitle=sub6,color='maroon',icon=icon("arrow-down"),width=NULL)
                                         }
                                  )  
                                  
                                ),
                                fluidRow(
                                  tags$div(h3(strong("100 najdrahších zmlúv")), align="center"),
                                  column(4,
                                    selectizeInput('topTable', 'Vyberte obdobie', choices=c("Deň","Mesiac"), multiple=F)
                                  )
                                ),
                                
                                fluidRow(
                                  column(12,
                                    dataTableOutput("table")
                                  )
                                )
                                
                        ),
                        
                        #First App UI - REZORTY ------
                        tabItem(tabName ="rezorty",
                              titlePanel("Zmluvné sumy podľa rezortu"),
                              wellPanel(  
                                fluidRow(
                                  
                                  column(6,
                                         selectizeInput('rezort', 'Rezorty',NULL, multiple=T)
                                         ,downloadButton('download', 'Stiahnuť dáta grafu') %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('REZORTY_DOWNLOAD'); return true;")
                                  ),
                                  
                                  column(2,
                                         radioButtons('metrika1','Zobrazenie na grafe',list('Suma','Počet zmlúv','Priemerná zmluvná čiastka'))
                                         ),
                                  
                                  column(4,
                                         dateRangeInput('mesiac', 'Rok a Mesiac', format="yyyy-mm",
                                                        start=min, end=max,min=min, max=max, startview = "month")
                                         
                                  )
                                  
                    
                                )
                              ),
                                fluidRow(    
                                  #hr(),
                                  column(12,
                                    plotlyOutput("plot")
                                  )
                                )
                                
                        ),
                        #Second App UI - KRAJE-----
                        tabItem(tabName ="kraj",
                              titlePanel("Zmluvné sumy podľa krajov a okresov"),
                              wellPanel(  
                                fluidRow(   
                                  column(4,
                                         selectizeInput('kraj2', 'Kraj', NULL, multiple=T)
                                         ,downloadButton('download2', 'Stiahnuť dáta grafu') %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('KRAJ_DOWNLOAD'); return true;")
                                  ),
                                  column(2,
                                         radioButtons('metrika2','Zobrazenie na grafe',list('Suma','Počet zmlúv','Priemerná zmluvná čiastka'))
                                  ),
                                  column(2,
                                         radioButtons('kto2','Zmluvná strana',list('Objednávateľ','Dodávateľ'))
                                  ),
                                  column(4,
                                         dateRangeInput('mesiac2', 'Rok a Mesiac', format="yyyy-mm",
                                                        start=min2, end=max2,min=min2, max=max2, startview = "month")
                                         )
                                  )
                                ),
                                fluidRow(
                                  column(12,
                                    plotlyOutput("plot2")
                                  )
                                )
                        ),
                        #Third App UI - MESTA -----
                        tabItem(tabName ="mesto",
                              titlePanel("Porovnanie miest"),
                              wellPanel(  
                                fluidRow(
                                 
                                  column(4,
                                         selectizeInput('mesto3', 'Mestá alebo obce', NULL, multiple=T)
                                         ,downloadButton('download3', 'Stiahnuť dáta grafu') %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('MESTO_DOWNLOAD'); return true;")
                                  ),
                                  column(2,
                                         radioButtons('metrika3','Zobrazenie na grafe',list('Suma','Počet zmlúv','Priemerná zmluvná čiastka','Suma v prepočte na obyvateľa'))
                                  ),
                                  column(2,
                                         radioButtons('kto3','Zmluvná strana',list('Objednávateľ','Dodávateľ',"Obe strany"))
                                  ),
                                  column(4,
                                         
                                         sliderInput('rok3', 'Rok', timeFormat="%Y",
                                                     min=min3, max=max3,value=c(min3,max3),step=1, sep="")
                                         
                                  )
                                )
                              ),
                                fluidRow(
                                  column(12,
                                    plotlyOutput("plot3")
                                  )
                                )
                                
                        ),
                        #Fourth App UI - Zmluvne strany-----
                        tabItem(tabName ="zs",
                              titlePanel("Porovnanie zmluvných strán"),
                              wellPanel(
                                fluidRow(
                                  
                                  column(4,
                                         selectizeInput('zs4', 'Zmluvná strana', NULL, multiple=T)
                                         ,downloadButton('download4', 'Stiahnuť dáta grafu') %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('ZS_DOWNLOAD'); return true;")
                                  ),
                                  column(2,
                                         radioButtons('metrika4','Zobrazenie na grafe',list('Suma','Počet zmlúv','Priemerná zmluvná čiastka'))
                                  ),
                                  column(2,
                                         radioButtons('kto4','Zmluvná strana',list('Objednávateľ','Dodávateľ'))
                                  ),
                                  column(4,
                                         
                                         sliderInput('rok4', 'Rok', timeFormat="%Y",
                                                     min=min4, max=max4,value=c(min4,max4),step=1, sep="")
                                  )
                                )
                              ),
                                fluidRow(
                                  column(12,
                                    plotlyOutput("plot4")
                                  )
                                )
                                
                        ),
                        #Fifth App UI - TOPx -----
                        tabItem(tabName ="top",
                              titlePanel(paste0("TOP ",TOPx)),
                              wellPanel(
                                fluidRow(
                                  column(3,
                                         radioButtons('typ5','Výber rankingu',list('Rezorty','Kraj','Okresy','Mestá','Zmluvná strana'))
                                         ,downloadButton('download5', 'Stiahnuť dáta grafu') %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('TOP_DOWNLOAD'); return true;")
                                  ), 
                                  column(3,
                                         radioButtons('metrika5','Zobrazenie na grafe',list('Suma','Počet zmlúv'))
                                  ),
                                  column(3,
                                         radioButtons('kto5','Zmluvná strana',list('Objednávateľ','Dodávateľ',"Obe strany"))
                                  ),
                                  column(3,
                                         
                                         sliderInput('rok5', 'Rok', timeFormat="%Y",
                                                     min=min5, max=max5,value=c(min5,max5),step=1, sep="")
                                  ))
                                ),
                                fluidRow(
                                  column(12,
                                    plotlyOutput("plot5", height="500")
                                  )
                                )
                                
                        ),
                        #Sixth App UI - TYP ZMLUV -----
                        tabItem(tabName ="typ",
                              titlePanel("Porovnanie zmlúv podľa typu"),
                              wellPanel(  
                                fluidRow(
                                  
                                  column(4,
                                         selectizeInput('rezort6', 'Rezorty', NULL, multiple=T)
                                         ,downloadButton('download6', 'Stiahnuť dáta grafu') %>% tagAppendAttributes(onclick="yaCounter42956899.reachGoal('TYPE_DOWNLOAD'); return true;")
                                  ),
                                  column(2,
                                         selectizeInput('typ_z6', 'Typ zmluvy', 'darovacia zmluva', multiple=T)
                                  ),
                                  column(2,
                                         radioButtons('metrika6','Zobrazenie na grafe',list('Suma','Počet zmlúv','Priemerná zmluvná čiastka'))
                                  ),
                                  column(4,
                                         
                                         sliderInput('rok6', 'Rok', timeFormat="%Y",
                                                     min=min6, max=max6,value=c(min6,max6),step=1, sep="")
                                  )
                                  )
                                ),
                                fluidRow(
                                  column(12,
                                    plotlyOutput("plot6")
                                  )
                                )
                                
                        )
                                )
                        )
                      )

server <- function(input, output,session) {
  
  #First App Output - REZORT--------------
  updateSelectizeInput(session, 'rezort', choices = dataset$NazovRezortu, selected=c("Úrad vlády SR","Ministerstvo financií SR"), server=T)  
  
  dload<- reactive(
    {
      #subsetting dataset using input variables
      dataset<- subset(dataset, ym>=input$mesiac[1] & ym<=input$mesiac[2])
      dataset<- subset(dataset, NazovRezortu %in% input$rezort)
      
      
        dataset %<>% group_by(ym, year, month, NazovRezortu) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        #hoverinfo
        dataset$pSum<- paste(prettyNum(dataset$suma_spolu,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
        dataset$pPoc<- prettyNum(dataset$pocet,digits=2,big.mark = " ", scientific=F)
        dataset$pAvg<- paste(prettyNum(dataset$avg,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
        
      dataset
    })
  
  output$plot <- renderPlotly({
    
    dataset<- dload()
    
    #axis
    lx<- list(dtick=1, 
              tickmode="array", 
              tickvals=dataset$year,
              title="")
    
    ly<- list(
      title=""
    )
    
    #plotting
    if(input$metrika1=="Suma")
    {
      plot_ly(dataset, x=~ym, y=~suma_spolu, color=~NazovRezortu, type="scatter",mode = "markers+lines",
              hoverinfo="x+text",text = ~paste0("Suma: ",dataset$pSum,"<br>","Rezort: ", NazovRezortu)) %>% layout(xaxis=lx, yaxis=ly)
    }else if(input$metrika1=="Počet zmlúv"){
      plot_ly(dataset, x=~ym, y=~pocet, color=~NazovRezortu, type="scatter",mode = "markers+lines",
              hoverinfo="x+text",text = ~paste0("Počet zmlúv: ",dataset$pPoc,"<br>","Rezort: ", NazovRezortu)) %>% layout(xaxis=lx, yaxis=ly)
    }else if(input$metrika1=="Priemerná zmluvná čiastka"){
      plot_ly(dataset, x=~ym, y=~avg, color=~NazovRezortu, type="scatter",mode = "markers+lines",
              hoverinfo="x+text",text = ~paste0("Priemerná zmluvná čiastka: ",dataset$pAvg,"<br>","Rezort: ", NazovRezortu)) %>% layout(xaxis=lx, yaxis=ly)
    }

  })
  
  output$download<- downloadHandler(
    filename = "zmluvy.txt",
    content = function(file) {
      write.table(dload(), file,sep=";", quote=F, row.names=F, dec=",")
    }
  )
  
  #Second App Output - KRAJE --------------
  updateSelectizeInput(session, 'kraj2', choices = dataset2$kraj, selected=c("Nitriansky kraj","Prešovský kraj"), server=T)  
  
  dload2<- reactive(
    {
      if(input$kto2=='Objednávateľ')
      {
        dataset2<- subset(dataset2, strana=="objednavatel")
      }else if(input$kto2=='Dodávateľ')
      {
        dataset2<- subset(dataset2, strana=="dodavatel")
      }
      #subsetting dataset2 using input variables
      dataset2<- subset(dataset2, ym>=input$mesiac2[1] & ym<=input$mesiac2[2])
      dataset2<- subset(dataset2, kraj %in% input$kraj2)
      
      dataset2 %<>% group_by(kraj, okres) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T), pocet=sum(pocet, na.rm=T), avg=mean(priemerna_suma, na.rm=T)) %>% ungroup()
    }) 
  
  output$plot2 <- renderPlotly({
    
    dataset2<- dload2()
    
    #axis
    lx<- list(dtick=1, 
              tickmode="array", 
              tickvals=dataset2$kraj,
              title="")
    
    ly<- list(
      title=""
    )
    
    if(input$metrika2=="Suma")
    {
      plot_ly(dataset2, x=~kraj, y=~suma_spolu,  color=~okres) %>% add_bars() %>% 
        layout(xaxis=lx, yaxis=ly, barmode="stack", hovermode="closest")
    }else if(input$metrika2=="Počet zmlúv"){
      plot_ly(dataset2, x=~kraj, y=~pocet,  color=~okres) %>% add_bars() %>% 
        layout(xaxis=lx, yaxis=ly, barmode="stack", hovermode="closest")
    }else if(input$metrika2=="Priemerná zmluvná čiastka"){
      plot_ly(dataset2, x=~kraj, y=~avg,  color=~okres) %>% add_bars() %>% 
        layout(xaxis=lx, yaxis=ly, hovermode="closest")
    }
      
  })
  
  output$download2<- downloadHandler(
    filename = "zmluvy.txt",
    content = function(file) {
      write.table(dload2(), file,sep=";", quote=F, row.names=F, dec=",")
    })
  
  #Third App Output - MESTA --------------
  updateSelectizeInput(session, 'mesto3', choices = dataset3$mesto, selected=c("Prešov","Nitra"), server=T)  
  
  dload3<- reactive(
    {
      if(input$kto3=='Objednávateľ')
      {
        dataset3<- subset(dataset3, strana=="objednavatel")
      }else if(input$kto3=='Dodávateľ')
      {
        dataset3<- subset(dataset3, strana=="dodavatel")
      }
      
      #subsetting dataset3 using input variables
      dataset3<- subset(dataset3, year>=input$rok3[1] & year<=input$rok3[2])
      dataset3<- subset(dataset3, mesto %in% input$mesto3)
      
      dataset3 %<>% group_by(kraj, okres, mesto, strana) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),suma_na_obyvatela=sum(suma_na_obyvatela,na.rm=T), pocet=sum(pocet, na.rm=T),avg=mean(priemerna_suma, na.rm=T)) %>% ungroup()
    })
  
  output$plot3 <- renderPlotly({
    
    dataset3<- dload3()
    
    dataset3$strana<- ifelse(dataset3$strana == "dodavatel","Dodávateľ",
                             ifelse(dataset3$strana == "objednavatel","Objednávateľ",""))
    
    #hoverinfo
    dataset3$pSum<- paste(prettyNum(dataset3$suma_spolu,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
    dataset3$pSumPC<- paste(prettyNum(dataset3$suma_na_obyvatela,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
    dataset3$pPoc<- prettyNum(dataset3$pocet,digits=2,big.mark = " ", scientific=F)
    dataset3$pAvg<- paste(prettyNum(dataset3$avg,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
    
    
    if(input$kto3 == "Obe strany")
    {
      #plotting
      if(input$metrika3=="Suma")
      {
        plot_ly(dataset3, y=~paste(strana, "-",mesto), x=~suma_spolu,  color=~kraj, hoverinfo="text", text=~c(paste0("Suma: ",dataset3$pSum,"<br>","Okres: ",dataset3$okres, sep=" "))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""), hovermode="closest", showlegend=T, margin=list(l=200))
      }else if(input$metrika3=="Počet zmlúv"){
        plot_ly(dataset3, y=~paste(strana, "-",mesto), x=~pocet,  color=~kraj, hoverinfo="text", text=~c(paste0("Počet zmlúv: ",dataset3$pPoc,"<br>","Okres: ",dataset3$okres, sep=" "))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""), hovermode="closest", showlegend=T, margin=list(l=200))
      }else if(input$metrika3=="Priemerná zmluvná čiastka"){
        plot_ly(dataset3, y=~paste(strana, "-",mesto), x=~avg,  color=~kraj, hoverinfo="text", text=~c(paste0("Priemerná zmluvná čiastka: ",dataset3$pAvg,"<br>","Okres: ",dataset3$okres, sep=" "))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""), hovermode="closest", showlegend=T, margin=list(l=200))
      }else if(input$metrika3=="Suma v prepočte na obyvateľa"){
        plot_ly(dataset3, y=~paste(strana, "-",mesto), x=~suma_na_obyvatela,  color=~kraj, hoverinfo="text", text=~c(paste0("Suma v prepočte na obyvateľa: ",dataset3$pSumPC,"<br>","Okres: ",dataset3$okres, sep=" "))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""), hovermode="closest", showlegend=T, margin=list(l=200))
      }
    }else{
      
      if(input$metrika3=="Suma")
      {
        plot_ly(dataset3, y=~mesto, x=~suma_spolu,  color=~kraj, hoverinfo="text", text=~list(paste0("Suma: ",dataset3$pSum,"<br>","Okres: ",dataset3$okres, sep=" "))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""), hovermode="closest", showlegend=T, margin=list(l=200))
      }else if(input$metrika3=="Počet zmlúv"){
        plot_ly(dataset3, y=~mesto, x=~pocet,  color=~kraj, hoverinfo="text", text=~list(paste0("Počet zmlúv: ",dataset3$pPoc,"<br>","Okres: ",dataset3$okres, sep=" "))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""), hovermode="closest", showlegend=T, margin=list(l=200))
      }else if(input$metrika3=="Priemerná zmluvná čiastka"){
        plot_ly(dataset3, y=~mesto, x=~avg,  color=~kraj, hoverinfo="text", text=~list(paste0("Priemerná zmluvná čiastka: ",dataset3$pAvg,"<br>","Okres: ",dataset3$okres, sep=" "))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""), hovermode="closest", showlegend=T, margin=list(l=200))
      }else if(input$metrika3=="Suma v prepočte na obyvateľa"){
        plot_ly(dataset3, y=~mesto, x=~suma_na_obyvatela,  color=~kraj, hoverinfo="text", text=~list(paste0("Suma v prepočte na obyvateľa: ",dataset3$pSumPC,"<br>","Okres: ",dataset3$okres, sep=" "))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""), hovermode="closest", showlegend=T, margin=list(l=200))
      }
    }  
  })
  
  output$download3<- downloadHandler(
    filename = "zmluvy.txt",
    content = function(file) {
      write.table(dload3(), file,sep=";", quote=F, row.names=F, dec=",")
    })
  
  #Fourth App Output - ZMLUVNE STRANY --------------
  updateSelectizeInput(session, 'zs4', choices = dataset4$zs,selected=c("Úrad vlády Slovenskej republiky","Ministerstvo financií SR"), server=T) 
  
  dload4<- reactive(
    {
      if(input$kto4=='Objednávateľ')
      {
        dataset4<- subset(dataset4, strana=="objednavatel")
      }else if(input$kto4=='Dodávateľ')
      {
        dataset4<- subset(dataset4, strana=="dodavatel")
      }
      
      #subsetting dataset4 using input variables
      dataset4<- subset(dataset4, year>=input$rok4[1] & year<=input$rok4[2])
      dataset4<- subset(dataset4, zs %in% input$zs4)
      
      dataset4 %<>% group_by(year, prav_os, zs) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(priemerna_suma, na.rm=T)) %>% ungroup()  
      #hoverinfo
      dataset4$pSum<- paste(prettyNum(dataset4$suma_spolu,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
      dataset4$pPoc<- prettyNum(dataset4$pocet,digits=2,big.mark = " ", scientific=F)
      dataset4$pAvg<- paste(prettyNum(dataset4$avg,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
      
      dataset4
    })
  
  output$plot4 <- renderPlotly({
    
    dataset4<- dload4()
    
    #axis
    lx4<- list(dtick=1, 
              tickmode="array", 
              tickvals=dataset4$year,
              title="")
    
    ly4<- list(
      title=""
    )
    
    #plotting
    if(input$metrika4=="Suma")
    {
      plot_ly(dataset4, x=~year, y=~suma_spolu, color=~zs, type="scatter",mode = "markers+lines",
              hoverinfo="text",text=~c(paste0("Rok: ",dataset4$year,"<br>","Suma: ",dataset4$pSum,"<br>","Zmluvná strana: ",dataset4$zs,"<br>"))) %>% layout(xaxis=lx4, yaxis=ly4)
    }else if(input$metrika4=="Počet zmlúv"){
      plot_ly(dataset4, x=~year, y=~pocet, color=~zs, type="scatter",mode = "markers+lines",
              hoverinfo="text",text=~c(paste0("Rok: ",dataset4$year,"<br>","Počet zmlúv: ",dataset4$pPoc,"<br>","Zmluvná strana: ",dataset4$zs,"<br>"))) %>% layout(xaxis=lx4, yaxis=ly4)
    }else if(input$metrika4=="Priemerná zmluvná čiastka"){
      plot_ly(dataset4, x=~year, y=~avg, color=~zs, type="scatter",mode = "markers+lines",
              hoverinfo="text",text=~c(paste0("Rok: ",dataset4$year,"<br>","Priemerná zmluvná čiastka: ",dataset4$pAvg,"<br>","Zmluvná strana: ",dataset4$zs,"<br>"))) %>% layout(xaxis=lx4, yaxis=ly4)
    }
  })
  
  output$download4<- downloadHandler(
    filename = "zmluvy.txt",
    content = function(file) {
      write.table(dload4(), file,sep=";", quote=F, row.names=F, dec=",")
    })
  
  #Fifth App Output - TOPx --------------
  dload5<- reactive(
    {
      if(input$kto5=='Objednávateľ')
      {
        if(input$typ5!="Rezorty")
        {
          dataset5<- subset(dataset5, strana=="objednavatel")
        }
        
      }else if(input$kto5=='Dodávateľ')
      {
        if(input$typ5!="Rezorty")
        {
          dataset5<- subset(dataset5, strana=="dodavatel")
        }
      }
      
      #subsetting dataset5 using input variables
      dataset5<- subset(dataset5, year>=input$rok5[1] & year<=input$rok5[2])
      dataset5<- subset(dataset5, typ==input$typ5)
      
      if(input$typ5=='Rezorty')
      {
        dataset5 %<>% group_by(year, NazovRezortu) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        fctrs<- dataset5 %>% group_by(NazovRezortu) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        
        if(input$metrika5=="Suma")
        {
          fctrs$NazovRezortu <- factor(fctrs$NazovRezortu, levels = unique(fctrs$NazovRezortu)[order(fctrs$suma_spolu, decreasing = FALSE)])
        }else if(input$metrika5=="Počet zmlúv"){
          fctrs$NazovRezortu <- factor(fctrs$NazovRezortu, levels = unique(fctrs$NazovRezortu)[order(fctrs$pocet, decreasing = FALSE)])
        }else if(input$metrika5=="Priemerná zmluvná čiastka"){
          fctrs$NazovRezortu <- factor(fctrs$NazovRezortu, levels = unique(fctrs$NazovRezortu)[order(fctrs$avg, decreasing = FALSE)])
        }
        
        levels<- levels(fctrs$NazovRezortu)
        dataset5$NazovRezortu <- factor(dataset5$NazovRezortu, levels = levels)
        rm(fctrs,levels)
        
        dataset5<- as.data.frame(dataset5)
        
      }else if(input$typ5=='Kraj')
      {
        dataset5 %<>% group_by(year, kraj) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        fctrs<- dataset5 %>% group_by(kraj) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        
        if(input$metrika5=="Suma")
        {
          fctrs$kraj <- factor(fctrs$kraj, levels = unique(fctrs$kraj)[order(fctrs$suma_spolu, decreasing = FALSE)])
        }else if(input$metrika5=="Počet zmlúv"){
          fctrs$kraj <- factor(fctrs$kraj, levels = unique(fctrs$kraj)[order(fctrs$pocet, decreasing = FALSE)])
        }else if(input$metrika5=="Priemerná zmluvná čiastka"){
          fctrs$kraj <- factor(fctrs$kraj, levels = unique(fctrs$kraj)[order(fctrs$avg, decreasing = FALSE)])
        }
        levels<- levels(fctrs$kraj)
        dataset5$kraj <- factor(dataset5$kraj, levels = levels)
        rm(fctrs,levels)
        dataset5<- as.data.frame(dataset5)
        
      }else if(input$typ5=='Okresy')
      {
        dataset5 %<>% group_by(year, kraj,okres) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        fctrs<- dataset5 %>% group_by(okres) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        
        if(input$metrika5=="Suma")
        {
          fctrs$okres <- factor(fctrs$okres, levels = unique(fctrs$okres)[order(fctrs$suma_spolu, decreasing = FALSE)])
        }else if(input$metrika5=="Počet zmlúv"){
          fctrs$okres <- factor(fctrs$okres, levels = unique(fctrs$okres)[order(fctrs$pocet, decreasing = FALSE)])
        }else if(input$metrika5=="Priemerná zmluvná čiastka"){
          fctrs$okres <- factor(fctrs$okres, levels = unique(fctrs$okres)[order(fctrs$avg, decreasing = FALSE)])
        }
         levels<- levels(fctrs$okres)
        dataset5$okres <- factor(dataset5$okres, levels = levels)
        rm(fctrs,levels)
        dataset5<- as.data.frame(dataset5)
      }else if(input$typ5=='Mestá')
      {
        dataset5 %<>% group_by(year, kraj,okres,mesto) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        fctrs<- dataset5 %>% group_by(mesto) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        
        if(input$metrika5=="Suma")
        {
          fctrs$mesto <- factor(fctrs$mesto, levels = unique(fctrs$mesto)[order(fctrs$suma_spolu, decreasing = FALSE)])
        }else if(input$metrika5=="Počet zmlúv"){
          fctrs$mesto <- factor(fctrs$mesto, levels = unique(fctrs$mesto)[order(fctrs$pocet, decreasing = FALSE)])
        }else if(input$metrika5=="Priemerná zmluvná čiastka"){
          fctrs$mesto <- factor(fctrs$mesto, levels = unique(fctrs$mesto)[order(fctrs$avg, decreasing = FALSE)])
        }
        
        levels<- levels(fctrs$mesto)
        dataset5$mesto <- factor(dataset5$mesto, levels = levels)
        rm(fctrs,levels)
        dataset5<- as.data.frame(dataset5)
      }else if(input$typ5=='Zmluvná strana')
      {
        
        dataset5 %<>% group_by(year, kraj,okres,zs) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        fctrs<- dataset5 %>% group_by(zs) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
        
        if(input$metrika5=="Suma")
        {
          fctrs$zs <- factor(fctrs$zs, levels = unique(fctrs$zs)[order(fctrs$suma_spolu, decreasing = FALSE)])
        }else if(input$metrika5=="Počet zmlúv"){
          fctrs$zs <- factor(fctrs$zs, levels = unique(fctrs$zs)[order(fctrs$pocet, decreasing = FALSE)])
        }else if(input$metrika5=="Priemerná zmluvná čiastka"){
          fctrs$zs <- factor(fctrs$zs, levels = unique(fctrs$zs)[order(fctrs$avg, decreasing = FALSE)])
        }
        levels<- levels(fctrs$zs)
        dataset5$zs <- factor(dataset5$zs, levels = levels)
        rm(fctrs,levels)
        dataset5<- as.data.frame(dataset5)
      }
    })
  
  output$plot5 <- renderPlotly({
    
    dataset5<- dload5()
    
    dataset5$pSum<- paste(prettyNum(dataset5$suma_spolu,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
    dataset5$pPoc<- prettyNum(dataset5$pocet,digits=2,big.mark = " ", scientific=F)
    dataset5$pAvg<- paste(prettyNum(dataset5$avg,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
    
    
    if(input$typ5=='Rezorty')
    {
      n<- n_distinct(dataset5$NazovRezortu)
      #plotting
      if(input$metrika5=="Suma")
      {
        plot_ly(dataset5, y=~NazovRezortu, x=~suma_spolu, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Suma: ",dataset5$pSum,"<br>","Názov rezortu: ",dataset5$NazovRezortu,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }else if(input$metrika5=="Počet zmlúv"){
        plot_ly(dataset5, y=~NazovRezortu, x=~pocet, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Počet zmlúv: ",dataset5$pPoc,"<br>","Názov rezortu: ",dataset5$NazovRezortu,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }else if(input$metrika5=="Priemerná zmluvná čiastka"){
        plot_ly(dataset5, y=~NazovRezortu, x=~avg, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Priemerná zmluvná čiastka: ",dataset5$pAvg,"<br>","Názov rezortu: ",dataset5$NazovRezortu,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }
     
    }else if(input$typ5=='Kraj')
    {
      #plotting
      if(input$metrika5=="Suma")
      {
        plot_ly(dataset5, y=~kraj, x=~suma_spolu, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Suma: ",dataset5$pSum,"<br>","Kraj: ",dataset5$kraj,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }else if(input$metrika5=="Počet zmlúv"){
        plot_ly(dataset5, y=~kraj, x=~pocet, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Počet zmlúv: ",dataset5$pPoc,"<br>","Kraj: ",dataset5$kraj,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }else if(input$metrika5=="Priemerná zmluvná čiastka"){
        plot_ly(dataset5, y=~kraj, x=~avg, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Priemerná zmluvná čiastka: ",dataset5$pAvg,"<br>","Kraj: ",dataset5$kraj,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title=""),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }

    }else if(input$typ5=='Okresy')
    {
      n<- n_distinct(dataset5$okres)
      #plotting
      if(input$metrika5=="Suma")
      {
        plot_ly(dataset5, y=~okres, x=~suma_spolu, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Suma: ",dataset5$pSum,"<br>","Kraj: ",dataset5$kraj,"<br>","Okres: ",dataset5$okres,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }else if(input$metrika5=="Počet zmlúv"){
        plot_ly(dataset5, y=~okres, x=~pocet, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Počet zmlúv: ",dataset5$pPoc,"<br>","Kraj: ",dataset5$kraj,"<br>","Okres: ",dataset5$okres,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }else if(input$metrika5=="Priemerná zmluvná čiastka"){
        plot_ly(dataset5, y=~okres, x=~avg, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Priemerná zmluvná čiastka: ",dataset5$pAvg,"<br>","Kraj: ",dataset5$kraj,"<br>","Okres: ",dataset5$okres,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }
    }else if(input$typ5=='Mestá')
    {
      n<- n_distinct(dataset5$mesto)
      #plotting
      if(input$metrika5=="Suma")
      {
        plot_ly(dataset5, y=~mesto, x=~suma_spolu, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Suma: ",dataset5$pSum,"<br>","Kraj: ",dataset5$kraj,"<br>","Okres: ",dataset5$okres,"<br>","Mesto: ",dataset5$mesto,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }else if(input$metrika5=="Počet zmlúv"){
        plot_ly(dataset5, y=~mesto, x=~pocet, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Počet zmlúv: ",dataset5$pPoc,"<br>","Kraj: ",dataset5$kraj,"<br>","Okres: ",dataset5$okres,"<br>","Mesto: ",dataset5$mesto,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }else if(input$metrika5=="Priemerná zmluvná čiastka"){
        plot_ly(dataset5, y=~mesto, x=~avg, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Priemerná zmluvná čiastka: ",dataset5$pAvg,"<br>","Kraj: ",dataset5$kraj,"<br>","Okres: ",dataset5$okres,"<br>","Mesto: ",dataset5$mesto,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=400))
      }
    }else if(input$typ5=='Zmluvná strana')
    {
      n<- n_distinct(dataset5$zs)
      #plotting
      if(input$metrika5=="Suma")
      {
        plot_ly(dataset5, y=~zs, x=~suma_spolu, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Suma: ",dataset5$pSum,"<br>","Zmluvná strana: ",dataset5$zs,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=700))
      }else if(input$metrika5=="Počet zmlúv"){
        plot_ly(dataset5, y=~zs, x=~pocet, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Počet zmlúv: ",dataset5$pPoc,"<br>","Zmluvná strana: ",dataset5$zs,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=700))
      }else if(input$metrika5=="Priemerná zmluvná čiastka"){
        plot_ly(dataset5, y=~zs, x=~avg, color=~as.factor(year), hoverinfo="text", text=~c(paste0("Priemerná zmluvná čiastka: ",dataset5$pAvg,"<br>","Zmluvná strana: ",dataset5$zs,"<br>","Rok: ",dataset5$year))) %>% add_bars() %>% 
          layout(xaxis=list(title=""), yaxis=list(title="", range=c(n-TOPx-0.5,n)),barmode="stack", hovermode="closest", showlegend=T, margin=list(l=700))
      }
    }
    
  })
  
  output$download5<- downloadHandler(
    filename = "zmluvy.txt",
    content = function(file) {
      write.table(dload5(), file,sep=";", quote=F, row.names=F, dec=",")
    })
  
  #Sixth App Output - TYP ZMLUV--------------
  updateSelectizeInput(session, 'rezort6', choices = dataset6$NazovRezortu,selected=c("Úrad vlády SR","Ministerstvo financií SR"), server=T)
  updateSelectizeInput(session, 'typ_z6', choices = dataset6$typ_zmluvy,selected=c("iný","zmluva o dielo"), server=T)
  
  dload6<- reactive({
    #subsetting dataset6 using input variables
    dataset6<- subset(dataset6, year>=input$rok6[1] & year<=input$rok6[2])
    dataset6<- subset(dataset6, NazovRezortu %in% input$rezort6)
    dataset6<- subset(dataset6, typ_zmluvy %in% input$typ_z6)
    
    dataset6 %<>% group_by(NazovRezortu, typ_zmluvy) %>% summarise(suma_spolu=sum(suma_spolu, na.rm=T),pocet=sum(pocet, na.rm=T),avg=mean(avg, na.rm=T)) %>% ungroup()  
    #hoverinfo
    dataset6$pSum<- paste(prettyNum(dataset6$suma_spolu,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
    dataset6$pPoc<- prettyNum(dataset6$pocet,digits=2,big.mark = " ", scientific=F)
    dataset6$pAvg<- paste(prettyNum(dataset6$avg,digits=2,big.mark = " ", scientific=F),"€",sep=" ")
    
    dataset6
  })
  
  output$plot6 <- renderPlotly({
    
    dataset6<- dload6()
    
    #plotting
    if(input$metrika6=="Suma")
    {
      plot_ly(dataset6, y=~NazovRezortu, x=~suma_spolu,  color=~typ_zmluvy, hoverinfo="text", text=~c(paste0("Suma:",dataset6$pSum,"<br>","Rezort: ",dataset6$NazovRezortu,"<br>","Typ zmluvy: ",dataset6$typ_zmluvy))) %>% add_bars() %>% 
        layout(xaxis=list(title=""), yaxis=list(title=""), barmode="stack", hovermode="closest", showlegend=T, legend = list(x=max(dataset6$suma_spolu)) ,margin=list(l=300))
    }else if(input$metrika6=="Počet zmlúv"){
      plot_ly(dataset6, y=~NazovRezortu, x=~pocet,  color=~typ_zmluvy, hoverinfo="text", text=~c(paste0("Počet zmlúv:",dataset6$pPoc,"<br>","Rezort: ",dataset6$NazovRezortu,"<br>","Typ zmluvy: ",dataset6$typ_zmluvy))) %>% add_bars() %>% 
        layout(xaxis=list(title=""), yaxis=list(title=""), barmode="stack", hovermode="closest", showlegend=T, legend = list(x=max(dataset6$suma_spolu)) ,margin=list(l=300))
    }else if(input$metrika6=="Priemerná zmluvná čiastka"){
      plot_ly(dataset6, y=~NazovRezortu, x=~avg,  color=~typ_zmluvy, hoverinfo="text", text=~c(paste0("Priemerná zmluvná čiastka:",dataset6$pAvg,"<br>","Rezort: ",dataset6$NazovRezortu,"<br>","Typ zmluvy: ",dataset6$typ_zmluvy))) %>% add_bars() %>% 
        layout(xaxis=list(title=""), yaxis=list(title=""),  hovermode="closest", showlegend=T, legend = list(x=max(dataset6$suma_spolu)) ,margin=list(l=300))
    }
  })
  
  output$download6<- downloadHandler(
    filename = "zmluvy.txt",
    content = function(file) {
      write.table(dload6(), file,sep=";", quote=F, row.names=F, dec=",")
    })
 
  output$table<- renderDataTable(escape=F,{
    if(input$topTable=="Deň")
    {
      top10d
    }else if(input$topTable=="Mesiac")
    {
      top50m
    }
  })

   
}


shinyApp(ui, server)
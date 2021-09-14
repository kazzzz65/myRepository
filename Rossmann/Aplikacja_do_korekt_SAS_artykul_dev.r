# zaciągnięcie bibliotek

library(shiny)
library(tidyverse)
library(data.table)
library(DT)
library(plotly)
library(dplyr)
library(plyr)
library(shinythemes)
library(odbc)
library(dbplyr)
library(sqldf)
library(promises)
library(future)
library(ipc)
library(shinycssloaders)
library(openxlsx)

#zaciągnięcie danych potrzebnych na początku w ui

# sciezka <- dirname(rstudioapi::getActiveDocumentContext()$path)
p_grupy <- character(0)
# 
# con <- dbConnect(odbc::odbc(),
#                  dsn = "Hive z LLAP Cloudera",
#                  driver = "Cloudera ODBC Driver for Apache Hive",
#                  dbname = "training",
#                  Trusted_Connection = "True",
#                  encoding = "CP1250",
#                  port = 10500)
# 
# con_ross <- DBI::dbConnect(odbc::odbc(),
#                            dsn = "rossod32",
#                            database = "ROSSORA",
#                            encoding = "CP1250",
#                            uid = "hati",
#                            pwd = "hati")
# 
# 
wszystkie_sklepy <- character(0)
# as.data.table(dbGetQuery(con, paste("SELECT DISTINCT dic.id_store as id_store FROM dict.dic_stores dic WHERE dic.close_date IS NULL ORDER BY dic.id_store", sep ="")))[id_store<=9000]
artykuly <- character(0)
  # as.data.table(dbGetQuery(con_ross, paste("SELECT ARTNR1, ORDERKZ FROM ROSSORA.DBO.ACCESS_ARTIKEL", sep="")))[, ARTNR1 := as.numeric(ARTNR1)][ORDERKZ == 1, ARTNR1]
# 
# 
# ##elementy potrzebne do zakładki z alertami
# lista_plikow_wyd <- list.files(sciezka)
# lista_plikow_wyd_48 <- grep("^ALERT_48_", lista_plikow_wyd, value = TRUE)
# 
# lista_plikow_wyd_57 <- grep("^ALERT_57_", lista_plikow_wyd, value = TRUE)
# 
# nazwa_48 <- sort(lista_plikow_wyd_48, decreasing = TRUE)[1]
# nazwa_57 <- sort(lista_plikow_wyd_57, decreasing = TRUE)[1]
# 
# sklepy_z_alertem_48_read <- fread(paste(sciezka, nazwa_48, sep="/"))
# sklepy_z_alertem_48_d <- as.data.table(sklepy_z_alertem_48_read)
# sklepy_z_alertem_48_list <- as.list(sklepy_z_alertem_48_d[,'StoreId'])
# 
# sklepy_z_alertem_57_read <- fread(paste(sciezka, nazwa_57, sep="/"))
# sklepy_z_alertem_57_d <- as.data.table(sklepy_z_alertem_57_read)
# sklepy_z_alertem_57_list <- as.list(sklepy_z_alertem_57_d[,'StoreId'])

ui <- fluidPage(
  theme = shinytheme("united"),
  
  # stworzenie layoutu podzielonego na zakładki analiza i korekta
  navbarPage(title=
    "Rossmann", id = 'menu',
    tabPanel("Analiza sklepów SAS", 
             
             column(3,
                    wellPanel(
                      selectizeInput(inputId = "sklepy", 
                                  label = "Wybierz numery sklepów:",
                                  multiple = TRUE,
                                  choices = wszystkie_sklepy,
                                  options = list(closeAfterSelect = T)),
                      dateInput(inputId = "data_prognozy",
                              label = "Wybierz datę prognozy:",
                              value = Sys.Date()-1),
                    actionButton(inputId = "generuj", label = "Generuj wykres", icon("line-chart"), style="color: #fff; background-color: #228B22; border-color: #228B22")
                    )
                    
             ),
             column(9,tabsetPanel(
               tabPanel("Wykres", fluidRow(
               plotlyOutput("prognoza_total")%>%withSpinner(type = 5, color = '#808080')
               )),
               tabPanel("Opis zakładki", fluidRow(
                 htmlOutput("opis_analizy")
               ))
             ))  
    ),
    tabPanel("Wprowadzanie korekt", 
             column(3,  
                    wellPanel(
                      radioButtons(inputId = "automat", label = h3("Automatyczna korekta?"),
                                   choices = c("tak", "nie"),
                                   selected = "nie"),
                      conditionalPanel(condition = "input.automat == 'tak'", 
                      radioButtons(inputId = "kierunek", label = h3("Ustal kierunek korekty"),
                                   choices = c("dowolna", "zmniejszająca", "zwiększająca"),
                                   selected = "dowolna"),
                     
                      numericInput("ograniczenie", 
                                   label = "Podaj miesięczną wartość graniczną prognozy dla artykułu",
                                   value = 10,
                                   step = 1)),
                      selectizeInput(inputId = "sklep_input", 
                                     label = "Wybierz numer sklepu:",
                                     choices = wszystkie_sklepy,
                                     options = list(closeAfterSelect = T)
                      ),
                      numericInput("korekta", 
                                   label = "Podaj rozmiar korekty",
                                   value = 1,
                                   step = 0.1),
                      selectizeInput(
                        inputId = "pgrupa",
                        label = "Wybierz p-grupę:",
                        choices = p_grupy,
                        multiple = TRUE,
                        options = list(closeAfterSelect = T)),
                      selectizeInput(
                        inputId = "anty_pgrupa",
                        label = "Odfiltruj p-grupę:",
                        choices = p_grupy,
                        multiple = TRUE,
                        options = list(closeAfterSelect = T)),
                      selectizeInput(
                        inputId = "wgrupa",
                        label = "Wybierz w-grupę:",
                        choices = p_grupy,
                        multiple = TRUE,
                        options = list(closeAfterSelect = T)),
                      selectizeInput(
                        inputId = "agrupa",
                        label = "Wybierz a-grupę:",
                        choices = p_grupy,
                        multiple = TRUE,
                        options = list(closeAfterSelect = T)),
                      radioButtons(inputId = "radio", label = h3("Sposób wybrania artykułów"),
                                   choices = c("Wybierz z listy", "Wklej lub wpisz"),
                                   selected = "Wybierz z listy"),
                      conditionalPanel(condition = "input.radio == 'Wybierz z listy'", selectInput(
                        inputId = "artykul",
                        label = "Wybierz numery artykułów:",
                        choices = p_grupy,
                        multiple = TRUE)),
                      conditionalPanel(condition = "input.radio == 'Wklej lub wpisz'", textAreaInput("lis_art",
                        "Lista artykułów", "", 
                        cols = 1, resize = "vertical", 
                        placeholder = "Wklej listę artykułów przedzielonych enterem"))
                     ,
                      actionButton(inputId = "symuluj", label = "Symuluj", icon("line-chart"), style="color: #fff; background-color: #5BC85B; border-color: black"),
                      actionButton(inputId = "zatwierdz", label = "Zatwierdź", icon("clipboard-check"), style="color: #fff; background-color: green; border-color: black", width = 110),
                      actionButton(inputId = "cofnij", label = "Cofnij", icon("step-backward"), style="color: #fff; background-color: orange; border-color: black", width = 110),
                      actionButton(inputId = "wyczysc", label = "Wyczyść", icon("skull-crossbones"), style="color: #fff; background-color: red; border-color: black", width = 110),
                      downloadButton("zapisz", label = "Zapisz", style="color: #fff; background-color: blue; border-color: black", width = 110),
                      downloadButton("pobierz", label = "Pobierz dane", style="color: #fff; background-color: light-blue; border-color: black", width = 110)
                     
                     )
                    ),
                    column(9,tabsetPanel( tabPanel("Wykres i tabela", fluidRow(
                      conditionalPanel(condition = "input.automat == 'tak'",
                      sliderInput(inputId = "data_przedz_auto",
                                  label = "Wybierz przedział czasowy, który ma zostać wzięty do wygenerowania korekty automatycznej:",
                                  min = as.Date(Sys.Date()-460),
                                  max = as.Date(Sys.Date()-1),
                                  value = c(as.Date(Sys.Date()-32),as.Date(Sys.Date()-2)),
                                  timeFormat="%Y-%m-%d",
                                  width = 1550),
                      sliderInput(inputId = "korekta_przedz",
                                  label = "Wybierz przedział korekty automatycznej: ",
                                  step = 0.1,
                                  min = 0,
                                  max = 10,
                                  value = c(0.6, 2),
                                  width = 1550)),
                      sliderInput(inputId = "data_przedz",
                                  label = "Wybierz przedział czasowy korekty:",
                                  min = Sys.Date(),
                                  max = as.Date(Sys.Date()+120),
                                  value = c(Sys.Date(),as.Date(Sys.Date()+30)),
                                  timeFormat="%Y-%m-%d",
                                  width = 1550)),
                      radioButtons(inputId = "wybor_agregacji", label = h3("Wybierz sposób agregacji danych"),
                                   choices = c("per dzień", "tygodniowa"),
                                   selected = "per dzień"),
                    fluidRow(
                      plotlyOutput("prognoza_po_korektach")%>%withSpinner(type = 5, color = '#808080')),
                    fluidRow(
                      DT::dataTableOutput("tabela_z_korektami")%>%withSpinner(type = 5, color = '#808080'))
                    
                    ),
                    tabPanel("Opis zakładki", fluidRow(
                      htmlOutput("opis_korekty")
                    ))
                    ))
             ),
    tabPanel("Weryfikacja prognoz",
             
             column(3,
                    wellPanel(
                      helpText("Wybierz numer sklepu i daty prognoz.")
                    ),   
                    wellPanel(
                      selectInput(inputId = "sklep_porownanie", 
                                  label = "Wybierz sklep:",
                                  multiple = F,
                                  choices = wszystkie_sklepy),
                      
                      dateInput(inputId = "data_prognozy_1",
                                label = "Wybierz pierwszą datę prognozy:",
                                value = Sys.Date()-1),
                      dateInput(inputId = "data_prognozy_2",
                                label = "Wybierz drugą datę prognozy:",
                                value = Sys.Date()-2),
                      actionButton(inputId = "przelicz_porownanie", label = "Przelicz dane", icon("calculator"), 
                                   style="color: #fff; background-color: #228B22; border-color: #228B22"),
                      selectizeInput(inputId = "pgrupa_por", 
                                     label = "Wybierz p-grupę:",
                                     multiple = T,
                                     choices = p_grupy,
                                     options = list(closeAfterSelect = T)),
                      selectizeInput(inputId = "wgrupa_por", 
                                     label = "Wybierz w-grupę:",
                                     multiple = T,
                                     choices = p_grupy,
                                     options = list(closeAfterSelect = T)),
                      actionButton(inputId = "generuj_porownanie", label = "Generuj wykres", icon("line-chart"), 
                                   style="color: #fff; background-color: #228B22; border-color: #228B22")
                    )
             ),
             column(9, tabsetPanel(tabPanel("Wykres", fluidRow(sliderInput(inputId = "data_przedz_por",
                                                                 label = "Wybierz przedział czasowy analizy:",
                                                                 min = as.Date(Sys.Date()-460),
                                                                 max = as.Date(Sys.Date()+60),
                                                                 value = c(as.Date(Sys.Date()-120),as.Date(Sys.Date()+60)),
                                                                 timeFormat="%Y-%m-%d",
                                                                 width = 1550)),
             fluidRow(
             DT::dataTableOutput("tabela")%>%withSpinner(type = 5, color = '#808080')),
             fluidRow(
               plotlyOutput("wykres_porownanie")%>%withSpinner(type = 5, color = '#808080')
             ), 
             fluidRow(
               plotlyOutput("wykres_porownanie_zl")%>%withSpinner(type = 5, color = '#808080')
             )
             
             ),
             
             tabPanel("Opis zakładki", fluidRow(
               htmlOutput("opis_porownania")
             )))
             ))  
    # ),
    # tabPanel("Kanibalizacja",
    #          column(3,
    #                 wellPanel(
    #                   helpText("Wybierz numer sklepu i datę prognozy.")
    #                 ),   
    #                 wellPanel(
    #                   selectInput(inputId = "sklep_kanibalizacja", 
    #                               label = "Wybierz sklep:",
    #                               multiple = F,
    #                               choices = wszystkie_sklepy),
    #                   actionButton(inputId = "generuj_kan", label = "Generuj wykres", icon("line-chart"), 
    #                                style="color: #fff; background-color: #228B22; border-color: #228B22")
    #                 )
    #          ),
    #          column(9, tabsetPanel(tabPanel("Wykres", fluidRow(
    #            plotlyOutput("wykres_kan")%>%withSpinner(type = 5, color = '#808080')
    #          )),
    #          tabPanel("Opis zakładki", fluidRow(
    #            htmlOutput("opis_kan")
    #          )))
    #          )  
    # ),
    # tabPanel("Alerty",
    #          
    #          column(3,
    #                 wellPanel(
    #                   helpText("Wybierz alert i numer sklepu.")
    #                 ),   
    #                 wellPanel(
    #                   selectInput(inputId = "wybor_alertu", 
    #                               label = "Wybierz alert:",
    #                               multiple = F,
    #                               choices = list('48', '57')),
    #                   selectInput(inputId = "sklep_alert", 
    #                               label = "Wybierz numer sklepu z alertem:",
    #                               multiple = F,
    #                               choices = sklepy_z_alertem_48_list),                      
    #                   actionButton(inputId = "generuj_alert", label = "Generuj wykres", icon("line-chart"), 
    #                                style="color: #fff; background-color: #228B22; border-color: #228B22")
    #                 )
    #          ),
    #          column(9, tabsetPanel(tabPanel("Wykres", fluidRow(
    #            plotlyOutput("alert_wykres")%>%withSpinner(type = 5, color = '#808080')
    #          )),
    #          tabPanel("Opis zakładki", fluidRow(
    #            htmlOutput("opis_alertow")
    #          )))
    #          )  
    # )
  ))


server <- function(input, output, session){
  
# opisy zakładek
  output$opis_analizy = renderText({
    "<b>Założenia:</b> <br/>
    Ta zakładka służy do przesłania zapytań do bazy i przeliczenia danych.<br/>
    Wykres zawiera realizacje na 460 dni w tył od dzisiejszego dnia oraz prognozy na 60 dni wprzód, na wybranych sklepach(agregacja) przy danej dacie prognozy.<br/><br/>
   
    
    <strong>Instrukcja obsługi:</strong><br/>
    - <strong>Wybierz numery sklepów</strong> - Wybieramy nr sklepów na których chcemy pracować pamiętając, że wybranie większej liczby niż ok.5 może spowodować spowolnione działanie aplikacji.,<br/>
    - <strong>Wybierz datę prognozy</strong> - Wybieramy datę prognozy,<br/>
    - <strong>kliknij 'Generuj'</strong> -  Nastąpi przeliczenie które może potrwać od 1 do 5 minut, po czym wygeneruje się zbiorczy wykres realizacji na 460 dni w tył od dzisiejszego dnia oraz prognozy na 60 dni wprzód.
    Po ukazaniu się wykresu przejdź do zakładki 'Wprowadzanie korekt'.<br/><br/> 
    <br/>"       
  })
  
  output$opis_korekty = renderText({
    "<b>Założenia:</b> <br/>
    Ta zakładka służy do filtrowania danych, symulowania korekt, zatwierdzania ich i zapisu.<br/>
    Wykres zawiera wyfiltrowane dane oraz symulację wprowadzanej korekty.<br/>
    W tabeli znajdują się korekty, posortowane w kolejności dodawania.<br/>
    Pamiętaj o tym by wykonać najpierw przeliczenie na poprzedniej zakładce.<br/>
    Na wykresie pojawiają sie informacje o realizacji, prognozie, prognozie po symulowanej korekcie, aktualnym stoku sklepowym oraz capacity.<br/>
    Capacity jest oznaczone poziomą linią jako capacity w dniu poprzedzającym prognozę dla wybranej grupy artykułów. Miara ta jest jedynie przydatna dla pojedynczych artykułów lub niewielkich grup.<br/>
    W przypadku p-grup i całych sklepów capacity nie będzie widoczne na wykresie.<br/>
    Korekta automatyczna to korekta wyliczana automatycznie per artykuł na danym filtrowaniu, na podstawie wybranych okresów jako iloraz średnich realizacji i prognozy.</br> 
    Korekta jest w przedziałach [0.5, 0.95] i [1.05, 2], z buforem 0.1, ograniczeniem ze względu na wielkość prognozy w okresie wybranym jako przedział czasowy korekty, zaokrąglona do wielokrotności 0.05, z możliwością wybrania jej kierunku. <br/><br/>
    
    <strong>Instrukcja obsługi:</strong><br/>
    - <strong>Automatyczna korekta?</strong> - Wybieramy czy korekta ma się wygenerować automatycznie per artykuł, czy dokonujemy ręcznej korekty na danym filtrowaniu,<br/>
    - <strong>Wybierz numer sklepu</strong> - Wybieramy nr sklepu,<br/>
    - <strong>Wybierz przedział czasowy korekty</strong> -  Wybieramy przedział od do nałożenia korekty, należy pamiętać o tym, żeby daty na sklepo-artykule dla różnych korekt nie nakładały się,<br/>
    - <strong>Wybierz przedział czasowy, który ma zostać wzięty do wygenerowania korekty automatycznej:</strong> -  Wybieramy przedział od do, który ma zostać wzięty do automatycznej korekty jako wzór realizacji,<br/>
    - <strong>Podaj rozmiar korekty</strong> - Podajemy rozmiar korekty, możemy manipulować nim za pomocą 'strzałeczek' o 0,1,<br/>
    - <strong>Ustal kierunek korekty</strong> - Ustalamy, czy automatyczna korekta ma się ograniczyć do któregoś kierunku czy pozostać 'dowolna',<br/>
    - <strong>Podaj wartość graniczną prognozy dla artykułu</strong> - Podajemy wartość prognozy w wybranym okresie od której artykuły są brane do korekty,<br/>
    - <strong>Wybierz p-grupę</strong> -  Wybieramy p-grupy,<br/>
    - <strong>Odfiltruj p-grupę</strong> -  Wybieramy p-grupy, na których nie chcemy wprowadzać korekty,<br/>
    - <strong>Wybierz w-grupę</strong> -  Wybieramy w-grupy,<br/>
    - <strong>Wybierz a-grupę</strong> -  Wybieramy a-grupy,<br/>
    - <strong>Sposób wybrania artykułów</strong> -  Wybieramy czy chcemy wkleić lub wpisać artykuły czy może wybrać z listy wyfiltrowanej na podstawie wcześniejszych filtrów ,<br/>
    - <strong>Wybierz sposób agregacji</strong> -  Wybieramy w jaki sposób dane mają być agregowane na wykresie,<br/>
    - <strong>Przycisk 'Symuluj'</strong> -  Powoduje wygenerowania wykresu na wyfiltrowanyh danych,<br/>
    - <strong>Przycisk 'Zatwierdź'</strong> -  Powoduje zatwierdzenie korekty,<br/>
    - <strong>Przycisk 'Cofnij'</strong> -  Powoduje usunięcie ostatniej korekty z tabeli,<br/>
    - <strong>Przycisk 'Wyczyść'</strong> -  Powoduje usunięcie wszystkich korekt z tabeli,<br/>
    - <strong>Przycisk 'Zapisz'</strong> -  Powoduje zapisanie korekt do pliku xlsx.<br/>
    - <strong>Przycisk 'Pobierz dane'</strong> -  Powoduje zapisanie aktualnie wyfiltrowanych danych (po kliknięciu symuluj!) do pliku xlsx.<br/>
    <br/>"       
  })
  
  output$opis_porownania = renderText({
    "<b>Założenia:</b> <br/>
    Ta zakładka służy do porównania prognoz z różnymi datami przeliczeń na danym sklepie<br/>"       
  })
  
  output$opis_kan = renderText({
    "<b>Założenia:</b> <br/>
    Ta zakładka służy do wizualizacji kanibalizacji na sklepie<br/>"       
  })
  
  output$opis_alertow = renderText({
    "<b>Założenia:</b> <br/>
    Ta zakładka służy do wizualizacji prognoz i realizacji na sklepach z alertami 48 i 57<br/>"       
  })
  
  rv <- reactiveValues(g = 0, h = 0, tabela_korekta_final = data.table(`Numer sklepu`=numeric(), `Numer artykułu`=numeric(), `Data od`=as.Date(character()), `Data do`=as.Date(character()), `Typ korekty` = numeric(), `Wartość korekty` = as.double(), `Typ prognozy` = numeric()),
                       temp1 = 0,  wszystkie_artykuly = c(), tabela_korekta = data.table(), wszystkie_sklepy = list(),
                       stok_sklep = data.table(id_store = numeric(), id_article= numeric(), forecast_date = as.Date(character()), stok_sklepy=numeric()),
                       capacity = data.table(id_store = numeric(), id_article= numeric(), capacity=numeric()), capacity1 = data.table(id_store = numeric(), id_article= numeric(), capacity=numeric()),
                       dane_do_korekty = data.table(id_store = numeric(), id_article= numeric(), korekta=numeric()))
  
  
  dane_real <- eventReactive(input$generuj, {
    
    # podstawowa tabela zawierająca wyniki przeliczenia
    # 5 kwerend hadoopowych 1-2 prognoza, 3 - realizacja, 4 - stok. 5 - capacity
    
    
    sklepy <-  paste(shQuote(input$sklepy, type="csh"), collapse=", ") 
    
    s <- Sys.time()
    process_step_run_id_full <- dbGetQuery(con, paste("SELECT calculation_date as calculation_date, max(process_step_run_id) as process_step_run_id, max(process_run_id) as process_run_id, max(calculation_id) as calculation_id FROM detail_sas.sales_forecast_hist WHERE calculation_date in ('", input$data_prognozy ,"') GROUP BY calculation_date", sep=""))
    s1<- Sys.time()
    print(s1-s)
    
    process_step_run_id_full <- as.data.table(process_step_run_id_full)  
    
    s <- Sys.time()
    dane_real <- dbGetQuery(con, paste("select
                                           SAS.id_store as id_store,
                                           SAS.id_article as id_article,
                                           DICT.product_group as product_group,
                                           DICT.type_group as type_group,
                                           DICT.art_group as art_group,
                                           SAS.forecast_date as forecast_date,
                                           sum(SAS.forecast_quantity_stat) as forecast_quantity_stat,
                                           sum(SAS.forecast_quantity_corr) as forecast_quantity_corr,
                                           sum(SAS.forecast_value) as forecast_value
                                           from detail_sas.sales_forecast_hist SAS
                                           join dict.dic_articles DICT
                                           ON SAS.id_article = DICT.id_article 
                                           WHERE SAS.id_store in (", sklepy ,") 
                                           AND SAS.process_step_run_id in (", process_step_run_id_full[, process_step_run_id], ") 
                                           and SAS.process_run_id in (", process_step_run_id_full[, process_run_id], ") 
                                           and SAS.calculation_id in (", process_step_run_id_full[, calculation_id], ") 
                                           AND SAS.forecast_date >= '", input$data_prognozy  ,"' 
                                           AND SAS.forecast_date <= '", input$data_prognozy + 60 ,"' 
                                          group by SAS.id_store, SAS.id_article, DICT.product_group, DICT.type_group, DICT.art_group,
                                          SAS.forecast_date;", sep=""))
    s1<- Sys.time()
    print(s1-s)
    
    s <- Sys.time()
    dane_real_2 <- dbGetQuery(con, paste("SELECT
                                         REAL.id_store as id_store,
                                         REAL.id_article as id_article,
                                         DICT.product_group as product_group,
                                         DICT.type_group as type_group,
                                          DICT.art_group as art_group,
                                         REAL.working_date as working_date,
                                         sum(REAL.sale_cnt) as sale_cnt,
                                         sum(REAL.sale_value) as sale_value
                                         FROM detail_sales.tickets REAL
                                         join dict.dic_articles DICT
                                         ON REAL.id_article = DICT.id_article 
                                         WHERE REAL.id_store in (", sklepy ,") AND REAL.working_date >= '", Sys.Date() -460  ,"' 
                                         GROUP BY REAL.id_article, REAL.id_store, REAL.working_date, DICT.product_group,
                                          DICT.type_group, DICT.art_group;", sep=""))
    
    s1<- Sys.time()
    print(s1-s)
    
    s <- Sys.time()
    rv$stok_sklep <- dbGetQuery(con, paste("SELECT STOK.id_store as id_store,
                                           STOK.id_article as id_article,  STOK.calculate_date as forecast_date,
                                           sum(STOK.stan_999) as stok_sklepy
                                           FROM detail_inventory.stock_article_store_daily STOK 
                                           where STOK.id_store in (", sklepy ,") AND STOK.calculate_date >='", Sys.Date() -30 ,
                                        "'AND STOK.calculate_date <='", input$data_prognozy - 1 ,
                                        "'GROUP BY STOK.id_store, STOK.id_article, STOK.calculate_date;", sep=""))
    s1<- Sys.time()
    print(s1-s)
    rv$stok_sklep <- as.data.table(rv$stok_sklep)
    rv$stok_sklep[, tydzien := format(forecast_date, "%Y-%V")]
    rv$stok_sklep <- rv$stok_sklep[stok_sklepy >= 0]
    rv$stok_sklep <- unique(rv$stok_sklep)
    
    s <- Sys.time()
    rv$capacity1 <- dbGetQuery(con, paste("SELECT id_store as id_store, id_article as id_article,
                                          sum(capacity) as capacity FROM detail_snap.planogram_saa 
                                          WHERE id_store in (", sklepy ,") AND planogram_date ='", input$data_prognozy - 1 ,
                                         "'GROUP BY id_store, id_article, planogram_date;", sep=""))
    s1<- Sys.time()
    print(s1-s)
   
    dane_real <- as.data.table(dane_real)
    dane_real <- na.omit(dane_real)
  
    dane_real_2 <- as.data.table(dane_real_2)
    dane_real_2 <- na.omit(dane_real_2)
    dane_real <- merge(dane_real, dane_real_2, by.x=c("id_store", "id_article", "product_group", "forecast_date", "type_group", "art_group"),
                       by.y=c("id_store", "id_article", "product_group", "working_date", "type_group", "art_group"), all=TRUE)
    
    dane_real[, tydzien := format(forecast_date, "%Y-%V")]
    
    
    #usunięcie pgrup-śmieci
    lista_niechcianych_pgrup <- list("Test", "Scanning", "Torby na zakupy", "Produkty wylistowane przed 01.01.2010", "Materiały reklamowe", "Artykuły testowe", "Stare i nieprzypisane", "Nieprzypisane", "Nieprzypięte", "Nieskategoryzowane", "Artykuły techniczne", "Wystrój sklepów", "Online-dom", "NF Traffic", "Koszty dostaw")
    
    dane_real <- dane_real[!(product_group %in% lista_niechcianych_pgrup)]
    
    print("zakończyłem obliczać dane_real")
    print(dane_real)
    return(dane_real)
  })
  

  

  
  
  tabela <- eventReactive(input$generuj, {
    #przygotowanie danych do zbiorczego wykresu
    
    req(dane_real())
    rv$g<- as.data.table(dane_real())
    rv$h <- rv$g[, .(forecast_quantity_stat = sum(forecast_quantity_stat, na.rm = T), forecast_quantity_corr= sum(forecast_quantity_corr, na.rm = T), forecast_value = sum(forecast_value, na.rm = T), sale_cnt = sum(sale_cnt, na.rm = T), sale_value = sum(sale_value, na.rm = T)), by=c("forecast_date","product_group","type_group", "art_group", "id_store", "id_article")]
    rv$h <- merge(data.table(forecast_date = seq(min(rv$h[, forecast_date]), max(rv$h[, forecast_date]), by="days")), rv$h, by=c("forecast_date"), all.x=TRUE)
    for (i in names(rv$h)){
      rv$h[is.na(get(i)), (i):=0]
    }
    tabela_wynik <- as.data.table(rv$h)
    return(tabela_wynik)
  })
  
  wykres1 <- eventReactive(input$generuj,{
    tabela_wynik<- tabela()[, .(forecast_quantity_stat = sum(forecast_quantity_stat, na.rm = T), forecast_quantity_corr= sum(forecast_quantity_corr, na.rm = T), forecast_value = sum(forecast_value, na.rm = T), sale_cnt = sum(sale_cnt, na.rm = T), sale_value = sum(sale_value, na.rm = T)) , by = "forecast_date"]
    
    ay <- list(
      overlaying = "y",
      side = "right",
      title = "sztuki",
      range = c(min(tabela_wynik$sale_cnt, tabela_wynik$forecast_quantity_stat, tabela_wynik$forecast_quantity_corr) / max(tabela_wynik$sale_value, tabela_wynik$forecast_value) * min(tabela_wynik$sale_value, tabela_wynik$forecast_value), as.numeric(max(tabela_wynik$sale_cnt, tabela_wynik$forecast_quantity_stat, tabela_wynik$forecast_quantity_corr))+1)
    )
    w <- plot_ly(tabela_wynik, x = ~forecast_date) %>%
      add_bars(y = ~sale_value, color = I("steelblue3"), name = "realizacja_zł", opacity = 0.3) %>%
      add_bars(y = ~forecast_value, color = I("firebrick3"), name = "prognoza_zł", opacity = 0.3) %>%
      add_lines(y = ~sale_cnt, color = I("steelblue2"), name = "realizacja_szt", yaxis = "y2") %>%
      add_lines(y = ~forecast_quantity_stat, color = I("firebrick2"), name = "prognoza_szt", yaxis = "y2") %>%
      add_lines(y = ~forecast_quantity_corr, color = I("orange"), name = "prognoza_szt_corr", yaxis = "y2") %>%
      layout(
        title = paste("Prognoza total per dzień", sep="") , yaxis2 = ay, 
        yaxis=list(range = c(as.numeric(min(tabela_wynik$sale_value, tabela_wynik$forecast_value)), as.numeric(max(tabela_wynik$sale_value, tabela_wynik$forecast_value))), title = "zł"),
        barmode = "group", xaxis = list(title="data")
        
      )
    return(w)
  }
  
  
  )
  
  output$prognoza_total <- renderPlotly({
    wykres1()
  })
  
  # observy na selectach

  
  observeEvent(input$menu ,{
    
    updateSelectizeInput(session, "sklep_input",
                      choices = input$sklepy,
                      options = list(closeAfterSelect = T))
  }, ignoreInit = T)
  
  observe({
    updateSelectizeInput(session, "pgrupa",
                      choices = unique(dane_real()[id_store==input$sklep_input][order(product_group)][, product_group]),
                      options = list(closeAfterSelect = T)
    )
  })
  
  observe({
    updateSelectizeInput(session, "anty_pgrupa",
                      choices = unique(dane_real()[id_store==input$sklep_input][order(product_group)][, product_group]),
                      options = list(closeAfterSelect = T)
    )
  })
  
  observe({
    if(is.null(input$pgrupa)){c =unique(dane_real()[id_store==input$sklep_input][order(type_group)][, type_group])}
    else{c = unique(dane_real()[id_store==input$sklep_input][product_group %in% input$pgrupa][order(type_group)][, type_group])}
    updateSelectizeInput(session, "wgrupa",
                      choices = c,
                      options = list(closeAfterSelect = T)
    )
  })
  
  
  observe({
    if(is.null(input$pgrupa)&is.null(input$wgrupa)){c =unique(dane_real()[id_store==input$sklep_input][order(art_group)][, art_group])}
    else if((!is.null(input$pgrupa))&(is.null(input$wgrupa))){c = unique(dane_real()[id_store==input$sklep_input][product_group %in% input$pgrupa][order(art_group)][, art_group])}
    else if((is.null(input$pgrupa))&(!is.null(input$wgrupa))){c = unique(dane_real()[id_store==input$sklep_input][type_group %in% input$wgrupa][order(art_group)][, art_group])}
    else{c = unique(dane_real()[id_store==input$sklep_input][product_group %in% input$pgrupa][type_group %in% input$wgrupa][order(art_group)][, art_group])}
    updateSelectizeInput(session, "agrupa",
                      choices = c,
                      options = list(closeAfterSelect = T)
    )
  })
  

  
  
  observe({
  if((!is.null(input$pgrupa))&(is.null(input$wgrupa))){c = unique(dane_real()[id_store==input$sklep_input][product_group %in% input$pgrupa][, id_article])}
  else if (!is.null(input$pgrupa)&!is.null(input$wgrupa)&is.null(input$agrupa)){c = unique(dane_real()[id_store==input$sklep_input][product_group %in% input$pgrupa]
                    [type_group %in% input$wgrupa][, id_article])}
  else if(!is.null(input$pgrupa)&!is.null(input$wgrupa)&!is.null(input$agrupa)){c = unique(dane_real()[id_store==input$sklep_input][product_group %in% input$pgrupa]
                    [type_group %in% input$wgrupa][art_group %in% input$agrupa][, id_article])}
  else if(is.null(input$pgrupa)&is.null(input$wgrupa)&!is.null(input$agrupa)){c = unique(dane_real()[id_store==input$sklep_input]
                                                                                           [art_group %in% input$agrupa][, id_article])} 
  else if(is.null(input$pgrupa)&!is.null(input$wgrupa)&is.null(input$agrupa)){c = unique(dane_real()[id_store==input$sklep_input]
                                                                                           [type_group %in% input$wgrupa][, id_article])}   
  else{c = unique(dane_real()[id_store==input$sklep_input]
                  [type_group %in% input$wgrupa][art_group %in% input$agrupa][, id_article])} 
    updateSelectizeInput(session, "artykul",
                      choices = c,
                      options = list(closeAfterSelect = T)
    )
  })
  
  
  observeEvent(input$cofnij, {
    if(NROW(rv$tabela_korekta_final)>0){
    showNotification("Bieżąca korekta została wycofana, działaj dalej :P", type = "warning")
    rv$tabela_korekta_final  <- setdiff(rv$tabela_korekta_final, rv$tabela_korekta)}
  })
  
  observeEvent(input$wyczysc, {
    showNotification("Tabela z korektami została wyczyszczona, zacznij od nowa.", type = "warning")
    rv$tabela_korekta_final = data.table(`Numer sklepu`=numeric(), `Numer artykułu`=numeric(), `Data od`=as.Date(character()), `Data do`=as.Date(character()), `Typ korekty` = numeric(), `Wartość korekty` = as.double(), `Typ prognozy` = numeric())
    rv$wszystkie_artykuly = c()  
     })
  
  updated_artykul <- eventReactive(input$symuluj, {
    if(input$radio=='Wybierz z listy'){
      return(input$artykul)}
    else{lista_art <- gsub(" ", "", input$lis_art)
    lista_art <- strsplit(lista_art, "\n")[[1]]
    return(lista_art)}})
  
  
  
  tabela_filtr <- eventReactive(input$symuluj, { 
    # przygotowanie i filtrowanie danych w różnych agregacjach
    
    
    rv$g<- as.data.table(dane_real())
    if(input$wybor_agregacji=="tygodniowa"){
    rv$h <- rv$g[, .(forecast_quantity_stat = sum(forecast_quantity_stat, na.rm = T), forecast_quantity_corr= sum(forecast_quantity_corr, na.rm = T), forecast_value = sum(forecast_value, na.rm = T), sale_cnt = sum(sale_cnt, na.rm = T), sale_value = sum(sale_value, na.rm = T)),
                 by=c("tydzien", "product_group","type_group", "art_group", "id_store","id_article")]
    rv$h <- merge(data.table(tydzien = format(seq(min(dane_real()[, forecast_date], na.rm = T), max(dane_real()[, forecast_date], na.rm = T), by="week"),"%Y-%V")), rv$h, by=c("tydzien"), all.x=TRUE)
    for (i in names(rv$h)){
      rv$h[is.na(get(i)), (i):=0]
    }
    tabela_filtr <- rv$h[, korekta := NULL]
    tabela_filtr <- tabela_filtr[, korekta_final := 1]
    
    temp <- data.table(tydzien=seq(as.Date(input$data_przedz[1]),as.Date(input$data_przedz[2]), by="week"))
    temp[, tydzien := format(tydzien, "%Y-%V")]
    
    tabela_filtr <- merge(tabela_filtr, temp, by=c("tydzien"), all.x=TRUE)
    daty <- data.table(tydzien = format(seq(min(rv$stok_sklep[, forecast_date], na.rm = T), max(rv$stok_sklep[, forecast_date], na.rm = T), by="week"),"%Y-%V"))
    stok_sklep <- rv$stok_sklep[, .(stok_sklepy = sum(stok_sklepy, na.rm = T)), by=c("tydzien", "id_store","id_article")]
    stok_sklep <- merge(stok_sklep, daty, by='tydzien', all.y = T)
    
    tabela_filtr <- merge(tabela_filtr, stok_sklep, by = c("id_store", "id_article", "tydzien"), all = T)
    
    }
    
    else{
      rv$h <- rv$g[, .(forecast_quantity_stat = sum(forecast_quantity_stat, na.rm = T), forecast_quantity_corr= sum(forecast_quantity_corr, na.rm = T), forecast_value = sum(forecast_value, na.rm = T), sale_cnt = sum(sale_cnt, na.rm = T), sale_value = sum(sale_value, na.rm = T)),
                   by=c("forecast_date", "product_group","type_group", "art_group", "id_store","id_article")]
      rv$h <- merge(data.table(forecast_date = seq(min(dane_real()[, forecast_date], na.rm = T), max(dane_real()[, forecast_date], na.rm = T), by="days")), rv$h, by=c("forecast_date"), all.x=TRUE)
      for (i in names(rv$h)){
        rv$h[is.na(get(i)), (i):=0]
      }
      tabela_filtr <- rv$h[, korekta := NULL]
      tabela_filtr <- tabela_filtr[, korekta_final := 1]
      
      temp <- data.table(forecast_date=seq(as.Date(input$data_przedz[1]),as.Date(input$data_przedz[2]), by="days"))

      tabela_filtr <- merge(tabela_filtr, temp, by=c("forecast_date"), all.x=TRUE)
      daty <- data.table(forecast_date = seq(min(rv$stok_sklep[, forecast_date], na.rm = T), max(rv$stok_sklep[, forecast_date], na.rm = T), by="days"))
      stok_sklep <- rv$stok_sklep[, .(stok_sklepy = sum(stok_sklepy, na.rm = T)), by=c("forecast_date", "id_store","id_article")]
      stok_sklep <- merge(stok_sklep, daty, by='forecast_date', all.y = T)
      
      tabela_filtr <- merge(tabela_filtr, stok_sklep, by = c("id_store", "id_article", "forecast_date"), all = T)
      
    }
    
    #filtry
    
    tabela_filtr1 <- tabela_filtr[id_store == input$sklep_input]

    if(input$radio=="Wybierz z listy"){
      if(!is.null(input$pgrupa)){tabela_filtr1 <- tabela_filtr1[product_group %in% input$pgrupa]}
      if(!is.null(input$anty_pgrupa)){tabela_filtr1 <- tabela_filtr1[!product_group %in% input$anty_pgrupa]}
      if(!is.null(input$wgrupa)){tabela_filtr1 <- tabela_filtr1[type_group %in% input$wgrupa]}
      if(!is.null(input$agrupa)){tabela_filtr1 <- tabela_filtr1[art_group %in% input$agrupa]}}
    if(!is.null(updated_artykul())){tabela_filtr1 <- tabela_filtr1[id_article %in% updated_artykul()]}
    
    rv$capacity <- as.data.table(rv$capacity1)
    rv$capacity <- rv$capacity[, capacity := as.numeric(capacity)]
    rv$capacity <- rv$capacity[id_store %in% tabela_filtr1[, id_store]]
    rv$capacity <- rv$capacity[id_article %in% tabela_filtr1[, id_article]]
    return(tabela_filtr1)
  })
  
  # przekształcenie danych pochodzących z inputów w ten sposób by wykres nie odświeżał się przy każdej zmianie
  
  updated_pgrupa <- eventReactive(input$symuluj, {input$pgrupa})
  
  updated_anty_pgrupa <- eventReactive(input$symuluj, {input$anty_pgrupa})
  
  updated_wgrupa <- eventReactive(input$symuluj, {input$wgrupa})
  
  updated_agrupa <- eventReactive(input$symuluj, {input$agrupa})
  
  updated_sklep <- eventReactive(input$symuluj, {input$sklep_input})
  
  updated_wybor_agregacji <- eventReactive(input$symuluj, {input$wybor_agregacji})

  updated_korekta <- eventReactive(input$symuluj, {input$korekta})
  
  updated_automat <- eventReactive(input$symuluj, {input$automat})

  updated_kierunek <- eventReactive(input$symuluj, {input$kierunek})

  updated_ograniczenie <- eventReactive(input$symuluj, {input$ograniczenie})
  
  output$prognoza_po_korektach <- renderPlotly({
    # przygotowanie danych do wykresu, w tym przeprowadzenie korekty automatycznej
    # wynikiem jest wykres po filtrowaniu i nałożeniu korekty
    
    
    req(tabela_filtr())
    
  
    tytul_pgrupa <- if(length(updated_pgrupa())<=3){paste(updated_pgrupa(), collapse=", ")}
    else{paste(updated_pgrupa()[1], updated_pgrupa()[2], updated_pgrupa()[3], "...", sep = ", ")}
    
    tytul_wgrupa<-if(length(updated_wgrupa())<=3){paste(updated_wgrupa(), collapse=", ")}
    else{paste(updated_wgrupa()[1], updated_wgrupa()[2], updated_wgrupa()[3], "...", sep = ", ")}
    
    tytul_agrupa<-if(length(updated_agrupa())<=3){paste(updated_agrupa(), collapse=", ")}
    else{paste(updated_agrupa()[1], updated_agrupa()[2], updated_agrupa()[3], "...", sep = ", ")}
    
    tabela_filtr_wykres <- as.data.table(tabela_filtr())
    
    if(updated_wybor_agregacji()=="tygodniowa"){
      
    if(updated_automat()=="tak"){
      
      # stworzenie tabel pomocniczych zawierających dane porównawcze do korekty automatycznej, Zsumowaną wartość realizacji za dany okres i prognozy
      
      real_do_korekty <- tabela_filtr_wykres[tydzien %in% format(seq(as.Date(input$data_przedz_auto[1]),as.Date(input$data_przedz_auto[2]), by="week"),"%Y-%V"), .(sale_cnt = sum(sale_cnt, na.rm = T)), by = c("id_store", "id_article")]
      prog_do_korekty <- tabela_filtr_wykres[tydzien %in% format(seq(as.Date(input$data_przedz[1]),as.Date(input$data_przedz[2]), by="week"),"%Y-%V"), .(forecast_quantity_stat = sum(forecast_quantity_stat, na.rm = T)), by = c("id_store", "id_article")]
      
      dane_do_korekty <- merge(real_do_korekty, prog_do_korekty, by = c("id_store", "id_article"), all = T)
      
      print(dane_do_korekty)
      
      # filtrowanie artykułów do tych które występują zarówno w realizacji jak i w prognozie, oraz mającuch miesięczną prognozę wyższą niż 10 sztuk  
      
      dane_do_korekty <- dane_do_korekty[id_article %in% real_do_korekty[, id_article]&id_article %in% prog_do_korekty[, id_article]]
      
      dane_do_korekty <- dane_do_korekty[!(forecast_quantity_stat<updated_ograniczenie())]
      
      
      # okres by wyliczyć średnie relizacji i prognozy i porównać je
      
      okres_prog <- NROW(as.data.table(format(seq(as.Date(input$data_przedz[1]),as.Date(input$data_przedz[2]), by="week"),"%Y-%V")))-1
      okres_real <- NROW(as.data.table(format(seq(as.Date(input$data_przedz_auto[1]),as.Date(input$data_przedz_auto[2]), by="week"),"%Y-%V")))-1

      # wyliczamy korektę automatyczną dla każdego artykułu dzieląc średnią realizacje z podanego okresu przez średnią prognozę z okresu na który ma być nałożona korekta
      # parametry:
      # - zakres od 0.9 do 1.1 korekty jest wyłączony
      # - bufor 0.1 na korektę zmniejszającą i 0.2 na zwiększającą
      # - ograniczenie na minimalną i maksymalną wartość korekty, default: 0.6 i 2.0
      
      dane_do_korekty[, korekta := round(as.numeric(sale_cnt/okres_real)/as.numeric(forecast_quantity_stat/okres_prog),2)][, korekta := ifelse(korekta<=1.1&korekta>=0.9, 1, korekta)][, korekta := ifelse(korekta>1, korekta-0.2, korekta+0.1)][, korekta := ifelse(korekta<=1.1&korekta>=0.9, 1, korekta)][, korekta := ifelse(korekta<input$korekta_przedz[1], input$korekta_przedz[1], korekta)][, korekta := ifelse(korekta>input$korekta_przedz[2], input$korekta_przedz[2], korekta)]
      
      dane_do_korekty <- dane_do_korekty[, korekta, by = c("id_store", "id_article")]
      dane_do_korekty <- na.omit(dane_do_korekty)
      dane_do_korekty <- dane_do_korekty[!is.na(korekta)][!(korekta==1)]
      
      # filtrowanie korekt w zależności od kierunku
      
      if(updated_kierunek()=="zmniejszająca"){
        dane_do_korekty <- dane_do_korekty[!(`korekta`>1)] 
      }else if(updated_kierunek()=="zwiększająca"){
        dane_do_korekty <- dane_do_korekty[!(`korekta`<1)] 
      }
      
      rv$dane_do_korekty <- dane_do_korekty[, korekta := round_any(korekta, 0.05)] 
      
      tabela_filtr_wykres <- merge(tabela_filtr_wykres, rv$dane_do_korekty, by = c("id_store", "id_article"), all.x = T)
    }else{
      tabela_filtr_wykres[, korekta := updated_korekta()]
    }
    # przemnożenie prognozy przez korektę
      
    tabela_filtr_wykres[, forecast_quantity_stat_new := ifelse(!is.na(korekta), forecast_quantity_stat * korekta, forecast_quantity_stat)]
    
    # przygotowanie dnych do wykresu
        
    tabela_filtr_wykres <- as.data.table(tabela_filtr_wykres[, .(stok_sklepy = sum(stok_sklepy, na.rm = T), forecast_quantity_stat = sum(forecast_quantity_stat, na.rm = T), forecast_quantity_stat_new = sum(forecast_quantity_stat_new, na.rm = T), forecast_quantity_corr= sum(forecast_quantity_corr, na.rm = T), forecast_value = sum(forecast_value, na.rm = T), sale_cnt = sum(sale_cnt, na.rm = T), sale_value = sum(sale_value, na.rm = T)), by=c("tydzien")])
    daty <- data.table(tydzien = format(seq(min(dane_real()[, forecast_date], na.rm = T), max(dane_real()[, forecast_date], na.rm = T), by="week"),"%Y-%V"))
    tabela_filtr_wykres <- merge(tabela_filtr_wykres, daty, by='tydzien', all.y = T)
    tabela_filtr_wykres[is.na(tabela_filtr_wykres)] <- 0
    tabela_filtr_wykres[!(tydzien %in% format(seq(as.Date(input$data_przedz[1]),as.Date(input$data_przedz[2]), by="week"),"%Y-%V")), forecast_quantity_stat_new := 0]

    # linia z ostatnim capacity na artykule
    capa <- sum(rv$capacity[, capacity], na.rm = T)
    print(paste("capacity: ",capa, sep=""))
    
    ay <- list(
      overlaying = "y",
      side = "right",
      title = "stok",
      range = c(as.numeric(max(tabela_filtr_wykres$stok_sklepy) / max(tabela_filtr_wykres$sale_cnt) * min(tabela_filtr_wykres$sale_cnt)),
                as.numeric(max(tabela_filtr_wykres[, stok_sklepy])))
    )
    
    plot_ly(data = tabela_filtr_wykres, x = ~tydzien) %>% 
      add_lines(y = ~sale_cnt, color = I("steelblue2"), name = "realizacja_szt") %>% 
      add_lines(y = ~forecast_quantity_stat, color = I("firebrick2"), name = "prognoza_szt") %>% 
      add_lines(y = ~forecast_quantity_corr, color = I("orange"), name = "prognoza_szt_corr") %>% 
      add_lines(y = ~forecast_quantity_stat_new, color = I("green"), name = "prognoza_szt_stat_new") %>% 
      add_lines(y = ~stok_sklepy, color = I("wheat"), name = "stok sklepowy", opacity = 0.7, yaxis = 'y2') %>%
      add_lines(y = capa, color = I("turquoise1"), name = "capacity", opacity = 0.7, yaxis = 'y2', if(is.null(updated_artykul())) {visible=FALSE}) %>%
      layout(
        title = if(is.null(updated_pgrupa())){paste("Prognoza, sklep: ",updated_sklep(), sep="")}
        else{paste("Prognoza: ", tytul_pgrupa, tytul_wgrupa, tytul_agrupa, ", sklep: ", updated_sklep(), sep=" ")},
        yaxis = list(title="sztuki", range = c(min(tabela_filtr_wykres$sale_cnt), max(max(tabela_filtr_wykres$sale_cnt),max(tabela_filtr_wykres$forecast_quantity_stat_new)))), xaxis = list(title="data"), yaxis2 = ay
      )
  
    
    }else{
      
      if(updated_automat()=="tak"){
        
        # stworzenie tabel pomocniczych zawierających dane porównawcze do korekty automatycznej, Zsumowaną wartość realizacji za dany okres i prognozy
        
        real_do_korekty <- tabela_filtr_wykres[forecast_date %in% seq(as.Date(input$data_przedz_auto[1]),as.Date(input$data_przedz_auto[2]), by="days"), .(sale_cnt = sum(sale_cnt, na.rm = T)), by = c("id_store", "id_article")]
        prog_do_korekty <- tabela_filtr_wykres[forecast_date %in% seq(as.Date(input$data_przedz[1]),as.Date(input$data_przedz[2]), by="days"), .(forecast_quantity_stat = sum(forecast_quantity_stat, na.rm = T)), by = c("id_store", "id_article")]
 
        dane_do_korekty <- merge(real_do_korekty, prog_do_korekty, by = c("id_store", "id_article"), all = T)
        
        # filtrowanie artykułów do tych które występują zarówno w realizacji jak i w prognozie, oraz mającuch miesięczną prognozę wyższą niż 10 sztuk  
        
        dane_do_korekty <- dane_do_korekty[id_article %in% real_do_korekty[, id_article]][id_article %in% prog_do_korekty[, id_article]]
        dane_do_korekty <- dane_do_korekty[!(forecast_quantity_stat<updated_ograniczenie())]
       
        # okres by wyliczyć średnie relizacji i prognozy i porównać je
        
        okres_prog <- NROW(as.data.table(seq(as.Date(input$data_przedz[1]),as.Date(input$data_przedz[2]), by="days")))-1
        okres_real <- NROW(as.data.table(seq(as.Date(input$data_przedz_auto[1]),as.Date(input$data_przedz_auto[2]), by="days")))-1
        
        # wyliczamy korektę automatyczną dla każdego artykułu dzieląc średnią realizacje z podanego okresu przez średnią prognozę z okresu na który ma być nałożona korekta
        # parametry:
        # - zakres od 0.9 do 1.1 korekty jest wyłączony
        # - bufor 0.1 na korektę zmniejszającą i 0.2 na zwiększającą
        # - ograniczenie na minimalną i maksymalną wartość korekty, default: 0.6 i 2.0
        
        dane_do_korekty[, korekta := round(as.numeric(sale_cnt/okres_real)/as.numeric(forecast_quantity_stat/okres_prog),2)][, korekta := ifelse(korekta<=1.1&korekta>=0.9, 1, korekta)][, korekta := ifelse(korekta>1, korekta-0.2, korekta+0.1)][, korekta := ifelse(korekta<=1.1&korekta>=0.9, 1, korekta)][, korekta := ifelse(korekta<input$korekta_przedz[1], input$korekta_przedz[1], korekta)][, korekta := ifelse(korekta>input$korekta_przedz[2], input$korekta_przedz[2], korekta)]
        
        dane_do_korekty <- dane_do_korekty[, korekta, by = c("id_store", "id_article")]
        dane_do_korekty <- na.omit(dane_do_korekty)
        dane_do_korekty <- dane_do_korekty[!is.na(korekta)][!(korekta==1)]
        
        # filtrowanie korekt w zależności od kierunku
        
        if(updated_kierunek()=="zmniejszająca"){
          dane_do_korekty <- dane_do_korekty[!(`korekta`>1)] 
        }else if(updated_kierunek()=="zwiększająca"){
          dane_do_korekty <- dane_do_korekty[!(`korekta`<1)] 
        }
        
        rv$dane_do_korekty <- dane_do_korekty[, korekta := round_any(korekta, 0.05)] 
        
        
        tabela_filtr_wykres <- merge(tabela_filtr_wykres, rv$dane_do_korekty, by = c("id_store", "id_article"), all.x = T)
        
      }else{
        tabela_filtr_wykres[, korekta := updated_korekta()]
      }
      
      # przemnożenie prognozy przez korektę
      
      
      tabela_filtr_wykres[, forecast_quantity_stat_new := ifelse(!is.na(korekta), forecast_quantity_stat * korekta, forecast_quantity_stat)]
      
      # przygotowanie dnych do wykresu
      
      tabela_filtr_wykres <- as.data.table(tabela_filtr_wykres[, .(stok_sklepy = sum(stok_sklepy, na.rm = T), forecast_quantity_stat = sum(forecast_quantity_stat, na.rm = T), forecast_quantity_stat_new = sum(forecast_quantity_stat_new, na.rm = T), forecast_quantity_corr= sum(forecast_quantity_corr, na.rm = T), forecast_value = sum(forecast_value, na.rm = T), sale_cnt = sum(sale_cnt, na.rm = T), sale_value = sum(sale_value, na.rm = T)), by=c("forecast_date")])
      
      daty <- data.table(forecast_date = seq(min(dane_real()[, forecast_date], na.rm = T), max(dane_real()[, forecast_date], na.rm = T), by="days"))
      
      tabela_filtr_wykres <- merge(tabela_filtr_wykres, daty, by="forecast_date", all.y = T)
      tabela_filtr_wykres[is.na(tabela_filtr_wykres)] <- 0
      tabela_filtr_wykres[!(forecast_date %in% seq(as.Date(input$data_przedz[1]),as.Date(input$data_przedz[2]), by="days")), forecast_quantity_stat_new := 0]
      
    # linia z ostatnim capacity na artykule
        
    capa <- sum(rv$capacity[, capacity], na.rm = T)
    print(paste("capacity: ",capa, sep=""))
    
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "stok",
        range = c(as.numeric(max(tabela_filtr_wykres$stok_sklepy) / max(tabela_filtr_wykres$sale_cnt) * min(tabela_filtr_wykres$sale_cnt)),
                  as.numeric(max(tabela_filtr_wykres[, stok_sklepy])))
      )
      
    plot_ly(data = tabela_filtr_wykres, x = ~forecast_date) %>% 
      add_lines(y = ~sale_cnt, color = I("steelblue2"), name = "realizacja_szt") %>% 
      add_lines(y = ~forecast_quantity_stat, color = I("firebrick2"), name = "prognoza_szt") %>% 
      add_lines(y = ~forecast_quantity_corr, color = I("orange"), name = "prognoza_szt_corr") %>% 
      add_lines(y = ~forecast_quantity_stat_new, color = I("green"), name = "prognoza_szt_stat_new") %>% 
      add_lines(y = ~stok_sklepy, color = I("wheat"), name = "stok sklepowy", opacity = 0.7, yaxis = 'y2') %>%
      add_lines(y = capa, color = I("turquoise1"), name = "capacity", opacity = 0.7, yaxis = 'y2', if(is.null(updated_artykul())) {visible=FALSE}) %>%
      layout(
        title = if(is.null(updated_pgrupa())){paste("Prognoza, sklep: ",updated_sklep(), sep="")}
        else{paste("Prognoza: ", tytul_pgrupa, tytul_wgrupa, tytul_agrupa, ", sklep: ", updated_sklep(), sep=" ")},
        yaxis = list(title="sztuki", range = c(min(tabela_filtr_wykres$sale_cnt), max(max(tabela_filtr_wykres$sale_cnt),max(tabela_filtr_wykres$forecast_quantity_stat_new)))), xaxis = list(title="data"), yaxis2 = ay
      )
    }
    
  })
  
  tabela_pobierz <- eventReactive(input$symuluj,{
    tabela_pobierz <- merge(tabela_filtr(), rv$capacity, by = c("id_store", "id_article"), all = T)
    View(tabela_pobierz)
    return(tabela_pobierz)
  })
  
  output$pobierz <- downloadHandler(
    
    filename = function() {
      paste(sciezka, "/dane_sklep_",updated_sklep(), ".xlsx", sep="")},
    content = function(file) {
      write.xlsx(tabela_pobierz(), file, sep=";", dec=",")
    }
    
  )
  
  observeEvent(input$zatwierdz,{
    # stworzenie tabeli finalnej
    
    
    print("jestem w ostatnim bloku")
    data_od <- input$data_przedz[1]
    data_do <- input$data_przedz[2]
    rv$temp1 <- dane_real()[, .(min1 = min(forecast_date), max1=max(forecast_date)), by=c("id_store", "id_article")]
    rv$g <- as.data.table(dane_real())
    
    if(input$radio=="Wybierz z listy"){
    if(!is.null(updated_pgrupa())){rv$g <- as.data.table(rv$g)[product_group %in% updated_pgrupa()]}
    if(!is.null(updated_anty_pgrupa())){rv$g <- as.data.table(rv$g)[!product_group %in% updated_anty_pgrupa()]}
    if(!is.null(updated_wgrupa())){rv$g <- as.data.table(rv$g)[type_group %in% updated_wgrupa()]}
    if(!is.null(updated_agrupa())){rv$g <- as.data.table(rv$g)[art_group %in% updated_agrupa()]}}
    if(!is.null(updated_artykul())){rv$g <- as.data.table(rv$g)[id_article %in% updated_artykul()]}
    

    rv$temp1 <- rv$temp1[id_store %in% updated_sklep()]
    rv$tabela_korekta <- data.table()
    rv$tabela_korekta <- as.data.table(unique(rv$g[forecast_date >= Sys.Date() & forecast_date <= data_do, .(id_store, id_article)]))
    rv$tabela_korekta <- merge(rv$tabela_korekta, rv$temp1, by=c("id_article", "id_store"))
    if(updated_automat()=="tak"){
    rv$tabela_korekta <- merge(rv$tabela_korekta, rv$dane_do_korekty, by=c("id_article", "id_store"), all.x = T)}
    else{rv$tabela_korekta <- rv$tabela_korekta[, korekta := updated_korekta()]}

    rv$tabela_korekta <- rv$tabela_korekta[, .(`Numer sklepu` = as.numeric(updated_sklep()), `Numer artykułu` = id_article, `Data od` = as.Date(data_od, origin = "1970-01-01"), `Data do`= as.Date(data_do, origin = "1970-01-01"), `Typ korekty` = 1, `Wartość korekty` = as.numeric(korekta), `Typ prognozy` = 0)]
    
    if(updated_kierunek()=="zmniejszająca"){
    rv$tabela_korekta <- rv$tabela_korekta[!(`Wartość korekty`>1)] 
    }else if(updated_kierunek()=="zwiększająca"){
      rv$tabela_korekta <- rv$tabela_korekta[!(`Wartość korekty`<1)] 
    }
    rv$tabela_korekta <- rv$tabela_korekta[!is.na(`Wartość korekty`)]
    rv$tabela_korekta <- rv$tabela_korekta[!(`Wartość korekty`==1)]
      
    # obsłużenie błędu z nakładaniem się korekt
    
    temp_check <- merge(rv$tabela_korekta, rv$tabela_korekta_final, by=c("Numer sklepu", "Numer artykułu"), allow.cartesian=T, suffixes = c('.new','.old' ))
    temp_check_flag <- temp_check[, flag := ifelse(((`Data od.new`<=`Data do.old`&`Data do.new`>=`Data od.old`)|(`Data do.new`>=`Data od.old`&`Data od.new`<=`Data do.old`)|(`Data od.new`>=`Data od.old`&`Data do.new`<=`Data do.old`)|(`Data do.new`==`Data od.old`&`Data od.new`==`Data do.old`)), 1, 0)]
    if(NROW(temp_check_flag[`Numer artykułu` %in% rv$tabela_korekta$`Numer artykułu`][flag==1])==0){
      showNotification(paste("Korekta została zatwierdzona. Ilość dodanych wierszy: ", NROW(rv$tabela_korekta), ", średni wpis: ", round(mean(rv$tabela_korekta[, `Wartość korekty`]), 2),  sep = ""), type = "message", duration = 20)
      rv$tabela_korekta_final <- rbind(rv$tabela_korekta, rv$tabela_korekta_final)
    }else{
      showNotification(paste("Daty na sklepo-artykule nakładają się. Np. na: Sklepie ", temp_check_flag[flag==1][1, `Numer sklepu`],
                             ", artykule ", temp_check_flag[flag==1][1, `Numer artykułu`], ", przy następujących datach poprzedniej korekty: ",
                             rv$tabela_korekta_final[1, `Data od`],", ", rv$tabela_korekta_final[1, `Data do`], sep=""), type = "error", duration = NULL)
      rv$tabela_korekta <- rv$tabela_korekta[!(`Numer artykułu` %in% temp_check_flag$`Numer artykułu`)]
    }
    
    rv$wszystkie_artykuly <- unique(append(rv$wszystkie_artykuly, rv$tabela_korekta[, `Numer artykułu`]))
    print(rv$wszystkie_artykuly)
    rv$tabela_korekta_final <- rv$tabela_korekta_final[`Numer artykułu` %in% rv$wszystkie_artykuly]
    rv$tabela_korekta_final <- unique(rv$tabela_korekta_final)
    rv$wszystkie_sklepy <- unique(rv$tabela_korekta_final[, `Numer sklepu`])
   
    print(rv$tabela_korekta_final)
  })
  
  output$tabela_z_korektami = DT::renderDataTable({
    DT::datatable(rv$tabela_korekta_final, options = list(paging = TRUE, searching = FALSE))
  })
  
  output$zapisz <- downloadHandler(
    
    filename = function() {
      paste(sciezka, "/plik_korekty_",  paste(rv$wszystkie_sklepy, collapse="_") ,"_", Sys.Date(),".xlsx", sep="")},
    content = function(file) {
      write.xlsx(rv$tabela_korekta_final, file, sep=";", dec=",")
    }
    
    )
  
  rv2 <- reactiveValues(stare_calc_date = 0, nowe_calc_date = 0)
  

  
  
  dane_porownanie <- eventReactive(input$przelicz_porownanie, {
    
    sklepy <-  paste(shQuote(input$sklep_porownanie, type="csh"), collapse=", ")
    
    daty_prognozy <- c(input$data_prognozy_1, input$data_prognozy_2)
    daty_prognozy <-  paste(shQuote(daty_prognozy, type="csh"), collapse=", ") 
    
    s <- Sys.time()
    process_step_run_id_full <- dbGetQuery(con, paste("SELECT count(*) maximum , calculation_date, process_step_run_id, process_run_id, calculation_id
                                                     FROM detail_sas.sales_forecast_hist 
                                                     WHERE calculation_date in (", daty_prognozy ,")      
                                                     GROUP BY calculation_date, process_step_run_id, process_run_id, calculation_id
                                                     order by maximum desc;", sep=""))  
    s1<- Sys.time()
    print(s1-s)
    
    process_step_run_id_full <- as.data.table(process_step_run_id_full)
    process_step_run_id_full <- setDT(process_step_run_id_full)[order(-maximum)][,.SD[1,], by = .(calculation_date)] 
    
    calculation_date <-  paste(shQuote(process_step_run_id_full[, calculation_date], type="csh"), collapse=", ")
    process_step_run_id <-  paste(shQuote(process_step_run_id_full[,process_step_run_id], type="csh"), collapse=", ")
    process_run_id <-  paste(shQuote(process_step_run_id_full[, process_run_id], type="csh"), collapse=", ")
    calculation_id <-  paste(shQuote(process_step_run_id_full[, calculation_id], type="csh"), collapse=", ")
    
    first_date = as.Date(substring(calculation_date, 16,25))
    second_date <- first_date + 60
    # kwerenda realizująca wydobycie tabeli z prognozami
    
    start_time <- Sys.time()
    
    prognozy_dla_sklepow  <- dbGetQuery(con, paste("select SAS.calculation_date as calculation_date,
                                           SAS.id_store as id_store,
                                           SAS.id_article as id_article,
                                           SAS.forecast_date as forecast_date,
                                           DICT.product_group as product_group,
                                           DICT.type_group as type_group,
                                           sum(SAS.forecast_quantity_stat) as forecast_quantity_stat,
                                           sum(SAS.forecast_quantity_corr) as forecast_quantity_corr,
                                           sum(SAS.forecast_value) as forecast_value
                                           from detail_sas.sales_forecast_hist SAS
                                           join dict.dic_articles DICT
                                           ON SAS.id_article = DICT.id_article
                                           WHERE id_store in (", sklepy, ")
                                           AND process_step_run_id in (", process_step_run_id, ")
                                           and process_run_id in (", process_run_id, ")
                                           and calculation_id in (", calculation_id, ")
                                           AND forecast_date >= '", first_date  ,"'
                                           AND forecast_date <= '", second_date ,"'
                                          group by SAS.id_store, DICT.product_group, DICT.type_group, SAS.id_article,
                                          SAS.forecast_date, SAS.calculation_date;", sep=""))
    end_time <- Sys.time()
    print(end_time-start_time)
    start_time <- Sys.time()
    
    #kwerenda realizująca wydobycie tabeli z realizacjami
    
    
    realizacje_dla_sklepow <- dbGetQuery(con, paste("SELECT
                                         REAL.id_store as id_store,
                                         REAL.id_article as id_article,
                                         DICT.product_group as product_group,
                                         DICT.type_group as type_group,
                                         REAL.working_date as working_date,
                                         sum(REAL.sale_cnt) as sale_cnt,
                                         sum(REAL.sale_value) as sale_value
                                         FROM detail_sales.tickets REAL
                                         join dict.dic_articles DICT
                                         ON REAL.id_article = DICT.id_article 
                                         WHERE REAL.id_store in (", sklepy ,") AND REAL.working_date >= '", Sys.Date() -460  ,"' 
                                         GROUP BY REAL.id_article, REAL.id_store, REAL.working_date, DICT.product_group,
                                          DICT.type_group;", sep=""))
    
    end_time <- Sys.time()
    print(end_time-start_time)
    
    
    #przekształcenie tabel z prognozami i realizacjami
    
    prognozy_dla_sklepow <- as.data.table(prognozy_dla_sklepow)
    prognozy_dla_sklepow <- na.omit(prognozy_dla_sklepow)
    realizacje_dla_sklepow <- as.data.table(realizacje_dla_sklepow)
    realizacje_dla_sklepow <- na.omit(realizacje_dla_sklepow)
    
    
    #połączenie tabeli;
    
    prognozy_i_realizacje <- merge(prognozy_dla_sklepow, realizacje_dla_sklepow,
                                   by.x=c("id_store","product_group", "id_article", "type_group", "forecast_date"), by.y=c("id_store", "product_group", "id_article", "type_group", "working_date"), all=TRUE)
    
    rv2$stare_calc_date <-min(process_step_run_id_full[, calculation_date])
    rv2$nowe_calc_date <-max(process_step_run_id_full[, calculation_date])
    print("zakończyłem obliczać dane_porownanie")
    
    return(prognozy_i_realizacje)
  })
  
  output$tabela = DT::renderDataTable({
    req(dane_porownanie())
    DT::datatable(NULL)
  })
  
  observe({
    updateSelectizeInput(session, "pgrupa_por",
                         choices = unique(dane_porownanie()[id_store==input$sklep_porownanie][order(product_group)][, product_group]),
                         options = list(closeAfterSelect = T)
    )
  })
  
  observe({
    if(is.null(input$pgrupa_por)){c =unique(dane_porownanie()[id_store==input$sklep_porownanie][order(type_group)][, type_group])}
    else{c = unique(dane_porownanie()[id_store==input$sklep_porownanie][product_group %in% input$pgrupa_por][order(type_group)][, type_group])}
    updateSelectizeInput(session, "wgrupa_por",
                         choices = c,
                         options = list(closeAfterSelect = T)
    )
  })

  
  
  tabela_filtr_por <- eventReactive(input$generuj_porownanie, { 
    req(dane_porownanie())
    
    g<- as.data.table(dane_porownanie())
    
    h <- g[, .(forecast_quantity_stat = sum(forecast_quantity_stat, na.rm = T), forecast_quantity_corr= sum(forecast_quantity_corr, na.rm = T), 
               forecast_value = sum(forecast_value, na.rm = T), sale_cnt = sum(sale_cnt, na.rm = T), sale_value = sum(sale_value, na.rm = T)), 
           by=c("forecast_date", "calculation_date","product_group", "id_article", "type_group", "id_store")]
    h <- merge(data.table(forecast_date = seq(input$data_przedz_por[1], input$data_przedz_por[2], by="days")), h, by=c("forecast_date"), all.x=TRUE)
    for (i in names(h)){
      h[is.na(get(i)), (i):=0]
    }
    tabela_filtr <- as.data.table(h)
    tabela_filtr1 <- tabela_filtr[id_store == input$sklep_porownanie]
    
    print(tabela_filtr1)
   
    
      if(!is.null(input$pgrupa_por)){tabela_filtr1 <- tabela_filtr1[product_group %in% input$pgrupa_por]}
      if(!is.null(input$wgrupa_por)){tabela_filtr1 <- tabela_filtr1[type_group %in% input$wgrupa_por]}

    print("zakończyłem obliczać tabelę filtr")
    
    return(tabela_filtr1)
  })
  
  updated_sklep_porownanie <- eventReactive(input$generuj_porownanie, {input$sklep_porownanie})
  
  output$wykres_porownanie <- renderPlotly({
    req(tabela_filtr_por())
    tab_wykres_por <- as.data.table(tabela_filtr_por())
    
    tab_wykres_por <- tab_wykres_por[, .(sale_cnt = sum(sale_cnt, na.rm = T), forecast_quantity_corr= sum(forecast_quantity_corr, na.rm = T)), by = c("forecast_date", "calculation_date")]

    tab_wykres_por[is.na(tab_wykres_por)] <- 0
    
    sklep = updated_sklep_porownanie()
    #generowanie wykresu i przekształcenia mające na celu oddzielenie prognoz z różnymi "calculation_date"
    # sposób data.table
    
    plot_ly() %>%
      add_lines(data = tab_wykres_por, x = ~forecast_date, 
                y = ~sale_cnt, color = I("steelblue2"), name = "realizacja") %>%
      add_lines(data = tab_wykres_por[calculation_date==rv2$stare_calc_date],
                x = ~forecast_date, y = ~forecast_quantity_corr, color = I("firebrick2"), name = "prognoza 2")  %>%
      add_lines(data = tab_wykres_por[calculation_date==rv2$nowe_calc_date],
                x = ~forecast_date, y = ~forecast_quantity_corr, color = I("orange"), name = "prognoza 1")  %>%
      layout(
        title = paste("Weryfikacja prognoz na sztukach dla sklepu nr: ", sklep, sep=""), yaxis = list(range = c(0, as.numeric(max(tab_wykres_por[, sale_cnt])))))
  })

  output$wykres_porownanie_zl <- renderPlotly({
    req(tabela_filtr_por())
    tab_wykres_por <- as.data.table(tabela_filtr_por())
    
    tab_wykres_por <- tab_wykres_por[, .(sale_value = sum(sale_value, na.rm = T), forecast_value= sum(forecast_value, na.rm = T)), by = c("forecast_date", "calculation_date")]

    tab_wykres_por[is.na(tab_wykres_por)] <- 0
    
    sklep = updated_sklep_porownanie()
    #generowanie wykresu i przekształcenia mające na celu oddzielenie prognoz z różnymi "calculation_date"
    # sposób data.table

    plot_ly() %>%
      add_lines(data = tab_wykres_por, x = ~forecast_date,
                y = ~sale_value, color = I("steelblue2"), name = "realizacja") %>%
      add_lines(data = tab_wykres_por[calculation_date==rv2$stare_calc_date],
                x = ~forecast_date, y = ~forecast_value, color = I("firebrick2"), name = "prognoza 2")  %>%
      add_lines(data = tab_wykres_por[calculation_date==rv2$nowe_calc_date],
                x = ~forecast_date, y = ~forecast_value, color = I("orange"), name = "prognoza 1")  %>%
      layout(
        title = paste("Weryfikacja prognoz na obrocie dla sklepu nr: ", sklep, sep=""), yaxis = list(range = c(0, as.numeric(max(tab_wykres_por[, sale_value])))))
  })

  
  dane_kanibalizm <- eventReactive(input$generuj_kan,{
    sciezka <- "Z:/USERS/DZ_Magazyn/DZIAL ANALIZ/SEKCJA PROGNOZOWANIA/ML_Forecasts"
    kanibalizacja <- fread(paste(sciezka, "/Dane_do_generatorów/15_perfect_distance.csv", sep=""))
    kanibalizacja <- kanibalizacja[, data := as.Date(fastPOSIXct(data))]
    
    sklep <- input$sklep_kanibalizacja
    obrot <- dbGetQuery(con, paste("SELECT REAL.id_store as id_store, sum(REAL.sale_value) as sale_value, REAL.working_date as working_date
                               FROM detail_sales.tickets REAL WHERE REAL.id_store in (", sklep ,") AND REAL.working_date >= '2017-01-01'
                               GROUP BY REAL.id_store, REAL.working_date", sep=""))
    kanibalizacja_podglad <- kanibalizacja[id_store == sklep]
    kanibalizacja_podglad <- merge(kanibalizacja_podglad, obrot, by.x=c("data"), by.y=c("working_date"))
    kanibalizacja_podglad <- kanibalizacja_podglad[sale_value > 0.01]
    return(kanibalizacja_podglad)                               
  })
  
  updated_sklep_kan <- eventReactive(input$generuj_kan, {input$sklep_kanibalizacja})
  
  output$wykres_kan <- renderPlotly({
    req(dane_kanibalizm())
    sklep <- updated_sklep_kan()
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "obrót"
    )
    
    plot_ly(dane_kanibalizm(), x = ~data) %>%
      add_lines(y = ~closest_neighbour, name = 'closest_neighbour',mode = 'lines', color = I('purple'), type = 'scatter') %>%
      add_lines(y = ~neighbours_r_1km, name = 'neighbours_r_1km',mode = 'lines', color = I('cornflowerblue'), type = 'scatter') %>%
      add_lines(y = ~neighbours_r_3km, name = 'neighbours_r_3km',mode = 'lines', color = I('green'), type = 'scatter') %>%
      add_lines(y = ~neighbours_r_5km, name = 'neighbours_r_5km',mode = 'lines', color = I('red'), type = 'scatter') %>% 
      add_lines(y = ~sale_value, name = 'obrót',mode = 'lines', color = I('orange'), type = 'scatter', yaxis = "y2", opacity = 0.3) %>% 
      layout(
        title = paste("Sklepy w pobliżu, a obrót dla sklepu: ", sklep, sep="") , yaxis2 = ay,
        xaxis = list(title="x")
      )
  })
  
  dane_alert <- eventReactive(input$generuj_alert,{
    
    if(input$wybor_alertu=='48'){
      
      sklepy_z_alertem_48 <- paste(shQuote(input$sklep_alert, type="csh"), collapse=", ")
      data_alertu <- substring(nazwa_48, 10,17)
      data_prognozy <- as.Date(data_alertu,format = "%Y %m %d")
      data_prognozy_30dni <- data_prognozy - 30
      daty_prognozy_48 <- append(data_prognozy,data_prognozy_30dni)
      daty_prognozy_48 <-  paste(shQuote(daty_prognozy_48, type="csh"), collapse=", ")
      
      #Wysłanie zapytania w celu wydobycia tabeli "process_step_run_id_full" z informacjami identyfikującymi konkretne prognozy oraz przekształcenie jej
      
      start_time <- Sys.time()
      process_step_run_id_full <- dbGetQuery(con, paste("SELECT count(*) maximum , calculation_date, process_step_run_id, process_run_id, calculation_id
                                                  FROM detail_sas.sales_forecast_hist 
                                                  WHERE calculation_date in (", daty_prognozy_48 ,")      
                                                  GROUP BY calculation_date, process_step_run_id, process_run_id, calculation_id
                                                  order by maximum desc;", sep=""))
      end_time <- Sys.time()
      print(end_time-start_time)  
      
      process_step_run_id_full <- as.data.table(process_step_run_id_full)
      process_step_run_id_full <- setDT(process_step_run_id_full)[order(-maximum)][,.SD[1,], by = .(calculation_date)]
      rv$process_step_run_id_full_48 <- as.data.table(process_step_run_id_full)
      
      
      calculation_date <-  paste(shQuote(process_step_run_id_full[, calculation_date], type="csh"), collapse=", ")
      process_step_run_id <-  paste(shQuote(process_step_run_id_full[,process_step_run_id], type="csh"), collapse=", ")
      process_run_id <-  paste(shQuote(process_step_run_id_full[, process_run_id], type="csh"), collapse=", ")
      calculation_id <-  paste(shQuote(process_step_run_id_full[, calculation_id], type="csh"), collapse=", ")
      
      first_date = as.Date(substring(calculation_date, 16,25))
      second_date <- first_date + 60
      
      #kwerenda realizująca wydobycie tabeli z prognozami 
      
      start_time <- Sys.time()
      
      prognozy_dla_sklepow_z_alertem  <- dbGetQuery(con, paste("select SAS.calculation_date,
                                         SAS.id_store as id_store,
                                         SAS.forecast_date as forecast_date,
                                         sum(SAS.forecast_quantity_stat) as forecast_quantity_stat,
                                         sum(SAS.forecast_quantity_corr) as forecast_quantity_corr,
                                         sum(SAS.forecast_value) as forecast_value
                                         from detail_sas.sales_forecast_hist SAS
                                         WHERE id_store in (", sklepy_z_alertem_48, ") 
                                         AND process_step_run_id in (", process_step_run_id, ") 
                                         and process_run_id in (", process_run_id, ") 
                                         and calculation_id in (", calculation_id, ") 
                                         AND forecast_date >= '", first_date  ,"' 
                                         AND forecast_date <= '", second_date ,"'
                                        group by SAS.id_store,
                                        SAS.forecast_date, SAS.calculation_date;", sep=""))
      end_time <- Sys.time()
      print(end_time-start_time)
      prognozy_dla_sklepow_z_alertem
      start_time <- Sys.time()
      
      #kwerenda realizująca wydobycie tabeli z realizacjami
      
      realizacje_dla_sklepow_z_alertem <- dbGetQuery(con, paste("SELECT
                                         REAL.id_store as id_store,
                                         REAL.date_tran_warsaw_d,
                                         sum(REAL.sale_cnt) as sale_cnt,
                                         sum(REAL.sale_value) as sale_value
                                         FROM detail_sales.tickets REAL
                                         WHERE id_store in (", sklepy_z_alertem_48 ,") AND REAL.date_tran_warsaw_d >= '", first_date  ,"' 
                                         GROUP BY REAL.id_store, REAL.date_tran_warsaw_d"))
      
      end_time <- Sys.time()
      print(end_time-start_time)
      realizacje_dla_sklepow_z_alertem
      
      
      #przekształcenie tabel z prognozami i realizacjami
      
      prognozy_dla_sklepow_z_alertem <- as.data.table(prognozy_dla_sklepow_z_alertem)
      prognozy_dla_sklepow_z_alertem <- na.omit(prognozy_dla_sklepow_z_alertem)
      realizacje_dla_sklepow_z_alertem <- as.data.table(realizacje_dla_sklepow_z_alertem)
      realizacje_dla_sklepow_z_alertem <- na.omit(realizacje_dla_sklepow_z_alertem)
      
      
      #połączenie tabeli;
      
      prognozy_i_realizacje <- merge(prognozy_dla_sklepow_z_alertem, realizacje_dla_sklepow_z_alertem,
                                     by.x=c("id_store", "forecast_date"), by.y=c("id_store", "date_tran_warsaw_d"), all=TRUE)
      
      return(prognozy_i_realizacje)}else{
        
        ################################################57###############################################################  
        
        
        sklepy_z_alertem_57 <- paste(shQuote(input$sklep_alert, type="csh"), collapse=", ")
        data_alertu_57 <- substring(nazwa_57, 10,17)
        data_prognozy_57 <-   as.Date(data_alertu_57,format = "%Y %m %d")
        data_prognozy_wczoraj <- data_prognozy_57 - 1
        daty_prognozy_57 <- append(data_prognozy_57,data_prognozy_wczoraj)
        daty_prognozy_57 <- paste(shQuote(daty_prognozy_57, type="csh"), collapse=", ")
        
        #Przekształcenie tabelki z alertem 57 tak by wyciągnąć odpowiednie daty z diffDays
        
        sklepy_z_alertem_57_d[, dlugosc := str_count(diffDays, ',')+1]
        vector_lengths <- ((sklepy_z_alertem_57_d[,4]))
        maximum_length <- lapply(vector_lengths, function(x) x[which.max(abs(x))])
        maximum_length <- as.integer(maximum_length) 
        maximum_length[1]
        
        sklepy_z_alertem_57_d <- setDT(sklepy_z_alertem_57_d)[,paste0("diffDays_", 1:maximum_length[1]) := tstrsplit(diffDays, ",")][,-"diffDays", with=F]
        
        colA = paste("diffDays_", 1:maximum_length, sep = "")
        sklepy_z_alertem_57_unpivot <-  melt(sklepy_z_alertem_57_d, id.vars = c("StoreId"),
                                             measure.vars = colA)
        
        sklepy_z_alertem_57_unpivot <- sklepy_z_alertem_57_unpivot[, variable :=NULL]
        sklepy_z_alertem_57_unpivot <- na.omit(sklepy_z_alertem_57_unpivot)
        str(sklepy_z_alertem_57_unpivot)
        
        sklepy_z_alertem_57_unpivot[, Daty:= as.Date(value)]
        str(sklepy_z_alertem_57_unpivot[, Daty])
        
        
        #Wysłanie zapytania w celu wydobycia tabeli "process_step_run_id_full" z informacjami identyfikującymi konkretne prognozy oraz przekształcenie jej
        
        start_time <- Sys.time()
        process_step_run_id_full <-dbGetQuery(con, paste(
          "SELECT count(*) maximum , calculation_date, process_step_run_id, process_run_id, calculation_id
        FROM detail_sas.sales_forecast_hist 
        WHERE calculation_date in (", daty_prognozy_57 ,")      
        GROUP BY calculation_date, process_step_run_id, process_run_id, calculation_id
        order by maximum desc;", sep=""))
        end_time <- Sys.time()
        print(end_time-start_time)  
        process_step_run_id_full <- as.data.table(process_step_run_id_full)
        process_step_run_id_full<- setDT(process_step_run_id_full)[order(-maximum)][,.SD[1,], by = .(calculation_date)]
        rv$process_step_run_id_full_57 <- as.data.table(process_step_run_id_full)
        
        
        
        calculation_date <-  paste(shQuote(process_step_run_id_full[, calculation_date], type="csh"), collapse=", ")
        process_step_run_id <-  paste(shQuote(process_step_run_id_full[,process_step_run_id], type="csh"), collapse=", ")
        process_run_id <-  paste(shQuote(process_step_run_id_full[, process_run_id], type="csh"), collapse=", ")
        calculation_id <-  paste(shQuote(process_step_run_id_full[, calculation_id], type="csh"), collapse=", ")
        first_date = as.Date(substring(calculation_date, 16,25))
        second_date <- first_date + 60
        
        process_step_run_id_full
        
        start_time <- Sys.time()
        
        #kwerenda realizująca wydobycie tabeli z prognozami 
        
        prognozy_dla_sklepow_z_alertem_57  <- dbGetQuery(con, paste("select SAS.calculation_date,
                                         SAS.id_store as id_store,
                                         SAS.forecast_date as forecast_date,
                                         sum(SAS.forecast_quantity_stat) as forecast_quantity_stat,
                                         sum(SAS.forecast_quantity_corr) as forecast_quantity_corr,
                                         sum(SAS.forecast_value) as forecast_value
                                         from detail_sas.sales_forecast_hist SAS
                                         WHERE id_store in (", sklepy_z_alertem_57 ,") 
                                         AND process_step_run_id in (", process_step_run_id, ") 
                                         and process_run_id in (", process_run_id, ") 
                                         and calculation_id in (", calculation_id, ") 
                                         AND forecast_date >= '", first_date  ,"' 
                                         AND forecast_date <= '", second_date ,"'
                                        group by SAS.id_store,
                                        SAS.forecast_date, SAS.calculation_date;", sep=""))
        end_time <- Sys.time()
        print(end_time-start_time)
        
        #odpowiednie przekształcenia tabel do połączenia
        prognozy_dla_sklepow_z_alertem_57 <- as.data.table(prognozy_dla_sklepow_z_alertem_57)
        prognozy_dla_sklepow_z_alertem_57 <- na.omit(prognozy_dla_sklepow_z_alertem_57)
        
        sklepy_z_alertem_57_unpivot = subset(sklepy_z_alertem_57_unpivot, select = -c(value))
        
        #połączenie tabel
        
        prognozy_i_daty_alertow_57 <- merge(prognozy_dla_sklepow_z_alertem_57, sklepy_z_alertem_57_unpivot,
                                            by.x=c("id_store"), by.y=c("StoreId"), all.x=TRUE, allow.cartesian = TRUE)
        
        return(prognozy_i_daty_alertow_57)}
  })
  
  updated_alert <- eventReactive(input$generuj_alert, {input$wybor_alertu})
  
  updated_sklep_alert <- eventReactive(input$generuj_alert, {input$sklep_alert})
  
  
  output$alert_wykres <- renderPlotly({
    req(dane_alert())
    #wyodrębnienie calc_date: stare i nowe ##
    if (updated_alert()=='48'){
      stare_calc_date <-min(rv$process_step_run_id_full_48[, calculation_date])
      nowe_calc_date <-max(rv$process_step_run_id_full_48[, calculation_date])
      sklep = updated_sklep_alert()
      #generowanie wykresu i przekształcenia mające na celu oddzielenie prognoz z różnymi "calculation_date"
      # sposób data.table
      
      plot_ly() %>%
        add_lines(data = dane_alert()[calculation_date==stare_calc_date & id_store==sklep], x = ~forecast_date, 
                  y = ~sale_cnt, color = I("steelblue2"), name = "realizacja") %>%
        add_lines(data = dane_alert()[calculation_date==stare_calc_date & id_store==sklep],
                  x = ~forecast_date, y = ~forecast_quantity_corr, color = I("firebrick2"), name = "prognoza sprzed 30 dni")  %>%
        add_lines(data = dane_alert()[calculation_date==nowe_calc_date & id_store==sklep],
                  x = ~forecast_date, y = ~forecast_quantity_corr, color = I("firebrick3"), name = "prognoza dzisiejsza")  %>%
        layout(
          title = paste("Prognozy i realizacje z alertem nr 48 dla sklepu nr: ", sklep, sep=""), yaxis = list(range= c(0, as.numeric(max(dane_alert()[, forecast_quantity_corr]))))
        )
    }else{
      stare_calc_date <-min(rv$process_step_run_id_full_57[, calculation_date])
      nowe_calc_date <-max(rv$process_step_run_id_full_57[, calculation_date])
      sklep = updated_sklep_alert()
      #generowanie wykresu i przekształcenia mające na celu oddzielenie prognoz z różnymi "calculation_date"
      # sposób data.table
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "alert_57",
        range = c(0:1),
        visible = FALSE
      )
      
      plot_ly(data = dane_alert()[calculation_date==stare_calc_date & id_store==sklep], x = ~forecast_date) %>%
        add_lines(data = dane_alert()[calculation_date==stare_calc_date & id_store==sklep],
                  y = ~forecast_quantity_corr, color = I("firebrick2"), name = "prognoza wczorajsza")  %>%
        add_lines(data = dane_alert()[calculation_date==nowe_calc_date & id_store==sklep]
                  , y = ~forecast_quantity_corr, color = I("mediumpurple1"),line= list(dash="dashdot"),  name = "prognoza dzisiejsza")  %>%
        add_trace(data = dane_alert()[forecast_date==Daty & id_store==sklep],
                  color = I("yellow2"), type="bar",
                  x = ~forecast_date, y=1, hoverinfo = 'y', name= "dni z alertem 57", opacity = 0.3, yaxis = "y2")%>%
        layout(
          title = paste("prognozy i daty alertów 57 dla sklepu nr:", sklep, sep=""), yaxis2 = ay ,
          xaxis= list(title="Data"), yaxis=list(title="Sztuki", range = c(0, as.numeric(max(dane_alert()[, forecast_quantity_corr])))), bargap = 0.9)
      
      
      
    }
  })
  
  
  }

shinyApp(ui=ui, server = server)
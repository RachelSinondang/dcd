header <- dashboardHeader(title = "Coffeshop Sentiment Analysis")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Business Case", 
                 tabName = "case", 
                 icon = icon("dollar-sign")),
        menuItem(text = "Sentiment Plot", 
                 tabName = "sentiment", 
                 icon = icon("smile")),
        menuItem(text = "Competitor", 
                 tabName = "rival", 
                 icon = icon("users"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "case",
                mainPanel(width = 12, 
                    p("Sentiment analysis merupakan suatu proses memahami dan mengelompokkan emosi (positif, negative, netral) yang terdapat dalam tulisan menggunakan teknik Analisa teks di mana melaluinya kita dapatmengamati respon/ emosi penulis text terhadap suatu hal seperti suatu product, service, campaign, dan lain-lain. Mengetahui respon/ suara customer sangatlah penting untuk pengembangan bisnis. Sebagai contoh, dari melihat sentimen kita bisa menganalisa aspek apa yang dinilai kurang memuaskan customer dan hal tersebut dapat menjadi evaluasi untuk perkembangan produk/ bisnis ke depannya."),
                    p("Dashboard ini berisi machine learning yang akan mengelompokkan suatu review terhadap Epoch Cafe ke dalam kelas positif/ negative/ neutral berdasarkan aspek seperti coffee, food, seat, service, parking, alcohol, hours, vibe, price, location, internet, local. Dari situ kita melihat apa hal negative yang perlu untuk diperbaiki dan apa hal positif yang perlu dipertahankan yang dapat dilihat dari kata terbanyak di masing-masing sentiment."),
                    p("Selain itu pada dashboard ini kita akan sentiment coffeeshop competitor berdasarkan masing-masing aspek. Respon yang negative bisa menjadi celah/ kesempatan bagi kita untuk menawarkan kepada customer solusi dari respon negative tersebut, sedangkan respon positive dapat kita gunakan sebagai pembelajaran untuk pengembangan produk bisnis kita."),
                    br()
                )),
        tabItem(tabName = "sentiment", 
                     h1("Sentiment Dashboard", align = "center"),
                     fluidRow(
                         box(title = "Input",
                             background = "black",
                             width = 5,
                             fileInput("file1", "Choose CSV File",
                                       accept = c(
                                           "text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                             ),
                             tags$hr(),
                             checkboxInput("header", "Header", TRUE),
                             selectInput(inputId = "test", 
                                         label = "Pilih Aspek",
                                         choices = epoch.siap %>% 
                                             select(-c(review,service_sent)) %>%
                                             colnames())
                         )
                         
                          
                         ,
                         box(title = "Plot",
                             background = "maroon",
                             width = 6,
                             solidHeader = TRUE,
                             plotlyOutput(outputId = "plot_sentimen")),
                         
                         box(title = "Word",
                             background = "blue",
                             width = 10,
                             solidHeader = TRUE,
                             plotlyOutput(outputId = "plot_pred"))
                     )
    )
    ,
        
        
        tabItem(tabName = "rival", 
                h1("Competitor's Sentiment", align = "center"),
                fluidRow(
                    tabBox(id="tabchart1", width = 12, height = "1200px",
                           tabPanel("Shop's Rating",
                                    selectInput(inputId = "coffee", 
                                                label = "Pilih Coffeeshop",
                                                choices = unique(yelp_shop$coffee_shop_name)
                                                
                                    ),
                                    
                                    plotlyOutput(outputId = "plot_pivot")
                           ),
                           tabPanel("Aspect-Based Rating",
                                    selectInput(inputId = "shop", 
                                                label = "Pilih Aspek",
                                                choices = yelp_shop %>% 
                                                    select_if(is.numeric) %>% 
                                                    select(-c(num_reviews, rating)) %>%
                                                    colnames()
                                                
                                    ),
                                    sliderInput(inputId = "banyak",
                                                  "Frequency:",
                                                  min = 1,  max = 66, value = 15),
                                    
                                    plotlyOutput(outputId = "plot_aspek")
                                    
                                        
                                        
                                    
                           ),
                           tabPanel("Wordcloud", 
                                    selectInput(inputId = "coff", 
                                                label = "Pilih Coffeeshop",
                                                choices = unique(clean_3$coffee_shop_name)
                                    
                                    ),
                                    selectInput(inputId = "aspek", 
                                                  label = "Pilih Aspek",
                                                  choices = unique(clean_3$parameter)
                                                  
                                    ),sliderInput(inputId = "freq",
                                                  "Frequency:",
                                                  min = 1,  max = 50, value = 10),
                                    
                                    plotlyOutput(outputId = "plot_word"),
                                    
                                                 
                           )
                    )
                )
        )
        
    ))




dashboardPage(
    header = header,
    body = body,
    sidebar = sidebar, 
    skin = "green"
)

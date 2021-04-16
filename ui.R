library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
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
                 icon = icon("users")),
        menuItem(text = "Review Dataset", 
                 tabName = "data", 
                 icon = icon("database"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "case",
                mainPanel(width = 12,
                          img(src = "epochlagi.png",height = 125, width = 300),
                          p("Sentiment analysis merupakan suatu proses memahami dan mengelompokkan emosi (positif, negative, netral) yang terdapat dalam tulisan menggunakan teknik Analisa teks di mana melaluinya kita dapatmengamati respon/ emosi penulis text terhadap suatu hal seperti suatu product, service, campaign, dan lain-lain. Mengetahui respon/ suara customer sangatlah penting untuk pengembangan bisnis. Sebagai contoh, dari melihat sentimen kita bisa menganalisa aspek apa yang dinilai kurang memuaskan customer dan hal tersebut dapat menjadi evaluasi untuk perkembangan produk/ bisnis ke depannya."),
                          p("Dashboard ini berisi machine learning yang akan mengelompokkan suatu review terhadap Epoch Cafe ke dalam kelas positif/ negative/ neutral berdasarkan aspek seperti coffee, food, seat, service, parking, alcohol, hours, vibe, price, location, internet, local. Dari situ kita melihat apa hal negative yang perlu untuk diperbaiki dan apa hal positif yang perlu dipertahankan yang dapat dilihat dari kata terbanyak di masing-masing sentiment.", style = "color:blue"),
                          p("Selain itu pada dashboard ini kita akan sentiment coffeeshop competitor berdasarkan masing-masing aspek. Respon yang negative bisa menjadi celah/ kesempatan bagi kita untuk menawarkan kepada customer solusi dari respon negative tersebut, sedangkan respon positive dapat kita gunakan sebagai pembelajaran untuk pengembangan produk bisnis kita."),
                          br()
                )),
        tabItem(tabName = "sentiment", 
                h1("Sentiment Dashboard", align = "center"),
                mainPanel(width = 12,
                          p("Saya menggunakan model Machine Learning Naive Bayes untuk memprediksi sentiment dari masing-masing review yang diberikan oleh customers lalu mengelompokkannya untuk mendapatkan insight berapa jumlah masing-masing sentiment yang diperoleh dalam tiap aspek. Model Naive Bayes adalah model Machine Learning yang memanfaatkan Bayes Theorem for conditional probability (kejadian dependent events), namun menerapkan asumsi Naive pada hubungan antar prediktornya. Asumsi naive tersebut di antaranya adalah hubungan predictor dengan target variable saling dependen serta hubungan antar predictor saling independent. Selain itu, asumsi lainnya adalah bahwa antar predictor memiliki bobot yang sama untuk melakukan prediksi. Selanjutnya, dicari kata-kata apa yang paling sering muncul pada masing-masing sentiment."),
                          br()
                ),
                fluidRow(
                    box(title = "Input",
                        background = "black",
                        width = 6,
                        fileInput("file1", "Choose Epoch Cafe's Review File",
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
                        plotlyOutput(outputId = "plot_sentimen"),
                        valueBoxOutput("accuracy")),
                    
                    box(title = "Word",
                        background = "blue",
                        width = 12,
                        solidHeader = TRUE,
                        plotlyOutput(outputId = "plot_pred")),
                    
                    box(title = "Data",
                        background = "green",
                        width = 12,
                        solidHeader = TRUE,
                        dataTableOutput(outputId = "table_test"))
                )
        )
        ,
        
        
        tabItem(tabName = "rival", 
                h1("Competitor's Sentiment", align = "center"),
                fluidRow(
                    tabBox(id="tabchart1", width = 12, height = "1200px",
                           tabPanel("Shop's Rating",
                                    box(title = "About",
                                        background = "blue",
                                        width = 12,
                                        solidHeader = TRUE,
                                        p("Plot di bawah ini ditujukan untuk melihat aspek yang unggul dan aspek yang lemah dari competitor bisnis kita, yang ke depannya dapat dijadikan pembelajaran bagi pengembangan bisnis kita.")),
                                    
                                    box(title = "Plot",
                                        background = "green",
                                        width = 12, height = 600,
                                        solidHeader = TRUE,
                                        "Shop's Rating",
                                        selectInput(inputId = "coffee", 
                                                    label = "Pilih Coffeeshop",
                                                    choices = unique(yelp_shop$coffee_shop_name)
                                                    
                                        ),
                                        
                                        plotlyOutput(outputId = "plot_pivot")
                                    )
                           ),
                           tabPanel("Aspect-Based Rating",
                                    box(title = "About",
                                        background = "blue",
                                        width = 12,
                                        solidHeader = TRUE,
                                        p("Salah satu manfaat lain dari sentiment analysis adalah untuk mengetahui sentiment consumer terhadap competitor bisnis kita. Melalui plot di samping, kita hendak melihat competitor dengan rating tertinggi dan terendah pada masing-masing aspek. Dari competitor dengan rating tertinggi, kita dapat mempelajari apa yang membuat customer memberi penilaian yang bagus. Sedangkan dari competitor dengan rating terendah, kita dapat mempelajari apa yang menjadi celah competitor tersebut yang dapat kita kembangkan di bisnis milik kita.")),
                                    
                                    box(title = "Plot",
                                        background = "maroon",
                                        width = 12,height = 1000,
                                        solidHeader = TRUE,
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
                                        
                                        plotlyOutput(outputId = "plot_aspek",width = 10,height = "auto"))
                                    
                                    
                                    
                                    
                                    
                                    
                           ),
                           tabPanel("Words",
                                    box(title = "About",
                                        background = "blue",
                                        width = 12,
                                        solidHeader = TRUE,
                                        p("Setelah mengetahui rating dari masing-masing competitor kita, kini kita ingin melihat kata apa yang paling sering disebut oleh customer di tiap sentiment mengenai masing-masing aspek café competitor bisnis.")),
                                    
                                    box(title = "Plot",
                                        background = "purple",
                                        width = 12,height = 800,
                                        solidHeader = TRUE,
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
                                        
                                        plotlyOutput(outputId = "plot_word",width = "700px",height = "1200px")
                                    )
                           )
                    )
                )
        ),
        
        tabItem(tabName = "data", 
                selectInput(inputId = "cafe", 
                            label = "Pilih Coffeeshop",
                            choices = unique(clean$coffee_shop_name)
                            
                ),
                dataTableOutput(outputId = "data_cafe"))
        
    ))




dashboardPage(
    header = header,
    body = body,
    sidebar = sidebar, 
    skin = "green"
)

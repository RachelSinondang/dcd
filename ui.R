header <- dashboardHeader(title = "Coffeshop Sentiment Analysis")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Business Case", 
                 tabName = "gap", 
                 icon = icon("dollar-sign")),
        menuItem(text = "Sentiment Plot", 
                 tabName = "sentiment", 
                 icon = icon("chart-line")),
        menuItem(text = "Rival", 
                 tabName = "rival", 
                 icon = icon("database"))
    )
)

body <- dashboardBody(
    tabItems(
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
                             width = 10,
                             solidHeader = TRUE,
                             plotlyOutput(outputId = "plot_sentimen"))
                     )
    )
    ,
        
        
        tabItem(tabName = "rival", 
                h1("Sentiment Coffeeshop lain", align = "center"),
                fluidRow(
                    tabBox(id="tabchart1", width = 12, height = "1000px",
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
                                    
                                    plotOutput(outputId = "plot_word"),
                                    
                                                 
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

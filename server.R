function(input,output){
    
   
    
    output$plot_sentimen <- renderPlotly({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        epoch_test <- read.csv(inFile$datapath, header = input$header)
        
        test.clean <-  as.data.frame(epoch_test %>%
            mutate(review_text = review_text %>%
                       str_to_lower() %>% # transform menjadi huruf kecil
                       replace_url()  %>% 
                       replace_html() %>% 
                       replace_contraction() %>%
                       replace_word_elongation() %>% 
                       replace_internet_slang() %>% 
                       replace_emoji(.) %>% 
                       replace_emoticon(.) %>% 
                       str_remove_all(pattern = "[[:digit:]]") %>% # remove number
                       str_remove_all(pattern = "[[:punct:]]") %>% 
                       str_remove_all(pattern = "%") %>%
                       str_remove_all(pattern = "\\$") %>% # remove dollar sign
                       str_remove_all(pattern = "\\|") %>%
                       str_remove_all(pattern = "\\=") %>%
                       str_remove_all('[\\&]+') %>% 
                       str_remove_all('[\\"]+') %>% 
                       str_remove_all(pattern = "\\+") %>%
                       str_remove_all(pattern = "\\>") %>%
                       str_remove_all(pattern = "\\<") %>%
                       str_squish()
            ))
        

        data_up_train <- upSample(x = epoch.siap %>% select(-input$test), 
                                  y = as.factor(epoch.siap[,input$test]), 
                                  yname = input$test) 
        
        epoch.train.dtm <- DocumentTermMatrix(VCorpus(VectorSource(data_up_train$review)))
        
        epoch.test.dtm <- DocumentTermMatrix(VCorpus(VectorSource(test.clean$review_text)))
        
        epoch.train.dtm <- removeSparseTerms(epoch.train.dtm, 0.995)
        
        bernoulli_conv <- function(x){
            x <- as.factor(ifelse(x > 0, 1, 0))
            return(x)
        }
        
        data_train_bn <- apply(X = epoch.train.dtm, MARGIN = 2, FUN = bernoulli_conv)
        data_test_bn <- apply(X = epoch.test.dtm, MARGIN = 2, FUN = bernoulli_conv)
        
        model_naive <- naiveBayes(x = data_train_bn, # data prediktor
                                  y = data_up_train[,input$test], # data target
                                  laplace = 1)
        
        epoch_predClass <- predict(object = model_naive, 
                                   newdata = data_test_bn,
                                   type = "class")
        
        prediksi <- data.frame(epoch_predClass) %>% `colnames<-`("predict")
        
        freq_pred <- prediksi %>% group_by(predict) %>% summarise(total = n())
        
        plot_sentiment <- ggplot(data = freq_pred ,mapping = aes(x = predict, y = total, text = glue(
            "jumlah: {total}"))) +
            geom_col(aes(fill = predict))
        
        ggplotly(plot_sentiment, tooltip = "text")
        
        })
    
    
    
    
    output$plot_pivot <- renderPlotly({
        
        yelp_pivot <- yelp_shop %>%
            select(-c(num_reviews,rating)) %>%
            pivot_longer(!coffee_shop_name, names_to = "parameter", values_to = "value") %>%
            mutate(coffee_shop_name = as.factor(coffee_shop_name))
        
        plot_pivot <- ggplot(data = yelp_pivot %>% filter(coffee_shop_name == input$coffee) ,mapping = aes(x = value, y = parameter, text = glue(
            "nilai: {value}"))) +
            geom_col(aes(fill = value))
        
        ggplotly(plot_pivot, tooltip = "text")
        
        
        
    })
    
    output$plot_aspek <- renderPlotly({
        
    
        plot_aspek <- ggplot(data = yelp_pivot %>% filter(parameter == input$shop) %>% arrange(desc(value)) %>% head(input$banyak) ,mapping = aes(x = value, y = reorder(coffee_shop_name,value), text = glue(
            "nilai: {value}"))) +
            geom_col(aes(fill = value))+ 
            labs(y= "Coffee Shop Name", x="Value", 
                 title= "Best sentiment coffeeshop", 
                 caption = "Source: Kaggle")
        
        ggplotly(plot_aspek, tooltip = "text", height = 700,width = 700)
        
        
        
        
    })
    
    output$plot_word <- renderPlot({
        
        
        aa <- clean_3 %>% 
            filter(coffee_shop_name == input$coff, 
                   parameter == input$aspek) %>%
            unnest_tokens(word, review_text)%>%
            mutate(word = textstem::lemmatize_words(word)) %>%
            anti_join(stop_words) %>% 
            count(word, value, sort = T) %>% 
            group_by(value) %>% 
            top_n(input$freq)
        
        ggplot(aa, aes(label = word)) +
            ggwordcloud::geom_text_wordcloud(aes(size=n, color = value)) +
            facet_wrap(~value, scales = "free_y") +
            scale_size_area(max_size = 20) +
            labs(title = "Sentiment's Wordcloud") +
            theme_minimal()
        
    })
    
    
    
    
    
    
    
}


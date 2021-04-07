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
        
        
        
        
        epoch.test.dtm <- DocumentTermMatrix(VCorpus(VectorSource(test.clean$review_text)))
        
        epoch.train.dtm <- removeSparseTerms(epoch.train.dtm, 0.995)
        
        bernoulli_conv <- function(x){
            x <- as.factor(ifelse(x > 0, 1, 0))
            return(x)
        }
        
        
        data_test_bn <- apply(X = epoch.test.dtm, MARGIN = 2, FUN = bernoulli_conv)
        
        if(input$test == "overall_sent") {epoch_predClass <- predict(object = md_overall, 
                                   newdata = data_test_bn,
                                   type = "class")}
        
        if(input$test == "vibe_sent") {epoch_predClass <- predict(object = md_vibe, 
                                                                     newdata = data_test_bn,
                                                                     type = "class")}
        
        if(input$test == "tea_sent") {epoch_predClass <- predict(object = md_tea, 
                                                                  newdata = data_test_bn,
                                                                  type = "class")}
        
        if(input$test == "price_sent") {epoch_predClass <- predict(object = md_price, 
                                                                 newdata = data_test_bn,
                                                                 type = "class")}
        
        if(input$test == "seating_sent") {epoch_predClass <- predict(object = md_seat, 
                                                                 newdata = data_test_bn,
                                                                 type = "class")}
        
        if(input$test == "parking_sent") {epoch_predClass <- predict(object = md_parking, 
                                                                 newdata = data_test_bn,
                                                                 type = "class")}
        
        if(input$test == "location_sent") {epoch_predClass <- predict(object = md_loc, 
                                                                 newdata = data_test_bn,
                                                                 type = "class")}
        
        if(input$test == "alcohol_sent") {epoch_predClass <- predict(object = md_alcohol, 
                                                                 newdata = data_test_bn,
                                                                 type = "class")}
        
        if(input$test == "coffee_sent") {epoch_predClass <- predict(object = md_coffee, 
                                                                 newdata = data_test_bn,
                                                                 type = "class")}
        
        if(input$test == "food_sent") {epoch_predClass <- predict(object = md_food, 
                                                                 newdata = data_test_bn,
                                                                 type = "class")}
        
        if(input$test == "internet_sent") {epoch_predClass <- predict(object = md_int, 
                                                                 newdata = data_test_bn,
                                                                 type = "class")}
        
        if(input$test == "local_sent") {epoch_predClass <- predict(object = md_local, 
                                                                 newdata = data_test_bn,
                                                                 type = "class")}
        
        
        prediksi <- data.frame(epoch_predClass) %>% `colnames<-`("predict")
        
        freq_pred <- prediksi %>% group_by(predict) %>% summarise(total = n())
        
        plot_sentiment <- ggplot(data = freq_pred ,mapping = aes(x = predict, y = total, text = glue(
            "jumlah: {total}"))) +
            geom_col(aes(fill = predict))
        
        ggplotly(plot_sentiment, tooltip = "text")
        
    })
    
    output$plot_pred <- renderPlotly({
        
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
        
        
        
        epoch.test.dtm <- DocumentTermMatrix(VCorpus(VectorSource(test.clean$review_text)))
        
        epoch.train.dtm <- removeSparseTerms(epoch.train.dtm, 0.995)
        
        bernoulli_conv <- function(x){
            x <- as.factor(ifelse(x > 0, 1, 0))
            return(x)
        }
        
        data_test_bn <- apply(X = epoch.test.dtm, MARGIN = 2, FUN = bernoulli_conv)
        
        if(input$test == "overall_sent") {epoch_predClass <- predict(object = md_overall, 
                                                                     newdata = data_test_bn,
                                                                     type = "class")}
        
        if(input$test == "vibe_sent") {epoch_predClass <- predict(object = md_vibe, 
                                                                  newdata = data_test_bn,
                                                                  type = "class")}
        
        if(input$test == "tea_sent") {epoch_predClass <- predict(object = md_tea, 
                                                                 newdata = data_test_bn,
                                                                 type = "class")}
        
        if(input$test == "price_sent") {epoch_predClass <- predict(object = md_price, 
                                                                   newdata = data_test_bn,
                                                                   type = "class")}
        
        if(input$test == "seating_sent") {epoch_predClass <- predict(object = md_seat, 
                                                                     newdata = data_test_bn,
                                                                     type = "class")}
        
        if(input$test == "parking_sent") {epoch_predClass <- predict(object = md_parking, 
                                                                     newdata = data_test_bn,
                                                                     type = "class")}
        
        if(input$test == "location_sent") {epoch_predClass <- predict(object = md_loc, 
                                                                      newdata = data_test_bn,
                                                                      type = "class")}
        
        if(input$test == "alcohol_sent") {epoch_predClass <- predict(object = md_alcohol, 
                                                                     newdata = data_test_bn,
                                                                     type = "class")}
        
        if(input$test == "coffee_sent") {epoch_predClass <- predict(object = md_coffee, 
                                                                    newdata = data_test_bn,
                                                                    type = "class")}
        
        if(input$test == "food_sent") {epoch_predClass <- predict(object = md_food, 
                                                                  newdata = data_test_bn,
                                                                  type = "class")}
        
        if(input$test == "internet_sent") {epoch_predClass <- predict(object = md_int, 
                                                                      newdata = data_test_bn,
                                                                      type = "class")}
        
        if(input$test == "local_sent") {epoch_predClass <- predict(object = md_local, 
                                                                   newdata = data_test_bn,
                                                                   type = "class")}
        
        
        test_predict <- data.frame("review" = test.clean %>% select(review_text), "predict" = epoch_predClass)
        
        bb <- test_predict %>% 
            unnest_tokens(word, review_text)%>%
            mutate(word = textstem::lemmatize_words(word)) %>%
            anti_join(stop_words) %>% 
            count(word, predict, sort = T) %>% 
            group_by(predict) %>% 
            top_n(10)
        
        plot_pred <- ggplot(data = bb ,mapping = aes(x = n, y = word, text = glue(
            "jumlah: {n}"))) +
            geom_col(aes(fill = predict))+ 
            facet_wrap(~predict) +
            labs(y= "word", x="frequency", 
                 title= "Kata dengan frekuensi terbanyak", 
                 caption = "Source: Kaggle")
        ggplotly(plot_pred, tooltip = "text")
        
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
    
    output$plot_word <- renderPlotly({
        
        
        aa <- clean_3 %>% 
            filter(coffee_shop_name == input$coff, 
                   parameter == input$aspek) %>%
            unnest_tokens(word, review_text)%>%
            mutate(word = textstem::lemmatize_words(word)) %>%
            anti_join(stop_words) %>% 
            count(word, value, sort = T) %>% 
            group_by(value) %>% 
            top_n(input$freq)
        
        plot_word <- ggplot(data = aa ,mapping = aes(x = n, y = word, text = glue(
            "jumlah: {n}"))) +
            geom_col(aes(fill = value))+ 
            facet_wrap(~value) +
            labs(y= "word", x="frequency", 
                 title= "Kata dengan frekuensi terbanyak", 
                 caption = "Source: Kaggle")
        
        ggplotly(plot_word, tooltip = "text", height = 800)
        
    })
    
    
    
    
    
    
    
}


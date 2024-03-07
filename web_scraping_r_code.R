#Ardigo, Apavaloaei, Vitali

library(rvest)
library(purrr)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(scales)
library(tm)
library(wordcloud)
library(igraph)
library(ggraph)
library(reshape2)
library(corpustools)
library(stringr)
library(httr)
library(RSelenium)
library(robotstxt)
library(wordcloud2)
library(textplot)
library(cld2)
library(quanteda)
library(quanteda.textmodels)
library(topicmodels)


#amazon_reviews = function(id, page) {
#  url = paste0("https://www.amazon.co.uk/fire-7-tablet/product-reviews/",id ,"/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=", page)
#  html = read_html(url) 
#  title = html %>% html_elements("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>% html_text2()
#  title = title %>%  c(html %>% html_elements("[class='a-size-base review-title a-color-base review-title-content a-text-bold']") %>% html_text2())
  
  # Review text (the same for UK and not-UK)
#  text = html %>% html_elements("[class='a-size-base review-text review-text-content']") %>% html_text2()
  
   #Review stars (UK and not-UK)
#  star = html %>% html_elements("[data-hook='review-star-rating']") %>% html_text2()
#  star = star %>% c(html %>% html_elements("[data-hook='cmps-review-star-rating']") %>% html_text2())
  
  # Return a tibble
#  tibble(title, text, star, page = page) %>%return()
#}

#id = "B09ZVHRTV5"
#page = 1:40
#data = map_df(page, ~amazon_reviews(id = "B09ZVHRTV5", page = .))
#data$doc_id = 1:nrow(data)

#The above commented part of code allows us to scrape the reviews from the Amazon website. Due to technical problem with the Amazon webpage, is not possible to directly scrape the data,
#so, we will use a given dataset containing 200 reviews already scraped 


#Scraping of the total number of submitted reviews
url = paste0("https://www.amazon.co.uk/fire-7-tablet/product-reviews/B09ZVHRTV5/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews")
html = read_html(url)
tot_rev = html %>% html_elements(xpath = "//*[(@id = 'filter-info-section')]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'a-size-base', ' ' ))]") %>% html_text2()


#Scraping of the description of the product
url = paste0("https://www.amazon.co.uk/fire-7-tablet/dp/B09ZVHRTV5/ref=cm_cr_arp_d_product_top?ie=UTF8")       
html = read_html(url)
description = html %>% html_elements(xpath = "//*[(@id = 'feature-bullets')]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'a-list-item', ' ' ))]") %>% html_text2()
description = tibble(description)


#Scraping of the fastest delivery time (we scraped it from the italian Amazon webpage as on Amazon.UK the product is temporarily out of stock, so the fastest delivery is not given)
url = paste0("https://www.amazon.it/tablet-fire-7-schermo-da-7-16-gb-modello-2022/dp/B099HC8X6H")
html = read_html(url)
fastest_delivery_info = html %>% html_elements(xpath = "//*[(@id = 'mir-layout-DELIVERY_BLOCK-slot-PRIMARY_DELIVERY_MESSAGE_LARGE')]//span") %>% html_text2()
fastest_delivery = tibble(fastest_delivery_info)[2,]
#In the fast delivery and description scrape process, is easier to take the elements of interest using xpaths instead of css identifiers, like we did for the reviews scraping 


#Now we start to use the given database, loading it on the software from the dekstop
setwd("C:/Desktop")
data = read_rds("dati_amazon.rds")    #We have transformed the given .rda file containing the data into an rds file (importing manually the .rda file and using it with the write_rds function), and we read it


#Process to clean the data
data = data %>% select(title = reviews.title, text = reviews.text, username = reviews.username, star_score = reviews.rating)
data = data %>% mutate(doc_id = 1:nrow(data))


data$title_lang = detect_language(data$title)
data$text_lang = detect_language(data$text)
table(Text = data$text_lang, Title = data$title_lang, useNA = "always")    
data %>% filter(is.na(title_lang))                                       #We can observe that all reviews are in English, even those that are given an NA value 
data = data %>% select(-title_lang, -text_lang)


data %>% summarise(mean_length = mean(str_length(text)) , max_length = max(str_length(text)) , min_length = min(str_length(text)))  #Some statistical information on the data 
data %>% summarise(mean_rating = mean(star_score), min_rating = min(star_score), max_rating = max(star_score))                      #Some statistical information on the star score
data %>% count(star_score) %>% mutate(perc = round(n/sum(n), 2))                                                                    #Percentage of the  number of recensions with each star_score


#Process to clean the data
data_tidytext = data %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word) %>% arrange(desc(n))   #tokenization and elimination of stop words


#Effect of the stop words
n_words_before = data %>% unnest_tokens(word, text) %>% summarise(length(word))
n_words_after = data %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% summarise(length(word))
difference = n_words_before - n_words_after      #3281 words have been dropped due to the stop_words 


#Graphic visualizations of the most common tokenns
dev.new(width  = 3000, height = 1500, view =  "px")
data_tidytext %>% slice_max(n, n = 15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_col(show.legend = FALSE, fill = "steelblue") + 
                  ylab("Token count") + ggtitle("Most common tokens in reviews") + coord_flip()

dev.new(width  = 3000, height = 1500, view =  "px")
wordcloud(data_tidytext$word, data_tidytext$n, scale = c(5, 0.7),  min.freq = 2,  max.words = 100, random.order = F, rot.per = 0.15, colors = brewer.pal(8, "Paired"))


#Dictionary based analysis: tidy vs udpipe
#Tidy
bing = get_sentiments("bing")
clean_data = data %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% select(doc_id, word)
tidy_data = clean_data %>% inner_join(bing) %>% count(doc_id, sentiment) %>% pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

#The following process makes sure that reviews with a positive and negative sentiment equal to 0, that are not taken into account during the inner join phase with bing and pivot_wider phase made above, 
#are nevertheless considered, avoiding the risk of complicating or compromising the comparison between methods
neutral = tibble(doc_id = 1:nrow(data), neutral = 0)                                 
tidy_data = tidy_data %>% full_join(neutral) %>% mutate(positive = ifelse(is.na(positive), 0, positive), negative = ifelse(is.na(negative), 0, negative)) %>% arrange(doc_id)    #Process to eliminate all NA values from positive and negative sentiment created by the full join function

data$tidy_sentiment =  tidy_data$positive - tidy_data$negative                #Sentiment net computation

#Graphical visualization of the tokens contribution to the sentiment
tidy_count_rating = data %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) 
dev.new(width  = 3000, height = 1500, view =  "px")
tidy_count_rating %>% group_by(sentiment) %>% slice_max(n, n = 10, with_ties = F) %>% ungroup() %>% mutate(word = reorder(word, n)) %>% ggplot(aes(n, word, fill = sentiment)) + 
                      geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + labs(x = "Contribution to sentiment",y = NULL) +  theme_bw() + 
                      scale_fill_manual(values = c("red","darkgreen"))     


#Udpipe
udpipe_data = tibble(doc_id = data$doc_id, text = data$text)
udpipe_data$text = iconv(udpipe_data$text, to = "UTF-8")
udpipe_data = udpipe(udpipe_data, "english-gum")
view(udpipe_data)

bing_dict = get_sentiments("bing") %>% mutate(sentiment = ifelse(sentiment == "negative", -1, 1)) %>% rename(term = "word", polarity = "sentiment")
udpipe_score = txt_sentiment(x = udpipe_data, term = "token", polarity_terms = bing_dict, polarity_negators = c("no", "not", "neither"), 
                             polarity_amplifiers = c("really", "very", "super"), polarity_deamplifiers = c("barely", "hardly"),amplifier_weight = 0.8, n_before = 2, n_after = 0, constrain = T)
data$udpipe_sentiment = udpipe_score$overall$sentiment_polarity 


dev.new(width  = 3000, height = 1500, view =  "px")
data %>% ggplot() + geom_histogram(aes(scale(tidy_sentiment)), fill = "lightblue", col = "black", binwidth = 0.9) + xlab("Tidy")+ labs(title = "Sentiment Distribution - Tidy")
dev.new(width  = 3000, height = 1500, view =  "px")
data %>% ggplot() +  geom_histogram(aes(scale(udpipe_sentiment)), fill = "lightblue", col = "black", binwidth = 0.9) + xlab("Udpipe")+ labs(title = "Sentiment Distribution - Udpipe")


data = data %>% mutate(tidy_pol = ifelse( tidy_sentiment > 0, "positive", ifelse(tidy_sentiment == 0, "neutral", "negative")), 
                       udpipe_pol = ifelse( udpipe_sentiment > 0, "positive", ifelse(udpipe_sentiment == 0, "neutral", "negative")))
table(TIDY = data$tidy_pol, UDPIPE = data$udpipe_pol)                                                           #Comparison between tidy and udpipe methods



#Difference between the dictionaries composition
bing = get_sentiments("bing")
afinn =  get_sentiments("afinn")
nrc = get_sentiments("nrc")

sent.b = data %>% select(doc_id, sentiment_net = tidy_sentiment) %>% mutate(method = "bing")
sent.n = clean_data %>%  inner_join(nrc) %>% filter(sentiment %in% c("positive", "negative")) %>% count(doc_id, sentiment, sort = T) %>% 
         pivot_wider(names_from = sentiment, values_from = n, values_fill =  0)%>%  mutate(sentiment_net = positive - negative, method = "nrc") %>% select(doc_id, sentiment_net, method)
sent.a = clean_data %>% inner_join(afinn) %>%  group_by(doc_id) %>% summarise(sentiment_net = sum(value)) %>% mutate(method = "afinn")
sent_all = bind_rows(sent.a,sent.n,sent.b)

sent_all_wide = sent_all %>%  pivot_wider(names_from = method, values_from = sentiment_net, values_fill = 0) %>% inner_join(data) %>% select(doc_id, text, afinn, bing, nrc) %>% arrange(doc_id)
sent_all_wide %>% select(-text, -doc_id) %>% cor()           #Correlation between various lexicons 

sent_all_wide %>% arrange(desc(bing)) %>% view()


#LDA
lda_data = data %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(doc_id, word)
lda_data = lda_data  %>% cast_dtm(doc_id, word, n)
lda_model = LDA(lda_data, k=2, control = list(seed = 1234))
lda_topics_per_word = tidy(lda_model, matrix = "beta")

lda_topics_per_word = lda_topics_per_word %>% filter(term != "tablet")   #We deleted the word "tablet" and repeated the graphic display
  
dev.new(width  = 3000, height = 1500, view =  "px")
lda_topics_per_word %>% group_by(topic) %>% slice_max(beta, n = 15) %>% ungroup() %>% mutate(term = reorder(term, beta)) %>% ggplot(aes(term, beta, fill = factor(topic))) + 
                        geom_col(show.legend = F)  +facet_wrap(~ topic, scales = "free") + coord_flip()

lda_documents_per_topic = tidy(lda_model, matrix = "gamma")
view(lda_documents_per_topic)


if (require("tidyverse")==FALSE){
  install.packages("tidyverse")
  library(tidyverse)
}
if (require("tidytext")==FALSE){
  install.packages("tidytext")
  library(tidytext)
}
if (require("glue")==FALSE){
  install.packages("glue")
  library(glue)
}
if (require("stringr")==FALSE){
  install.packages("stringr")
  library(stringr)
}
if (require("sqldf")==FALSE){
  install.packages("sqldf")
  library(sqldf)
}
text_to_check <-script_df[792,"current_dialogue"]
text_to_check <- RegWhy.Do.ReplaceAll(text_to_check,RegWhy.QuestionMark,"")
tokens <- data_frame(text=text_to_check) %>% unnest_tokens(word, text)

# get the sentiment from the first text: 
tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words
#remove.factors(script_df)
script_df$current_title <- as.character(script_df$current_title)
script_df$current_scene <- as.character(script_df$current_scene)
script_df$current_stage_direction <- as.character(script_df$current_stage_direction)
script_df$current_character <- as.character(script_df$current_character)
script_df$current_character_plus_direction <- as.character(script_df$current_character_plus_direction)
script_df$current_dialogue <- as.character(script_df$current_dialogue)
str(script_df)
for (i in 1:nrow(script_df)){
  text_to_check <-script_df[i,"current_dialogue"]
  
  if (nchar(text_to_check) >0){
    text_to_check <- RegWhy.Do.ReplaceAll(text_to_check,RegWhy.QuestionMark,"")
    tokens <- data_frame(text=text_to_check) %>% unnest_tokens(word, text)
    positive_count=0
    negative_count=0
    joined_tokens <- inner_join(tokens,get_sentiments("bing"), by=c("word"))
    postive_count <- sqldf("select count(*) from joined_tokens where sentiment='positive'")
    negative_count <- sqldf("select count(*) from joined_tokens where sentiment='negative'")
    sentiment_lean <-positive_count -negative_count
    #%>% # pull out only sentiment words
    #counted_tokens <-  count(joined_tokens,sentiment)# %>% # count the # of positive & negative words
    #spread_tokens <-  spread(counted_tokens,sentiment, n, fill = 0) # %>% # made data wide rather than narrow
    #preset to zero if not found

    #positive_count <- spread_tokens["positive"][[1]]
    #negative_count <- spread_tokens["negative"][[1]]
    print(paste("text",text_to_check,"Positive:",positive_count," Negative:", negative_count,"Sentiment:",sentiment_lean))
      #mutate(sentiment = positive - negative)
  }
  
}

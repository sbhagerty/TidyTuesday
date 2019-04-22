library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(LaCroixColoR)

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

tidy_anime_subset <- tidy_anime %>% select(name, synopsis) %>% unique() %>% na.omit() 

tidy_synopsis<-tidy_anime_subset %>% unnest_tokens(word, synopsis)

counts<-tidy_synopsis %>% count(word, sort = TRUE)

nrc_sentiments <- get_sentiments('nrc') #%>% filter(sentiment == "joy")

synopsis_sentiments<-tidy_synopsis %>% group_by(name)%>% inner_join(nrc_sentiments) %>% count(sentiment, sort = TRUE)
synopsis_sentiments<- synopsis_sentiments %>% filter( sentiment != 'positive' & sentiment != 'negative')

top_sentiment<- synopsis_sentiments %>% group_by(name) %>% filter(n == max(n))

top_sentiment<-top_sentiment %>% group_by(sentiment) %>% count()

palette<-lacroix_palette("MelonPomelo", n = 8, type = "continuous")
ggplot(top_sentiment, aes(x=reorder(sentiment, nn), y = nn, fill=sentiment))+
  geom_bar(stat='identity')+
  theme_classic() +
  xlab('Top Emotion in Synopsis')+
  ylab('Number of Anime')+theme(legend.position = 'None')+
  scale_fill_manual(values=palette)+coord_flip()+
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(size=26), axis.text=element_text(size=16))
  

library(gganimate)
library(tidyverse)
library(extrafont)

meteorites<-readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

meteorite_proportions<-meteorites %>% group_by(year, fall) %>% 
  summarise(count=n())%>%
  spread(key=fall, value=count, fill=0) %>%
  mutate(total= Fell+Found, prop_found = Found / (Found + Fell)) %>%
  mutate(prop_fell = Fell / (Fell+Found))%>%
  gather(prop_fell, prop_found, key='type', value ='prop')

mini_meteorite_proportions<-filter(meteorite_proportions, year >= 1940)

plot<-ggplot(mini_meteorite_proportions,aes(x="", y=prop, fill=type, label=total))+
  geom_bar(stat='identity', width=1)+
  theme_void()+
  scale_fill_manual(values=c("#C2226C","#FD6E68"), name="", labels=c("Fell", "Found"))+
  coord_polar("y", start=0)+
  theme(plot.background = element_rect(fill="#F1EAD7"),
        plot.title= element_text(hjust=0.5, color="black", family="Verdana Bold", size=18),
        legend.direction="vertical", 
        legend.position = c(0.1,0.93),
        legend.text = element_text(size=14),
        plot.subtitle = element_text(hjust = 0.5, size=14))

plot<-plot+
  transition_states(year)+
  ggtitle('In {closest_state} there were {(filter(mini_meteorite_proportions, year == as.integer(closest_state))[4])[[1,1]]} meteorites', subtitle = 'Proportion of those meteorites that were found vs. fell')




animate(plot, nframes=2*length(unique(mini_meteorite_proportions$year)))
anim_save('meteorite.gif')


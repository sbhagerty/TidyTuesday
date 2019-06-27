library(tidyverse)
library(extrafont)
library(maps)
library(cowplot)
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

ufo_shapes<-ufo_sightings %>% mutate(ufo_shape = case_when(ufo_shape =='other'~'unknown', TRUE~ufo_shape))
rm(ufo_sightings)
encounter_by_shape<-ufo_shapes %>% 
  group_by(ufo_shape) %>% 
  summarize(mean_encounter = mean(encounter_length, na.rm=TRUE), count=n())%>% 
  filter(count>100)%>%
  drop_na()%>%
  top_n(5, mean_encounter)

pal<-c('#F5D3A3','#487DE7','#99CFD5','#AD3479','#A43CCB')

p1<-ggplot(encounter_by_shape)+
  geom_bar(aes(x=reorder(ufo_shape, -mean_encounter), y=(mean_encounter/60), fill=ufo_shape), stat="identity")+
  theme(panel.grid.major=element_line(linetype=3,color='white', size=0.15),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill='black'),
        plot.background = element_rect(fill='black'),
        legend.key = element_rect(fill='black', color='black'),
        legend.background = element_rect(fill='black'), 
        text = element_text(color='white', family='Courier'),
        axis.text = element_text(color='white'),
        plot.title = element_text(family='Courier',face='bold',hjust=0.5, size=11),
        legend.position='None')+
  scale_fill_manual(values=pal)+
  xlab('')+
  ylab("")+
  ggtitle('Top 5 UFO shapes (with >100 sightings) \nby average encounter lengths (min)')


cone_ufo<- filter(ufo_shapes, ufo_shape == 'cone')
rm(ufo_shapes)
world<-map_data('world')
map<-ggplot(world)+geom_polygon(aes(x=long, y=lat, group=group), color='white', fill='black', size=.1)+
geom_point(data=cone_ufo,aes(x=longitude, y=latitude, size=encounter_length), color='#F5D3A3', shape=17)+
theme(panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.background = element_rect(fill='black'),
      plot.background = element_rect(fill='black'),
      legend.key = element_rect(fill='black', color='black'),
      legend.background = element_rect(fill='black'), 
      text = element_text(color='white', family='Courier'),
      axis.text = element_blank(),
      plot.title = element_text(family='Courier',face='bold',hjust=0.5),
      plot.subtitle = element_text(size=10, hjust=0.5),
      plot.caption=element_text(family='Courier', size=8,hjust=0.5),
      legend.position='None')+
  xlab('')+
  ylab('')+
  labs(title = 'Where do the cone UFOs visit?',
       subtitle= "Triangle size reflects encounter length",
       caption = "The longest encounter reportedly lasted 8 years \n (seemingly occurred over multiple visits).")

plot_grid(p1, map,nrow=2)

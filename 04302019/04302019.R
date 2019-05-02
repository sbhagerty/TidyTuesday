library(tidyverse)
library(lubridate)
library(gganimate)
library(emojifont)
library(beyonce)

bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")
mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")

bird_collisions_MonthlyCounts <- bird_collisions %>%
                                 mutate(month = month(date, label=TRUE), year=year(date)) %>%
                                 group_by(month, year, locality) %>%
                                 summarise(collisions = n())

ggplot(bird_collisions_MonthlyCounts, aes(x=year, y=collisions, color=month))+
  geom_point()+
  geom_smooth(se=FALSE)+
  theme_classic()+
  facet_grid(locality~.)+
  guides(color=guide_legend(title="Month"))

subset<-bird_collisions %>%
        mutate(month = month(date, label=TRUE), year=year(date))#%>%
        #filter(locality == "CHI") %>% 
        #filter(month == "Sep" | month == "Oct" ) 

subset <- subset %>%
          group_by(genus, species, month, year, locality) %>% 
          summarise(collision = n())

monthly <- subset %>% group_by(month, year, locality)%>% summarise('Collision Count' = sum(collision))

NumberOfSpecies <-subset %>% group_by(month, year, locality) %>% summarise('Species Count' = n())

ggplot(NumberOfSpecies, aes(x=year, y=numberOfSpecies, color= month))+
  geom_point()+theme_classic()+xlab("Year")+ylab("Species Count with Collisions")+
  facet_grid(.~locality)

CollisionsAndSpecies<-left_join(monthly,NumberOfSpecies)

CollisionsAndSpecies<- CollisionsAndSpecies %>% gather('Collision Count', 'Species Count', key="CountType",value="Count")
CollisionsAndSpecies <- CollisionsAndSpecies %>%
                        mutate(locality= case_when(locality == "CHI"~"Greater Chicago Area",
                                                   locality == "MP"~ "McCormick Place")) 
                        


plot<-ggplot(CollisionsAndSpecies, aes(x=year, y=Count, color= month))+scale_color_manual(values = beyonce_palette(18,7, type = "continuous"))+
  geom_smooth(se=FALSE)+xlab("Year")+
  facet_grid(CountType~locality, scales = "free_y")+
  guides(color=guide_legend(title="Month"))+
  theme(text=element_text(face = "bold",size=12),strip.background=element_blank(), #strip.text=element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line=element_line(colour = "black"))+theme(axis.text=element_text(colour="black"))+theme(panel.spacing.x=unit(0.5,"cm"))

ggsave("output.jpg")

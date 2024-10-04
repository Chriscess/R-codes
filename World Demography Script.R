###Load packages 

library(dplyr)
library(tidyverse)
library(patchwork)
Demography_data<- read.csv("Demographic-Data.csv")

#### Rename columns
Demography_data<- Demography_data %>% 
  rename(Country= 'Country.Name',
         Birth_rate = 'Birth.rate',
         Pop2024 = 'Pop..2024',
         Intern_user = 'Interenet..user',
         Inc_group = 'Income.group')

####Summary statistics for population
Demography_data %>% 
  group_by(Continent) %>% 
  summarise(mean = mean(Pop2024),
            std = sd(Pop2024),
            median = median(Pop2024),
            minimum = min(Pop2024),
            maximum = max(Pop2024),
            count= n())
## Summary Statistics for Density
Demography_data %>% 
  group_by(Continent) %>% 
  summarise(mean= mean(Density),
            std= sd(Density),
            median= median(Density),
            maximum= max(Density),
            minimum=min(Density))
## Summary Statistics for Birth rate
Demography_data %>% 
  group_by(Continent) %>% 
  summarise(mean= mean(Birth_rate),
            std= sd(Birth_rate),
            median= median(Birth_rate),
            maximum= max(Birth_rate),
            minimum=min(Birth_rate))
## Summary Statistics for Internet User
Demography_data %>% 
  group_by(Continent) %>% 
  summarise(mean= mean(Intern_user),
            std= sd(Intern_user),
            median= median(Intern_user),
            maximum= max(Intern_user),
            minimum=min(Intern_user))

#### Box plot showing income group for  continent
Pop_mil_2024<- log10(Demography_data$Pop2024)
Density_mil<- log10(Demography_data$Density)

Demography_data %>% 
  ggplot(aes(x= reorder(Inc_group,Pop_mil_2024), y= Pop_mil_2024, fill= Continent))+
  geom_boxplot()+
  labs(x= "Continent",
       y = "Population",
       title = "Income Group for Continent")+
  theme_minimal()+
  theme(axis.text = element_text(size = 12,angle = 90) ,
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 17))
#### Boxplot showing population by continent
ggplot(Demography_data,aes(x=reorder(Continent, Pop_mil_2024), y= Pop_mil_2024, fill=Continent))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Continent",
       y = "Population",
       title = "Population by Continent")+
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 17))

###Scatter plot showing birth rate in diferent continent
d1<- ggplot(Demography_data, aes( x= Birth_rate, y = Pop_mil_2024))+
  geom_point(aes(colour = Continent), size = 1)+
  labs(x = "Birth Rate",
       y = "Population",
       title = "Birth rate in differnt continent")

d2<- ggplot(Demography_data, aes( x= Birth_rate, y = Pop_mil_2024))+
  geom_point(aes(colour = Continent), size = 1)+
  labs(x = "Birth Rate",
       y = "Population",
       title = "Birth rate in differnt continent")+
  facet_wrap(~Continent)
d1/ d2

#### Bar chat shwing population by density and Population by continent
Density_sum<-  Demography_data %>% 
  group_by(Continent) %>% 
  summarise(mean=mean(Density))

p1<- ggplot(Density_sum,aes(x=reorder(Continent,-mean), y= mean, fill = Continent))+
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.5)+
  labs(x= "Continent", y="Density",
       title= "Population by Density")+
  theme_minimal()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 17))

p2<- ggplot(Demography_data,aes(x=Continent,y= Pop_mil_2024, fill= Continent))+
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.5)+
  labs(x="Continent", y= "Population",
       title= "Population by Continent")+
  theme_minimal()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 17))
p1 /p2

#### Barchart showing internet usage by continent
Intern_summary<- Demography_data %>% 
  group_by(Continent) %>% 
  summarise(mean=mean(Intern_user))


ggplot(Intern_summary,aes(x=reorder(Continent,mean ), y= mean,fill = Continent))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(x= "Continent", y="Internet User",
       title= "Internet Usage by Continent")+
  theme_minimal()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 17))
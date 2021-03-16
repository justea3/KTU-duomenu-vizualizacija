#8 variantas ecoActCode = 412000
library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(viridis)
library(ggplot2)
data <- read_csv("../data/lab_sodra.csv")

#1Uzduotis
dataByCode <- data %>% filter(ecoActCode == "412000")
dataByCode %>%  ggplot(aes(x = avgWage)) + geom_histogram(bins = 150, fill = "red", col = "black")
ggsave("../img/1uzduotis.png")

#2Uzduotis
dataByCode$month<-as.character(dataByCode$month)
dataByCode$month<-as.yearmon(dataByCode$month, "%Y%m")
dataByCode$month <- as.Date(dataByCode$month)
dataByCode$month <-format(dataByCode$month, format="%Y %m")


avgWageSorted <-dataByCode %>% group_by(code) %>% summarise(average = mean(avgWage)) %>% arrange(desc(average)) %>% head(5)

dataByCode %>% filter(code %in% avgWageSorted$code) %>% ggplot(aes(x= month, y = avgWage, group = name, color = name)) + geom_line() + 
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Top 5 companies with the biggest average wage") +
  theme_ipsum() + 
  theme(axis.text.x = element_text(size = 8)) 

  
ggsave("../img/2uzduotis.png")

#3Uzduotis
dataByCode %>% filter(code %in% avgWageSorted$code) %>% group_by(name) %>% slice_max(numInsured, with_ties = FALSE) %>%
  ggplot(aes(x = reorder(name, -numInsured), y= numInsured)) + geom_col(aes(fill=name)) + 
  theme(axis.text.x = element_blank()) +xlab('name') + ylab('apdraustieji')

ggsave("../img/3uzduotis.png")





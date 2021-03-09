library(tidyverse)
data <- read_csv("data/lab_sodra.csv")

summary(data)

data %>%
  group_by(ecoActName) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

hist(data$avgWage, breaks = 200)


data %>%
  filter(tax < 100000) %>%
  ggplot(aes(x = tax)) +
  geom_histogram(bins = 200)

data %>%
  filter(code == 2324126) %>%
  ggplot(aes(x = month, y = avgWage)) +
    geom_line() + 
    geom_point()

ggsave("google.png")

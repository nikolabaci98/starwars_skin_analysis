
library(tidyverse)
library(dplyr)
library(splitstackshape)


?starwars

human_characters <- starwars %>%
  filter(species == "Human") %>%
  select(c(name, skin_color, sex, species, films))

human_characters <- cSplit(human_characters, 5, ",", "long") #split films vector by comma
human_characters$films <- sub("c\\(", "", human_characters$films) #remove "c("
human_characters$films <- sub("\\)", "", human_characters$films) #remove ")"
human_characters$films <- sub("\"", "", human_characters$films) #remove first "
human_characters$films <- sub("\"", "", human_characters$films) # remove second "


human_characters <- human_characters %>%
  mutate(skin_color, dark_skin = case_when(skin_color %in% c("fair", "light", "white", "pale") ~ 0, 
                   skin_color %in% c("tan", "dark") ~ 1)) %>%
  mutate(films, release_year = case_when(films == "A New Hope" ~ 1977,
                                         films == "The Empire Strikes Back" ~ 1980,
                                         films == "Return of the Jedi" ~ 1983,
                                         films == "The Phantom Menace" ~ 1999,
                                         films == "Attack of the Clones" ~ 2002,
                                         films == "Revenge of the Sith" ~ 2005,
                                         films == "The Force Awakens" ~ 2015))




human_summary <- human_summary %>%
  ungroup() %>%
  add_row(films = "A New Hope", dark_skin = 1, release_year = 1977, human_actors = 0)



human_summary %>%
  ggplot(mapping = aes(x = films, y = human_actors)) +
  geom_bar(mapping = aes(fill = dark_skin),stat = "identity", position = "dodge2", show.legend = FALSE) +
  theme_bw() +
  coord_flip() +
  ylab("Count") + 
  xlab("Movies") +
  scale_y_continuous(n.breaks = 20)


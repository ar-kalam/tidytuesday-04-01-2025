# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-04-01')
## OR
tuesdata <- tidytuesdayR::tt_load(2025, week = 13)

pokemon_df <- tuesdata$pokemon_df


# What types have the highest stats on average

library(tidyverse)
pokemon_df %>%
  view()

# adding base stat total 
pokemon_df <- pokemon_df %>% 
  mutate(BST = hp + attack + defense + special_attack + special_defense + speed)
pokemon_df$BST


type_bst <- pokemon_df %>% 
  pivot_longer(cols = c(type_1, type_2), values_to = "multi_types") %>% 
  select(c(pokemon, multi_types, BST)) %>%
  drop_na() %>% 
  group_by(multi_types) %>%
  summarise(BST = mean(BST)) %>%
  arrange(desc(BST))

type_plot <- ggplot(type_bst, aes(x= reorder(multi_types, BST), y = BST, fill = factor(multi_types))) + 
  geom_col() + 
  geom_text(aes(label = round(BST, 0)), hjust = -0.2) + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c(
    "fire" = "red",
    "water" = "blue",
    "grass" = "green",
    "electric" = "yellow",
    "psychic" = "magenta",
    "ice" = "lightblue",
    "dragon" = "#301934",
    "dark" = "black",
    "fairy" = "pink",
    "fighting" = "brown",
    "steel" = "darkgray",
    "ghost" = "violet",
    "ground" = "tan",
    "rock" = "darkgoldenrod",
    "bug" = "lightgreen",
    "flying" = "skyblue",
    "poison" = "purple",
    "normal" = "beige"
  )) + 
  labs(title = "Average Base Stat Total of Pokemon Types",
       x = "Type", y = "Base Stat Total") + 
  coord_flip(ylim = c(350, NA)) + 
  theme(axis.text.y = element_text(size = 12))
  
type_plot

abundant_type <- pokemon_df %>% 
  pivot_longer(cols = c(type_1, type_2), values_to = "multi_types") %>% 
  count(multi_types) %>%
  drop_na()

abundant_type

abundant_type_plot <- ggplot(abundant_type, aes(x = reorder(multi_types, n), y = n, fill = factor(multi_types))) + geom_col() + 
  scale_fill_manual(values = c(
  "fire" = "red",
  "water" = "blue",
  "grass" = "green",
  "electric" = "yellow",
  "psychic" = "magenta",
  "ice" = "lightblue",
  "dragon" = "#301934",
  "dark" = "black",
  "fairy" = "pink",
  "fighting" = "brown",
  "steel" = "darkgray",
  "ghost" = "violet",
  "ground" = "tan",
  "rock" = "darkgoldenrod",
  "bug" = "lightgreen",
  "flying" = "skyblue",
  "poison" = "purple",
  "normal" = "beige"
)) + 
  coord_flip() + 
  theme(legend.position = "none") + 
  geom_text(aes(label = n, hjust = -0.2)) + 
  labs(x = "Types", y = "Number of Pokemon", title = "Number of Pokemon of each type")

abundant_type_plot

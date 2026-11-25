#model comparison lab
rm(list = ls())
# Perform only once
#install.packages("palmerpenguins")
library(palmerpenguins)

pacman::p_load(tidyverse, patchwork, palmerpenguins, janitor)

penguins_raw
#clean names
penguins_clean <- clean_names(penguins_raw)
#convery yes no to 1, 0
penguins_clean <- penguins_clean%>%
  mutate(clutch_completion = ifelse(clutch_completion == "Yes", 1, 0))
penguins_clean

unique(penguins_clean$species)

#you can also make a vector instead of copy and paste the names
penguins_clean <- penguins_clean%>%
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap"))

penguins_clean <- penguins_clean%>%
  drop_na(
    culmen_length_mm,
    culmen_depth_mm,
    flipper_length_mm,
    sex,
    body_mass_g)

penguins_clean

#analyze
#install.packages("MuMIn")
library(MuMIn)

penguins_clean%>%
  ggplot(aes(x = clutch_completion,
             y = species))+
  geom_point()

peng_binom <- glm(clutch_completion ~ species + culmen_length_mm + culmen_depth_mm + flipper_length_mm + body_mass_g + sex, 
                  data = penguins_clean, family = "binomial")



penguins_clean$clutch_completion


#model development
options(na.action = "na.fail")
m <- peng_binom
m_set <- dredge(m, rank = "AIC")
m_sub <- subset(m_set, delta < 2)

list_m <- get.models(m_set, subset = delta < 2)
list_m[[1]]

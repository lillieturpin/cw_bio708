
rm(list = ls())
pacman::p_load(tidyverse, patchwork)

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)
print(df_fish)

#glm exercise

fish_pois <- glm(n_sp ~ distance + cat_area + hull_area,
    data =df_fish, family = "poisson")
summary(fish_pois)




mtcars

mt_binom <- glm(am ~ mpg +hp +wt, data = mtcars, family = "binomial")
summary(mt_binom)

mt_gaus <- glm(am ~ mpg +hp +wt, data = mtcars, family = "gaussian")
summary(mt_gaus)

#effect size

df_fish <- df_fish%>%
  mutate(std_dist = c(scale(distance)),
         std_cat = c(scale(cat_area)),
         std_hull = c(scale(hull_area)))

fish_scale <- glm(n_sp ~ std_dist + std_cat + std_hull, 
                  data = df_fish, 
                  family = "poisson")
summary(fish_scale)

#offset term

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_offset.csv"
df_offset <- read_csv(url)
df_offset

df_offset <- df_offset%>%
  mutate(density = count/area)

g_area <- df_offset%>%
  ggplot(aes(x = area,
         y = count))+
  geom_point()

g_nitrate <- df_offset%>%
  ggplot(aes(x = nitrate,
             y = count))+
  geom_point()

g_density <- df_offset%>%
  ggplot(aes(x = density,
             y = count))+
  geom_point()

g_area | g_nitrate | g_density

## not good den_offset <- glm(density ~ nitrate, data = df_offset, family = "poisson")

m_pois_off <- glm(count ~ nitrate + offset(log(area)), data = df_offset, family = "poisson")
m_normal <- glm(count ~ nitrate + offset(log(area)), data = df_offset, family = "gaussian")

#overdispersion

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_tadpole.csv"
df_tadpole <- read_csv(url)
df_tadpole

g_aqveg <- df_tadpole%>%
  ggplot(aes(x = aqveg,
             y = tadpole))+
  geom_point()

g_permanence <- df_tadpole%>%
  ggplot(aes(x = permanence,
             y = tadpole))+
  geom_point()

g_aqveg | g_permanence

m_tad <- glm(tadpole ~ aqveg + permanence, 
                  data = df_tadpole, 
                  family = "poisson")

summary(m_tad)

mean(df_tadpole$tadpole)
sd(df_tadpole$tadpole)
var(df_tadpole$tadpole)

# negative binomial is better
MASS::glm.nb(tadpole ~ aqveg + permanence, data = df_tadpole)%>%
  summary()

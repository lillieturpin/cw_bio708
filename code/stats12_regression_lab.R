#regression lab
rm(list = ls())
pacman::p_load(tidyverse)

df_iris <- iris
df_iris


df_iris %>%
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width))+
  geom_point()
#manual
df_spec <- unique(iris$Species)
#setosa
df_iris%>%
  filter(Species == df_spec[1])%>%
  lm(Sepal.Width ~ Petal.Width, .)
#versicolor
df_iris%>%
  filter(Species == df_spec[2])%>%
  lm(Sepal.Width ~ Petal.Width, .)
#virginica
df_iris%>%
  filter(Species == df_spec[3])%>%
  lm(Sepal.Width ~ Petal.Width, .)
#perform using lapply
list_lm <- lapply(X = df_spec,
       function(s){
         iris%>%
           filter(Species == s)%>%
           lm(Sepal.Width ~ Petal.Width, data = .)
       })
#additional predictor, Petal.Length by species
list_lm2 <- lapply(X = df_spec,
     FUN =  function(s){
         iris%>%
           filter(Species == s)%>%
           lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)
       })

#manual 
#setosa
df_iris%>%
  filter(Species == df_spec[1])%>%
  lm(Sepal.Width ~ Petal.Width +Petal.Length, .)
#versicolor
df_iris%>%
  filter(Species == df_spec[2])%>%
  lm(Sepal.Width ~ Petal.Width+Petal.Length, .)
#virginica
df_iris%>%
  filter(Species == df_spec[3])%>%
  lm(Sepal.Width ~ Petal.Width+Petal.Length, .)
#compare r^2

list_lm[[1]]%>%summary()
list_lm2[[1]]%>%summary()

#lm(y ~ x + x2, data = data )


#multiple r^2 vs adjusted r^2
# & look up difference in meaning between the two
#increase invariable very often increases the multiple r^2

iris_x <- iris%>%
  mutate(useless =rnorm(150))

m0 <- lm(Sepal.Width ~ Petal.Width, data = iris_x)
m1 <- lm(Sepal.Width ~ Petal.Width + useless, data = iris_x)

summary(m0)
summary(m1)
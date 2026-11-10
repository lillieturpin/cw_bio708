

rm(list = ls())
pacman::p_load(tidyverse, patchwork)


#read data
df_count <- read_csv("data_src/data_garden_count.csv")
df_count

df_count%>%
  ggplot(aes(x = nitrate, y = count))+
  geom_point()

#fit lm()
##use geom abline

m_lm <- lm(count ~ nitrate, data = df_count)

df_count%>%
  ggplot(aes(x = nitrate, y = count))+
  geom_point()+
  geom_abline( intercept = coef(m_lm)[1],
               slope= coef(m_lm)[2])

#poisson regression for count data
m_pois <- glm(count ~ nitrate, data = df_count, family = "poisson")
summary(m_pois)

##use intercept and slope as prediction with geom abline


df_count%>%
  ggplot(aes(x = nitrate, y = count))+
  geom_point()+
  geom_abline( intercept = coef(m_pois)[1],
               slope= coef(m_pois)[2])

## link function, log(lambda) = alpha + beta* x for Poisson

x <- seq(min(df_count$nitrate),
         max(df_count$nitrate),
         length = 100)
a <- coef(m_pois)[1] #intercept in log scale]
b<- coef(m_pois)[2] # slope in log scale

log_y_pred <- a + b * x
y_pred <- exp(log_y_pred)

df_pred <- tibble(y_pred = y_pred,
                  nitrate =x)

df_count%>%
  ggplot(aes(x = nitrate, y = count))+
  geom_point()+
  geom_line(data = df_pred,
            aes( y= y_pred))

#compare results between lm() and glm()

summary(m_lm)
summary(m_pois)


#proportional data

#binomial
df_mussel <- read_csv("data_src/data_mussel.csv")
df_mussel

df_mussel%>%
  mutate(p = n_fertilized / n_examined)%>%
  ggplot(aes(x = density, y = p))+
  geom_point()

#binomial regression
#n_examined is possible maximum success
#n_fertilized is success number
#binomial dist takes two parameters
#Bin(N,p) where N is number of trials, p is success of prob
  
#cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized)  

m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density, data = df_mussel, family = "binomial")
summary(m_binom)

 ##draw prediction 

z <- seq(min(df_mussel$density),
         max(df_mussel$density),
         length = 100)


theta <- coef(m_binom)

logit_y_pred <- theta[1]+ theta[2] *z
y2_pred <- exp(logit_y_pred)/(1 + exp(logit_y_pred))

df_pred2 <- tibble(y2_pred = y2_pred,
                  density = z)

df_mussel%>%
  mutate(p = n_fertilized / n_examined)%>%
  ggplot(aes(x = density, y = p))+
  geom_point()+
  geom_line(data = df_pred2,
            aes( y= y2_pred))
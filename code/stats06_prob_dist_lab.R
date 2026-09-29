rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)



df_ex <- rnorm(n = 50, mean = 10, sd = 2)

mu <- mean(df_ex)
sigma <- sd(df_ex)

ex_min <- floor(min(df_ex))
ex_max <- ceiling(max(df_ex))


bin <- seq(ex_min, ex_max, by = 1)

p <- NULL
for( i in 1:(length(bin)-1)) {
  #prob up to bin [i=1]
  ex_u <- pnorm(bin[i +1],  mean = mu, sd = sigma) 
  #prob up to bin[i]
  ex_l <- pnorm(bin[i], mean = mu, sd = sigma)
  ##= difference of ex_u and ex_l
  p[i]<- ex_u - ex_l }

 df_prob<-tibble(p=p, bin = bin[-length(bin)]+0.5)%>%
   mutate(freq = p* length (df_ex))
 
 #histogram
  
   df_x <- tibble (x =df_ex)
   
   

df_x %>% 
  ggplot(aes(x = x)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5) + # bin's center position
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "hotpink") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "hotpink")





# second part -------------------------------------------------------------


df_xp <- rpois(1000, 7)

lambda <- mean(df_xp)


xp_min <- floor(min(df_xp))
xp_max <- ceiling(max(df_xp))


bin <- seq(xp_min, xp_max, by = 1)
pm <- dpois(x = bin, lambda = lambda)

df_x2 <- tibble(x = df_xp)

df_prob2 <- tibble(pm=pm, bin=bin)%>%
  mutate(freq = pm * nrow(df_x2))


df_x2%>%
  ggplot(aes(x = df_xp))+
  geom_histogram(binwidth = 1, center = 0) + #changed binwidth to 1 so the histogram would not have gaps
  geom_line(data = df_prob2,
            aes(x= bin, y= freq ),
            linetype = "dashed", color = "purple")+
  geom_point(data = df_prob2,
             aes(x=bin, y= freq),
             color = "purple")
# probability distribution

rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)


# load csv data on R
df_h0 <- read_csv("data_src/data_plant_height.csv")

df_h0 %>% 
  ggplot(aes(x = height))+
  geom_histogram(
    binwidth = 1,
    center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height)))

# draw prob dist 
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

#fig
tibble(y = pd, x = x)%>%
  ggplot(aes(x =x, y=y))+
  geom_line()+
  labs(y = "Probability density",
       x = "Plant height") #change name function

#convert prob dist to freq
p10 <- pnorm(q = 10, mean = mu, sd = sigma)
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
p20-p10

x_min <- floor(min(df_h0$height))
x_max <- ceiling(max(df_h0$height))
bin <- seq(x_min, x_max, by =1)

p <- NULL
for( i in 1:(length(bin)-1)){
  #prob up to bin [i=1]
  p_up <- pnorm(bin[i +1],  mean = mu, sd = sigma) 
  #prob up to bin[i]
  p_low <- pnorm(bin[i], mean = mu, sd = sigma)
  ##= difference of p_up and p_low
  p[i]<- p_up - p_low
}

#data frame
df_prob <- tibble(p, bin=bin[-length(bin)]+ 0.5)%>%
  mutate(freq = p * nrow(df_h0))
#combine data and PDF
df_h0 %>%
  ggplot(aes(x=height))+
  geom_histogram(
    binwidth = 1,
    center = 0.5)+
  geom_point(data = df_prob,
              aes(y = freq,
                  x = bin),
              color = "salmon") +
  geom_line(data = df_prob,
            aes(y =freq,
                x = bin),
            color = "salmon")




# discrete variable -------------------------------------------------------


df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)

df_count%>%
  ggplot(aes(x = count))+
  geom_histogram(binwidth = 0.5, center = 0)

x <- seq(0, 10, by =1)

lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)
pm
#figure

tibble(y = pm, x=x)%>%
  ggplot(aes(x=x, y=y))+
  geom_line(linetype = "dashed")+
  geom_point()+
  labs(x = "Count",
       y = "Probability")

df_prob <- tibble(x=x, y=pm)%>%
  mutate(freq = y * nrow(df_count))

df_count%>%
  ggplot(aes(x =count))+
  geom_histogram(binwidth = 0.5, center = 0) +
  geom_line(data = df_prob,
            aes(x=x, y= freq),
            linetype = "dashed", color = "purple")+
  geom_point(data = df_prob,
             aes(x=x, y=freq),
             color = "purple")



#morethan two groups anova
pacman::p_load(tidyverse)
rm(list= ls())

df_anova <- read_csv("data_src/data_fish_length_anova.csv")
distinct(df_anova, lake)

df_anova %>%
  ggplot(aes(x = lake, y = length))+
  geom_violin(quantiles.linetype = 0.5, #original draw_quatiles
              alpha = 0.2)+
  geom_jitter(height = 0, width = 0.1, alpha = 0.2)

#anova
aov(length ~ lake, data = df_anova) #variable on left what you want to explain #right is predictor

#between group
#overall mean
mu <- mean(df_anova$length)
#group specific mean
df_g <- df_anova %>%
  group_by(lake)%>%
  summarize(mu_g = mean(length), dev_g = (mu_g - mu)^2, #dev from overall mean
            n = n())
df_g

ss_b <- df_g %>%
  mutate(ss_g = dev_g*n)%>%
  pull(ss_g)%>%
  sum()
ss_b

ss_w <- df_anova%>%
  group_by(lake)%>%
  mutate(mu_g = mean(length))%>% #mutate gives you the mean for each individual not just the 3 different summarize would give you
  ungroup()%>%
  mutate(dev_i = (length - mu_g)^2)%>%
  pull(dev_i)%>%
  sum()
ss_w

#overall variability
ss_o <- sum((df_anova$length - mu)^2)
ss_b +ss_w

summary(aov(length ~ lake, data = df_anova))

#convert variability to variance

sig_b <- ss_b / 2
sig_w <- ss_w / (nrow(df_anova) - n_distinct(df_anova$lake))

f_value <- sig_b / sig_w

#null hypothesis dist
f <- seq(0,10, by = 0.01)
pd <- df(f, df1 = 2, df2 = 147)

tibble(x = f, y = pd)%>%
  ggplot(aes(x = f, y= pd))+
  geom_line()+
  geom_vline(xintercept = f_value, color = "hotpink")+
  labs( x = "F statistic",
        y = "Probability Density")
#calc pvalue


p_value <- 1 - pf(f_value, df1 =2, df2 =147)
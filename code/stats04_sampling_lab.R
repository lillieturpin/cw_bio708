# sampling

rm(list = ls()) #clears environment
pacman::p_load(tidyverse, patchwork)

df_h0 <- read_csv("data_src/data_plant_height.csv")

mu <- mean(df_h0$height)

#true variance
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

#random sample
df_i <- df_h0 %>%
  sample_n( size =50)

mu_i <- var_i <- var_ub_i <- NULL #corrected must be before for()
# for loop
for (i in 1:100) {
  df_i <- df_h0 %>%
    sample_n( size =50)
  
  #mean subset
  mu_i[i] <- mean(df_i$height)
  
  #variance subset
  var_i[i] <- sum((df_i$height - mu_i[i])^2) / nrow(df_i)
  var_ub_i[i] <- var(df_i$height)
}

# draw histograms ----
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)
g_mu
# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))
g_var_ub

g_mu / g_var_ub

#sample size 100

df_h0 <- read_csv("data_src/data_plant_height.csv")

mu2 <- mean(df_h0$height)

#true variance
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

#random sample
df_i2 <- df_h0 %>%
  sample_n( size =100)

mu_i2 <- var_i2 <- var_ub_i2 <- NULL #corrected must be before for()
# for loop
for (i in 1:100) {
  df_i2 <- df_h0 %>%
    sample_n( size =100)
  
  #mean subset
  mu_i2[i] <- mean(df_i2$height)
  
  #variance subset
  var_i2[i] <- sum((df_i2$height - mu_i2[i])^2) / nrow(df_i2)
  var_ub_i2[i] <- var(df_i2$height)
}

# draw histograms ----
df_sample <- tibble(mu_hat2 = mu_i2,
                    var_hat2 = var_i2,
                    var_ub_hat2 = var_ub_i2)

# histogram for mu
g_mu2 <- df_sample %>% 
  ggplot(aes(x = mu_hat2)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for unbiased variance
g_var_ub2 <- df_sample %>% 
  ggplot(aes(x = var_ub_hat2)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i2, var_ub_i2)),
                               max(c(var_i2, var_ub_i2))))


(g_mu / g_var_ub) | (g_mu2 / g_var_ub2)

# section 2 ---------------------------------------------------------------

mu <- mean(df_h0$height)

#true variance
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)
#nonrandom sample
df_h10 <- df_h0 %>% 
  filter(height >= 10)


mu_10 <- var_10 <- var_ub_10 <- NULL #corrected must be before for()
# for loop
for (i in 1:100) {
  df_h10sample <- df_h10 %>%
    sample_n( size =50)
  
  #mean subset
  mu_10[i] <- mean(df_h10sample$height)
  
  #variance subset
  var_10[i] <- sum((df_h10sample$height - mu_10[i])^2) / nrow(df_h10sample)
  var_ub_10[i] <- var(df_h10sample$height)
}

# draw histograms ----
df_sample <- tibble(mu_hat10 = mu_10,
                    var_hat10 = var_10,
                    var_ub_hat10 = var_ub_10)

# histogram for mu
g_mu10 <- df_sample %>% 
  ggplot(aes(x = mu_hat10)) +
  geom_histogram() +
  geom_vline(xintercept = mu)


# histogram for unbiased variance
g_var_ub10 <- df_sample %>% 
  ggplot(aes(x = var_ub_hat10)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_10, var_ub_10)),
                               max(c(var_10, var_ub_10))))



#comparison of both
(g_mu / g_var_ub) | (g_mu2 / g_var_ub2) | (g_mu10 / g_var_ub10)



# extra pivot -------------------------------------------------------------------
#I think my graphs look odd here for some reason
df_sample %>%
  pivot_longer(
    cols = everything(),
    names_to = "measure",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram()+
  facet_wrap(facets =~ measure)

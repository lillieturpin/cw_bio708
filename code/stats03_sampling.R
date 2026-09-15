# sampling

rm(list = ls()) #clears environment
pacman::p_load(tidyverse, patchwork)

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id =1:10, height =h, unit = "cm")
df_h1 <- df_h1 %>%
  mutate(mu_height= mean(h),
         var_height = sum((height - mu_height)^2) / nrow(.))

#2 more variables
#muheight
# varheight
h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)


# parameter ---------------------------------------------------------------



#read csv file from source
# load csv data on R
df_h0 <- read_csv("data_src/data_plant_height.csv")

# show the first 10 rows
print(df_h0)

mu <- mean(df_h0$height)


print(mu)

#true variance
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

#random sampling
df_i <- df_h0 %>%
  sample_n( size =10)

mu_i <- var_i <- NULL #corrected must be before for()
# for loop
for (i in 1:1000) {
  df_i <- df_h0 %>%
    sample_n( size =10)
  #mean subset
  mu_i <- mean(df_i$height)
  
  #variance subset
  var_i <- sum((df_i$height - mu_i)^2) / nrow(df_i)
}



df_i <- df_h0 %>%
  sample_n( size =10)

#mean subset
mu_i <- mean(df_i$height)

#variance subset
var_i <- sum((df_i$height - mu_i)^2) / nrow(df_i)

#visualization
df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

g_mu <- df_sample%>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram()+
  geom_vline(xintercept = mu)

g_var <- df_sample%>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram()+
  geom_vline(xintercept = sigma2)


# how pathwork works

g_mu + g_var

g_mu/g_var


# bias corrected version --------------------------------------------------



mu_i <- var_i <- var_ub_i <- NULL

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
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

# histogram for variance
# scale_x_continuous() adjusts scale in x-axis
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

g_mu / g_var / g_var_ub

df_sample <- df_sample %>%
  mutate(var_ub_hat = var_ub_i)

g_var_ub <- df_sample %>%
  ggplot(aes(x = var_ub_hat))+
  geom_histogram()+
  geom_vline(xintercept = sigma2)

# combined figure
g_var <- g_var +scale_x_continuous(limits = c(0, 80))

g_mu / g_var / g_var_ub
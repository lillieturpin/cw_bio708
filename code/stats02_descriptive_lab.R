# Stats descriptive lab



# central tendency --------------------------------------------------------

z <-exp(rnorm(n = 100, mean = 0, sd = 0.1))
z
library(tidyverse)
z_for_histo <- as_tibble(z)



am_z <- sum(z)/length(z)
gm_z <- prod(z)^(1/length(z))
med_z <- median(z)

ggplot(data = z_for_histo, 
       aes(x = z))+
  geom_histogram()+ 
  geom_vline(xintercept = am_z, color ="blue")+ 
  geom_vline(xintercept = gm_z, color="purple")+ 
  geom_vline(xintercept = med_z, color="yellow")


z_rev <- c(-z + max(z) + 0.5)
z_rev_for_histo <- as_tibble(z_rev)

am_z_rev <- sum(z_rev)/length(z_rev)
gm_z_rev <- prod(z_rev)^(1/length(z_rev))
med_z_rev <- median(z_rev)

ggplot(data = z_rev_for_histo, 
       aes(x = z_rev))+
  geom_histogram()+ 
  geom_vline(xintercept = am_z_rev, color ="pink")+ 
  geom_vline(xintercept = gm_z_rev, color="orange")+ 
  geom_vline(xintercept = med_z_rev, color="red")

# variation ---------------------------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

m <- (w*1000)

m_var <- sum((m - mean(m))^2) / length(m)
w_var <- sum((w - mean(w))^2) / length(w)
m_var
w_var

sqrt(m_var)
sqrt(w_var)

ad_m <- abs(m - median(m))
ad_m
ad_w <- abs(w - median(w))
ad_w

mad_m <- (median(ad_m))
mad_m
mad_w<- (median(ad_w))
mad_w

sd_m <- (sqrt(m_var))
(cv_m <- (sd_m/ mean(m)))

sd_w <- (sqrt(w_var))
(cv_w <- (sd_w/ mean(w)))


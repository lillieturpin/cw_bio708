#Likelihood

rm(list = ls())
pacman::p_load(tidyverse, patchwork)

#poisson dist likelihood

#prob of observing 3 given lambda
(pr3 <- dpois(x = 3,lambda = 3.5))

#try 0.1-10 as pssoible values of lambda
(pr3 <- dpois(x = 3,lambda = 3.5))
lambda <- seq(0.1, 10, by = 0.1)


#not useful cannot arrange (pr0 <- dpois(x = 3,lambda = lambda))

#must use tibble
df_pois <- tibble(lambda = lambda,
       pr = dpois(3, lambda = lambda))%>%
  arrange


# vector case y = 3, 2, 5
#prob of observing 3 data points together
#use lamba 3.5 again

y <- c(3, 2, 5)

(pry <- dpois(x = y,lambda = 3.5))

like <- pry[1]*pry[2]*pry[3]

like
like <- prod(pry)
#likelihood for each lambda value
#prob of observing vector y
lambda <- seq(0.01, 10, by = 0.01)
like_i <- sapply(lambda, function(x){prod(dpois(y, lambda = x))})

df_poislike <- tibble(lambda = lambda,
                  like = like_i)%>%
  arrange(desc(like))
df_poislike

#sample mean of y
mean(y)

#why sample mean matches amx likelihood est
dpois(x= 3, lambda = 3.5)
(3.5^3 * exp(-3.5)) / factorial(3)

#visual
# visualize
df_poislike %>% 
  ggplot(aes(x = lambda,
             y = like)) +
  geom_line() +
  scale_y_continuous(trans = "log") +
  labs(y = "Likelihood")


# get likelihood from glm

# load garden plant data
df_count <- read_csv("data_src/data_garden_count.csv")

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

logLik(m_pois) #result should not be positive

summary(m_pois)
#likelihood cannot be compared across data sets
#likelihood only works if samples are independent


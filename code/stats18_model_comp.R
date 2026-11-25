#model comparison

rm(list = ls())
pacman::p_load(tidyverse, patchwork)

#model fit

set.seed(1)

#sample size
n <- 100

#true intercept and slope
b <- c(0.1,0.5)

#hypo predict
x1 <- rnorm(n=n, mean = 0, sd = 1)


#design matrix
X <- model.matrix(~x1)

#simulated y
y_hat <- X %*% b
y <- rnorm(n=n, mean = y_hat,sd = 0.5)

df0 <- tibble(y = y, x1 = x1)

df0 %>%
  ggplot(aes(x = x1, y=y))+
  geom_point()

m1 <- lm(y ~ x1, data = df0)
summary(m1)

#adding unrelated variables

df0 <- df0%>%
  mutate(x2 = rnorm(n=n))

m2 <- lm(y ~ x1 + x2, data = df0)
summary(m2)


summary(m1$adj.r.squared)
summary(m2$adj.r.squared)

#non normal case
df_count <- read_csv("data_src/data_garden_count.csv")%>%
  mutate(moisture = runif(nrow(.), 0, 100))

m_pois0 <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")

m_pois1 <- glm(count ~ nitrate + moisture, data = df_count, family = "poisson")

m_pois0
m_pois1
s_pois0 <- summary(m_pois0)
s_pois1 <- summary(m_pois1)

##for mdoel 0 with nitrate
pseudoR <- 1 - s_pois0$deviance / s_pois0$null.deviance
adj.pseudoR <- 1 - ((s_pois0$deviance - 1) / s_pois0$null.deviance)

#model 1
pseudoR1 <- 1 - s_pois1$deviance / s_pois1$null.deviance
adj.pseudoR1 <- 1 - ((s_pois0$deviance - 2) / s_pois0$null.deviance)


#likelihood ratio test

##normal dist case
anova(m1, m2, test = "Chisq")

#poisson case
anova(m_pois0, m_pois1, test = "Chisq")


#AIC
##out of sample prediction
##AIC = 2K - 2 ln L

#get aic value from model
AIC(m1)
AIC(m2)

#poisson case

AIC(m_pois0)
AIC(m_pois1)

#when you see |AIC1 - AIC 2| > 2, their models predictive capacities are 
#meaningfully different


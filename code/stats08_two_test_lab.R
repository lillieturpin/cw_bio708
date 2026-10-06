rm(list = ls())

pacman::p_load(tidyverse)

df_fl <- read_csv("data_src/data_fish_length.csv")

xs <- rnorm(n=10, mean=10, sd=5)
ys <- rnorm(n=10, mean=12, sd=5)
xl <- rnorm(n=100, mean=10, sd=5)
yl <- rnorm(n=100, mean= 12, sd=5)

t.test(xs, ys, var.equal = TRUE)
t.test(xl, yl, var.equal = TRUE)


# uncertainty -------------------------------------------------------------

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

df_ab <- tibble(group = c(rep("a1", 10), rep("a2", 10),
                          rep("b1", 10), rep("b2", 10)),
                value = c(a1, a2, b1, b2))


df_ab_mu <- df_ab %>%
  group_by(group) %>%
  summarize(mu_l = mean(value),
            sd_l = sd(value))
df_ab_mu


df_a <- df_ab %>%
  filter(group == "a1" | group == "a2")

df_a_mu <- df_ab_mu %>%
  filter(group == "a1" | group == "a2")

df_a %>%
  ggplot(aes(x = group, y = value)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25) +
  geom_segment(data = df_a_mu, aes(x = group,
                                   xend = group,
                                   y = mu_l - sd_l,
                                   yend = mu_l + sd_l)) +
  geom_point(data = df_a_mu,
             aes(x = group, y = mu_l),
             size = 2) +
  labs(x = "group", y = "value")
#welchs test
t.test(a1, a2, var.equal = FALSE)
t.test(b1, b2, var.equal = FALSE)


# null hypothesis ---------------------------------------------------------

mu <- mean(df_fl$length)
sig <- sd(df_fl$length)


x <- df_fl %>%
  filter(lake == "a") %>%
  pull(length)
y <- df_fl %>%
  filter(lake == "b") %>%
  pull(length)


t_obs <- t.test(x, y, var.equal = TRUE)$statistic
p_obs <- t.test(x, y, var.equal = TRUE)$p.value


df_t <- NULL
for (i in 1:100) {
  x_null <- rnorm(n = 50, mean = mu, sd = sig)
  y_null <- rnorm(n = 50, mean = mu, sd = sig)
  df_t[i] <- t.test(x_null, y_null, var.equal = TRUE)$statistic
}


tibble(df_t) %>%
  ggplot(aes(x = df_t)) +
  geom_histogram(binwidth = 0.5) +
  geom_vline(xintercept = t_obs, color = "hotpink") +
  geom_vline(xintercept = abs(t_obs), color = "hotpink") +
  labs(x = "t-statistic", y = "Count")

sum(abs(df_t) > abs(t_obs)) / length(df_t)



# bonus welchs test -------------------------------------------------------


df_t <- df_fl %>%
  group_by(lake) %>%
  summarize(mu_l = mean(length), var_l = var(length), n = n())

v_mu <- pull(df_t, mu_l)
v_var <- pull(df_t, var_l)
v_n <- pull(df_t, n)


t_welch <- (v_mu[1] - v_mu[2]) / sqrt((v_var[1] / v_n[1]) + (v_var[2] / v_n[2]))


df_welch <- ((v_var[1] / v_n[1]) + (v_var[2] / v_n[2]))^2 /
  (((v_var[1] / v_n[1])^2 / (v_n[1] - 1)) + ((v_var[2] / v_n[2])^2 / (v_n[2] - 1)))


pr_down <- pt(q = t_welch, df = df_welch)
pr_up <- 1 - pt(q = abs(t_welch), df = df_welch)
p_welch <- pr_down + pr_up

t_welch
df_welch
p_welch

#check
t.test(x, y, var.equal = FALSE)



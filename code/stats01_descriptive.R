#descriptive stats

#call everytime
library(tidyverse)
#vectors x and y
x<- c(15.9, 15.1, 21.9, 13.3, 24.4)
y<- c(15.9, 15.1, 21.9, 53.3, 24.4)
x
y
#arithmetic mean
(mu_x <- sum(x)/ length(x))
mean(x)
(mu_y <- sum(y)/ length(y))
mean(y)

#geometric mean
(mu_x_ge <- prod(x)^(1/length(x)))
exp(sum(log(x)) / length(x))

mu_y_ge <- prod(y)^(1/length(y))

# median

(med_x <- median(x))
(med_y <- median(y))






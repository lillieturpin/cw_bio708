#call every time

library(tidyverse)
#alt o folds sections
# point figure ------------------------------------------------------------
#create section crtl shift R

#basic point figure
iris %>% 
  ggplot(
    mapping = aes(x = Sepal.Length, y = Sepal.Width)
        
  ) +
  geom_point()


# # error no aes
# # shortcut ctrl shift c
# iris %>% 
#   ggplot(
#     mapping = x = Sepal.Length, y = Sepal.Width)
#     
#   ) +
#   geom_point()

#color by species

iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point()

#without pipe
ggplot(data = iris, 
       mapping = aes(x = Sepal.Length,
                     y = Sepal.Width))

#color by species

# iris %>% 
#   ggplot(aes(x = Sepal.Length,
#              y = Sepal.Width)
#   color = Species)) +
#   geom_point()

#everything colored together
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)
  ) +
  geom_point(color = "purple")


# line figure -------------------------------------------------------------

df0 <- tibble(
     x = rep(1:50, 3),
     y = x * 2)
 
 df0 %>% 
   ggplot(aes(x = x,
              y = y)) +
   geom_line()
 

# histogram ---------------------------------------------------------------

iris %>% 
   ggplot(aes(x = Sepal.Length)) +
   geom_histogram()
 
 #change bin width
 iris %>% 
   ggplot(aes(x = Sepal.Length)) +
   geom_histogram(binwidth = 0.5)
 
 #change bin number
 iris %>% 
   ggplot(aes(x = Sepal.Length)) +
   geom_histogram(bins=50)


# boxplot -----------------------------------------------------------------

 iris %>% 
   ggplot(aes(x = Species,
              y = Sepal.Length)) +
   geom_boxplot()

 #change fill by species
 iris %>% 
   ggplot(aes(x =Species,
              y =Sepal.Length,
              fill = Species)) +
   geom_boxplot()
 
 #change fill by species but consistent color
 iris %>% 
   ggplot(aes(x=Species,
              y=Sepal.Length,
              fill = Species)) +
   geom_boxplot(color = "darkgrey")
 
 #color arg changes border color
 iris %>% 
   ggplot(aes(x =Species,
              y =Sepal.Length,
              color = Species)) +
   geom_boxplot()
 
 iris %>% 
   ggplot(aes(x =Species,
              y =Sepal.Length,
              color = Species,
              fill = Species)) +
   geom_boxplot()
 

# funplot -----------------------------------------------------------------


 # Libraries
 library(ggplot2)
 library(dplyr)
 
 # Load dataset from github
 data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)
 
 # Make the histogram
 data %>%
   filter( price<300 ) %>%
   ggplot( aes(x=price)) +
   geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + theme_classic()
  
 iris %>% 
   ggplot(aes(x = Species)) +
   geom_density(fill ="#69b3a2", color="#e9ecef", alpha=0.8) + theme_classic()
 
 
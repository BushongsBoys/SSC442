# Load libraries 
library(tidyverse)
library(gapminder)

# Exersize 1 

# Assign mpg to a variable
data1 = mpg

# Create a plot comparing disp and hwy
v1 <- ggplot(data = data1, mapping = aes(x = displ, y = hwy)) + 
  geom_point()
# The plot above captured the intuitive relationship I expected: an inverse relationship between disp and hwy 

# Create a plot comparing class and drv
v2 <- ggplot(data = data1, mapping = aes(x = class, y = drv)) + 
  geom_point()
# The above plot showed no useful information because all class of cars could be any combination of these features. No useful insight can be made from it 

#Exersize 1b - add a color level of class to v1 
v3 <- ggplot(data = data1, mapping = aes(x = displ, y = hwy, color = class)) + 
  geom_point()



# Below Code is examples from the notes
# ggplot example from notes
gapminder
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()


# Exploring our saved p in more detail
p + geom_point()
p + geom_smooth()

p + geom_point() + geom_smooth(method = "lm") + scale_x_log10()

# Try it: The scale_x_log10() function changes the scale to a logrithmic scale with base 10
# What this means is that each tick mark represents 10 times the previous one. It is used to spread out 
# data that has most of its points at lower x values because each tick mark will have larger intervals as 
# you go further positive on the x axis. Therefore, more points fall into this interval and the data will be 
# more spread out. It is important to note that the relationship looks linear, but that's only because of the scaling. 
# In actuality there is a logrithmic relationship between the two variables. 

library(scales)
p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar)

p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::number)

# Try it: The dollar sign changes the labels out of scientific and into a numerical amount in dollars. 
# There are also other options such as number which just gives you the number scale and several others which make a similar trasnformation with slightly different outcomes.



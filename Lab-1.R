# Load libraries 
library(tidyverse)
library(gapminder)

# Exersize 1 

# Assign mpg to a variable - hi there 
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

# Exersize 2
bankData <- read.csv("bank.csv")

# Subset data to only 
month_y <- as_tibble(bankData[,c("month","y")])

successRateFrame <- month_y %>%
  group_by(month) %>% 
  summarize(Count = n(), Y = sum(y == "yes"), N = sum(y == "no"), Success_Rate = sum(y == "yes")/n()) 

successRatePlot <- ggplot(data = successRateFrame, mapping = aes(x= reorder(month, -Success_Rate) , y = Success_Rate)) + 
  geom_bar(stat = "identity", color = "purple", fill = "purple") + 
  labs(x = "Month", y = "Subscription Success Rate",title = "Subscription Success Rate by Month") + 
  theme(plot.title = element_text(hjust = .5, size = 18)) + 
  geom_text(
    aes(label=round(Count)), 
    size = 5, fontface = 2, color = 'black', 
    hjust=0.5, vjust=-1) + 
  ylim(0,.5)













##########################################################################
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

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = 'yellow'))
p + geom_point() + scale_x_log10()
# Try it : When running above code, it gives you a legend labeled yellow, but the dots are red, interesting
# Try it : Mapping in the graph is determined by data in color 
# If you put anything in aes() it is treated as data which doesn't make sense

# Correct way to do it
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point() + geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + scale_x_log10()
# Try it: color changes the line to orange, size changes the thickness of the line, se determines whether to include confidence interval, method determines smoothing function to use

p + geom_point(alpha = 0.3) +  # alpha makes the points more standard
  geom_smooth(method = "gam") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data Points are country-years",
       caption = "Source: Gapminder")

library(scales)
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))
p + geom_point()
p + geom_point() + scale_x_log10(labels = dollar)
p + geom_point() + scale_x_log10(labels = dollar) + geom_smooth()

# Try it: the fill argument changes the fill of each point. Also, the match of colors and error bands makes this visualization a little noisy

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) + geom_smooth() + scale_x_log10()
# Try it: There is only one smooth line rather and one per continent because the color is determined 
# in geom_point rather than in ggplot. This keeps the data together, and doesn't create essentially 
# a pool of data per continent. 

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
  geom_smooth(mapping = aes(color = continent, fill = continent)) +
  scale_x_log10() +
  geom_smooth(mapping = aes(color = continent), method = "gam")
# Try it: What is bad about the graph is there is multiple curves for each continent. 
# For this reason we should set aes at the top rather than at each one as it will double count things





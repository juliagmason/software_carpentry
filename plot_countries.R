## Programming in R

# libraries ----
library(tidyverse)

# data ----
url <- 'https://raw.githubusercontent.com/OHI-Science/data-science-training/master/data/gapminder.csv'
gapminder <- read_csv(url) # View(gapminder)

# ggplot: after filter by country ----
gapminder %>%
  filter(country == "Afghanistan") %>%
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Afghanistan")

# ggsave: after filter by country & plot ----
png <- "gdp_Afghanistan.png"

g <- gapminder %>%
  filter(country == "Afghanistan") %>%
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Afghanistan")

ggsave(filename = png, plot = g) 

# function: for plotting any country ----
plot_country <- function(cntry){
  # debug?
  # browser()
  png <- paste0("gdp_", cntry, ".png")
  # tell us what country you're doing. 
  cat ("country_plot(", cntry, ")\n") # cat pastes and prints. \n means new line
  
  g <- gapminder %>%
    filter(country == cntry) %>%
    ggplot(aes(x = year, y = gdpPercap)) +
    geom_point() +
    geom_smooth() +
    labs(title = cntry)
  
  ggsave(filename = png, plot = g)
}

plot_country ("Afghanistan")

country_list <- as.list ((unique (gapminder$country)))
lapply (country_list, plot_country)

countries <- head (unique (gapminder$country))
for (k in 1:length (countries)){
  country <- countries[k]
  cat (sprintf("%03d: %s \n", k, countries[k])) # 03 pads 3 zeros. %d is digit (k is a digit), %s tells it that countries is a string
  plot_country (country)
}

# can also do it this way. Can do it for any vector
for (k in countries){
  plot_country (k)
 
}

# function with ifelse statement ----

# create new folder
dir.create("developed")
dir.create("developing")
## WOWOW option command down arrow copies the line

is_developed <- function (cntry, threshold = 12000) {
  gapminder %>% 
    filter (country == cntry) %>% 
    summarise (
      mean_gdp = mean (gdpPercap) # this is a dataframe. need to turn into a single value
    ) %>% 
    .$mean_gdp > threshold # .$ trick extracts df value. "." represents current path or "here"
  
}

is_developed ("United States")
is_developed ("United States", threshold = 5000000000)

# one way to specify a folder to save things in a function is to put folder = "." in your function terms. 
# country_plot <- function (cntry, folder = ".")
# then can specify specific folder names in your function call. 
# country_plot ("Peru", "developing")

# or, modify png name within function. 

# rework plot_country function for flexible filename
plot_country <- function(cntry, png){
  # debug?
  # browser()
  
  # tell us what country you're doing. 
  cat ("country_plot(", cntry, ")\n") # cat pastes and prints. \n means new line
  
  g <- gapminder %>%
    filter(country == cntry) %>%
    ggplot(aes(x = year, y = gdpPercap)) +
    geom_point() +
    geom_smooth() +
    labs(title = cntry)
  
  ggsave(filename = png, plot = g)
}


# loop over countries, and save in appropriate folders
for (k in countries){
  if (is_developed(k)) {
    png <- paste0 ("developed/gdp_", k, ".png")
    } else{
    png <- paste0 ("developing/gdp_", k, ".png")
    }

    plot_country (k, png)
}

# it's easy to make your own package. make a new folder with the package name
# cursor inside function: code --> insert Roxygen skeleton. Fill in parameters, name, etc descriptions
library (devtools)


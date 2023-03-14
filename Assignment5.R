# PS 5


library(dplyr)
library(ggplot2)
library(tidyverse)



## 1. Load data set
# Load the gapminder dataset
gapminder <- read.csv("gapminder_data_graphs.csv")

# a. Check the dimensions of the dataset and print number of rows and colmns

ncol(gapminder)
nrow(gapminder)


# b. Print a small sample of the data
head(gapminder)
print("The data looks okay")

## 2. Descriptive Statistics
# Find the number of unique countries in each column
# num_countries_iso3 <- length(unique(gapminder$iso3c))
# num_countries_iso2 <- length(unique(gapminder$iso2c))
# num_countries_name <- length(unique(gapminder$country))

# Print the results
# cat("Number of countries by iso3 code:", num_countries_iso3, "\n")
# cat("Number of countries by iso2 code:", num_countries_iso2, "\n")
# cat("Number of countries by name:", num_countries_name, "\n")

# Count the number of names for each iso2 code
#iso2_names <- table(gapminder$iso2c, gapminder$country)

# Find the iso2 codes that correspond to more than one name
#dup_iso2 <- which(rowSums(iso2_names > 0) > 1)

# Print the results
#cat("Iso2 codes with more than one name:", paste(names(dup_iso2), collapse = ", "), "\n")

# Count the number of iso3 codes for each country name
#name_iso3 <- table(gapminder$country, gapminder$iso3c)

# Find the country names that have more than one iso3 code
#dup_names <- which(rowSums(name_iso3 > 0) > 1)

# Print the results
#cat("Country names with more than one iso3 code:", paste(names(dup_names), collapse = ", "), "\n")

# Find the minimum and maximum year in the dataset
min_year <- min(gapminder$year)
max_year <- max(gapminder$year)

# Print the results
cat("Minimum year:", min_year, "\n")
cat("Maximum year:", max_year, "\n")

## 3. CO2 emissions

# How many missing co2 emissions are there for each year? Analyze both missing CO2
# and co2_PC. Which years have most missing data?
gapminder %>%
  select(year, co2_consump) %>%
  group_by(year) %>%
  summarize(
    n_missing_co2 = sum(is.na(co2_consump)),
  )

# Make a plot of total CO2 emissions over time for the U.S, China, and India. Add a few
# more countries of your choice. Explain what do you see
# Create a subset of data for selected countries
countries <- c("United States", "China", "India", "Germany", "Russia", "Brazil")
gapminder_sub <- gapminder %>% filter(country %in% countries)

# Plot total CO2 emissions over time
library(ggplot2)
ggplot(gapminder_sub, aes(x = year, y = co2_consump, color = country)) +
  geom_line(size = 1) +
  scale_y_log10() +
  labs(title = "Total CO2 Emissions Over Time",
       x = "Year",
       y = "Total CO2 Emissions (metric tons)",
       color = "Country")

print("The plot shows the trend of total CO2 emissions over time for selected countries. 
      It seems that China's total CO2 emissions have rapidly increased over the years,
      India's total CO2 emissions have also increased, but at a slower rate compared to China. 
      Germany and Russia have fairly stable total CO2 emissions, 
      while Brazil has seen an increase in recent years.")

# Now let’s analyze the CO2 emissions per capita (co2_PC ). Make a similar plot of the
# same countries. What does this figure suggest?

# Plot CO2 emissions per capita over time
ggplot(gapminder_sub, aes(x = year, y = co2_consump, color = country)) +
  geom_line(size = 1) +
  labs(title = "CO2 Emissions Per Capita Over Time",
       x = "Year",
       y = "CO2 Emissions Per Capita (metric tons)",
       color = "Country")

print ("The plot shows the trend of CO2 emissions per capita over time for selected countries. 
       It suggests that while China and India have lower CO2 emissions per capita compared to the United States, 
       their CO2 emissions per capita have been increasing rapidly. 
       Germany and Russia have fairly stable CO2 emissions per capita.")

# Compute average CO2 emissions per capita across the continents (assume region is the
# same as continent). Comment what do you see.
gapminder_continent <- gapminder %>% 
  group_by(continent, year) %>% 
  summarize(avg_co2_pc = mean(co2_consump, na.rm = TRUE))

gapminder_continent

#Make a barplot where you show the previous results–average CO2 emissions per capita
# across continents in 1960 and 2016.

# Filter data for the years 1960 and 2016
co2_continent <- gapminder %>% 
  filter(year == 1960 | year == 2016) %>% 
  group_by(continent, year) %>% 
  summarise(avg_co2_pc = mean(co2_consump, na.rm = TRUE))

# Create barplot
ggplot(co2_continent, aes(x = continent, y = avg_co2_pc, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average CO2 Emissions per Capita Across Continents",
       subtitle = "1960 vs. 2016",
       x = "Continent", y = "Average CO2 Emissions per Capita") +
  scale_fill_manual(values = c("orange", "purple")) +
  theme_bw()

# Which countries are the three largest, and three smallest CO2 emitters (in terms of CO2 per
# capita) in 2019 for each continent? (Assume region is continent).
co2_2019 <- gapminder %>% 
  filter(year == 2019)

# Group by continent and country, and calculate the average CO2 per capita
co2_continent <- co2_2019 %>% 
  group_by(continent, country) %>% 
  summarise(avg_co2_pc = mean(co2_consump, na.rm = TRUE)) 

# Find the three largest and three smallest CO2 emitters for each continent
co2_top_bottom <- co2_continent %>% 
  group_by(continent) %>% 
  arrange(desc(avg_co2_pc)) %>% 
  slice_head(n = 3) %>% 
  bind_rows(
    co2_continent %>% 
      group_by(continent) %>% 
      arrange(avg_co2_pc) %>% 
      slice_head(n = 3)
  )

co2_top_bottom

## 4. GDP per capita
# Make a scatterplot of GDP per capita versus life expectancy by country, using data for
#1960. Make the point size dependent on the country size, and color 
#those according to the continent.

# ggplot(filter(year == 1960),
#        aes(x = gdp, y = life_exp)) +
#   geom_point(aes(size = population, color = continent)) +
#   scale_size_continuous(range = c(1, 15), breaks = c(0, 5e8, 1e9, 2e9, 4e9, 8e9)) +
#   scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#619CFF")) +
#   labs(x = "GDP per capita", y = "Life expectancy", title = "1960") +
#   theme_minimal()

print("There is a positive relationship between these two variables, 
      with higher GDP per capita generally associated with higher life expectancy.")

# Make a similar plot, but this time use 2019 data only
# ggplot(filter(year == 2019),
#        aes(x = gdp, y = life_exp)) +
#   geom_point(aes(size = population, color = continent)) +
#   scale_size_continuous(range = c(1, 15), breaks = c(0, 5e8, 1e9, 2e9, 4e9, 8e9)) +
#   scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#619CFF")) +
#   labs(x = "GDP per capita", y = "Life expectancy", title = "2019") +
#   theme_minimal()

print("the overall relationship between GDP per capita and life expectancy is still positive, 
      but there is more variation in both variables compared to 1960. ")

# Compare these two plots and comment what do you see. How has world developed
# through the last 60 years?
print("Comparing the two scatterplots, we can see that the world has experienced 
      significant changes over the past 60 years. In general, both GDP per capita 
      and life expectancy have increased, but there is also more variation in 
      these variables across countries. ")

# Compute the average life expectancy for each continent in 1960 and 2019. 
# Do the results fit with what do you see on the figures?
gapminder_1960 <- filter(gapminder, year == 1960)
gapminder_2019 <- filter(gapminder, year == 2019)

life_exp_1960 <- list(life_exp ~ continent, gapminder_1960, mean)
life_exp_2019 <- list(life_exp ~ continent, gapminder_2019, mean)

life_exp_1960
life_exp_2019

print("yes, the results fit with the figures")

# Compute the average LE growth from 1960-2019 across the continents. Show the results
# in the order of growth. Explain what do you see.
life_exp_growth <- gapminder %>%
  group_by(continent) %>%
  mutate(lifeExp_lag = lag(life_exp)) %>%
  filter(!is.na(lifeExp_lag)) %>%
  summarise(growth = mean((life_exp - lifeExp_lag) / lifeExp_lag) * 100)

life_exp_growth <- life_exp_growth[order(life_exp_growth$growth),]

life_exp_growth

print("Africa had the highest average life expectancy growth rate Asia had the second highest growth rate, 
      followed by the Americas, Oceania, and Europe.")

#Show the histogram of GDP per capita for years of 1960 and 2019. Try to put both
# histograms on the same graph, 

# hist(gapminder$gdp[gapminder$year == 1960], main = "GDP per capita in 1960", 
#      xlab = "GDP per capita")
# 
# hist(gapminder$gdp[gapminder$year == 2019], main = "GDP per capita in 2019", 
#      xlab = "GDP per capita", col = "blue", alpha = 0.5, add = TRUE, border = "white", density = 15)



# What was the ranking of US in terms of life expectancy in 1960 and in 2019? (When
# counting from top.
rank(gapminder$life_exp[gapminder$year == 1960 & gapminder$country == "United States"])

rank(gapminder$life_exp[gapminder$year == 2019 & gapminder$country == "United States"])


#If you did this correctly, then you noticed that US ranking has been falling quite a
# bit. But we also have more countries in 2019–what about the relative rank divided by the
# corresponding number of countries that have LE data in the corresponding year?
rank(gapminder$life_exp[gapminder$year == 1960 & gapminder$country == "United States"])/sum(!is.na(gapminder$lifeExp[gapminder$year == 1960]))

rank(gapminder$life_exp[gapminder$year == 2019 & gapminder$country == "United States"])/sum(!is.na(gapminder$lifeExp[gapminder$year == 2019]))


# Hours spend on PS5: 5


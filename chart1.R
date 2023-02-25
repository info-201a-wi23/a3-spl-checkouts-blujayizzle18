library("dplyr")
library("stringr")
library(ggplot2)

A3_data <- read.csv("C:\\Users\\jayja\\Desktop\\A3Data.csv")

A3_data <- A3_data %>% 
  filter(MaterialType == "BOOK") %>% 
  filter(Creator == "Rowling, J. K.")

A3_data <- A3_data %>% 
  filter(str_starts(tolower(Title), "harry potter and the "))


#Total Number of Checkouts of all books per year
Checkouts_per_year <- A3_data %>% 
  group_by(CheckoutYear) %>% 
  summarize(Annual_Checkouts = sum(Checkouts))

#Average Number of Checkouts of all books per year
Average_per_year <- A3_data %>% 
  group_by(CheckoutYear) %>% 
  summarize(Average_Checkouts = mean(Checkouts))

Plot1_data <- left_join(Checkouts_per_year, Average_per_year, by = "CheckoutYear")

colnames(Plot1_data) <- c('Year', 'Annual_Series_Total_Checkouts', 'Annual_Series_Average_Checkouts')

Plot1_data <- Plot1_data %>% 
  mutate(Annual_Series_Average_Checkouts_Scaled = Plot1_data$Annual_Series_Average_Checkouts * 100)
  


ggplot(Plot1_data) +
  geom_line(aes(x= Year, y = Annual_Series_Total_Checkouts, color = "Total Checkouts")) +
  geom_line(aes(x = Year, y = Annual_Series_Average_Checkouts_Scaled, color = "Average Checkouts 10x")) +
  scale_x_continuous(breaks = seq(2005,2023,5)) +
  labs(
    title = "Checkouts Across the Harry Potter Series from 2005-2023",
    y = "Checkouts",
    x = "Year",
    color = "Legend" +
      scale_color_manual(name = colors)
  )

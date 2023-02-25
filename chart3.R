library("dplyr")
library("stringr")
library(ggplot2)


A3_data <- read.csv("C:\\Users\\jayja\\Desktop\\A3Data.csv")

A3_data_book <- A3_data %>% 
  filter(MaterialType == "BOOK") %>% 
  filter(Creator == "Rowling, J. K.")

A3_data_book <- A3_data_book %>% 
  filter(str_starts(tolower(Title), "harry potter and the "))

#Total Number of Checkouts of all books per year
Checkouts_per_year_books <- A3_data_book %>% 
  group_by(CheckoutYear) %>% 
  summarize(Annual_Checkouts = sum(Checkouts))


A3_data_ebook <- A3_data %>% 
  filter(MaterialType == "EBOOK")

A3_data_ebook <- A3_data_ebook %>% 
  filter(str_starts(tolower(Title), "harry potter and the "))

Checkouts_per_year_ebooks <- A3_data_ebook %>% 
  group_by(CheckoutYear) %>% 
  summarize(Annual_Checkouts = sum(Checkouts))

plot3_data <- left_join(Checkouts_per_year_books, Checkouts_per_year_ebooks, by = "CheckoutYear")

ggplot(plot3_data) +
  geom_line(aes(x= CheckoutYear, y = Annual_Checkouts.x, color = "Book Checkouts")) +
  geom_line(aes(x = CheckoutYear, y = Annual_Checkouts.y, color = "EBook Checkouts")) +
  labs(
    title = "Checkouts of Harry Potter EBooks and Books from 2005-2023",
    y = "Checkouts",
    x = "Year",
    color = "Legend" +
      scale_color_manual(values = colors)
  )
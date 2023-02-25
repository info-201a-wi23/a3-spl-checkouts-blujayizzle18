library("dplyr")
library("stringr")
library(ggplot2)


A3_data <- read.csv("C:\\Users\\jayja\\Desktop\\A3Data.csv")

A3_data <- A3_data %>% 
  filter(MaterialType == "BOOK") %>% 
  filter(Creator == "Rowling, J. K.")

A3_data <- A3_data %>% 
  filter(str_starts(tolower(Title), "harry potter and the "))

Chamber_of_Secrets <- A3_data %>% 
  filter(str_starts(tolower(Title), "harry potter and the chamber of secrets")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(CheckoutYear, sum(Checkouts)) %>% 
  distinct(CheckoutYear, .keep_all = TRUE)

Sorcerers_Stone <- A3_data %>% 
  filter(str_starts(tolower(Title), "harry potter and the sorcerer's stone")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(CheckoutYear, sum(Checkouts)) %>% 
  distinct(CheckoutYear, .keep_all = TRUE)

Prisoner_of_Azkaban <- A3_data %>% 
  filter(str_starts(tolower(Title), "harry potter and the prisoner of azkaban")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(CheckoutYear, sum(Checkouts)) %>% 
  distinct(CheckoutYear, .keep_all = TRUE)

Goblet_of_Fire <- A3_data %>% 
  filter(str_starts(tolower(Title), "harry potter and the goblet of fire")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(CheckoutYear, sum(Checkouts)) %>% 
  distinct(CheckoutYear, .keep_all = TRUE)

Order_of_the_Pheonix <- A3_data %>% 
  filter(str_starts(tolower(Title), "harry potter and the order of the phoenix")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(CheckoutYear, sum(Checkouts)) %>% 
  distinct(CheckoutYear, .keep_all = TRUE)

Half_Blood_Prince <- A3_data %>% 
  filter(str_starts(tolower(Title), "harry potter and the half-blood prince")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(CheckoutYear, sum(Checkouts)) %>% 
  distinct(CheckoutYear, .keep_all = TRUE)

Deathly_Hallows <- A3_data %>% 
  filter(str_starts(tolower(Title), "harry potter and the deathly hallows")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(CheckoutYear, sum(Checkouts)) %>% 
  distinct(CheckoutYear, .keep_all = TRUE)

Checkout_per_year_per_book <- left_join(Sorcerers_Stone, Chamber_of_Secrets, by = "CheckoutYear")
Checkout_per_year_per_book <- left_join(Checkout_per_year_per_book, Prisoner_of_Azkaban, by = "CheckoutYear")
Checkout_per_year_per_book <- left_join(Checkout_per_year_per_book, Goblet_of_Fire, by = "CheckoutYear")
Checkout_per_year_per_book <- left_join(Checkout_per_year_per_book, Order_of_the_Pheonix, by = "CheckoutYear")
Checkout_per_year_per_book <- left_join(Checkout_per_year_per_book, Half_Blood_Prince, by = "CheckoutYear")
Checkout_per_year_per_book <- left_join(Checkout_per_year_per_book, Deathly_Hallows, by = "CheckoutYear")

colnames(Checkout_per_year_per_book) <- c('Year','Sorcerers_Stone', 'Chamber_of_Secrets', 'Prisoner_of_Azkaban', 'Goblet_of_Fire', 'Order_of_the_Pheonix','HalfBlood_Prince', 'Deathly_Hallows')


ggplot(Checkout_per_year_per_book) +
  geom_line(aes(x= Year, y = Sorcerers_Stone, color = "Sorcerer's Stone")) +
  geom_line(aes(x = Year, y = Chamber_of_Secrets, color = "Chamber of Secrets")) +
  geom_line(aes(x= Year, y= Prisoner_of_Azkaban, color = "Prisoner of Azkaban")) +
  geom_line(aes(x = Year, y = Goblet_of_Fire, color = "Goblet of Fire")) +
  geom_line(aes(x = Year, y = Order_of_the_Pheonix, color = "Order of the Pheonix")) +
  geom_line(aes(x = Year, y = HalfBlood_Prince, color = "Half-Blood Prince")) +
  geom_line(aes(x = Year, y = Deathly_Hallows, color = "Deathly Hallows")) +
  labs(
    title = "Checkouts of the Harry Potter Books from 2005-2023",
    y = "Checkouts",
    x = "Year",
    color = "Legend" +
      scale_color_manual(values = colors)
  )




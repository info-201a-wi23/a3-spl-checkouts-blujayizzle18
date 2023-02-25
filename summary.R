library("dplyr")
library("stringr")

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

#Year with most number of total checkouts and number of checkouts SUMPOINT 1 SUMPOINT 3
Year_with_most_checkouts <- Checkouts_per_year %>% 
  filter(Annual_Checkouts == max(Annual_Checkouts)) %>% 
  summarize(CheckoutYear, Annual_Checkouts)


#Average number of Checkouts of all books across all years SUMPOINT 2
Average_yearly_checkouts <- Checkouts_per_year %>% 
  summarize(mean(Annual_Checkouts))

#Total Number of Checkouts of all book ever SUMPOINT 4
Total_Checkouts <- Checkouts_per_year %>% 
  summarize(sum(Annual_Checkouts))
  


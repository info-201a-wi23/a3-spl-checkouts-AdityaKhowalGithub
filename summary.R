spl_df <- read.csv("2013-2023-5-Checkouts-SPL.csv")

library("dplyr")
library("stringr")
library("ggplot2")
library("scales")
# Then convert that column to a date value
spl_df <- spl_df %>%
  mutate(date = paste(CheckoutYear, CheckoutMonth, "01", sep = "-")) %>%
  mutate(date = as.Date(date))

### Summary Information
average_number_checkouts <- spl_df %>%
  summarise(avg_check = mean(Checkouts, na.rm = FALSE))
  
interested_book <- spl_df %>%
  filter(grepl('The Sea of Monsters: Percy Jackson and the Olympians Series, Book 2', Title)) %>%
  select(Checkouts, CheckoutMonth, MaterialType) 

interested_book_month <- interested_book %>%
  group_by(CheckoutMonth) %>%
  summarise(checkoutSum = sum(Checkouts))


max_month_check <- interested_book_month %>%
  filter(checkoutSum == max(checkoutSum, na.rm = T)) %>%
  select(CheckoutMonth)
  
least_month_check <- interested_book_month %>%
  filter(checkoutSum == min(checkoutSum, na.rm = T)) %>%
  select(CheckoutMonth)

month_check_ebook <- interested_book %>%
  filter(MaterialType == "EBOOK") %>%
  group_by(CheckoutMonth) %>%
  summarise(checkoutSum = sum(Checkouts))


max_month_check_ebook <- month_check_ebook %>%
  filter(checkoutSum == max(checkoutSum, na.rm = T)) %>%
  select(CheckoutMonth)

least_month_check_ebook <- month_check_ebook %>%
  filter(checkoutSum == min(checkoutSum, na.rm = T)) %>%
  select(CheckoutMonth)

Paper_checkout_Type <- spl_df %>%
  filter(MaterialType == "BOOK") %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts))

change_in_print_checkout <- ggplot(Paper_checkout_Type) +
  geom_line(aes(x = date, y = total_checkouts)) +
  labs(x = "Month/Year", y = "Total Paper Checkouts per month", title = "Paper Checkouts Over Time") +
  theme_minimal()

Paper_checkout_Type <- Paper_checkout_Type %>%
  mutate(change = total_checkouts - lag(total_checkouts))

total_change <- sum(Paper_checkout_Type$change, na.rm = TRUE)







#Write a summary paragraph of findings that includes the 5 values calculated from your summary information R script

#These will likely be calculated using your DPLYR skills, answering questions such as:
  
 # - What is the average number of checkouts for each item?
  #- What is the month or year with the most/least checkouts for a book that you're interested in?
#- What is the month or year with the most/least checkouts for ebooks?
#- How has the number of print book checkouts changed over time?

#Feel free to calculate and report values that you find relevant.


ebook_df <- spl_df %>%
  filter(MaterialType == "EBOOK")

ebooks_per_month <- ebook_df %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts))

# View the results with a line chart
lineplot1 <- ggplot(ebooks_per_month) +
  geom_line(aes(x = date, y = total_checkouts)) +
  labs(x = "Month/Year", y = "Total Ebook Checkouts per month", title = "Ebook Checkouts Over Time") +
  theme_minimal()



spl_df <- spl_df %>%
  mutate(date = paste(CheckoutYear, CheckoutMonth, "01", sep = "-")) %>%
  mutate(date = as.Date(date))

Horizon_checkout_Type <- spl_df %>%
  filter(CheckoutType == "Horizon") %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts))

lineplot2 <- ggplot(Horizon_checkout_Type) +
  geom_line(aes(x = date, y = total_checkouts)) +
  labs(x = "Month/Year", y = "Total Horizon Checkouts per month", title = "Horizon Checkouts Over Time") +
  theme_minimal()

checkout_type <- spl_df %>%
  group_by(CheckoutType) %>%
  summarise(total_checkouts = sum(Checkouts)) 
  
checkout_type <- checkout_type %>%
    mutate(group = ifelse(total_checkouts > 1000000, "above", "below"))




normalPlot <- ggplot(checkout_type, aes(x = CheckoutType, y = total_checkouts, fill = CheckoutType)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) 


split_plot <- ggplot(checkout_type, aes(x = CheckoutType, y = total_checkouts, fill = CheckoutType)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +
  facet_grid(group ~ ., scales = "free_y")



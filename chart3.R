
spl_df <- read.csv("2013-2023-5-Checkouts-SPL.csv")

library("dplyr")
library("stringr")
library("ggplot2")
library("scales")
# Then convert that column to a date value
spl_df <- spl_df %>%
  mutate(date = paste(CheckoutYear, CheckoutMonth, "01", sep = "-")) %>%
  mutate(date = as.Date(date))

checkout_type <- spl_df %>%
  group_by(CheckoutType) %>%
  summarise(total_checkouts = sum(Checkouts)) 
  
checkout_type <- checkout_type %>%
    mutate(group = ifelse(total_checkouts > 1000000, "above", "below"))




normalPlot <- ggplot(checkout_type, aes(x = CheckoutType, y = total_checkouts, fill = CheckoutType)) +
  geom_bar(stat = "identity") +
  labs(x = "Checkout Type", y = "Total Checkouts", title = "Number of Checkouts per Type") +
  scale_y_continuous(labels = comma) 


split_plot <- ggplot(checkout_type, aes(x = CheckoutType, y = total_checkouts, fill = CheckoutType)) +
  geom_bar(stat = "identity") +
  labs(x = "Checkout Type", y = "Total Checkouts", title = "Number of Checkouts per Type") +
  scale_y_continuous(labels = comma) +
  facet_grid(group ~ ., scales = "free_y")

spl_df <- read.csv("2013-2023-5-Checkouts-SPL.csv")

library("dplyr")
library("stringr")
library("ggplot2")
library("scales")
# Then convert that column to a date value
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
  scale_y_continuous(labels = comma) +
  theme_minimal()

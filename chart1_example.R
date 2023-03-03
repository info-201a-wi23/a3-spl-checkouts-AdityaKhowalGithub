spl_df <- read.csv("2013-2023-5-Checkouts-SPL.csv")

library("dplyr")
library("stringr")
library("ggplot2")
library("scales")
# Then convert that column to a date value
spl_df <- spl_df %>%
  mutate(date = paste(CheckoutYear, CheckoutMonth, "01", sep = "-")) %>%
  mutate(date = as.Date(date))

ebook_df <- spl_df %>%
  filter(MaterialType == "EBOOK")

ebooks_per_month <- ebook_df %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts))

# View the results with a line chart
lineplot1 <- ggplot(ebooks_per_month) +
  geom_line(aes(x = date, y = total_checkouts)) +
  labs(x = "Month/Year", y = "Total Ebook Checkouts per month", title = "Ebook Checkouts Over Time") +
  scale_y_continuous(labels = comma) +
  theme_minimal()



# Getting Started with A3 — SPL Library Checkout Data

# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")

# Exercise 1: Load the data
# Download and unzip one or more of the SPL datasets and load here from a file path
spl_df <- read.csv("2013-2023-5-Checkouts-SPL.csv")

# Exercise 2: We want to create a new date column ("date") with the month AND year and a default first day of the month
# Make a new column with checkout month, checkout year, and a default day value ("01") *pasted* together like so: 2020-10-01
# Then convert that column to a date value
spl_df <- spl_df %>%
  mutate(date = paste(CheckoutYear, CheckoutMonth, "01", sep = "-")) %>%
  mutate(date = as.Date(date))

# Exercise 3: Filter using string detect for one of the following authors, or an author of your choice:
# J.R.R. Tolkien, Colleen Hoover, Jane Austen, George Orwell, Stephen King, Michelle Obama

author_df <- spl_df %>%
  filter(str_detect(Creator, "J.R.R. Tolkien|Colleen Hoover|Jane Austen|George Orwell|Stephen King|Michelle Obama"))

# Exercise 4: Now calculate the sum total checkouts for your author's works for *each month/date*

checkouts_per_month <- author_df %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts))

# View the results with a line chart
ggplot(checkouts_per_month, aes(x = date, y = total_checkouts)) +
  geom_line() +
  labs(x = "Month/Year", y = "Total Checkouts", title = "Checkouts Over Time") +
  theme_minimal()
# Exercise 5: Then plot their checkouts by date, and include a title and x,y axes label
# If needed, set x and y axis limits
# Then share in the Discord channel #spl-data





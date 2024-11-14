# chapter 20 excel--------------------------------------------------------------
library(readxl)
library(tidyverse)
library(writexl)

students <- read_excel("data/students.xlsx")
students <- read_excel("data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = c("numeric", "text", "text", "text", "text")
)
students <- students |>
  mutate(
    age = if_else(age == "five", "5", age),
    age = parse_number(age)
  )

# more worksheets
excel_sheets("data/penguins.xlsx") # view worksheets
penguins_Tor <- read_excel("data/penguins.xlsx",
  sheet = "Torgersen",
  na = "NA"
)
penguins_Bis <- read_excel("data/penguins.xlsx",
  sheet = "Biscoe",
  na = "NA"
)
penguins_Dre <- read_excel("data/penguins.xlsx",
  sheet = "Dream",
  na = "NA"
)
penuins <- bind_rows(penguins_Tor, penguins_Bis, penguins_Dre)

# creating a tibble and store to local base
bake_sale <- tibble(
  item = factor(c("brownie", "cupcake", "cookie")),
  quantity = c(10, 5, 8)
)
# format_header change original bold colnames
write_xlsx(bake_sale, "data/bake_sale.xlsx", format_headers = F)


# chapter21 database---------------------------------------------------------------
library(DBI)
library(dbplyr)
library(tidyverse)
# install.packages("duckdb")
library(duckdb)

con <- DBI::dbConnect(duckdb::duckdb())
dbWriteTable(con, "mpg", ggplot2::mpg)
dbWriteTable(con, "diamonds", ggplot2::diamonds)
dbListTables(con)
con |>
  dbReadTable("diamonds") |>
  as_tibble()

sql <- "
  SELECT carat, cut, clarity, color, price
  FROM diamonds WHERE price > 15000
"
as_tibble(dbGetQuery(con, sql))

diamonds_db <- tbl(con, "diamonds")
diamonds_db

big_diamonds_db <- diamonds_db |>
  filter(price > 15000) |>
  select(carat:clarity, price)
big_diamonds_db |>
  show_query()

dbplyr::copy_nycflights13(con)
flights <- tbl(con, "flights")
planes <- tbl(con, "planes")


# chapter22 arrow ---------------------------------------------------------
# install.packages("arrow")
library(arrow)
library(tidyverse)
library(dbplyr, warn.conflicts = FALSE)
library(duckdb)

curl::multi_download(
  "https://r4ds.s3.us-west-2.amazonaws.com/seattle-library-checkouts.csv",
  "data/seattle-library-checkouts.csv",
  resume = TRUE
)
# large dataset, using open_dataset()
seattle_csv <- open_dataset(
  sources = "data/seattle-library-checkouts.csv",
  format = "csv"
)
# 41 million rows
seattle_csv |>
  glimpse()

seattle_csv |>
  count(CheckoutYear, wt = Checkouts) |>
  arrange(CheckoutYear) |>
  collect()

# parquet type
pq_path <- "data/seattle-library-checkouts"
seattle_csv |>
  group_by(CheckoutYear) |>
  write_dataset(path = pq_path, format = "parquet")

library(tidyverse)
library(repurrrsive)
View(gh_repos)
repos <- tibble(gh_repos) |>
  unnest_longer(gh_repos) |>
  unnest_wider(gh_repos)

repos_name10 <- tibble(gh_repos) |>
  unnest_longer(gh_repos) |>
  unnest_wider(gh_repos) |>
  names() |>
  head(10)

repos_sel <- tibble(gh_repos) |>
  unnest_longer(gh_repos) |>
  unnest_wider(gh_repos) |>
  select(id, full_name, description)

chars <- tibble(got_chars) |>
  unnest_wider(got_chars)

characters <- chars |>
  select(id, titles) |>
  unnest_longer(titles) |>
  filter(titles != "") |>
  rename(title = titles)

gmaps_c1 <- gmaps_cities |>
  unnest_wider(json) |>
  unnest_longer(results) |>
  unnest_wider(results)

gmaps_c1 |>
  select(city, formatted_address, geometry) |>
  unnest_wider(geometry) |>
  unnest_wider(location)
locations <- gmaps_c1 |>
  select(city, formatted_address, geometry) |>
  unnest_wider(geometry) |>
  select(!location:viewport) |>
  unnest_wider(bounds) |>
  rename(ne = northeast, sw = southwest) |>
  unnest_wider(c(ne, sw), names_sep = "_")

writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"',
  con = "~/.Renviron"
)

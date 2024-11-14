# chapter7 data import--------------------------------------

# install.packages("janitor") # nolint
library(tidyverse)
library(janitor)

# load data，第一步看列名，第二步看数据类型
# ""可以被识别为NA,添加'N/A'
students <- read_csv("r4ds/data/students.csv", na = c("N/A", ""))
# ’studnet ID'和‘full name’包含空格，重新命名
students |>
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )

# 第二种方法，automatic rename columns names
students <- students |>
  clean_names() |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )

# dataframe没有列名,直接指定列名
read_csv(
  "1,2,3
  4,5,6",
  # col_names = FALSE #nolint
  col_names = c("x", "y", "z")
)

# load data form multiple csv files
sales_files <- c(
  "data/01-sales.csv", "data/02-sales.csv",
  "data/03-sales.csv"
)
read_csv(sales_files, id = "file")
# 2nd method
sales_files <- list.files("data",
  pattern = "sales\\.csv$",
  full.names = T
)
read_csv(sales_files, id = "file")

# write data, 1st
write_csv(students, "data/students-2.csv")
# factor information is lost, reading from a plain-text file, unreliable
read_csv("students-2.csv")
# load exact r object, 2nd
write_rds(students, "data/students.rds")
read_rds("students.rds")

y <- 1:4
mean(y)

reprex::reprex()

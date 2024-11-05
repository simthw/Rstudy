library(tidyverse)
# install.packages("janitor")
library(janitor)
# load data from csv file
students <- read_csv("data/students.csv", na = c("N/A", ""))
students |> 
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )
# automatic rename columns names 
students <- students |> 
  janitor::clean_names() |> 
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )
# load data form multiple csv files
sales_files <- c("data/01-sales.csv", "data/02-sales.csv", 
                 "data/03-sales.csv")
read_csv(sales_files, id = "file")
# 2nd method
sales_files <- list.files("data", pattern = "sales\\.csv$",
                          full.names = T)
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


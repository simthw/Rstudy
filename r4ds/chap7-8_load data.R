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

# 数据类型识别
read_csv("logical,numeric,date,string\n
TRUE,1,2021-01-15,abc\n
false,4.5,2021-02-15,def\n
T,Inf,2021-02-16,ghi")

# 出现’.'非默认语法，对识别数据类型有影响，可不加‘\n'
# 输出结果为char类型
read_csv("x\n
10\n
.\n
20\n
30")
# 查看问题原因
problems(df)
# 指定‘.’为NA
csv1 <- read_csv("x\n
10\n
.\n
20\n
30", na = ".")

csv2 <- ("x,y,z\n1,2,3")
# 指定列的数据类型为character,不指定的话就是numeric
read_csv(csv2,
  col_types = cols(.default = col_character())
)
# 只读取x列
read_csv(csv2,
  col_types = cols_only(x = col_character())
)

# 列出'r4ds/data'文件夹下所有csv文件
csv_files <- list.files("r4ds/data",
  pattern = "\\.csv$",
  full.names = TRUE
)

# 从'r4ds/data'文件夹读取多个csv文件
sales_files <- c(
  "r4ds/data/01-sales.csv", "r4ds/data/02-sales.csv",
  "r4ds/data/03-sales.csv"
)
# 添加'id='file'列,用于区分不同数据来源
read_csv(sales_files, id = "file")

# 将line9-23中读入并处理过的的‘students'存储
write_csv(students, "r4ds/data/students-2.csv")
# factor information is lost, reading from a plain-text file
read_csv("r4ds/data/students-2.csv")

# 'meal_plan',处理好的factor的信息丢失，使用rds文件存储可保存处理过的信息
write_rds(students, "r4ds/data/students.rds")
read_rds("r4ds/data/students.rds")

# 第二种方式，使用arrow包,以parquet文件存储
# install.packages("arrow", type = "mac.binary") mac安装
library(arrow)
write_parquet(students, "r4ds/data/students.parquet")
read_parquet("r4ds/data/students.parquet")

## 手动创建dataframe
# 1. 使用tibble()函数，按照列创建
tibble(
  x = c(1, 2, 5),
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
)
# 2. 使用tribble()函数，按照行创建
tribble(
  ~x, ~y, ~z,
  1, "h", "0.08",
  2, "m", "0.83",
  5, "g", "0.60"
)

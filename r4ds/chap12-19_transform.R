# chapter12 --------------------------------------------
library(tidyverse)
library(nycflights13)

# 逻辑运算符号
flights |>
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay < 20))
# 改写这个代码
flights |>
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
  ) |>
  filter(daytime & approx_ontime)
# 改写,只保留需要的列
flights |>
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used"
  )

## 解释NA的工作方式
# 不知道a的年纪
age_a <- NA
# 不知道b的年纪
age_b <- NA
# a和b的年纪相同?输出NA，因为不知道
age_a == age_b
# 'dep_time==NA'会让每行都生成NA
flights |>
  filter(dep_time == NA)

# 使用is.na()函数
is.na(c(TRUE, NA, FALSE))
is.na(c(1, NA, 3))
is.na(c("a", NA, "b"))
flights |>
  filter(is.na(dep_time))
# 将NA放在最前面
flights |>
  filter(month == 1, day == 1) |>
  arrange(desc(is.na(dep_time)), dep_time)

# 理解NA
df <- tibble(x = c(TRUE, FALSE, NA))
# NA的值可能是TRUE或FALSE，因此NA|FALSE的值是NA
df |>
  mutate(
    and = x & NA,
    or = x | NA
  )

# 比较下面2个代码，’month==11'创建了一个逻辑向量
flights |>
  filter(month == 11 | month == 12)
# 逻辑向量|12，不报错但会选中每一行数据
flights |>
  filter(month == 11 | 12)
# 'month==11|12'运行机制见下面代码
flights |>
  mutate(
    nov = month == 11,
    final = nov | 12,
    .keep = "used"
  )

# 使用'%in%'避免'=='或者'|'的问题
1:10 %in% c(1, 5, 11)
# 找到11月和12月的航班
flights |>
  filter(month %in% c(11, 12))

## 比较对NA处理的不同，NA %in% NA is TRUE
c(1, 2, NA) == NA
# [1] NA NA NA
c(1, 2, NA) %in% NA
# [1] FALSE FALSE  TRUE
flights |>
  filter(dep_time %in% c(NA, 0800))

# 使用any和all函数，any类似于'|',all类似‘&’
flights |>
  group_by(year, month, day) |>
  summarise(
    all_delay = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

# mean(x)给出true的比例，sum(x)给出true的个数
flights |>
  group_by(year, month, day) |>
  summarise(
    all_delay = mean(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )
# 每天航班的平均延迟
flights |>
  filter(arr_delay > 0) |>
  group_by(year, month, day) |>
  summarise(
    ave_delay = mean(arr_delay),
    n = n(),
    .groups = "drop"
  )

# 每天航班的平均提前和延误
flights |>
  group_by(year, month, day) |>
  summarise(
    ave_delay = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ave_before = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# if_else函数
x <- c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")
# ‘0’需要单独处理
if_else(x == 0, "0", if_else(x > 0, "+ve", "-ve", "zero"))
if_else(x < 0, -x, x)
x1 <- c(NA, 1, 2, NA)
y1 <- c(3, NA, 4, 6)
if_else(is.na(x1), y1, x1)

# 使用case_when
case_when(
  x == 0 ~ "0",
  x < 0 ~ "-ve",
  x > 0 ~ "+ve",
  is.na(x) ~ "NA"
)
# 多项条件匹配成功，只使用第一个条件
case_when(
  x == 0 ~ "0",
  x < 0 ~ "-ve",
  x > 0 ~ "+ve",
  # 添加一个条件
  x > 2 ~ "big",
  is.na(x) ~ "NA"
)
flights |>
  mutate(
    status = case_when(
      is.na(arr_delay) ~ "cancelled",
      arr_delay < -30 ~ "very early",
      arr_delay < -15 ~ "early",
      arr_delay <= 15 ~ "on time",
      arr_delay < 60 ~ "late",
      arr_delay < Inf ~ "very late"
    ),
    .keep = "used"
  )

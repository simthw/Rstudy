# longest depature delays
flights |> filter(dep_delay == max(dep_delay, na.rm = TRUE))
# earliest left in the morning
flights |> filter(dep_time == min(dep_time[dep_time < 1200], na.rm = TRUE))
# fastest flights
flights |>
  mutate(speed = distance / (air_time * 60)) |>
  filter(speed == max(speed, na.rm = TRUE))
# was there a flight on everyday of 2013
flights |> distinct(year, month, day)
# farthest distance, least distance
flights |> filter(distance == max(distance, na.rm = TRUE))
flights |> filter(distance == min(distance, na.rm = TRUE))
# automated cleaning column names
flights |>
  janitor::clean_names()

# page 51 --------------------------------------------------------------
# 将时间转换为分钟数的函数
convert_to_minutes <- function(time) {
  hour <- time %/% 100
  minute <- time %% 100
  return(hour * 60 + minute)
}
flights |>
  select(dep_time, sched_dep_time, dep_delay) |>
  mutate(
    dep_time_minutes = convert_to_minutes(dep_time),
    sched_dep_time_minutes = convert_to_minutes(sched_dep_time),
    diff = dep_time_minutes - sched_dep_time_minutes - dep_delay,
    .before = 1
  )
# 筛选dep_time,dep_delay,arr_time,arr_delay
flights |>
  select(dep_time, dep_delay, arr_time, arr_delay) |>
  head(5)
# starts_with函数
flights |>
  select(starts_with(c("dep", "arr"))) |>
  head(5)
# matches函数
flights |>
  select(matches("^dep|arr")) |>
  head(5)
# 多次筛选同一变量，结果只有一次
flights |>
  select(dep_time, dep_time, dep_time) |>
  head(5)
# any_of can specify the columns that have been assigned to a character vector
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |>
  select(
    any_of(variables)
  )
# 筛选时忽略大小写用ignore.case = TRUE # nolint
flights |>
  select(contains("TIME", ignore.case = TRUE)) |>
  head(5)
# 改名然后放在第一列
flights |>
  rename(air_time_min = air_time) |>
  relocate(air_time_min) |>
  head(5)
# arrange时要包含此列
flights |>
  select(tailnum, arr_delay) |>
  arrange(arr_delay) |>
  head(5)

# page 58 --------------------------------------------------------------
# delay最高的航司
flights |>
  group_by(carrier) |>
  slice_max(dep_delay, n = 1) |>
  arrange(dep_delay)
# delay最高和机场有关联吗？
flights |>
  group_by(carrier, dest) |>
  summarise(
    n = n()
  )
flights |>
  group_by(dest) |>
  summarise(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(delay))

# 一天中delay是如何变化的
ggplot(flights, aes(x = dep_time, y = dep_delay)) +
  geom_point(alpha = 0.1)

# slice_函数，n为负数时，列出所有行
flights |>
  slice_min(dep_delay, n = -2)

# count函数的作用
flights |>
  count(year, month, day, sort = TRUE)

#
df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)
df |>
  group_by(y)
df |>
  arrange(y) # 调整了y列的顺序

# summarise的特点
df |>
  # 按y组分组，y/z的话就是按y和z分组
  group_by(y) |>
  summarize(
    mean_x = mean(x),
  )
df |>
  group_by(y, z) |>
  summarise(mean_x = mean(x), .groups = "drop")

# 输出的结果按照分组合并
df |>
  group_by(y, z) |>
  summarise(mean_ = mean(x))
# 新添加一列计算结果
df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))

# page 72 --------------------------------------------------------------
cases2 <- table2 |>
  filter(type == "cases")
population2 <- table2 |>
  filter(type == "population")
rate <- (cases2[4] / population2[4]) * 10000

# page 103 --------------------------------------------------------------
read_csv("x,y\n1,'a,b'",
  quote = "'" # set quote to a single quote
)
# 变量名缺失
read_csv("a,b\n1,2,3\n4,5,6")
# A tibble: 2 x 2
#      a     b
#  <dbl> <dbl>
# 1     1    23
# 2     4    56

# 变量缺失value，填充’NA‘，缺失变量名，合并值
read_csv("a,b,c\n1,2\n1,2,3,4")

# '1,2'和'a,b'数据类型不同
read_csv("a,b\n1,2\na,b")

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying[1]
ggplot(annoying, aes(y = `2`, x = `1`)) +
  geom_point()
annoying |>
  mutate(`3` = `2` / `1`) |>
  rename(one = `1`, two = `2`, three = `3`)

# page210 --------------------------------------------------------------
# 含有NA的数据
flights |>
  mutate(
    dep_time_na = is.na(dep_time),
    sched_dep_time_na = is.na(sched_dep_time),
    dep_delay_na = is.na(dep_delay),
  ) |>
  count(dep_time_na, sched_dep_time_na, dep_delay_na)

# page213 --------------------------------------------------------------
# arr_delay missing but dep_delay not missing
flights |>
  filter(is.na(arr_delay & !is.na(dep_delay)))
# neither arr_time nor sched_arr_time missing, but arr_delay is
flights |>
  filter(!is.na(arr_time) & !is.na(sched_arr_time) & is.na(arr_delay))

# 计算dep_time列的NA数量
flights |>
  filter(is.na(dep_time)) |>
  count()
# 统计dep_time列为NA时，其他列也为NA的数量
flights |>
  filter(is.na(dep_time)) |>
  summarise(across(everything(), ~ sum(is.na(.))))

# dep_time为NA认定为航班取消，每天取消航班数
flights |>
  group_by(year, month, day) |>
  filter(is.na(dep_time)) |>
  summarise(n = n())

# page 219 --------------------------------------------------------------
x <- 0:20
if_else(x %% 2 == 0, "even", "odd")
x1 <- c("Monday", "Saturday", "Wednesday")
x2 <- c(-1, 2, -4, 6, -7, 2)
if_else(x2 < 0, -x2, x2)

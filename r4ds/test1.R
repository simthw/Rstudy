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

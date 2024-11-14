# chapter1 ----------------------------------------------------------------
# run in vscode, set wd
setwd("D:/100study/130_data/136_data for science")


# load library ------------------------------------------------------------
# libraries used in this script
library(ggplot2)
library(dplyr)
library(nycflights13)
library(palmerpenguins)
library(tidyverse)

# global level, 'color' can pass to each geom layer
ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm") # three lines
ggsave("figure/plot1.png", p1, width = 8, height = 6)

# local level, 'color', points colored on species instead of lines
ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species)) + # only points separated based on species
  geom_smooth(method = "lm")
ggsave("figure/plot2.png", p2, width = 8, height = 6)

# make plots beauty by adding labels
ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = body_mass_g)
) +
  # change points color and shapes based on species
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    caption = "Data come from the palmerpenguins package" # 说明
  )
ggsave("figure/plot3.png", p3, width = 8, height = 6)

# describe categorical variable and numeric variable
ggplot(
  data = penguins,
  mapping = aes(x = bill_depth_mm, y = species)
) +
  geom_boxplot() # 各物种depth分布图

# categorical, transforming variable to factor and use bar plot
library(forcats) # categorical,use function fct_inorder
ggplot(penguins, aes(x = fct_inorder(species))) +
  geom_bar()

# numerical variable的分布图, histogram plot
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

# density plot
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

# check difference of 2 figures
# color to margin of bars
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")
# fill all bars with red color
ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

# different binwidth of histogram
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.15)

## visualizing relationship between numericla variable and categorical
# side-by-side boxplot
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
# density plot
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)
# fill density curves
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5) # alpha: transparency

## two categorical variables
# stacked bar plot, 堆积条形图, island和species的关系
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()
# relative frequency, 每个island的species相对频率
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

## more numerical variables relation
# two numerical variables
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
# >2 variables, points represnets species, and shppe represent islands
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))
# 分图，‘~ + categorical variable’
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island) # 按island分图
ggsave("figure/penguins-facet.png")

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species))

# chapter3, data transformation -------------------------------------------
## filter(), select rows
flights |> # pipe operator, different appearance in vs code
  filter(dep_delay > 120)
# &, and operator
jan1 <- flights |> # save the result to 'jan1'
  filter(month == 1 & day == 1)
# %in%, variable equals one of values on the right
flights |>
  filter(month %in% c(1, 2)) # month is 1 or 2
# character, must add quotation
flights |>
  filter(dest %in% c("IAH", "HOU"))

## arrange(), sort rows
# in descending order, big-to-small
flights |>
  arrange(desc(dep_delay)) # most to least delayed
# sory by year, month, day and dep_time
flights |>
  arrange(year, month, day, dep_time)

# distinct(), select unique rows
flights |>
  distinct() # remove duplicate rows
# find all unique origin and destination pairs
flights |>
  distinct(origin, dest) # don't keep other columns
# keep other columns when filtering for unique rows
# find the first occurrence of a unique row in the dataset
flights |>
  distinct(origin, dest, .keep_all = TRUE)
# count numbers of occurrences and arrange numbers in descenging order
flights |>
  count(origin, dest, sort = TRUE)

## column operations
# add new coulmn to dataset, default position on the right side
# .before or .after change position
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60, # speed
    .after = day # right of column 'day'
  )
# .keep only keep 5 columns used in this calculation
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

# select columns, zoom in subsets of columns
flights |>
  select(year, month, day)
flights |>
  select(year:day)
flights |>
  select(where(is.character))

# rename columns
flights |>
  rename(tail_num = tailnum)

# move column variable position
flights |>
  relocate(time_hour, air_time)
flights |>
  relocate(starts_with("arr"), .before = dep_time)

# flights data,then filter,then mutate, then select, then arrange
flights |>
  filter(dest == "IAH") |> # filter rows with dest "IAH"
  mutate(speed = distance / air_time * 60) |> # calculate speed
  select(year:day, dep_time, carrier, flight, speed) |> # select columns
  arrange(desc(speed)) |> # fast->slow # nolint
  head(10)

# groups, counts
flights |>
  group_by(month) |> # subsequent operations work by month
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n() # counts, each group numbers
  )
flights |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1) |> # slice_max, n=1必须加
  # slice_max(arr_delay,n=1,with_ties=FALSE)|> #不保留相同值的行
  relocate(dest)

## groups based on more variable
daily <- flights |>
  group_by(year, month, day)

# grouped conflict with summarize
daily_flights <- daily |>
  # 对已分组数据汇总，.groups = "drop_last"表示不保留分组信息
  summarize(n = n(), .groups = "drop_last")

# ungroup(), 去除以前的分组
daily |>
  ungroup() |> # ungrouped dataframe
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  )
flights |>
  summarise(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = month # 按月分组
  ) |>
  arrange(month) # month按升序排列

## combination of plot and pipe
# 数据集写入到本地file
library(Lahman)
write.csv(Batting, "r4ds/data/Batting.csv", row.names = FALSE)
# 数据集中，如果n样本太小，performance不准确
batters <- Batting |>
  group_by(playerID) |>
  summarise(
    # batting average, performance
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  ) |>
  arrange(desc(n))

# |> for data tidy, + for plot
batters |>
  filter(n > 100) |>
  ggplot(aes(n, performance)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE)

# chapter4 code style ----------------------------------------------------
library(styler)
# |> and + operator in writing codes
# |> and + position
flights |>
  group_by(month) |>
  summarise(
    delay = mean(arr_delay, na.rm = TRUE)
  ) |> # )position
  ggplot(aes(x = month, y = delay)) +
  geom_point() +
  geom_line()

flights |>
  group_by(dest) |>
  summarise(
    distance = mean(distance),
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |>
  ggplot(aes(x = distance, y = speed)) +
  geom_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE,
    color = "white",
    linewidth = 4
  ) +
  geom_point()

# chapter5, data tidying--------------------------------------------------
table1
table2
table3

# compute rate per 10000
table1 |>
  mutate(rate = cases / population * 10000)
# compute cases per year
table1 |>
  group_by(year) |>
  summarise(total_cases = sum(cases))
# cases 随时间变化图
ggplot(table1, aes(x = year, y = cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country, shape = country)) +
  scale_x_continuous(breaks = c(1999, 2000))

## 长表数据，pivot_longer() function
billboard_longer <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE # drop NA values
  ) |>
  mutate(
    # 'wk1'->'1' # nolint
    week = parse_number(week)
  )
# 排名随时间变化
billboard_longer |>
  # group=track, 确保每个音乐有自己的一条线
  ggplot(aes(week, rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()

## pivoting的理解
# 手动创建一个tibble, how pivot_longer() works
df <- tribble(
  ~id, ~bp1, ~bp2,
  "a", 100, 120,
  "b", 140, 115,
  "c", 120, 125
)

# 转换为长表格式
df |> pivot_longer(
  cols = bp1:bp2, # 列名转换为新列的值
  names_to = "measurement", # 新列名
  values_to = "value" # 新列的值
)

# 结核病诊断数据集who2，sp/rel/ep, 诊断方法；
#  m/f，性别； 0-14/15-24/25-34/35-44/45-54/55-64/65, 年龄
who2 |>
  pivot_longer(
    cols = !(country:year),
    # 从'sp_rel_ep_m_f_15-24'中提取
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  )

# columns contain names of two variables
# 'household',dob:date of birth
household |>
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"), # '.valus'作用
    names_sep = "_",
    values_drop_na = TRUE
  )

## 宽表数据, pivot_wider() function
cms_patient_experience |>
  distinct(measure_cd, measure_title)

cms_patient_experience |>
  pivot_wider(
    id_cols = starts_with("org"), # nolint
    names_from = measure_cd,
    values_from = prf_rate
  )

# how pivot_wider() works
id <- c("a", "b", "b", "a", "a")
# 确定长表转宽表的列名来源
df1 |>
  distinct(measurement)
# 新的列名，填充新列的值
df1 |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )
df1 |>
  select(-measurement, -value) |>
  distinct() |>
  mutate(x = NA, y = NA, z = NA)

# 长表转宽表，对应值数量大于1的情况
df2 <- tribble(
  ~id, ~measurement, ~value,
  "a", "bp1", "100",
  "a", "bp1", "102",
  "a", "bp2", "120",
  "b", "bp1", "140",
  "b", "bp2", "115",
)
## ’a‘有两个bp1
df2 |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )
# 找到’df2‘有多个值的行
df2 |>
  group_by(id, measurement) |>
  summarise(n = n(), .groups = "drop_last") |>
  filter(n > 1)

# 'pivot'帮助文档
vignette("pivot", package = "tidyr")

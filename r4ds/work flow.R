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

# easier than conventional method
flights |>
  filter(dest == "IAH") |>
  mutate(speed = distance / air_time * 60) |>
  select(year:day, dep_time, carrier, flight, speed) |>
  arrange(desc(speed))
# groups, mean
flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = T),
    n = n()
  ) |>
  slice_head(n = 3) # take 3 rows
# groups based on more variable
daily <- flights |>
  group_by(year, month, day)
# grouped conflict with summarize
daily_flights <- daily |>
  summarize(n = n(), .groups = "drop_last")
# ungroup(), treat all rows as one group
daily |>
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = T),
    flights = n()
  )
# combination of plot and pipe
batters <- Lahman::Batting |>
  group_by(playerID) |>
  summarise(
    performance = sum(H, na.rm = T) / sum(AB, na.rm = T),
    n = sum(AB, na.rm = T)
  )
batters |>
  filter(n > 100) |>
  ggplot(aes(n, performance)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = F)


# dataset billboard -------------------------------------------------------
# pivot_longer() function
billboard_longer <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = T
  ) |>
  mutate(
    week = parse_number(week) # extract first number from a string
  )
billboard_longer |>
  ggplot(aes(week, rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()
# construct a small tibble, how pivot_longer() works
df <- tribble(
  ~id, ~bp1, ~bp2,
  "a", 100, 120,
  "b", 140, 115,
  "c", 120, 125
)
df |> pivot_longer(
  cols = bp1:bp2,
  names_to = "measurement",
  values_to = "value"
)
# coulumns contain "_"
who2 <- read.csv("who2.csv", sep = ",", row.names = 1)
who2 |>
  as_tibble(who2) |>
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  )

# columns contain names of two variables
family <- c("1", "2", "3", "4", "5")
dob_child1 <- c(
  "1998-11-26", "1996-06-22", "2002-07-11",
  "2004-10-10", "2000-12-05"
)
dob_child2 <- c(
  "2000-01-29", "na", "2004-04-05",
  "2009-08-27", "2005-02-28"
)
name_child1 <- c("Susan", "Mark", "Sam", "Craig", "Parker")
name_child2 <- c("Jose", "na", "Seth", "Khai", "Gracie")
household <- data.frame(
  family, dob_child1,
  dob_child2, name_child1, name_child2
) |>
  as_tibble(household)
household |>
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"),
    names_sep = "_",
    values_drop_na = T
  )
# pivot_wider()
cms_patient_experience <- read.csv("cms_patient_experience.csv",
  sep = ",", row.names = 1
)
cms_patient_experience <- as_tibble(cms_patient_experience)
cms_patient_experience |>
  distinct(measure_cd, measure_title)
cms_patient_experience |>
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )
# how pivot_wider() works
id <- c("a", "b", "b", "a", "a")
measurement <- c("bp1", "bp1", "bp2", "bp2", "bp3")
value <- c(100, 140, 115, 120, 105)
df1 <- data.frame(id, measurement, value)
df1 <- as_tibble(df1)
df1 |>
  distinct(measurement) |>
  pull()
#  [1] "bp1" "bp2" "bp3"
df1 |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )
# vignette("pivot", package = "tidyr")
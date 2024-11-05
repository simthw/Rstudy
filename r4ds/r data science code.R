
# load general packages ---------------------------------------------------
library(ggplot2)
library(tidyverse)
library(ggthemes)

# dataset penguins --------------------------------------------------------

# install.packages("palmerpenguins")
library(palmerpenguins)
glimpse(penguins)
# view(penguins)
# three lm lines based on local level,palmerpenguins::penguins
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
  ) + 
  geom_point() + 
  geom_smooth(method = "lm")
# one line based on global level
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point(mapping = aes(color = species, shape = species)) + 
  geom_smooth(method = "lm") + 
  labs(
    title = "body mass and flipper length",
    subtitle = "dimension for three type penguins",
    x = "flipper length (mm)", y = "body mass(g)",
    color = "Species", shape = "Species"  
    ) + 
      scale_color_colorblind()
# categorical data, no order, bar plot
ggplot(penguins, aes(x = species)) +
  geom_bar()
# categorical data, factors handle
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()
# quantitative data, histgram plot, determine data distribution
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
# binwidth 20 is too narrow
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)
# binwidth 2000 is too big
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)
# density plot
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()
# boxplot
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)
# two categorical variable, stacked bar plots, frequency
ggplot(penguins, aes(x = island, fill = species)) + 
  geom_bar()
# proportion
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
# >2 numerical variable
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species, shape =species)) + 
  facet_wrap(~island)
ggsave(filename = "penguin_plot.png")


# dataset flights ---------------------------------------------------------

# install.packages("nycflights13")
library(nycflights13)
flights  |>  
  filter(dep_delay > 120)
jan1 <- flights |> 
  filter(month == 1 & day == 1)
# combining | and ==
flights |> 
  filter(month %in% c(1, 2))
# character, must add quotation
flights |> 
  filter(dest %in% c("IAH", "HOU"))
# descending in big-to-small order
flights |> 
  arrange(desc(dep_delay))
# sory by year, month, day and dep_time
flights |> 
  arrange(year, month, day, dep_time)
# remove duplicate rows
flights |> 
  distinct()
# find all unique origin and des pairs
flights |> 
  distinct(origin, dest)
# keep other columns when filtering for unique rows
flights |> 
  distinct(origin, dest, .keep_all = T)
# find pairs occurrence numbers and sort them
flights |> 
  count(origin, dest, sort = T)
# mutate() for add new coulmn to dataset, .after change position of added
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )
# .keep only keep columns used in calculation
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )
# select columns
flights |> 
  select(year, month, day)
# move column variable position
flights |> 
  relocate(starts_with("arr"), .before = dep_time)
# any_of can specify the columns that have been assigned to a character vector
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |> 
  select(
    any_of(variables)
  )
# easier than conventional method
flights |> 
  filter(dest == "IAH") |>  
  mutate(speed = distance / air_time * 60) |>  
  select(year:day, dep_time, carrier, flight, speed) |>  
  arrange(desc(speed))
# groups, mean
flights |> 
  group_by(month) |> 
  summarize(avg_delay = mean(dep_delay, na.rm = T),
            n = n()) |> 
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
  summarize(avg_delay = mean(dep_delay, na.rm = T),
            flights = n())
# combination of plot and pipe
batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarise(
    performance = sum(H, na.rm = T)/sum(AB, na.rm = T),
    n = sum(AB, na.rm = T)
  )
batters |> 
  filter(n > 100) |> 
  ggplot(aes(n, performance)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = F)


# dataset billboard -------------------------------------------------------
# pivot_longer() function
billboard_longer <- billboard |> pivot_longer(
  cols = starts_with("wk"),
  names_to = "week",
  values_to = "rank",
  values_drop_na = T
) |> 
  mutate(
    week = parse_number(week) #extract first number from a string
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
dob_child1 <- c("1998-11-26", "1996-06-22", "2002-07-11",
                "2004-10-10", "2000-12-05")
dob_child2 <- c("2000-01-29", "na", "2004-04-05",
                "2009-08-27", "2005-02-28")
name_child1 <- c("Susan", "Mark", "Sam", "Craig", "Parker")
name_child2 <- c("Jose", "na", "Seth", "Khai", "Gracie")
household <- data.frame(family, dob_child1, 
                        dob_child2, name_child1, name_child2) |>
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
                                   sep = ",", row.names = 1)
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








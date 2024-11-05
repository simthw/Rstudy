# chapter1 ----------------------------------------------------------------
# run in vscode, set wd
setwd("D:/100study/130_data/132_jianguoyun/Tools notes/R data for science/r4ds")


# load library ------------------------------------------------------------
library(ggplot2)
library(dplyr)
# add data ‘penguins’
library(palmerpenguins)

# global level, 'color' can pass to each geom layer
p1 <- ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm") # three lines
ggsave("figure/plot1.png", p1, width = 8, height = 6)

# local level, 'color', points colored on species instead of lines
p2 <- ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species)) + # only points separated based on species
  geom_smooth(method = "lm")
ggsave("figure/plot2.png", p2, width = 8, height = 6)

# make plots beauty by adding labels
p3 <- ggplot(
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

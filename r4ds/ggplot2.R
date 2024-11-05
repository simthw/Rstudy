
# chapter9 ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, shape = class)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, size = class)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, alpha = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(color = "pink", shape = 17)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = "blue"))

# geom objects
# point
ggplot(mpg, aes(displ, hwy)) +
  geom_point()

# smooth line
ggplot(mpg, aes(displ, hwy, linetype = drv)) +
  geom_smooth()

# 3 colors based on drv and 3 linetypes
ggplot(mpg, aes(displ, hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(linetype = drv))

# only local layer ppoint 
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

# 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_point( data = mpg |> 
                filter(class == "2seater"), 
              color = "red" ) + 
  geom_point( data = mpg |> 
                filter(class == "2seater"), 
              shape = "circle open", size = 3, color = "red" )

ggplot(mpg, aes(x = hwy)) + 
  geom_histogram(binwidth = 2)

ggplot(mpg, aes(x = hwy)) + 
  geom_density()

ggplot(mpg, aes(x = hwy)) +
  geom_boxplot()

library(ggridges)
ggplot(mpg, aes(x = hwy, y = drv, fill = drv, color = drv)) + 
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv))

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~cyl)

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl)

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl, scales = "free_y")

ggplot(diamonds, aes(x = cut)) +
  geom_bar()

diamonds |> 
  count(cut) |> 
  ggplot(aes(x = cut, y = n)) + 
  geom_bar(stat = "identity")

ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()

ggplot(diamonds) + 
  stat_summary( aes(x = cut, y = depth), 
                fun.min = min, fun.max = max, fun = median )

ggplot(mpg, aes(x = drv, color = drv)) + 
  geom_bar()

ggplot(mpg, aes(x = drv, fill = drv)) + 
  geom_bar()

ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar()

ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(alpha = 1/5, position = "identity")

ggplot(mpg, aes(x = drv, color = class)) + 
  geom_bar(fill = NA, position = "identity")

ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "fill")

ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = "dodge")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "jitter")

bar <- ggplot(data = diamonds) + 
  geom_bar( mapping = aes(x = clarity, fill = clarity), 
            show.legend = FALSE, width = 1 ) + 
  theme(aspect.ratio = 1)
bar + coord_flip() 
bar + coord_polar()

# chapter10 ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(nycflights13)
# install.packages("pak")
# pak::pak("tidymodels") # 解决以前包版本低的作用 
library(tidymodels)

ggplot(diamonds, aes(carat)) +
  geom_histogram(binwidth = 0.5)

smaller <- diamonds |> 
  filter(carat < 3) 

ggplot(smaller, aes(carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
unusual <- diamonds |> 
  filter(y < 3 | y > 20) |> 
  select(price, x, y, z) |> 
  arrange(y)

carat_0.99 <- diamonds |> 
  filter(carat == 0.99)
carat_1 <- diamonds |> 
  filter(carat == 1)

diamonds2 <- diamonds |> 
  mutate(y = ifelse(y < 3 | y > 20, NA, y)) 

flights1 <- flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60),
  )
flights1 |> 
  ggplot(aes(sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), binwidth = 0.25) +
  facet_wrap(~cancelled, scales = "free_y")

ggplot(diamonds, aes(price)) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

ggplot(diamonds, aes(price, after_stat(density))) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

ggplot(diamonds, aes(cut, price)) +
  geom_boxplot()

ggplot(mpg, aes(class, hwy)) +
  geom_boxplot()

ggplot(mpg, aes(x = fct_reorder(class, hwy, median), hwy)) +
  geom_boxplot()

ggplot(mpg, aes(x = hwy, y = fct_reorder(class, hwy, median))) + 
  geom_boxplot()

diamonds |> 
  count(color, cut)

diamonds |> 
  count(color, cut) |> 
  ggplot(aes(color, cut)) +
  geom_tile(aes(fill = n))

diamonds <- diamonds |> 
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  )
diamonds_fit <- linear_reg() |> 
  fit(log_price ~ log_carat, data = diamonds)
diamonds_aug <- augment(diamonds_fit, new_data = diamonds) |> 
  mutate(.resid = exp(.resid))
ggplot(diamonds_aug, aes(carat, .resid)) +
  geom_point()
ggplot(diamonds_aug, aes(cut, .resid)) +
  geom_boxplot()


# chapter11 ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  labs(
    x = "engine displacement (L)",
    y = "highway fuel economy (mpg)",
    color = "car type",
    title = "fuel efficiency generally decreases with engine size",
    subtitle = "two seaters are an exception because of their light weight",
    caption = "data from fueleconomy.gov"
  )

df <- tibble(
  x = 1:10,
  y = cumsum(x^2)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(x[i]),
    y = quote(sum(x[i]^2, i == 1, n))
  )

label_info <- mpg |> 
  group_by(drv) |> 
  arrange(desc(displ)) |> 
  slice_head(n = 1) |> 
  mutate( drive_type = case_when( 
    drv == "f" ~ "front-wheel drive", 
    drv == "r" ~ "rear-wheel drive", 
    drv == "4" ~ "4-wheel drive" ) ) |> 
  select(displ, hwy, drv, drive_type)
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(se = FALSE) + 
  geom_text( data = label_info, 
             aes(x = displ, y = hwy, label = drive_type), 
             fontface = "bold", size = 5, hjust = "right", vjust = "bottom" ) +
  theme(legend.position = "none")

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(se = FALSE) + 
  geom_label_repel( data = label_info,
                    aes(x = displ, y = hwy, label = drive_type), 
                    fontface = "bold", size = 5, nudge_y = 2 ) + 
  theme(legend.position = "none")

potential_outliers <- mpg |> 
  filter(hwy > 40 | (hwy > 20 & displ > 5))
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_text_repel(data = potential_outliers, 
                  aes(label = model)) + 
  geom_point(data = potential_outliers, color = "red") + 
  geom_point(
    data = potential_outliers,
    color = "red", size = 3, shape = "circle open" 
    )

library(stringr)
trend_text <- "Larger engine sizes tend to\nhave lower fuel economy." |> 
  str_wrap(width = 30)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() + 
  annotate( geom = "label", x = 3.5, y = 38, 
            label = trend_text, hjust = "left", color = "red" ) + 
  annotate( geom = "segment", x = 3, y = 35, xend = 5, yend = 25,
            color = "red", arrow = arrow(type = "closed") )

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) + 
  scale_y_continuous(breaks = seq(15, 40, by = 5))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) + 
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  scale_color_discrete(labels = c("4" = "4-wheel", "f" = "front", "r" = "rear"))

ggplot(diamonds, aes(x = price, y = cut)) + 
  geom_boxplot(alpha = 0.05) + 
  scale_x_continuous(labels = label_dollar())

ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) + 
  scale_x_continuous( 
    labels = label_dollar(scale = 1/1000, suffix = "K"),
    breaks = seq(1000, 19000, by = 6000) )

ggplot(diamonds, aes(x = cut, fill = clarity)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Percentage", 
                     labels = label_percent())

presidential |> 
  mutate(id = 33 + row_number()) |> 
  ggplot(aes(x = start, y = id)) + 
  geom_point() + 
  geom_segment(aes(xend = end, yend = id)) + 
  scale_x_date(name = NULL, breaks = presidential$start, date_labels = "'%y")

base <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class))
base + 
  theme(legend.position = "right")
base + 
  theme(legend.position = "left")
base + 
  theme(legend.position = "top") + 
  guides(col = guide_legend(nrow = 3))
base + 
  theme(legend.position = "bottom") + 
  guides(col = guide_legend(nrow = 3))

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth(se = FALSE) + 
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 4)))

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d()

ggplot(diamonds, aes(x = log10(carat), y = log10(price))) + 
  geom_bin2d()
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d() +
  scale_x_log10() + 
  scale_y_log10()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv))
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) + 
  scale_color_brewer(palette = "Set1")
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = drv, shape = drv)) + 
  scale_color_brewer(palette = "Set1")

presidential |> 
  mutate(id = 33 + row_number()) |> 
  ggplot(aes(x = start, y = id, color = party)) + 
  geom_point() + 
  geom_segment(aes(xend = end, yend = id)) + 
  scale_color_manual(values = c(Republican = "#E81B23", Democratic = "#00AEF3"))

df <- tibble( 
  x = rnorm(10000), 
  y = rnorm(10000) 
)
ggplot(df, aes(x, y)) + 
  geom_hex() + 
  coord_fixed() + 
  labs(title = "Default, continuous", x = NULL, y = NULL)
ggplot(df, aes(x, y)) + 
  geom_hex() + 
  coord_fixed() + 
  scale_fill_viridis_c() +
  labs(title = "Viridis, continuous", x = NULL, y = NULL)
ggplot(df, aes(x, y)) + 
  geom_hex() + 
  coord_fixed() + 
  scale_fill_viridis_b() +
  labs(title = "Viridis, continuous", x = NULL, y = NULL)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) + 
  geom_smooth()
mpg |> 
  filter(displ >= 5 & displ <= 6 & hwy >= 10 & hwy <= 25) |> 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point(aes(color = drv)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = drv)) + 
  geom_smooth() + 
  scale_x_continuous(limits = c(5, 6)) + 
  scale_y_continuous(limits = c(10, 25))

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = drv)) + 
  geom_smooth() + 
  coord_cartesian(xlim = c(5, 6), ylim = c(10, 25))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) + 
  theme_bw()
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  labs( 
    title = "Larger engine sizes tend to have lower fuel economy", 
    caption = "Source: https://fueleconomy.gov." 
  ) + 
  theme( 
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal", 
    legend.box.background = element_rect(color = "black"), 
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.caption.position = "plot", 
    plot.caption = element_text(hjust = 0)
  )

p1 <- ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot() +
  labs(title = "Plot 2")
p3 <- ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 3")
p1 + p2
(p1 | p3) / p2

p1 <- ggplot(mpg, aes(x = drv, y = cty, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 1")  
p2 <- ggplot(mpg, aes(x = drv, y = hwy, color = drv)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Plot 2") 
p3 <- ggplot(mpg, aes(x = cty, color = drv, fill = drv)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 3") 
p4 <- ggplot(mpg, aes(x = hwy, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 4")  
p5 <- ggplot(mpg, aes(x = cty, y = hwy, color = drv)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~drv) + 
  labs(title = "Plot 5")
(guide_area() / (p1 + p2) / (p3 + p4) / p5) +
  plot_annotation( 
    title = "City and highway mileage for cars with different drivetrains",
    caption = "Source: https://fueleconomy.gov." 
  ) + 
  plot_layout( 
    guides = "collect",
    heights = c(1, 3, 2, 4) )& theme(legend.position = "top")

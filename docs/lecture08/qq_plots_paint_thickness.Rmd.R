library(tidyverse)
library(distributions3)

is <- sample(1:12)

paint_thickness <- data.frame(
  thickness = c(1.29, 1.12, 0.88, 1.65, 1.48, 1.59, 1.04, 0.83,
                1.76, 1.31, 0.88, 1.71, 1.83, 1.09, 1.62, 1.49),
  i = is[12],
  distribution = "paint"
)

X <- Normal(mu = mean(paint_thickness$thickness),
            sigma = mean(paint_thickness$thickness))

many_small_normal <- data.frame(i = is[1:11]) %>%
  mutate(distribution = "Normal",
         thickness = map(i, random, d = X, n = 16)) %>% ## for each i, use random with d = Normal(), n = 10
  unnest_longer(col = thickness)

all_12_samples <- bind_rows(
  many_small_normal,
  paint_thickness
) %>%
  group_by(i) %>%
  mutate(m = mean(thickness),
         s = sd(thickness))

ggplot(data = all_12_samples,
       aes(sample = thickness)) +
  geom_qq() +
  geom_abline(aes(intercept = m, slope = s)) +
  geom_qq_line(aes(color = "red")) +
  facet_wrap(~i, ncol = 4, scales = "free") +
  theme_bw() +
  theme(axis.text = element_blank())

all_12_samples %>% select(i, distribution) %>% unique()








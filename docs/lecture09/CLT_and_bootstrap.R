library(tidyverse)
library(distributions3)

theme_set(theme_bw())

#### Beta distribution
## Create population to sample from
X <- Beta(0.5, 0.5)
population <- tibble(x = random(X, 10000))

## Specify sample size
sample_size <- 35

## Create original sample
original_sample <- sample_n(population, size = sample_size)
original_xbar <- mean(original_sample$x)

## 5000 new samples from population
resamples <- tibble(i = 1:5000) %>%
  mutate(type = "new_sample",
         sample = map(i, ~sample_n(population, size = sample_size)$x),
         mean = map_dbl(sample, mean),
         sd = map_dbl(sample, sd),
         t_stat = (mean-0.5)/(sd/sqrt(sample_size)))

## Theoretically, t_stat should follow this distribution
T_14 <- StudentsT(df = sample_size - 1)

plot_pdf(T_14)

## Look at histogram for Xbar
ggplot(data = resamples,
       aes(x = t_stat)) +
  geom_histogram(aes(y = ..density..),
                 bins = 50) +
  stat_function(fun = pdf, args = list(d = T_14))

## Create 5000 bootstrap samples
bootstrap_samples <- tibble(i = 1:5000) %>%
  mutate(type = "bootstrap",
         sample = map(i, ~sample_n(original_sample, size = sample_size, replace = TRUE)$x),
         mean = map_dbl(sample, mean),
         sd = map_dbl(sample, sd),
         t_stat = (mean - original_xbar)/(sd/sqrt(sample_size)))

## Stack on top of each other
all_samples <- bind_rows(
  resamples,
  bootstrap_samples
)

ggplot(all_samples,
       aes(x = t_stat)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) +
  facet_grid(type ~ .) +
  stat_function(fun = pdf, args = list(d = T_14))

#### Exponential
X <- Exponential(0.2)
population <- random(X, 10000)

## Specify sample size
sample_size <- 20

##
resamples <- tibble(n_resample = 1:5000) %>%
  mutate(type = "resample",
         sample = map(n_resample, ~sample(population, size = sample_size)),
         mean = map_dbl(sample, mean),
         sd = map_dbl(sample, sd),
         t_stat = (mean - 1/0.2)/(sd/sqrt(sample_size)))

Xbar <- Normal(mu = 0.2, sigma = sqrt(0.2/sample_size))#sqrt(15*0.3*0.7/sample_size))

bootstrap_samples <- tibble(n_resample = 1:5000) %>%
  mutate(type = "bootstrap",
         sample = map(n_resample, ~sample(orig_sample, replace = TRUE)),
         mean = map_dbl(sample, mean),
         sd = map_dbl(sample, sd),
         t_stat = (mean - mean(orig_sample))/(sd/sqrt(sample_size)))

all_samples <- bind_rows(
  resamples,
  bootstrap_samples
)

ggplot(all_samples,
       aes(x = t_stat)) +
  geom_histogram(aes(y = ..density.., fill = type),
                 position = "identity",
                 alpha = 0.5,
                 binwidth = 0.5) +
  scale_fill_viridis_d()

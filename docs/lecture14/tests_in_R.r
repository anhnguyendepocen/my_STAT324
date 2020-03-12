library(tidyverse)
library(distributions3)

## Paint thickness data
paint_thickness <- tibble(thickness = c(1.29, 1.12, 0.88, 1.65, 1.48, 1.59, 1.04, 0.83,
                                        1.76, 1.31, 0.88, 1.71, 1.83, 1.09, 1.62, 1.49))

## One sample T-test "by hand" of H0: mu = 1.5 vs HA: mu != 1.5
paint_thickness %>%
 summarize(xbar = mean(thickness),
           SD = sd(thickness),
           n = n(),
           T_obs = (xbar - 1.5)/(SD/sqrt(n)),
           p_value = 2*cdf(StudentsT(df = n - 1), -abs(T_obs)))

## Build-in t.test function:
t.test(paint_thickness$thickness, mu = 1.5)

## One sample T-test "by hand" of H0: mu = 1.5 vs HA: mu > 1.5
paint_thickness %>%
  summarize(xbar = mean(thickness),
            SD = sd(thickness),
            n = n(),
            T_obs = (xbar - 1.5)/(SD/sqrt(n)),
            p_value = 1 - cdf(StudentsT(df = n - 1), T_obs))

## Build-in t.test function:
t.test(paint_thickness$thickness, mu = 1.5, alternative = "greater")

## 90% CI "by hand"
paint_thickness %>%
  summarize(xbar = mean(thickness),
            SD = sd(thickness),
            n = n(),
            LL = xbar - quantile(StudentsT(df = n - 1), 0.95)*SD/sqrt(n),
            UL = xbar + quantile(StudentsT(df = n - 1), 0.95)*SD/sqrt(n))

## 90% CI using build in t.test function:
t.test(paint_thickness$thickness, conf.level = 0.9)


## Corona virus data
library(lubridate) # extra package to work with dates

corona_virus <- read_csv(here::here("csv_data/corona_virus.csv")) %>%
  mutate(date = mdy(date)) %>%
  filter(date == max(date))

corona_virus %>%
  filter(`Country/Region` %in% c("Mainland China", "Italy")) %>%
  group_by(`Country/Region`) %>%
  summarize_at(vars(c(Confirmed, Deaths)), sum) %>%
  mutate(p = Deaths/Confirmed)

## Test H0: pi_italy = 0.0388 vs HA: pi_italy != 0.0388
## By hand
corona_virus %>%
  filter(`Country/Region` == "Italy") %>%
  summarize_at(vars(c(Confirmed, Deaths)), sum) %>%
  mutate(p = Deaths/Confirmed,
         SD_0 = sqrt(0.0388*(1-0.0388)/Confirmed),
         z_obs = (p - 0.0388)/SD_0,
         p_value = 2*cdf(Normal(), -abs(z_obs)),
         LL = p - quantile(Normal(), 0.975)*sqrt(p*(1-p)/Confirmed),
         UL = p + quantile(Normal(), 0.975)*sqrt(p*(1-p)/Confirmed))

## Build-in prop.test
prop.test(x = 631, n = 10149, p = 0.0388,
          correct = FALSE)
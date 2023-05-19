library(data.table)
library(tidyverse)
diet <- readr::read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/master/data/stcp-Rdataset-Diet.csv")
head(diet)
diet <- diet %>%
  mutate(weight_loss = weight6weeks - pre.weight,
         Dietf = factor(Diet, labels = LETTERS[1:3]),
         Person = factor(Person)) %>%
  drop_na()
setDT(diet)

df_b <- 3 - 1
df_w <- nrow(diet) - 3

# below is data.table syntax

mean_A <- mean(diet[Dietf == 'A', weight_loss])
mean_B <- mean(diet[Dietf == 'B', weight_loss])
mean_C <- mean(diet[Dietf == 'C', weight_loss])

# it is possible to do the same thing using dplyr:
diet %>% 
  group_by(Dietf) %>% 
  summarise(mean = mean(weight_loss))
# OR
diet %>% 
  filter(Dietf == 'A') %>% 
  pull(weight_loss) %>% 
  mean()
# but dplyr is a bit wordy in this case

mean_common <- mean(diet$weight_loss)

# calculate SSW
SSW <- sum(sum((diet[Dietf == 'A', weight_loss] - mean_A)^2), 
           sum((diet[Dietf == 'B', weight_loss] - mean_B)^2),
           sum((diet[Dietf == 'C', weight_loss] - mean_C)^2))


sample_size_A <- sum(diet$Dietf == 'A')
sample_size_B <- sum(diet$Dietf == 'B')
sample_size_C <- sum(diet$Dietf == 'C')

SSB <- sum(sample_size_A * (mean_A - mean_common)^2, 
    sample_size_B * (mean_B - mean_common)^2, 
    sample_size_C * (mean_C - mean_common)^2)

# to check if SSB and SSW calculated correctly
SST <- sum((diet$weight_loss - mean_common)^2)

SSB + SSW == SST # TRUE

# normalize
M_SSB <- SSB / df_b
M_SSW <- SSW / df_w

F_value <- M_SSB / M_SSW
F_value

p_value <- 1 - pf(F_value, df_b, df_w)
# plot(df(0.01:100, 2, 73))
p_value


fit_diet <- aov(weight_loss ~ Dietf, data = diet)
summary(fit_diet)

# so the F-value and p-value are the same

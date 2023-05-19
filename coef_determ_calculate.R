library(tidyverse)
library(patchwork)
set.seed(42)
df <- data.frame(
  x=c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
  y=c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3))

model <- lm(y ~ x, data = df)
df_mean <- df %>% 
  mutate(y1 = mean(y), x1 = x)
  # rename(x = x2)
df_regr <- df %>% 
  mutate(y1 = predict(model, data.frame(x = df$x)), x1 = x)
# predict(model, data.frame(x = df$x))
regr_plot <- df %>% 
  ggplot(aes(x, y))+
  geom_point()+
  geom_smooth(formula = 'y ~ x', method = 'lm', se = FALSE, linewidth = 0.7)+
  geom_segment(aes(x = x1, y = y1, xend = x, yend = y), 
               data = df_regr, linewidth = 0.1, linetype = 2)+
  ggtitle('Residual variation')+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

regr_plot
mean_plot <- df %>% 
  ggplot(aes(x, y))+
  geom_point()+
  geom_hline(yintercept = mean(df$y), color = 'blue', linewidth = 0.7)+
  geom_segment(aes(x = x1, y = y1, xend = x, yend = y), 
               data = df_mean, linewidth = 0.1, linetype = 2)+
  ggtitle('Total variation')+
  # geom_smooth(formula = 'y ~ x', method = 'lm', se = FALSE)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
mean_plot + regr_plot

summary(model)
sst <- sum((df$y - mean(df$y))^2)
ssr <- sst  - sum((df$y - predict(model, data.frame(x = df$x)))^2)
r2 <- ssr / sst
r2

sse <-  sum((predict(model, data.frame(x = df$x)) - mean(df$y))^2)
sse

df
df %>% 
  ggplot(aes(x, y))+
  geom_point()+
  geom_smooth(formula = 'y ~ x', method = 'lm', se = FALSE, linewidth = 0.7)+
  geom_segment(aes(x = x1, y = y1, xend = x, yend = y), 
               data = df_regr, linewidth = 0.1, linetype = 1, color = 'violet')+
  geom_hline(yintercept = mean(df$y), color = 'red', linewidth = 0.5)+
  geom_segment(aes(x = x1, y = y1, xend = x, yend = y), 
               data = df_mean, linewidth = 0.1, linetype = 2, color= 'green3')+
  ggtitle('Total variation')+
  # geom_smooth(formula = 'y ~ x', method = 'lm', se = FALSE)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

df_regr <- df_regr %>% 
  mutate(yupd = mean(df_regr$y))

df %>% 
  ggplot(aes(x, y))+
  geom_point()+
  geom_smooth(formula = 'y ~ x', method = 'lm', 
              se = FALSE, linewidth = 0.7, color = 'gray50')+
  geom_hline(yintercept = mean(df$y), color = 'red', linewidth = 0.5)+
  geom_segment(aes(x = x1, y = y, xend = x, yend = yupd), 
               data = df_regr, linewidth = 0.4, 
               linetype = 1, color = '#253494', alpha = 0.9)+
  geom_segment(aes(x = x1, y = y1, xend = x, yend = y), 
               data = df_regr, linewidth = 0.4, 
               linetype = 2, color= '#41b6c4')+
  # ggtitle('Total variation')+
  annotate('text', x = 6.1, y = 4.5, label = 'SSR', color= '#41b6c4')+
  annotate('text', x = 6.1, y = 5.8, label = 'SSE', color= '#253494')+
  geom_segment(aes(x = max(x), y = mean(y),
                   xend = max(x)+0.1, yend = mean(y)+0.1))+
  geom_segment(aes(x = max(x), y = max(y),
                   xend = max(x)+0.1, yend = max(y)-0.1))+
  geom_segment(aes(x = max(x)+0.1, y = mean(y)+0.1,
                   xend = max(x)+0.1, yend = max(y)-0.1))+
  annotate('text', x = 6.6, y = 5, label = 'SST')+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

mouse.data <- data.frame(
  weight=c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
  size=c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3))

mouse.data # print the data to the screen in a nice format

## plot a x/y scatter plot with the data
plot(mouse.data$weight, mouse.data$size)

## create a "linear model" - that is, do the regression
mouse.regression <- lm(size ~ weight, data=mouse.data)
## generate a summary of the regression
summary(mouse.regression)

## add the regression line to our x/y scatter plot
abline(mouse.regression, col="blue")

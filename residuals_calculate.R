library(patchwork)
library(tidyverse)
# model1 <- lm(happyScore ~ GDP, happy_score_data)
# pl_1res <- ggplot(data.frame(fit = fitted(model1), 
#                              res = residuals(model1)), 
#                   aes(x = fit, y = res)) + 
#   geom_point() + 
#   geom_hline(yintercept = 0) + 
#   xlab("Fitted") + 
#   ylab("Residuals")+theme_bw()
# pl_1res
# 
# pl_2res <- ggplot(happy_score_data, aes(GDP, happyScore))+
#   geom_point()+
#   geom_smooth(formula = 'y ~ x', method = 'lm', se=FALSE)+
#   theme_bw()
# pl_2res + pl_1res
# 

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
               data = df_regr, linewidth = 0.2, alpha = 0.7, linetype = 2,
               color = 'gray')+
  ggtitle('Residual variation')+
  geom_segment(aes(x = 0, y = y1, xend = x, yend = y1), 
               df_regr, linewidth = 0.5, 
               linetype = 2, color = 'red')+
  theme_bw()+
  scale_x_continuous(limits = c(0, 6.5), expand=c(0,0))+
  
  theme(plot.title = element_text(hjust = 0.5))

# regr_plot
mean_plot <- df %>% 
  ggplot(aes(x, y))+
  geom_point()+
  geom_hline(yintercept = mean(df$y), color = 'blue', linewidth = 0.7)+
  geom_segment(aes(x = x1, y = y1, xend = x, yend = y), 
               data = df_mean, linewidth = 0.4, linetype = 2, color = 'gray')+
  annotate('text', x = 1.2, y = mean(df$y)+0.2, label = 'y_mean')+
  ggtitle('Total variation (SST)')+
  # geom_smooth(formula = 'y ~ x', method = 'lm', se = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
# mean_plot + regr_plot

pl_1res <- ggplot(data.frame(fit = fitted(model), 
                             res = residuals(model)), 
                  aes(x = fit, y = res)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  xlab("Fitted") + 
  ylab("Residuals")+theme_bw()
regr_plot + pl_1res
  

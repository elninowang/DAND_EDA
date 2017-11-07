###
library(ggplot2)
library(dplyr)
data(diamonds)
summary(diamonds)

# 价格与x
ggplot(aes(x = price, y = x), data=diamonds) +
  geom_point(alpha = 1./10)

# 发现：价格与x


# 相关性
with(diamonds, cor.test(diamonds$price, diamonds$x))
with(diamonds, cor.test(diamonds$price, diamonds$y))
with(diamonds, cor.test(diamonds$price, diamonds$z))

# 价格与深度
ggplot(aes(x = depth, y = price), data=diamonds) +
  geom_point()

# 调整 - 价格与深度
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1./100) +
  xlim(55, 70) +
  scale_x_continuous(limits = c(57,67), breaks = seq(0,100,2))

# 典型深度范围
summary(diamonds$depth)

# 相关性 - 价格与深度
with(diamonds, cor.test(diamonds$depth, diamonds$price))

# 价格与克拉
ggplot(aes(x = price, y = carat), data=diamonds) +
  geom_point(alpha = 1./100) +
  xlim(0, quantile(diamonds$price, 0.99)) +
  ylim(0, quantile(diamonds$carat, 0.99))

# 价格与体积
diamonds$volume = diamonds$x * diamonds$y * diamonds$z
summary(diamonds$volume)
ggplot(aes(x = price, y = volume), data=diamonds) +
  geom_point(alpha = 1./100)

#发现 - 价格与体积
ggplot(aes(x = price, y = volume), data=diamonds) +
  geom_point(alpha = 1./100) +
  xlim(0, quantile(diamonds$price, 0.99)) +
  ylim(0, quantile(diamonds$volume, 0.99))



# 子集相关性
diamonds_subset <- subset(diamonds, diamonds$volume > 0. & diamonds$volume<800.)
with(diamonds_subset, cor.test(diamonds$price, diamonds$volume))

# 调整 - 价格与体积
ggplot(aes(x = price, y = volume), data=diamonds_subset) +
  geom_point(alpha = 1./100) +
  geom_smooth()

# 平均价格 - 净度
diamondsByClarity <- diamonds %>%
group_by(clarity) %>%
  summarise(mean_price = mean(price), 
            median_price = median(price), 
            min_price = min(price),
            max_price = max(price),
            n=n()) %>%
  arrange(clarity)
head(diamondsByClarity)

# 平均价格柱状图
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)
p1 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +
  geom_bar(stat = 'identity')  
p2 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
  geom_bar(stat = 'identity')
grid.arrange(p1, p2, ncol = 1)

# 平均价格的趋势




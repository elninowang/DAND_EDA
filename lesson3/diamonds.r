# library(ggplot2) #必须先加载 ggplot 包 
data(diamonds) #加载钻石数据集，ggplot 包自带该数据集 

# 练习
summary(diamonds)

# 价格直方图
qplot(x = price, data = diamonds, binwidth=400)

# 砖石数量
d1 <- diamonds[diamonds$price<500, ]
d2 <- diamonds[diamonds$price<250, ]
d3 <- diamonds[diamonds$price>=15000, ]

# 廉价钻石
library(gridExtra) 
g1 <- qplot(x = price, data = diamonds, binwidth=20) + 
  scale_x_continuous(limits = c(300, 2000), breaks = seq(0, 2000, 100))
g2 <- qplot(x = price, data = diamonds, binwidth=1) + 
  scale_x_continuous(limits = c(650, 750))
grid.arrange(g1, g2, ncol=1)

# 切工-价格直方图
qplot(x = price, data = diamonds) +
  facet_wrap(~diamonds$cut)
qplot(x = cut, y = price, data = diamonds, geom='boxplot')
by(diamonds$pric, diamonds$cut, summary)

# 标尺和多直方图
qplot(x = price, data = diamonds, binwidth=50) + 
  facet_wrap(~cut, scales="free_y")

# 由切工决定的每克拉价格
qplot(x = price/carat, data = diamonds) + 
  facet_wrap(~cut, scales="free_y") +
  scale_x_log10()

# 价格箱线图
library(gridExtra) 
g1 = qplot(x = cut, y = price, data = diamonds,
           geom = 'boxplot')
g2 = qplot(x = clarity, y = price, data = diamonds,
           geom = 'boxplot')
g3 = qplot(x = color, y = price, data = diamonds,
           geom = 'boxplot')
grid.arrange(g1, g2, g3,  ncol=2)
ggsave('priceHistogram.png')

# 四分位数间距 — IQR
by(diamonds$price, diamonds$color, summary)
IQR(subset(diamonds, color=='D')$price) 
IQR(subset(diamonds, color=='J')$price) 

# 由颜色表示的每克拉价格箱线图
qplot(x = color, y = price/carat, data = diamonds,
           geom = 'boxplot')

# 克拉频率多边形
qplot(x = carat, data = diamonds, binwidth=0.01,
      geom='freqpoly')


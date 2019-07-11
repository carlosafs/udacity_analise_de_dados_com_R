
getwd()

library(ggplot2)
data(diamonds)


names(diamonds)
View(diamonds)

#1 
#preço versus x
ggplot(aes(x = price, y = x), data = diamonds) +
  geom_point(col = "orange")

#3  
#correlação
cor.test(diamonds$price,  diamonds$x, method = "pearson")
cor.test(diamonds$price,  diamonds$y, method = "pearson")
cor.test(diamonds$price,  diamonds$z, method = "pearson")

#4
#preço versus profundidade
ggplot(aes(x = price, y = depth), data = diamonds) +
  geom_point(alpha = 0.01, col = "orange") + 
  scale_x_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 100))

#5
ggplot(aes(x = price, y = depth), data = diamonds) +
  geom_point(alpha = 0.01, col = "orange") + 
  scale_y_continuous(limits = c(50, 80), breaks = seq(50, 80, 2))

#7
cor.test(diamonds$depth,  diamonds$price, method = "pearson")

#8
ggplot(aes(x = price, y = carat), data = diamonds) +
  geom_point(col = "orange") +
  scale_x_continuous(limits = c(190, 19000), breaks = seq(190, 19000, 1000))

#9
dm <- diamonds
dm$volume <- (dm$x * dm$y * dm$z)

ggplot(aes(x = price, y = volume), data = dm) +
  geom_point(col = "orange")

#10
with(subset(dm, volume > 0 & volume <= 800), 
     cor.test(dm$volume, dm$price, method = "pearson"))

#12
#apagando registro com problemas

dm[dm$volume == 0 & dm$volume >= 800, ]

dma <- dm
dma = dma[dma$volume != 0, ]
dma = dma[dma$volume < 800, ]

View(dma)

cor.test(dma$volume, dma$price, method = "pearson")


#13

## biblioteca dplyr
install.packages("dplyr")
library(dplyr)

diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n()) %>%
  arrange(clarity)
head(diamondsByClarity)

#14
data(diamonds)
library(dplyr)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))


a1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
  geom_col(fill = "orange")

a2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) + 
  geom_col(fill = "orange")

library(gridExtra)

grid.arrange(a1, a2,ncol = 1)



getwd()

library(ggplot2)
data(diamonds)
names(diamonds)

ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point(color = "orange", alpha = 1/4) +
  stat_smooth(method = "lm") +
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)))

#carregando algumas bibliotecas
install.packages("memisc")

library(GGally)
library(scales)
library(memisc)
library(lattice)
library(mass)
library(car)
library(reshape2)
library(dplyr)


set.seed(20022012)

diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000), ]

ggpairs(diamond_samp, 
        lower = list(continuous = wrap("points", shape = I('.'))), 
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

## analisando claridade, corte e cor se tem relação com o quilate vs preco

library(RColorBrewer)

#transformação em raíz cubica x^(1/3)

cuberoot_trans = function() trans_new('cuberoot', 
                                      transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)
# grafico geral
ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point(alpha = 0.5, size = 1, position = "jitter") +
  scale_color_brewer(type = "div",
                     guide = guide_legend(title = "clarity", reverse = TRUE,
                                          override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle("Price (log10) vs\nCube Root of Carat and Clarity")

#grafico com coloracao na claridade
ggplot(aes(x = carat, y = price, colour = clarity), data = diamonds) +
  geom_point(alpha = 0.5, size = 1, position = "jitter") +
  scale_color_brewer(type = "div",
                     guide = guide_legend(title = "clarity", reverse = TRUE,
                                          override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle("Price (log10) vs\nCube Root of Carat and Clarity")

#grafico com coloracao no corte
ggplot(aes(x = carat, y = price, colour = cut), data = diamonds) +
  geom_point(alpha = 0.5, size = 1, position = "jitter") +
  scale_color_brewer(type = "div",
                     guide = guide_legend(title = "cut", reverse = TRUE,
                                          override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle("Price (log10) vs\nCube Root of Carat and Clarity")


#grafico com coloracao na cor
ggplot(aes(x = carat, y = price, colour = color), data = diamonds) +
  geom_point(alpha = 0.5, size = 1, position = "jitter") +
  scale_color_brewer(type = "div",
                     guide = guide_legend(title = "color", reverse = TRUE,
                                          override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle("Price (log10) vs\nCube Root of Carat and Clarity")


## construindo um modelo linear para o preco

m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)

mtable(m1, m2, m3, m4, m5)

#############################
# testando o modelo / algoritmo

thisDiamond = data.frame(carat = 1.00, cut = "Good",
                         color = "I", clarity = "VS1")

MeuDiamante = data.frame(carat = 0.23, cut = "Very Good",
                         color = "H", clarity = "VS1")


modelEstimate2 = predict (m5, newdata = MeuDiamante,
                         interval = "prediction", level = .95)

exp(modelEstimate)
exp(modelEstimate2)

#novo codigo

dat = data.frame(m4$model, m4$residuals) 

with(dat, sd(m4.residuals)) 

with(subset(dat, carat > .9 & carat < 1.1), sd(m4.residuals)) 

dat$resid <- as.numeric(dat$m4.residuals)
ggplot(aes(y = resid, x = round(carat, 2)), data = dat) + 
  geom_line(stat = "summary", fun.y = sd)






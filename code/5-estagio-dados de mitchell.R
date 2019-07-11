
## trabalhando com os dados de Mitchell

getwd()
```{r}
install.packages("alr3")
library(alr3)
data(Mitchell)
?Mitchell
```
View(Mitchell)
Mit <- data(Mitchell)

library(ggplot2)

ggplot(aes(x = Month, y = Temp), data = Mitchell) +
  geom_point() +
  scale_x_discrete(breaks = seq(0, 203, 12))

#correlação com os dados brutos
cor.test(Mitchell$Month, Mitchell$Temp , method = "pearson")


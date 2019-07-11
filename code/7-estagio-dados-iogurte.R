

getwd()

yo <- read.csv('yogurt.csv')

View(yo)

str(yo)

# transfomando uma variavel int em factor

yo$id <- factor(yo$id)

str(yo)

ggplot(aes(x = price), data = yo) +
  geom_histogram(fill = "orange")

summary(yo)
length(unique(yo$price))
table(yo$price)

## criando variável compra total

yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + 
                  plain + mixed.berry)

summary(yo$all.purchases)

ggplot(aes(x = all.purchases), data = yo, binwidth = 1) +
  geom_histogram(fill = "blue")
 
## grafico de dispesao

ggplot(aes(x = time, y = price), data = yo) +
  geom_jitter(alpha = 1/4, shape = 21, fill = "orange")


## vamos olhar mais detalhadamente, para isso, vamos pegar 16 compras aleatórias.

set.seed(4230)
sample.ids <- sample(levels(yo$id), 16)

ggplot(aes(x = time, y = price),
       data = subset(yo, id%in% sample.ids)) +
  facet_wrap( ~id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)









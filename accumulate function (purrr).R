library(tidyverse)


1:5 %>% 
  accumulate(function(x, y) x + y)



(iris2 <- iris %>% 
  mutate(clase = if_else(Species =="setosa", 0, 1))
)

summary(glm(clase ~ Sepal.Length, data=iris2, family = binomial))



models <- c("Sepal.Width","Petal.Length","Petal.Width") %>% 
  accumulate(function(x, y) paste(x, y, sep = ' + '),
             .init = "clase ~ Sepal.Length") %>% 
  set_names(1:length(.))

enframe(models, name = "model", value = "spec")


models %>% 
  map(glm, data = iris2) %>% 
  map(summary) %>% 
  map_dbl("aic") %>% 
  enframe(name = "modelo", value = "AIC coef")

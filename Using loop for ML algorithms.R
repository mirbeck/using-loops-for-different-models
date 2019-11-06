# Loop 1 - train a model with several methods

pacman :: p_load(caret,reshape, ggplot2, dplyr)

x <- runif(50, min = 0, max = 100) # generate 50 random number from 0 to 100
z <- runif(50, min = 0, max = 100)
a <- runif(50, min = 0, max = 100)
b <- runif(50, min = 0, max = 100)
y <- runif(50, min = 0, max = 100)

df <- as.data.frame(cbind(x,z,a,b,y))
df
set.seed(100)
in_training <- createDataPartition(df$y, p = 0.7, list = F)

train <- df[in_training,]
test <- df[-in_training,]

a <- c("lm", "rf","knn", "svmLinear", "svmRadial")

compare_model <- c()

for(i in a) {
  
  model <- train(y ~., data = train, method = i)
  
  pred <- predict(model, newdata = test)
  
  pred_metric <- postResample(test$y, pred)
  
  compare_model <- cbind(compare_model , pred_metric)
  
}

colnames(compare_model) <- a

compare_model

#Loop 2 - train a model with several combinations of variables

a <- c("y ~ x + a", "y ~ z + a", "y ~ a")

compare_var <- c()

for ( i in a) {
  
  model <- train(formula(i), data = train, method = "lm")
  
  pred <- predict(model, newdata = test)
  
  pred_metric <- postResample(test$y, pred)
  
  compare_var <- cbind(compare_var , pred_metric)
  
}

colnames(compare_var) <- a

compare_var

compare_var_melt <- melt(compare_var, varnames = c("metric", "model"))
compare_var_melt <- as.data.frame(compare_var_melt)
compare_var_melt

ggplot(compare_var_melt, aes(x=model, y=value))+
  geom_col()+
  facet_grid(metric~., scales="free")

#Loop 3 - train a model with several combinations of variables and methods

a <- c("y ~ x + a", "y ~ z + a", "y ~ a")
b <- c("lm", "knn")
compare_var_mod <- c()

for ( i in a) {
  for (j in b) {
    
    model <- train(formula(i), data = train, method = j)
    
    pred <- predict(model, newdata = test)
    
    pred_metric <- postResample(test$y, pred)
    
    compare_var_mod <- cbind(compare_var_mod , pred_metric)
    
  }
  
}

names_var <- c()
for (i in a) {
  for(j in b) {
    names_var <- append(names_var,paste(i,j))
  }
}


colnames(compare_var_mod) <- names_var

compare_var_mod

compare_var_mod_melt <- melt(compare_var_mod, varnames = c("metric", "model"))
compare_var_mod_melt <- as.data.frame(compare_var_mod_melt)
compare_var_mod_melt

ggplot(compare_var_mod_melt, aes(x=model, y=value))+
  geom_col()+
  facet_grid(metric~., scales="free")

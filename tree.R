---
title: "R Notebook"
output: html_notebook
---
```{r}
library(readxl)
MRA.df <- read_excel("C:/Users/Aysha Emelda/Downloads/MRA Analytics.xlsx")
set.seed(22)  
train.index <- sample(c(1:dim(MRA.df)[1]), dim(MRA.df)[1]*0.6)  
train.df <- MRA.df[train.index, ]
valid.df <- MRA.df[-train.index, ]

```
```{r}
library(rpart)
tr <- rpart(Price ~  Age+ KM + HP + Doors + cc +Quarterly_Tax+Weight , 
           data = train.df,
           method = "anova", minbucket = 1, maxdepth = 30, cp = 0.001)
install.packages("rpart.plot")
library(rpart.plot)
prp(tr)

```

```{r}
str(tr)

```
```{r}
t(t(tr$variable.importance))
```
```{r}
 Class<- seq(min(MRA.df$Price), 
            max(MRA.df$Price),
            (max(MRA.df$Price) - min(MRA.df$Price))/20)
Class
```
```{r}
Classed_Price <- .bincode(MRA.df$Price, 
                         Class, 
                         include.lowest = TRUE)

Classed_Price <- as.factor(Classed_Price)
Classed_Price

```
```{r}
train.df$Classed_Price <- Classed_Price[train.index]
valid.df$Classed_Price <- Classed_Price[-train.index]
```
```{r}
tr.classed <- rpart(Classed_Price ~  Age+ KM + HP + Doors + cc +Quarterly_Tax+Weight, data = train.df)
prp(tr.classed)
```
```{r}
t(t(tr.classed$variable.importance))
```
```{r}
accuracy(predict(tr, train.df), train.df$Price)
```
```{r}
installed.packages()
```
```{r}
install.packages("forecast")
```
```{r}
library(forecast)
```

```{r}
accuracy(predict(tr, train.df), train.df$Price)
```

```{r}
accuracy(predict(tr, valid.df), valid.df$Price)
```

```{r}
printcp(tr)
plotcp(tr)
```
```{r}
install.packages("Rtools")
install.packages("RevoScaleR")
```




```{r}

ptree<- prune(tr,
  cp= tr$cptable[which.min(tr$cptable[,"xerror"]),"CP"])
prp(ptree)

```
```{r}
new.ob <- data.frame(Age = 80, 
                         KM = 130000, 
                         HP = 110, 
                         Doors = 3, 
                         Quarterly_Tax = 100,
                     Weight=1165,
                     cc=2000)
```

```{r}
price.tr <- predict(tr, newdata = new.ob)

```
```{r}
accuracy(predict(ptree, train.df), train.df$Price)
```

```{r}
accuracy(predict(ptree, valid.df), valid.df$Price)
```
```{r}
price.tr <- predict(tr, newdata = new.ob)
price.tr.bin <- Class[predict(tr.classed, newdata = new.ob, type = "class")]
cat(paste("Regression Price Estimate: ",scales::dollar(price.tr,0.01)), 
      paste("Classification Price Estimate: ",scales::dollar(price.tr.bin,0.01)),
      sep='\n')
```


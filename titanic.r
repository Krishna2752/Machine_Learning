#importing the datasets
train=read.csv("train.csv")
test=read.csv("test.csv")
#removing unnecessary columns
train1=train[c(1,2,3,5,6)]
test1=test[c(1,2,4,5)]

#Missing Data
train1$Age = ifelse(is.na(train1$Age),
                    ave(train1$Age,FUN = function(x) mean(x, na.rm = T)),
                    train1$Age)
test1$Age = ifelse(is.na(test1$Age),
                    ave(test1$Age,FUN = function(x) mean(x, na.rm = T)),
                    test1$Age)
#fitting the model
classifier = glm(formula = Survived~.,
                 family = binomial,
                 data = train1)
#predicting test set
pred<-predict(classifier,type = 'response', newdata=test1)
y_pred = ifelse(pred>0.5,1,0)
#tabulating result
tabulation = table(y_pred)
tabulation
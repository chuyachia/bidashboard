# Month as numeric or character, numeric:reduce degree of freedom
# Data on promotion (markdown)
# anonymized data related to promotional markdowns that Walmart is running. MarkDown data is only available after Nov 2011, and is not available for all stores all the time. Any missing value is marked with an NA.
#library(caret)
library(dplyr)
library(tidyr)
library(Matrix)
library(xgboost)
library(caret)
df <- dbGetQuery(mydb,
                 'SELECT *
                 FROM features JOIN (SELECT * FROM train JOIN stores USING(STORE)) train
                ON(features.Store=train.Store AND features.Date==train.Date)')

df <- df[,!duplicated(colnames(df))]%>%
  separate(Date,c("Year","Month","Day"),"-")

df$Day<- as.numeric(df$Day)
df$Month<- as.numeric(df$Month)
df$Dept<- as.character(df$Dept)
df$Store<- as.character(df$Store)
df$IsHoliday<- as.character(df$IsHoliday)

df <- df%>% group_by(Year,Month)%>%
  mutate(Week = dense_rank(Day))%>%
  ungroup()%>%
  select(-Day)

df <- df%>%mutate_if(is.character,factor)

preproc <- preProcess(df, method = c("medianImpute"))
df <- predict(preproc, df)
# caret
#ctrl <- trainControl(method='LGOCV',
#                     index = list(TrainSet = 
#                                    seq(((nrow(df)-nrow(df)/10)+1):nrow(df))))

#M_xgb <- train(Weekly_Sales~.,
#                data=df,
#                trControl=ctrl,
#                method="xgbTree")
# xgboost
df_val<-df[((nrow(df)-nrow(df)/10)+1):nrow(df),]
df_train <- df[1:(nrow(df)-nrow(df)/10),]

previous_na_action <- options('na.action')
options(na.action='na.pass')
x_val <- sparse.model.matrix(Weekly_Sales ~ .-1, data = df_val)
x_train <- sparse.model.matrix(Weekly_Sales ~ .-1, data = df_train)
options(na.action=previous_na_action$na.action)


y_val <- df_val%>%dplyr::select(Weekly_Sales)%>% as.matrix()
y_train <- df_train%>%dplyr::select(Weekly_Sales)%>% as.matrix()

dtrain <- xgb.DMatrix(data = x_train, label=y_train)
dval <- xgb.DMatrix(data = x_val, label=y_val)

watchlist <- list(train=dtrain, validation=dval)
params <- list(booster = "gbtree", 
               objective = "reg:linear", 
               eta=0.1, gamma=0,lambda=6,alpha=0,
               max_depth=15)

bst <- xgb.train(data=dtrain, nthread = 2, nround=150, 
                 watchlist=watchlist, 
                 early_stopping_round = 3,
                 eval_metric = "rmse",
                 params=params,
                 verbose=0)
bst$best_score
bst$best_iteration

importance_matrix <- xgb.importance(model = bst,feature_names = x_train@Dimnames[[2]])
importance_matrix

xgb.plot.tree(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
xgb.plot.multi.trees(model = bst, feature_names =sparse_matrix@Dimnames[[2]], features_keep = 5)
head(importance)
importance
head(importance_gbl)
importance_gbl
importance_gbl<- importance_gbl[, lapply(Weight, format, scientific = FALSE)]
xgb.plot.importance(importance_matrix = importance)
df_non_num <- df%>%dplyr::select_if(is.character)%>% 
  mutate_all(funs(factor))
## Center, scale and impute for numeric data
cs <- preProcess(df_num,method=c("center","scale","bagImpute"))
df_num<- predict(cs,df_num)
df <- cbind(df_non_num,df_num,df_y)
sapply(df, class)
## MCA for categorical data
# reduce dimension of holiday and month together seems to work better
mca <- MCA(df,quali.sup=c(2,3), quanti.sup=c(5:14), graph = FALSE)
plot.MCA(mca, invisible=c("ind","quanti.sup"))
mca$eig
df <- cbind(mca$ind$coord,df_non_num$Dept,df_non_num$Year,df_num,df_y)

summary(df)
head(df)
hist(df$Temperature)
hist(df$Unemployment)
hist(df$Weekly_Sales)
length(which(is.na(df$MarkDown1)))/length(df$MarkDown1)
length(which(is.na(df$MarkDown2)))/length(df$MarkDown2)
length(which(is.na(df$MarkDown3)))/length(df$MarkDown3)
length(which(is.na(df$MarkDown4)))/length(df$MarkDown4)
length(which(is.na(df$MarkDown5)))/length(df$MarkDown5)


names(df)
set.seed(666)
M_base <- train(Weekly_Sales~.-Store,
                data=df,
                method="lm")
summary(M_base)
preds_lm <- predict(M_base,df)
RMSE(preds_lm, df$Weekly_Sales) # training error
plot(varImp(M_base),top="10",main="LM")
Enet <- train(Weekly_Sales~Temperature+Fuel_Price+IsHoliday+
                  Store+Year+Month+CPI+Unemployment,
                data=df,
                method="enet")
summary(Enet)
Enet
preds_enet <- predict(Enet,df)
RMSE(preds_enet, df$Weekly_Sales) 

svm <- train(Weekly_Sales~.,
              data=df,
              method="svmRadial")
svm
preds_svm <- predict(svm,df)
RMSE(preds_svm, df$Weekly_Sales) 

plot(varImp(svm),top="10",main="svm")

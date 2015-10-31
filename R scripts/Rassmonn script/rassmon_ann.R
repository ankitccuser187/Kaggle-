library(data.table)  
library(h2o)
library(caret)
library(Metrics)

train <- fread("C:/Users/ankitccuser/Downloads/Compressed/Rosmann stores sale/train.csv",stringsAsFactors = T)
test  <- fread("C:/Users/ankitccuser/Downloads/Compressed/Rosmann stores sale/test.csv",stringsAsFactors = T)
store <- fread("C:/Users/ankitccuser/Downloads/Compressed/Rosmann stores sale/store.csv",stringsAsFactors = T)

train <- merge(train,store,by="Store")
test <- merge(test,store,by="Store")

#change all na values to zero in both datasets
train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

#looking only those stores which are open and having non zero sales
train <- train[ which(train$Open=='1'),]
train <- train[ which(train$Sales!='0'),]

cat("train data column names and details\n")

cat("test data column names and details\n")

train<-train[sample(nrow(train)),]

train[,Date:=as.Date(Date)]
test[,Date:=as.Date(Date)]

# seperating out the elements of the date column for the train set
train[,month:=as.integer(format(Date, "%m"))]
train[,year:=as.integer(format(Date, "%y"))]
train[,Store:=as.factor(as.numeric(Store))]

test[,month:=as.integer(format(Date, "%m"))]
test[,year:=as.integer(format(Date, "%y"))]
test[,Store:=as.factor(as.numeric(Store))]

feature_set<-colnames(train)[!colnames(train)%in% c("Store","Date","Sales","Customers")]


cat("the promo_interval column is in text characteriacal and convert into numerical data ")
train$PromoInterval<-as.integer(train$PromoInterval)


## log transformation to not be as sensitive to high sales
## decent rule of thumb: 
## if the data spans an order of magnitude, consider a log transform
train[,logSales:=log1p(Sales)]

#partition the dataset into train and cross validation
tn<-sample(nrow(train),590000)


h2o.init(nthreads=-1,max_mem_size='6G')
## Load data into cluster from R
trainHex<-as.h2o(train[tn])
crossvalHex<-as.h2o(train[-tn])


deep_ann<-h2o.deeplearning(x=feature_set,y="logSales",training_frame = trainHex,hidden = c(10,5),epochs = 100)

summary((deep_ann))
testHex<-as.h2o(test)

predictions<-as.data.frame(h2o.predict(deep_ann,crossvalHex))

#return the predicition on the original scale
pred <- expm1(predictions[,1])




summary(pred)


#result_ann <- data.frame(Id=test$Id, Sales=pred)


#write.csv(result_ann, "result_ann.csv",row.names=F)

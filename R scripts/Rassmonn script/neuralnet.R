library(readr)
library(neuralnet)

cat("reading the train and test data\n")
train <- read_csv("C:/Users/ankitccuser/Downloads/Compressed/Rosmann stores sale/train.csv")
test  <- read_csv("C:/Users/ankitccuser/Downloads/Compressed/Rosmann stores sale/test.csv")
store <- read_csv("C:/Users/ankitccuser/Downloads/Compressed/Rosmann stores sale/store.csv")



# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
train <- merge(train,store,by="Store")
test <- merge(test,store,by="Store")

#if promo2 is zero then promo2sinceweek and promo2sinceyear also should be zero
tmp<-train[train$Promo2==0,]
tmp$Promo2SinceWeek<-0
tmp$Promo2SinceYear<-0
train<-train[train$Promo2!=0,]
train<-rbind(tmp,train)

tmp<-test[test$Promo2==0,]
tmp$Promo2SinceWeek<-0
tmp$Promo2SinceYear<-0
test<-test[test$Promo2!=0,]
test<-rbind(test,tmp)

rm(tmp)


# There are some NAs in the integer columns so conversion to zero
train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

cat("train data column names and details\n")
names(train)

cat("test data column names and details\n")
names(test)


# looking at only stores that were open in the train set
# may change this later
train <- train[ which(train$Open=='1'),]
train <- train[ which(train$Sales!='0'),]

train<-train[sample(nrow(train)),]

# seperating out the elements of the date column for the train set
train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer(format(train$Date, "%d"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)


# seperating out the elements of the date column for the test set
test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)


cat("convert the sales in log")
train$Sales<-log1p(train$Sales)

feature_set<-colnames(train)[!colnames(train)%in% c("Store","Date","Sales","Customers")]


cat("the promo_interval column is in text characteriacal and convert into numerical data ")
for (f in feature_set) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

nn<-neuralnet(formula = 
                Sales ~ DayOfWeek  +  Open +                     
                Promo      +      StateHoliday +             
                SchoolHoliday   +  StoreType  +               
                Assortment    +    CompetitionDistance  +     
                CompetitionOpenSinceMonth +  CompetitionOpenSinceYear  +
                Promo2            +    Promo2SinceWeek   +        
                Promo2SinceYear   +    PromoInterval   +          
                month   +   year  +    day  ,data = train,
                hidden = c(10,4),
                stepmax = 844338,
                err.fct = "sse",
                algorithm = "rprop+",
                linear.output = TRUE,
                rep = 2,
                learningrate.factor = list(minus = 0.5, plus = 1.2),
              )
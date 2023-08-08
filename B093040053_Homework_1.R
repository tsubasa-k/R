library(dplyr)
library(data.table)
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
library(ggplot2)
library(nortest)
library(TTR)
library(lattice)
library(caret)


# 1.1
data1 <- read.csv("C:\\Users\\USER\\Dataset\\kbopitchingdata.csv", header=TRUE, stringsAsFactors=FALSE)
data1 = data1[, sapply(data1, function(x) all(!is.na(x)))]



# 1.2
group <- cut(data1$year, breaks = c(0, 2006, 2011, 2016, 2021, 2025),
                  labels = c("<2005", "2006-2010","2011-2015", "2016-2020", "2021-2025"),
                  right = FALSE)
data1$year_interval <- group



# 1.3
aver_era <- data1 %>% filter(wins > 60) %>% group_by(year_interval) %>%
  summarise(aver_era = mean(sum(as.numeric(ERA))))
aver_era

# year_interval aver_era
# <2005         308.68
# 2006-2010     89.30
# 2011-2015     133.31
# 2016-2020     188.93
# 2021-2025     34.83



# 1.4
sqldf("SELECT team, sum(wins) as total_wins FROM data1 GROUP BY team ORDER BY total_wins")

#                     team total_wins
# 1            SSG Landers         66
# 2       Sammi Superstars        105
# 3           Woori Heroes        110
# 4         Chungbo Pintos        112
# 5          Kiwoom Heroes        235
# 6       MBC Blue Dragons        394
# 7       Pacific Dolphins        415
# 8                 KT Wiz        441
# 9  Ssangbangwool Raiders        449
# 10        Binggre Eagles        493
# 11          Nexen Heroes        613
# 12              NC Dinos        649
# 13      Hyundai Unicorns        830
# 14              OB Bears        927
# 15         Haitai Tigers       1189
# 16            Kia Tigers       1368
# 17            SK Wyverns       1437
# 18         Hanwha Eagles       1637
# 19          Doosan Bears       1668
# 20              LG Twins       2023
# 21          Lotte Giants       2317
# 22         Samsung Lions       2712



# 1.5
qplot(data1[, "average_age"], geom = "density") + xlab("average_age")
lapply(c(lillie.test, ad.test), function(t) t(data1$average_age))

#Q: Does it seem “normal”? 
#   Justify your answer with any statistical methods if you would like.
#A: Because the p-values of both of two normality test are all P > 0.05.
#   So, it seem "normal".

#   [[1]] Lilliefors (Kolmogorov-Smirnov) normality test
#         p-value = 0.09445
#   [[2]] Anderson-Darling normality test
#         p-value = 0.1436



# 1.6
#create scatterplot
plot(data1$average_age, data1$ERA)

#add lowess smoothing curve to plot
lines(lowess(data1$average_age, data1$ERA), col='red')

fit <- lm(average_age ~ ERA, data = data1)
#view summary of model
summary(fit)

#Q: Is the average_age associated with the ERA? Use any statistical
#   methods to justify your answers with a brief description
#A: No, the average_age is not associated with the ERA, they are independent.

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  24.6279     0.4953  49.727  < 2e-16 ***
# ERA          0.5368      0.1159   4.632 5.26e-06 ***



# 2.1
data2 <- read.csv("C:\\Users\\USER\\Dataset\\vaccine_2022_TW.csv", header=TRUE, stringsAsFactors=FALSE)
data2 = data2[, sapply(data2, function(x) all(!is.na(x)))]

domain = data2 %>% count(domainArticle, sort = TRUE)
name = domain[1, "domainArticle"]

#Q: Which domain has the most news?
#A: The domain "news.sina.com.tw" has the most news, it has 7187 news. 



# 2.2
news = filter(data2, domainArticle == name)
# convert column to date class
news$date <- as.Date(news$datetimeArticle)
num <- news %>% group_by(date) %>% summarise(news_per_day = n())



# 2.3
# sma is the most news domains in the way of moving average
sma <- round(SMA(num$news_per_day))
    
# Plot a line char and round off the number to integer and label date for every week on the x-axis.
ggplot(data = num, aes(x=date, y=sma)) + geom_line() +
  scale_x_date(date_breaks = "1 week", date_labels = "%m%d")



# 3.1
data3 <- read.csv("C:\\Users\\USER\\Dataset\\HW_car_purchasing_final.csv", header=TRUE, stringsAsFactors=FALSE)
data3 = data3[, sapply(data3, function(x) all(!is.na(x)))]

lapply(colnames(data3[, -7]), function(x){ #remove car_purchase_amount
  if(is.factor(data3[,x])){ #if it's factor, print box plot and do one-way anova
      print(qplot(data3[,x], data3[, "car_purchase_amount"], geom = "boxplot") + xlab(x) + ylab("car_purchase_amount"))
      one.way <- aov(data3$car_purchase_amount ~ data3[,x])
      print(summary(one.way))
  }else{ #if it's not, print point plot and do linear model
      print(qplot(data3[,x], data3[, "car_purchase_amount"], geom = "point") + xlab(x) + ylab("car_purchase_amount"))
      fit <- lm(data3$car_purchase_amount ~ data3[,x])
      print(summary(fit))
  }
})

#Q: Are “annual Salary” and “net worth” associated with “car purchase amount”?
#A: Because the both p-value < 0.05, “annual Salary” and “net worth” are associated with “car purchase amount”.



# 3.2
# Split the dataset into training set (70%) and testing set (30%) with set.seed(1)
set.seed(1)
train_idx = sample(1:nrow(data3), 0.7*nrow(data3))
train_d = data3[train_idx,]
test_d = data3[setdiff(1:nrow(data3), train_idx),] 

# Rescale continuous variables into the values ranging from 0 to 1 without centralizing
preProc <- preProcess(train_d, method = c("scale"))

# For other data sets (test, valid...), organize the data sets according to the pre-processing done by train, 
# and unify the format of all data
trainTrans <- predict(preProc, train_d)
testTrans <- predict(preProc, test_d)



# 3.3
# Write down a function that compute Mean Absolute Error (MAE)
MAE = function(pred, target){
  return(mean(abs(pred - target)))
}



# 3.4
# Build a general linear model with the rescaled training set
lm_select = lm(car_purchase_amount ~ age + annual_Salary + credit_card_debt + net_worth, trainTrans)

lm_all = lm(car_purchase_amount ~ ., trainTrans)

# Model with selected variables
pred = lm_select$fitted.values
print(round(MAE(pred, trainTrans$car_purchase_amount), 4))
pred = predict(lm_select, testTrans)
print(round(MAE(pred, testTrans$car_purchase_amount), 4))

# Model with all variables
pred = lm_all$fitted.values
print(round(MAE(pred, trainTrans$car_purchase_amount), 4))
pred = predict(lm_all, testTrans)
print(round(MAE(pred, testTrans$car_purchase_amount), 4))



# 3.5
# Remove the predictors with higher p-values(> 0.05), then build a new general linear model
lm_select_new = lm(car_purchase_amount ~ age + annual_Salary + net_worth, train_d)

# Model with selected variables with higher p-values(> 0.05)
pred = lm_select_new$fitted.values
print(round(MAE(pred, train_d$car_purchase_amount), 4))
# MAE: 1.1707
pred = predict(lm_select_new, test_d)
print(round(MAE(pred, test_d$car_purchase_amount), 4))
# MAE: 1.2124

#Q: Does the new model have lower errors in terms of training and testing MAE?
#A: No, the new model doesn't make too much difference.



# 3.6
# The new model that considers all the two-way interactions without removing any predictors.
lm_all2way = lm(car_purchase_amount ~ (.)^2, train_d)

# Model with all variables with 2-way interactions
pred = predict(lm_all2way, train_d)
print(round(MAE(pred, train_d$car_purchase_amount), 4))
# MAE: 1.129
pred = predict(lm_all2way, test_d)
print(round(MAE(pred, test_d$car_purchase_amount), 4))
# MAE: 1.2894


#Q: Does the new model have lower errors in terms of training and testing MAE?
#   Can this complex model with more parameters improve the prediction? 
#   Which model would you recommend to be used in real-world production and why?
#A: 1. Above the result in the new model, the training MAE has lower errors.
#      But the testing MAE has higher errors.
#   2. I don't think the complex model with more parameters can improve the prediction.
#      Because it may not have lower errors in terms of training and testing MAE.
#   3. The model with selected variables with higher p-values(> 0.05).
#      Because it is more simple and the performance is almost the same.
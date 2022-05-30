library(dplyr)
library(tidyverse)
library(ggthemes)

#Loading data
myStore <- read.csv("ohianistore.csv", stringsAsFactors = TRUE)
View(myStore)
str(myStore)

#Descriptive Analysis of the Profit
summary(myStore$Profit)
var(myStore$Profit)
sd(myStore$Profit)

#Descriptive Analysis of the Sales
summary(myStore$Sales)
var(myStore$Sales)
sd(myStore$Sales)

#Descriptive Analysis of the Quantity
summary(myStore$Quantity)
var(myStore$Quantity)
sd(myStore$Quantity)

#Descriptive Analysis of the Discount
summary(myStore$Discount)
var(myStore$Discount)
sd(myStore$Discount)

#DATA VISUALIZATION
#Histogram of Distribution of sales
ggplot(myStore)+
  geom_histogram(aes(x = Sales))+
  labs(title='Distribution of Sales',subtitle='average sales is $229.86')+
  ggthemes::theme_economist()+xlim(0,3000)+ylim(0,900)

#Histogram of Distribution of Discount
hist(myStore$Discount, main = "Distribution of Discount", 
     xlab = "average discount is 0.1562")

#Histogram of Distribution of Quantity
hist(myStore$Discount, main = "Distribution of Quantity", 
     xlab = "average quantity of product is 3.79")

#Histogram of Distribution of Profit
ggplot(myStore)+
  geom_histogram(aes(x = Profit))+
  labs(title='Distribution of Profit',subtitle='average profit is $28.657')+
  ggthemes::theme_fivethirtyeight()+xlim(0,1000)+ylim(0,300)

#Barchart showing the distribution of sales
ggplot(myStore)+geom_col(aes(x = Region,y = Sales),fill= '#36AE7C')+
  labs(title='Region-wise Distribution of Sales',subtitle='West Region generates the highest Sales')
+theme_classic()+scale_color_brewer()

#Comparing the western states generation of profit
myStore %>% 
  filter(State %in% c('Arizona','California')) %>% 
  ggplot() +
  geom_density(aes(x = Profit,color = State, fill = State), alpha = 0.5) + 
  labs(title='Comparison Of profits between states in the western Region',
       x = 'Profit',
       y = 'Frequency'
  )+
  theme_minimal()+xlim(-100,100)+
  scale_fill_brewer(palette = "YlOrBr", name = "State")


#This helps us create a simple barchart
#We are looking to find the top 5 states that generate the most profit
ggplot(myStore, aes(Profit, State)) + geom_bar(stat = "identity")

#Sub categories of segments that generate the most sales
myStore %>% 
  group_by(Sub.Category) %>% 
  summarize(count = n()) %>% 
  filter(count > 100) %>%
  ggplot() +
  geom_col(aes(x = Sub.Category, y = count), fill= '#1ca9c9')  +
  coord_flip() +
  ggthemes::theme_gdocs()


#BUILDING A MULTIPLE LINEAR REGRESSION MODEL TO PREDICT PROFIT

#Step1: Building the correlation matrix to check the correlation coefficient
#We use the correlation matrix to check the correlation between three or more variables
cor(myStore[c("Sales","Discount","Quantity")])

#Step2: Building our model
myStoreModel <- lm(Profit ~ Sales + Quantity + Discount, data = myStore)
summary(myStoreModel)

#Step3: Prediction
#This is used to predict the profit based on our trained data/model
myStore$predictt <- predict(myStoreModel, myStore)
#correlation between predicted and actual values
cor(myStore$predictt, myStore$Profit)


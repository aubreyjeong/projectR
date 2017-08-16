

## Group_project, team=potato

options(repos="http://cran.rstudio.com")
install.packages("GGally")
install.packages("ggplot2")
install.packages("hydroGOF")
install.packages("mvtnorm")
install.packages("lattice")
install.packages("corrplot")
install.packages("caret")
install.packages("NbClust")
install.packages("ggmap")
install.packages("hflights")
install.packages("DT")
install.packages("prettyR")
install.packages("psych")
install.packages("nortest")
install.packages("usdm")
install.packages("zoo")
install.packages("lmtest")


library(GGally)
library(ggplot2)
library(mvtnorm)
library(corrplot)
library(caret)
library(graphics)
library(NbClust)
library(ggmap)
library(hflights)
library(DT)
library(prettyR)
library(psych)
library(nortest)
library(car)
library(lm.beta)
library(usdm)
library(zoo) 
library(lmtest)

House <- readxl::read_excel(path="c:/R/practicedata/kc_house_data.xlsx",
                            sheet=1,
                            col_names=TRUE)
View(House)
str(House)


### 

summary(House) ## check the basic info about variables

House2 <- as.data.frame(House) ## to keep rawdata
str(House2)

House2[1:2] <- NULL ## delete the dat : id, date (We won't consider it now.)

## N>20000, spilit it & sampling it. 

# Splitting the Data Set 
ratio = sample(1:nrow(House2), size = 0.25*nrow(House2))
Test = House2[ratio,] #Test dataset 25% of total
Training = House2[-ratio,] #Train dataset 75% of total




###############################################
## <STEP1> DATA PRE-PROCESSING               ##
###############################################



## 1. Overview total datas' distribution and correlations. 

## (1) plot by using ggpairs() to see distribution of each data

plot(House2)

## retry to draw plot by using ggpair()
## We can do it at once, but I spilit it for convinient.

plot_g1 <- ggpairs(data=Training, columns=c(1:5),
                   mapping = aes(color = "dark green"),
                   axisLabels="show")
plot_g1

plot_g2 <- ggpairs(data=Training, columns=c(1,6:10),
                   mapping = aes(color = "dark green"),
                   axisLabels="show")
plot_g2

plot_g3 <- ggpairs(data=Training, columns=c(1,11:17),
                   mapping = aes(color = "dark green"),
                   axisLabels="show")
plot_g3

plot_g4 <- plot_g3 <- ggpairs(data=Training, columns=c(1,18:19),
                              mapping = aes(color = "dark green"),
                              axisLabels="show")
plot_g4

View(House2)


## (2) visualization Corr in all var _ Corrplot
## one by one ~ correalation of each variables (except id, date)

corH <- cor(House2)
corrplot(corH, type="full", method = "circle", main="Correlation")
print(round(corH, digits=3))


## corr_table : all variables

# corr between price vs sqft_living: 0.702
# corr between price vs grade: 0.673
# Corr between price vs sqft_above : 0.604
# Corr between price vs sqft_living15 : 0.587
# corr between price vs bathrooms: 0.525
# corr between price vs view: 0.401
# corr between price vs sqft_basement : 0.321
# corr between price vs lat: 0.312
# corr between price vs bedrooms: 0.308
# corr between price vs waterfront: 0.255
# Corr between price vs floors : 0.256
# Corr between price vs sqft_lot15 : 0.185
# corr between price vs sqft_lot : 0.0897
# corr between price vs yr_built : 0.0521
# Corr between price vs condition : 0.0327
# corr between price vs yr_renovated : 0.13
# corr between price vs long : 0.0214
# corr between price vs zipcode : -0.0527




###################################################################################################

## According to the result, I will handle/analyze the data in this way 

## cmmt. there are three types of var about size of house. 
##      sqft_living & sqft_lot / sqft_above & sqft_basement / sqft_living15 & sqft_lot15
##      we mainly use sqft_living as a basis of mesurement of house size.

## ST1. draw barplot to see the distribution of two catergorical data: grade, condition 
##      before putting in final model.
##      + handling the data only if I find outlier and missing values.

## ST2. draw boxplot with all discrete variables : floors, bedrooms, bathrooms, view, waterfront
##      before putting in final model.
##      + handling the data only if I find outlier and missing values.

## ST3. draw boxplot with all continuous variables : sqft_v 
##      + replace outliers only if I think it's necessary.

## ST4. compare distribution of price and price/sqft_living. -> choose one of them to apply to the model.

## ST5. (1) make new variable:yr_built_final by including both yr_built and yr_renovated. 
##      (2) get cor(price, yr_built_final) and decide whether I put this one in model or not.

## ST6-1. corr(price, Zipcode) is extremely low -> ignore it.

## ST6-2. clustering with the variables about location : lat, long 
##        to make the useful (dummy) variables to apply to final price forecasting model.

## ST7. change the type of categorical variables as.factor

##############################################################################

## ST8(H)-2sam_t. independent two sample test ~ diff in price depends on this dummy variable.
##                (1) make new dummy variable : exist_basement(0,1)
##                (2) watherfront   

## ST9(H)-ksam_t. independent k sample test;ANOVA with view 


##############################################################################

## ST10(H). making multiple linear regression model ~ price estimating model w/ refined vars
##          before making it, check the prerequisite and corr(price ~ all ind_Var) and choose vars to use.

## (+) ST11(H). using House (original data)
##              (*subset the data which house is sold in both 2014 and 2015*)
##              categorizing House$date in two ways by yrs(2014, 2015)
##              compare the difference of predicted price by using the different model
##              date:2014 ~ using sqft_living15, date:2015 ~using sqft_living15 instead of sqft_living


######################################################################################################


## 2. Uni-variate data analyzing

## ST1. Categorical data-ordinal : grade, condition

## frequency

prettyR::freq(House2$grade) 

prettyR::freq(House2$condition) 

## graph

barplot(sort(table(House2$grade), decreasing = TRUE),
        col = "purple",
        main = "grade")

barplot(sort(table(House2$condition), decreasing = TRUE),
        col = "purple",
        main = "condition")

## Only few categories. Do not need to worry about outliers.




## ST2-3. quantitative variables : all (only except grade & condition)

## summary, mean, median etc.

summary(House2)

## gragh

## ST2. discrete variables : bedrooms, bathrooms, floors, waterfront, view

## (1) bedrooms

boxplot1=boxplot(price~bedrooms, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. bedrooms", xlab="bedrooms", ylab="Price")

## It seems that value 11, 33 is totally outlier. 

## House2$bedrooms is discrete varible -> Can't use IQR accuately to locate outliers.

psych::describe(House2$bedrooms) 
## skew 1.97 -> It has some extreme values in right side.

par(mfrow = c(1,2))
hist(House2$bedrooms, xlab = "", col = "lightsteelblue",  main = "Bedrooms")
plot(density(House2$bedrooms), xlab="", col = "steelblue", main="Bedrooms")

## Checking bedrooms extremes as distibution is very skewed
print(subset(House2, House2$bedrooms > 10))

# There is one record each for bedroom 11 and 33. 
## 33 is extremly high value compared to mean(bedrooms) = 3.37
# 33 bedroom house has 1.75 bathrooms, sqft_living is also very less. Seems little odd.
# I suspect a data entry error here. Lets calculate the mean sqft_living of 3 bedroom house.

bed3 <- subset(House2,House2$bedrooms == 3)
bed3
print(tapply(bed3$sqft_living, bed3$bedrooms, mean))

# Mean sqft_living of 3 bedroom house is approx 1805 whereas for 33 bedrooms it is 1620.
# We can remove this, but I'm changing 33 bedrooms to 3. Not doing anything for 11 bedroom house.
House2[15871,2] <- 3 ## replace the outlier value

par(mfrow = c(1,2))
hist(House2$bedrooms, xlab = "", col = "lightsteelblue",  main = "Bedrooms")
plot(density(House2$bedrooms), xlab="", col = "steelblue", main="Bedrooms")

## Bedrooms finally seems having a linear relationship with price.



## (2) bathrooms

par(mfrow = c(1,1))

boxplot2=boxplot(price~bathrooms, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Bathrooms", xlab="Bathrooms", ylab="Price")

psych::describe(House2$bathrooms) 
## skew 1.28 -> It has some extreme values in right side.

par(mfrow = c(1,3))
hist(House2$bathrooms, breaks = 20, xlab = "", col = "lightsteelblue",  main = "Bathrooms")
plot(density(House2$bedrooms), xlab="", col = "steelblue", main="Bathrooms")
scatter.smooth(House2$bathrooms, House2$price, col="steelblue", xlab="", ylab="Price",main="Bathrooms",lpars=list(col="red", lwd=2))

# Distribution looks skewed.
# Lets see what is the ratio of bedrooms >= 6
print(subset(House2, House2$bathrooms >6))
print(prop.table(table(House2$bathrooms >= 6))) ## FALSE 0.999

## replace the value(>=6) according to their sqft_living values
## only if they are odd compared to other data which has similar sqft_living

bath4 <- subset(House2,House2$bathrooms == 4)
bath5 <- subset(House2,House2$bathrooms == 5)
bath6 <- subset(House2,House2$bathrooms == 6)
bath6.25 <- subset(House2,House2$bathrooms == 6.25)
mean(bath4$sqft_living) ## 4071.868
mean(bath5$sqft_living) ## 4851.857
mean(bath6$sqft_living) ## 6443.333
mean(bath6.25$sqft_living) ## 8345

House2[8547,3] <- 4 ## 9, 4050 
House2[20579,3] <- 5.75 ## 6, 6260

psych::describe(House2$bathrooms) ## skew 0.49
print(subset(House2, House2$bathrooms >6))

par(mfrow = c(1,3))
hist(House2$bathrooms, breaks = 20, xlab = "", col = "lightsteelblue",  main = "Bathrooms")
plot(density(House2$bedrooms), xlab="", col = "steelblue", main="Bathrooms")
scatter.smooth(House2$bathrooms, House2$price, col="steelblue", xlab="", ylab="Price",main="Bathrooms",lpars=list(col="red", lwd=2))


## hold off_below (Not necessarily)

#sqft_li1 <- subset(House2, House2$sqft_living > 7000)
#mean(sqft_li1$bathrooms) # 5.536

#mean(sqft_li1$bathrooms) # 5.536
#sqft_li4 <- subset(House2, House2$sqft_living > 10000)
#mean(sqft_li4$bathrooms) # 6.833
#sqft_li5 <- subset(House2, House2$sqft_living > 12000) ## only two vars=outliers
#mean(sqft_li5$bathrooms) # 8

#print(prop.table(table(House2$sqft_living >= 7000))) # 0.001
#print(prop.table(table(House2$sqft_living > 3673))) # 0.057
#print(prop.table(table(House2$sqft_living >= 5586))) ## 0.005 ## top 0.5% 

#sqft_li6 <- subset(House2, House2$sqft_living > 3673)
#mean(sqft_li6$bathrooms) # 3.487
#sqft_li7 <- subset(House2, House2$sqft_living >= 5586)
#mean(sqft_li7$bathrooms) # 4.54 



## (3) floors
par(mfrow = c(1,1))
boxplot3=boxplot(price~floors, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Floors", xlab="Floors", ylab="Price")

psych::describe(House2$floors) # skew 0.62


## (4) view

boxplot4=boxplot(price~view, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. View", xlab="View", ylab="Price")

psych::describe(House2$view) # skew 3.4

## No proper way to replace the values even if skew is really high. 
## even though skew is over 2, it has only few numbers of values


## (5) waterfront

boxplot5=boxplot(price~waterfront, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Waterfront", xlab="Waterfront", ylab="Price")

psych::describe(House2$waterfront) # 11.38 -> it's okay. this is kind of dummy_var






## ST3. Continuous variables : sqft_v

## (1) sqft_living

boxplot6=boxplot(price~sqft_living, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Sqft_living", xlab="Sqft_living", ylab="Price")

psych::describe(House2$sqft_living) ## skew 1.47
psych::describe(log(House2$sqft_living)) ## skew - 0.06 (Change the scale with log())

cor.test(House2$price, House2$sqft_living) # 0.702
cor.test(House2$price, log(House2$sqft_living)) # 0.612 (Change the scale with log())

## I will go with original sqft_living.

distIQR <- IQR(House2$sqft_living, na.rm = TRUE)
posIQR <- quantile(House2$sqft_living, probs = c(0.25,0.75), na.rm=TRUE)
posIQR
Down_lim <- posIQR[[1]] - distIQR*1.5
Up_lim <- posIQR[[3]] + distIQR*1.5
#* Up_lim <- posIQR[[3]] + distIQR*1.5
## sqft_living - 1.5*distIQR = 296 
## sqft_living + 1.5*distIQR = 3673

print(prop.table(table(House2$sqft_living > 3673))) # 0.057 ## top 5%
print(prop.table(table(House2$sqft_living >= 4980))) ## 0.010 ## top 1%
print(prop.table(table(House2$sqft_living >= 5586))) ## 0.005 ## top 0.5% 

print(prop.table(table(House2$sqft_living > 296))) ## 0.9995 ~= 1

## We can replace the outliers. However, in itself, it has high corr_value with price. 
## It almost seems price and sqft_living have a linear realationship


## (2) sqft_lot

boxplot7=boxplot(price~sqft_lot, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Sqft_lot", xlab="Sqft_lot", ylab="Price")

psych::describe(House2$sqft_lot) # skew 13.06
## Skew is really high, but cor(price, sqft_lot) is extremely low.
## I don't use this var in pricing model.


## (3) sqft_above

boxplot8=boxplot(price~sqft_above, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Sqft_above", xlab="Sqft_above", ylab="Price")

psych::describe(House2$sqft_above) # skew 1.46


## (4) sqft_basement

boxplot9=boxplot(price~sqft_basement, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Sqft_basement", xlab="Sqft_basement", ylab="Price")

psych::describe(House2$sqft_above) # skew 1.45

## (5) sqft_living15

boxplot10=boxplot(price~sqft_living15, data=Training, 
                  col=(c("gold","darkgreen")),
                  main="Price vs. Sqft_living15", xlab="Sqft_living15", ylab="Price")

psych::describe(House2$sqft_living15) # skew 1.11

## (6) sqft_lot15

boxplot11=boxplot(price~sqft_lot15, data=Training, 
                  col=(c("gold","darkgreen")),
                  main="Price vs. Sqft_lot15", xlab="Sqft_lot15", ylab="Price")

psych::describe(House2$sqft_lot15) # skew 9.51




## ST4. Compare distribution of price and price/sqft_living 

hist_price <- hist(House2$price, main = "Price histogram",  xlab = "Price per square feet")
hist_price

## maybe we should think about using price/sqft (*reduce the scale to get better dist)

House2$price_per_sqft<- House2$price/House2$sqft_living

hist_pricesq <- hist(House2$price_per_sqft, main = "Price per Square feet histogram", xlab = "Price per square feet") 

## Better distribution than price ~ it seems more closer to bell-shape(ND).

## visualization Corr in all var _ Corrplot including new variable.

H_numeric <- House2[,c(1:14, 18:20)]
corrplot(cor(H_numeric), method = "circle")

## Price has the highest positively correlations with sqft_living, grade, sqft_above and sqft_living15.
## Price_per_sqft has low correlation with most variables.

## Thus, I will use original price as a dependent valiable in model.



## ST5. (1) make new variable:yr_built_final by including both yr_built and yr_renovated. 
##      (2) get cor(price, yr_built_final) and decide whether I put this one in model or not.


for(i in 1:21613){
  if(House2$yr_renovated[i] !=0){
    House2$yr_built[i] <-House2$yr_renovated[i]
  }
}

cor(House2$price, House2$yr_built) # 0.054

## I will not use this new variable




## ST6.  location variables ~ land : lat, long -> Clustering

## Quick look at the land distributtion
par(mfrow=c(1,1))
plot_loc <- plot(House2$long, House2$lat, main = "Latitude and longitude plot",xlab = "Longitude",ylab = "Latitude", col = "purple")
plot_loc

View(House2)

## Clustering

## set the number of clusters.
nc <- NbClust(House2, min.nc=2, max.nc=40, method="Kmeans")
## TSS matrix is indefinite. There must be too many missing values. The index cannot be calculated.

## Just let's start with 20 clusters.

View(House2)

## In clustering, I will use price_per_sqft instead of price
## Just because, I need three variable which I use in this process are independent from each other.

House_clustering20 <- kmeans(scale(House2[,c(1,16,17)]), center=20, iter.max=100, trace=FALSE)
House_clustering20

## Due to the random nature of k-means I used 100 random starts to get the best possible clustering result, it is possible to use even more, but hardly necessary. Scale is used to normalize data with average = 0 and standard deviation = 1, normal procedure before clustering. I use Latitude, Longitude and Price per sqft as attributes.

## With 20 cluster the result is between_SS / total_SS = 90.2 %, already a good number, let¡¯s plot and see how it worked

House2$cluster<-factor(House_clustering20$cluster)

## plotting the clusters

ggplot(data= House2, aes(x = long, y = lat)) + geom_point(aes(color=cluster))

## Use ggmap() to bring googlemap of king county

getmap_king <- get_googlemap("king county", zoom = 10)
getmap_king <- ggmap(getmap_king)

## plotting the clusters on ggmap

map_cluster <- getmap_king + geom_point(data=House2, aes(x=long, y=lat, colour=cluster),size=0.5)
map_cluster

## density_plot

ggplot(House2, aes(price, fill = cluster)) + geom_density(position = "stack")

## It's too narrow, so I will use price_per_sqft as x-axis
ggplot(House2, aes(price_per_sqft, fill = cluster)) + geom_density(position = "stack")


## The latitude and longitude plotting shows the clusters location, looks good, the density plot shows that most clusters have a similar price per square feet.


# Creating dummy variables for the clusters

dummies <- data.frame(matrix(nrow = nrow(House2),ncol = 20))
for(i in 1:20){
  dummies[,i] <- ifelse(House2$cluster == i, 1,0)
}

nrow(House2)
sum(dummies) == nrow(House2)

kc <- cbind(House2,dummies)

## Dummies are useful when using linear regression, each dummy has a column, using 0 when that tuple is not on the cluster and 1 when it is in.




## ST7. change the type of categorical variables as.factor

str(House2$grade)
str(House2$condition)

House2$grade <- as.factor(House2$grade)
House2$condition <- as.factor(House2$condition)





## ST8. independent two sample test

## (1) dummy variable : exist_base 

## I wanna test the significance of existence of basement.

## To reduce the reduce the power of size impact, I make a new variable.
# sqft_basement_ratio <- House2$sqft_basement/House2$sqft_above
# cor.test(House2$price, sqft_basement_ratio) ## 0.114
# cor.test(House2$price_per_sqft, sqft_basement_ratio) ## -0.059

# exist_basement <- as.data.frame(House2$sqft_basement)

House2$exist_base <- House2$sqft_basement
View(House2)


for(i in 1:21613){
  if(House2$exist_base[i] != "0"){
    House2$exist_base[i] = "1"
  }
}
View(House2)


## H0 : There is no difference of mean(price_per_sqft) depends on the existence of basement.
## H1 : not H0


## 1) test of normality

## H0 : No observable difference between (both) data_set and normal distribution
## H1 : not H0

## n > 5,000 : we can't use shapiro normality test

by(House2$price, House2$exist_base, ad.test)


# House2$exist_base: 0, p-value < 2.2e-16
# House2$exist_base: 1, p-value < 2.2e-16

## both of p-values are 0.000 -> accept H1 with a=0.05
## Both group do not follow the normality 


## 2) Wilcoxon's Rank-sum test

wilcox.test(House2$price_per_sqft ~ House2$exist_base, 
            alternative="two.sided")
# W = 52925000, p-value = 5.838e-10

## the p-value in t-test is 0.000 -> accept H1 with a=0.05
## There is difference of mean(price_per_sqft) depends on the existence of basement at the confidence level of 95 percent




## (2) dummy variable : waterfront

str(House2$waterfront)
House2$waterfront <- as.character(House2$waterfront)


## H0 : There is no difference of mean(price_per_sqft) depends on the existence of waterfront.
## H1 : not H0


## 1) Normality test

## H0 : No observable difference between (both) data_set and normal distribution
## H1 : not H0

## n > 5,000 : we can't use shapiro normality test

by(House2$price, House2$waterfront, ad.test)

# House2$waterfront: 0, p-value < 2.2e-16
# House2$waterfront: 1, p-value = 9.56e-10

## both of p-values are 0.000 -> accept H1 with a=0.05
## Both group do not follow the normality 


## 2) Wilcoxon's Rank-sum test

wilcox.test(House2$price ~ House2$waterfront, 
            alternative="two.sided")
# W = 405500, p-value < 2.2e-16

## the p-value in t-test is 0.000 -> accept H1 with a=0.05
## There is difference of mean(price_per_sqft) depends on the existence of waterfront at the confidence level of 95 percent


House2$waterfront <- as.numeric(House2$waterfront)



## ST9(H)-ksam_t. independent k sample test; ANOVA with view 

## H0 : There is no difference of mean(price_per_sqft) depends on the value of view.
##      mu1=mu2=mu3=mu4
## H1 : not H0

## 1) Normality test

## H0 : No observable difference between each View and normal distribution
## H1 : not H0

by(House2$price, House2$view, ad.test)

## House2$view: 0, p-value < 2.2e-16
## House2$view: 1, p-value < 2.2e-16
## House2$view: 2, p-value < 2.2e-16
## House2$view: 3, p-value < 2.2e-16
## House2$view: 4, p-value < 2.2e-16

## all p-values are 0.000 -> accept H1 with a=0.05
## Both group do not follow the normality 


## 2) Kruskal-Wallis rank sum test

kruskal.test(House2$price_per_sqft ~ House2$view)

## Kruskal-Wallis chi-squared = 759.38, df = 4, p-value < 2.2e-16
## p-value is 0.000 -> accept H1 with a=0.05
## There is difference of mean(price_per_sqft) depends on the value of view


## 3) Multiple Comparisons = Post-Hoc

View(House2)

House2$fview <- factor(House2$view)

house.lm <- lm(price ~ fview, data = House2)
house.av <- aov(house.lm)
summary(house.av)
tukey.test <- TukeyHSD(house.av)
tukey.test


##                 Df    Sum Sq   Mean Sq   F value  Pr(>F)    
## fview           4  4.901e+14  1.225e+14    1093   <2e-16 ***

# $fview
#        diff       lwr        upr    p adj
#1-0 315716.65 265157.60  366275.69 0.000000
#2-0 295836.70 265681.66  325991.73 0.000000
#3-0 475401.08 434425.59  516376.57 0.000000
#4-0 967147.05 915585.01 1018709.08 0.000000
#2-1 -19879.95 -78016.94   38257.05 0.884204
#3-1 159684.44  95267.24  224101.63 0.000000
#4-1 651430.40 579811.77  723049.03 0.000000
#3-2 179564.39 129537.55  229591.22 0.000000
#4-2 671310.35 612299.02  730321.68 0.000000
#4-3 491745.96 426538.59  556953.34 0.000000

## There is the diffrence between View(2) and View(1) with a=0.05








## ST10(H). making multiple linear regression model ~ price estimating model


## I should choose the ind_var which I put in the regression model.

## Before putting ind_vars in regression model,
## each ind_vars should satisfy three assumptions : Normality & Multicollinearity

## Going through the pre-preocessing, 
## I decide to use these independent variables

House3 <- as.data.frame(House2)

View(House3)
str(House3)

House3[21:23] <- NULL
House3[11:19] <- NULL
House3[9] <- NULL
House3[5] <- NULL

## Let's see the correlation among all variables which I chose.
## cor() : only numeric var can use this function.

str(House3)

House3$grade <- as.numeric(House3$grade)

DT::datatable(round(cor(House3), digits = 3)) 

plot_final1 <- ggpairs(data=House3, columns=c(1, 2, 3, 4, 5),
                       mapping = aes(color = "dark green"),
                       axisLabels="show")
plot_final1

plot_final2 <- ggpairs(data=House3, columns=c(1, 6, 7, 8),
                       mapping = aes(color = "dark green"),
                       axisLabels="show")
plot_final2



## Multicollinearity


## quantitive vars : bedrooms, bathrooms, sqft_living, floor are highly related.
## grade is also higly related with these four variables, but it's quality variable, so let's consider it separately now. 

## If corr between variables is high(> 0.7) -> It might cause multicollinearity problem.

## Corr(price ~ bedrooms, bathrooms, sqft_living, floor)

# corr between sqft_living vs bedrooms : 0.591
# corr between sqft_living vs bathrooms : 0.755(*)
# corr between sqft_living vs floors : 0.354
# corr between bedrooms vs bathrooms :0.526
# corr between bedrooms vs floors : 0.257
# corr between bathrooms vs floors : 0.501


## Let's make new variable by using four variables(bedrooms, bathrooms, sqft_living, floor)

## I use corr_value between price and each variables as coefficient=slope 

House3$quan_spec <- (0.315*House3$bedrooms + 0.526*House3$bathrooms + 0.702*House3$sqft_living + 0.257*House3$floors)

View(House3) # 10 variables


House4 <- as.data.frame(House3)
House4[2:5]<- NULL


DT::datatable(round(cor(House4), digits = 3)) 

## the most hightest corr betweens ind_vars is 0.402 (waterfront & view)
## its less than 0.7 -> it seems fine to put these in regression model.


## Normality

psych::describe(House4$quan_spec) ## skew 1.47 kurtosis 5.23
psych::describe(House4$grade) ## skew 0.77 kurtosis 1.17
psych::describe(House4$waterfront) ## skew 11.38 kurtosis 127.59
psych::describe(House4$view) ## skew 3.4 kurtosis 10.89

## check the skewness and kurtosis to easy to check the normality of each vars.
## Only grade can be used according to this.
## We already modified and got rid of many variables to make the good pricing model.
## If we delete these three variables, there are no more variables to make the reg model. 
## Even if it's not satisfy normality assumption, I'm going to use this vars which have pretty good corr with price.



## making multiple linear regression

House4$grade <- as.numeric(House4$grade)

View(House4)
House4[5]<-NULL


##
##  Regression model without cluster dummies
##


linearmodel1 <- lm(price ~ ., data = House4)
# plot(House4$price, lineartest1$fitted.values, xlab = "Actual price", ylab = "Predicted price", main = "Actual and Prediction comparisson, no clusters")

summary(linearmodel1)

## step 1 ## 
## H0 : linear regression model is not valid.
## H1 : not H0

## F-statistic:  7733 on 4 and 21608 DF,  p-value: < 2.2e-16
## Accept H1 => This linear regression model is valid.

## ster 2 ##
## H0 : Each independent variables do not have influence on dependent variable;price.
## H1 : not H0


#                 Estimate Std. Error t value Pr(>|t|)    
#  waterfront   5.901e+05  2.022e+04   29.18   <2e-16 ***
#  view         7.058e+04  2.372e+03   29.75   <2e-16 ***
#  grade        9.450e+04  2.111e+03   44.76   <2e-16 ***
#  quan_spec    2.360e+02  3.882e+00   60.81   <2e-16 ***


## every p-value is less than 0.05
## It's okay to use all variables in this model.

## For just in case, let's do valiable selection

model.forward = step(linearmodel1, direction = "forward")
model.backward = step(linearmodel1, direction = "backward")
model.stepwise = step(linearmodel1, direction ="both")

## each method brings the same value of AIC : 534679.9 and use all variables.
## I will go with the linearmodel1 without changing anything.

## Accept H1 => Each independent variables have influence on dependent variable;price.



## step 3 ##

#  waterfront   5.901e+05  
#  view         7.058e+04  
#  grade        9.450e+04  
#  quan_spec    2.360e+02 

## When each indep_var increases by one unit, price will increase by upper amount of dollars.


## step 4 ##
## price = -4.552e+05 + 5.901e+05*waterfront + 7.058e+04*view + 9.450e+04*grade + 2.360e+02*quan_spec

## prediction model
# predict(linearmodel1, newdata=data.frame())
# predict(linearmodel1, newdata=data.frame(), interval = "predict")


## step 5 ##

## Multiple R-squared : 0.5887
## 0.589 => 0.589*100(%) = 58.9%
## This pricing model(or this four variables) can explain 58.9% of the difference of price.


## check the multicollinearity of this reg model
car::vif(linearmodel1)

# waterfront   view      grade    quan_spec 
# 1.193124   1.288156   2.400023   2.445779 

## there is no problem of multicollinearity


## Standardized Coefficients
## comparsion of the size of influence of each vars.

lm.beta::lm.beta(linearmodel1)

# (Intercept)  waterfront        view       grade   quan_spec 
#  0.0000000   0.1390695   0.1473213   0.3024939   0.4148831 


## To use this reg, I shoud check the indepence of residuals
## Durbin-Watson test
## H0 : residual ~ Normality
## H1 : not H0

dwtest(linearmodel1)

# DW = 1.9753, p-value = 0.03469

## p-value < 0.5 -> 
## DW value is close to 2 -> It means this model's residual are independent from each other.






##
##  Regression model with cluster dummies
##

House5 <- cbind(House4,dummies)
View(House5)
str(House5)

#Since X1, X2 ... X20 are complementary I removed X20 from the equation, linear regression demands it.

linearmodel2 <- lm(price ~ waterfront + view + grade + quan_spec + X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19, data = House5)

#Since X1, X2 ... X20 are complementary I removed X20 from the equation, linear regression demands it.

# plot(House4$price, lineartest1$fitted.values, xlab = "Actual price", ylab = "Predicted price", main = "Actual and Prediction comparisson, no clusters")

summary(linearmodel2)


## step 1 ## 
## H0 : linear regression model is not valid.
## H1 : not H0

## F-statistic: 1.178e+04 on 23 and 21589 DF,  p-value: < 2.2e-16
## Accept H1 => This linear regression model is valid.


## ster 2 ##
## H0 : Each independent variables do not have influence on dependent variable;price.
## H1 : not H0

#               Estimate  Std. Error t value  Pr(>|t|)    

#  waterfront   1.365e+05  8.897e+03  15.338  < 2e-16 ***
#  view         1.418e+04  1.053e+03  13.465  < 2e-16 ***
#  grade        3.052e+04  9.583e+02  31.851  < 2e-16 ***
#  quan_spec    1.223e+02  1.766e+00  69.234  < 2e-16 ***
#  X1           1.456e+06  7.655e+03 190.206  < 2e-16 ***
#  X2           1.078e+05  4.865e+03  22.154  < 2e-16 ***
#  X3           1.356e+05  3.877e+03  34.962  < 2e-16 ***
#  X4           2.478e+05  4.067e+03  60.941  < 2e-16 ***
#  X5           8.845e+05  5.471e+03 161.683  < 2e-16 ***
#  X6           2.719e+06  1.366e+04 198.956  < 2e-16 ***
#  X7          -4.496e+04  3.998e+03 -11.244  < 2e-16 ***
#  X8           5.350e+05  5.105e+03 104.806  < 2e-16 ***
#  X9           2.069e+05  4.083e+03  50.675  < 2e-16 ***
#  X10          4.519e+05  4.228e+03 106.877  < 2e-16 ***
#  X11          1.540e+04  3.810e+03   4.041 5.34e-05 ***
#  X12          9.478e+04  4.976e+03  19.049  < 2e-16 ***
#  X13          1.612e+05  6.033e+03  26.717  < 2e-16 ***
#  X14          2.542e+05  3.651e+03  69.612  < 2e-16 ***
#  X15          3.146e+05  4.669e+03  67.374  < 2e-16 ***
#  X16          9.156e+04  3.741e+03  24.471  < 2e-16 ***
#  X17         -5.525e+04  3.906e+03 -14.143  < 2e-16 ***
#  X18         -8.568e+03  3.869e+03  -2.215   0.0268 *  
#  X19          1.420e+05  3.780e+03  37.582  < 2e-16 ***


## every p-value is less than 0.05
## It's okay to use all variables in this model.

## For just in case, let's do valiable selection

model.forward = step(linearmodel2, direction = "forward")
model.backward = step(linearmodel2, direction = "backward")
model.stepwise = step(linearmodel2, direction ="both")

## each method brings the same value of AIC=497588.2 and use all variables.
## I will go with the lineartest1 without changing anything.

## Accept H1 => Each independent variables have influence on dependent variable;price.



## step 3 ##


#  waterfront   1.365e+05  
#  view         1.418e+04  
#  grade        3.052e+04  
#  quan_spec    1.223e+02  
#  X1           1.456e+06  
#  X2           1.078e+05  
#  X3           1.356e+05  
#  X4           2.478e+05  
#  X5           8.845e+05  
#  X6           2.719e+06  
#  X7          -4.496e+04  
#  X8           5.350e+05  
#  X9           2.069e+05  
#  X10          4.519e+05  
#  X11          1.540e+04  
#  X12          9.478e+04  
#  X13          1.612e+05 
#  X14          2.542e+05 
#  X15          3.146e+05  
#  X16          9.156e+04  
#  X17         -5.525e+04  
#  X18         -8.568e+03  
#  X19          1.420e+05  

## When each indep_var increases by one unit, price will increase by upper amount of dollars.


## step 4 ##
## price = -2.600e+04 + 1.365e+05*waterfront + 1.418e+04*view + 3.052e+04*grade + 1.223e+02*quan_spec + 1.456e+06*X1+1.078e+05*X2+1.356e+05*X3+2.478e+05*X4+8.845e+0*X5+2.719e+06*X6-4.496e+04*X7+5.350e+05*X8+2.069e+05*X9+4.519e+05*X10+1.540e+04*X11+9.478e+04*X12+1.612e+05*X13+2.542e+05*X14+3.146e+05*X15+9.156e+04*X16-5.525e+04*X17-8.568e+03*X18+1.420e+05*X19

## prediction model
# predict(linearmodel2, newdata=data.frame())
# predict(linearmodel2, newdata=data.frame(), interval = "predict")


## step 5 ##

## Multiple R-squared : 0.9262
## 0.926 => 0.926*100(%) = 92.6%
## This pricing model(or this five variables) can explain 92.6% of the difference of price.
## bedrooms, bathrooms, sqft_living, floors, waterfront, grade, view can explain 92.6% of the difference of price.

## check the multicollinearity of this reg model
car::vif(linearmodel2)

#waterfront       view      grade  quan_spec         X1         X2         X3         X4 
#1.286144   1.412980   2.752952   2.818604   1.419879   1.487174   2.059240   1.904673 
#X5         X6         X7         X8         X9        X10        X11        X12 
#1.614056   1.233812   1.936175   1.738534   2.024576   1.860107   2.133532   1.457411 
#X13        X14        X15        X16        X17        X18        X19 
#1.357496   2.437423   1.745278   2.312771   2.026573   2.204441   2.225525 

## there is no problem of multicollinearity


## Standardized Coefficients
## comparsion of the size of influence of each vars.

lm.beta::lm.beta(linearmodel2)

#Standardized Coefficients::
#  (Intercept)   waterfront         view        grade    quan_spec           X1           X2 
#0.000000000  0.032159090  0.029591840  0.097706408  0.214897307  0.419030477  0.049948903 
#X3           X4           X5           X6           X7           X8           X9 
#0.092758182  0.155494923  0.379768955  0.408580544 -0.028926556  0.255491046  0.133307573 
#X10          X11          X12          X13          X14          X15          X16 
#0.269494667  0.010912767  0.042516322  0.057551663  0.200931155  0.164557563  0.068804096 
#X17          X18          X19 
#-0.037224820 -0.006078882  0.103655720 


## To use this reg, I shoud check the indepence of residuals
## Durbin-Watson test
## H0 : residual ~ Normality
## H1 : not H0

dwtest(linearmodel2)

# DW = 1.9859, p-value = 0.1502

## p-value > 0.05 => distribution of residuals in this reg model is Normal dist. 
## DW value is close to 2 -> It means this model's residual are independent from each other.

hist(linearmodel2$residuals, xlab = "Residual", main = "Residual Distribution", breaks = 50)

## In this histogram, their distribution seems like ND


## => Linear regression model with cluster dummies is much better than lrm without cluster dummies.
## AIC is lower (534679.9>497588.2), 
## adjusted R-squared is higher (0.5887<0.9261)

## This is our final model. 
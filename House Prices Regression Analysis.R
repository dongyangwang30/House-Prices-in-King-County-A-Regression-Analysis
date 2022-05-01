##########################EDA############################

# Preparation 1: Clean up environment and set working directory
rm(list = ls())
getwd()
setwd('/Users/dongyangwang/Desktop/UW/Stat 504')


# Preparation 2: Load necessary packages
library(tidyverse)
library(readxl)
library(ggthemes)
library(quantreg)


# Step 1: Understanding the structure of the data
df <- read.csv('kc_house_data.csv')
head(df,10)
str(df)
summary(df)

# Appears no missing values. Report the structure of the dataset.


# Step 2: Drop unused and outdated variables
df <- select(df, -c(id, sqft_living, sqft_lot))


# Step 3: Format the variables

# To factor (with levels)
df$view <- as.factor(df$view)
df$grade <- as.factor(df$grade)
df$condition <- as.factor(df$condition)

# To character
df$zipcode <- as.character(df$zipcode)

# to boolean
df$renovation[df$yr_renovated > 0] <- 1
df$renovation[df$yr_renovated == 0] <- 0
df[df$yr_renovated == 0,]
View(df[df$yr_renovated == 0,])

# Step 4: Generate new variables
# Last renovation
df<- df %>% mutate(last_renovated = case_when(yr_renovated == 0 ~ yr_built,
                                              TRUE ~ yr_renovated))

# Year recorded
df<- df %>% mutate(yr_recorded = as.numeric(substr(date, 1,4)))

# Years present
df<- df %>% mutate(yrs_present = yr_recorded - yr_built)

# Several -1 values in the following variable: Sold before built
# More reasonable to change to 0
summary(df$yrs_present)
df$yrs_present[df$yrs_present < 0] = 0

# Total area
df$total_area = df$sqft_above + df$sqft_basement + df$sqft_living15 + df$sqft_lot15

# Total area except lot area
df$total_area_except_lot = df$total_area - df$sqft_lot15

# Drop date
df <- select(df, -c(date))

str(df)


# Step 5: Summary statistics and visualizations
attach(df)

# Dependent variable
summary(price)

# Right skewed (Outliers not shown, but shown below)
hist(price, breaks = 50, xlim = c(0,4e6))

# Outlier alert: once price is above 4 million
boxplot(price)
quantile(price,0.999)

# Numeric variables

# Years present variable:yrs_present
summary(yrs_present)
summary(yr_built)
summary(yr_renovated)

# Right skewed for yrs_present; renovation quite recent
hist(yrs_present, breaks = 30, xlim = c(0,1.2e2))
hist(yr_built)
hist(yr_renovated[yr_renovated>0], breaks = 30)

# Area variable: sqft_above, sqft_basement, sqft_living15, sqft_lot15
summary(sqft_above)
summary(sqft_basement)
summary(sqft_living15)
summary(sqft_lot15)

# All these four variables are right skewed
hist(sqft_above, breaks = 30, xlim = c(0,1e4))
hist(sqft_basement, breaks = 30, xlim = c(0,5e3))
hist(sqft_living15, breaks = 30, xlim = c(0,7e3))
hist(sqft_lot15, breaks = 100, xlim = c(5e2,9e5))

# Above and living are most normal variables
# Be careful about area of the lot: a lot of outliers
boxplot(sqft_lot15)

# numbers of rooms and floors variable:bedrooms, bathrooms, floors
summary(bedrooms)
summary(bathrooms)
summary(floors)

# histograms for these three variables
hist(bedrooms[bedrooms<11], breaks = 10)
hist(bathrooms, breaks = 50)
hist(floors)

# Outlier alert: Only 2 houses have more than 10 bedrooms -- possibly acceptable
boxplot(bedrooms)
bedrooms[bedrooms>10]

# Boolean variables
table(renovation)
table(waterfront)

# Categorical variables
plot(view)
plot(grade)
plot(condition)

# Location variables
hist(lat)
hist(long)
hist(as.numeric(zipcode), breaks = 100)

# Caution: some of the zip codes have fewer observations. Should this be a problem?

detach(df)


# Step 6: Explore interactions

# Categorical
# The plots for zip code: there are differences although plots are messy
length(unique(df$zipcode))
boxplot(df$price ~ df$zipcode)

zipcode_price = df %>%
  group_by(zipcode) %>%
  summarize(mean = mean(price)) %>%
  arrange(desc(mean)) 

# 70 counties all covered
zipcode_price %>%
  print(n = 70)

# Beautiful correlations
boxplot(df$price ~ df$grade)
boxplot(df$price ~ df$condition)
boxplot(df$price ~ df$view)

# Numeric
# The plots for area
ggplot(df, aes(x = total_area, y = price)) +
  geom_point()

ggplot(df, aes(x = total_area_except_lot, y = price)) +
  geom_point()

# Notice that except for lot area, other areas seem to correlated positively with price
# SUGGESTION: calculate area without lot and see if it is significant

ggplot(df, aes(x = sqft_living15, y = price)) +
  geom_point()

ggplot(df, aes(x = sqft_basement, y = price)) +
  geom_point()

ggplot(df, aes(x = sqft_above, y = price)) +
  geom_point()

ggplot(df, aes(x = sqft_lot15, y = price)) +
  geom_point()

#plots for number of rooms/floors
ggplot(df, aes(x = bedrooms, y = price)) +
  geom_point()

ggplot(df, aes(x = bathrooms, y = price)) +
  geom_point()

ggplot(df, aes(x = floors, y = price)) +
  geom_point()

#years of present plots
ggplot(df, aes(x = yrs_present, y = price)) +
  geom_point() 


#longitudes and latitudes plots
ggplot(df, aes(x = long, y = price)) +
  geom_point()

ggplot(df, aes(x = lat, y = price)) +
  geom_point()

# Boolean
boxplot(df$price ~ df$waterfront)
boxplot(df$price ~ df$renovation)


##########################Linear Regression############################

# Step 1:Data transformation
# Magnify latitude to 0~62.17(amplify 100 times)
df=df%>%mutate(lat=((lat-mean(lat))/sd(lat))*100)

# Transform floors into integral to avoid confusion
df$floors[df$floors == 3.5] = 6
df$floors[df$floors == 3] = 5
df$floors[df$floors == 2.5] = 4
df$floors[df$floors == 2] = 3
df$floors[df$floors == 1.5] = 2
df$floors[df$floors == 1] = 1

# Transform sqft_lot15/sqft_basement to categorical, better utilize quantiles
# Divide sqft_lot15 into 5 categories by quantile
quantile=quantile(df$sqft_lot15,seq(0,1,0.2))
df=df%>%mutate(sqft_lot15=case_when(sqft_lot15<=quantile[2]~0,
                                    sqft_lot15>quantile[2] & sqft_lot15<=quantile[3]~1,
                                    sqft_lot15>quantile[3] & sqft_lot15<=quantile[4]~2,
                                    sqft_lot15>quantile[4] & sqft_lot15<=quantile[5]~3,
                                    sqft_lot15>quantile[5] & sqft_lot15<=quantile[6]~4))

# Divide sqft_basement into 2 categories as the house has basement or not
df=df%>%mutate(sqft_basement=case_when(sqft_basement==0~0,
                                       sqft_basement>0~1))

# Examine explanatory data types,make sure they are correct
# sqft_above/lat/bedrooms/bathrooms/
# floors/grade/yrs_present/sqft_basement: Continuous
df=df%>%mutate(sqft_above=as.numeric(sqft_above),lat=as.numeric(lat),
               bedrooms=as.numeric(bedrooms),bathrooms=as.numeric(bathrooms),
               floors=as.numeric(floors),grade=as.numeric(grade),
               yrs_present=as.numeric(yrs_present),view=as.numeric(view))

# When put as.numeric function on column "view", it directly put plus 1 on
# the original data, to fix this:
df$view=df$view-1

#sqft_lot15/renovation/waterfront/view: Categorical
df=df%>%mutate(sqft_lot15=as.factor(sqft_lot15),renovation=as.factor(renovation),
               waterfront=as.factor(waterfront),sqft_basement=as.factor(sqft_basement))


# Step 2:Raw stepwise regression without intersection
lm0=lm(price~sqft_above+lat+bedrooms+bathrooms+floors+grade+yrs_present+view
       +sqft_basement+sqft_lot15+renovation+waterfront,data=df)
lm0.step=step(lm0,direction='both',k=2)
summary(lm0.step)

# Plot residual Q-Q and histogram.Here we face normality problem
par(mfrow=c(2,2))
plot(lm0.step)


# Step 3:Raw stepwise regression without intersection
#Here we take log transformation of price. This is because 
#1.narrow absolute deviation of price
#2.satisfy more of classical linear model assumptions(nonlinearity/normality/homoscedasticity)
#3.Good for interpretation:percentage change
lm1=lm(log(price)~sqft_above+lat+bedrooms+bathrooms+floors+grade+yrs_present+view
       +sqft_basement+sqft_lot15+renovation+waterfront,data=df)
lm1.step=step(lm1,direction='both',k=2)
summary(lm1.step)

# This time, residual seems to be normally distributed. Good
plot(lm1.step)


# Step 4:Possible Intersections
# renovation:yrs_present
# yrs_present^2:because the relationship between log(price) and yrs_present are quadratic
par(mfrow=c(1,1))
df%>%group_by(yrs_present)%>%summarize(avg_price=mean(log(price)))%>%arrange(desc(avg_price))%>%plot()

lm2=lm(log(price)~sqft_above+lat+bedrooms+bathrooms+floors+grade+I(yrs_present^2)+view
       +yrs_present*renovation+sqft_basement+sqft_lot15+waterfront,data=df)
lm2.step=step(lm2,direction='both',k=2)
summary(lm2.step)

par(mfrow=c(2,2))
plot(lm2.step)
# Results seem very good.All significant, with only one counter-intuitive
# Coefficient:yrs_present:renovation is negative


# Step 5: Regression on avg_price
df=df%>%mutate(avg_price=as.numeric(price/total_area_except_lot))
lm3=lm(log(avg_price)~lat+bedrooms+bathrooms+floors+grade+I(yrs_present^2)
       +yrs_present*renovation+sqft_lot15+waterfront+view,data=df)
lm3.step=step(lm3,direction='both',k=2)
summary(lm3.step)

# From the summary table we can see that the variable is not significant in the 
# regression model. Although stepwise regression maintains it, we exclude this 
# variable manually
lm3=lm(log(avg_price)~lat+bedrooms+bathrooms+grade+I(yrs_present^2)
       +yrs_present*renovation+sqft_lot15+waterfront+view,data=df)
lm3.step=step(lm3,direction='both',k=2)
summary(lm3.step)
plot(lm3.step)
# Results seems to be fine.But there are several counter-intuitive results:
# bedrooms negative;yrs_present positive;yrs_present:renovation negative


##########################Regression By Region#################################
# Step1 Preparation: sort out zip code within each area of interest
# Get zip code
zipcode <- sort(as.numeric(unique(df$zipcode)))

# Select and divide data to 3 regions: Central Seattle, Vicinity of Seattle and
# Bellevue
Sea.cen <- as.character(zipcode[42:60]) # Central Seattle
Sea.vic <- as.character(zipcode[61:70]) # Vicinity of Seattle
Bellevue <- as.character(98004:98008) #Bellevue


# Step2 Perform regression on each area separately
# ols regression of price in central Seattle
# Mean yrs_present in central Seattle
mean(df[which(df$zipcode %in% Sea.cen), ]$yrs_present)

cen <- lm(log(price) ~ sqft_above + lat + bedrooms + bathrooms + floors 
          + grade + yrs_present + I(yrs_present^2) + yrs_present*renovation
          + sqft_basement + sqft_lot15, 
          data = df[which(df$zipcode %in% Sea.cen), ])

summary(cen)

# ols regression of price in vicinity of Seattle
# Mean yrs_present in vicinity of Seattle
mean(df[which(df$zipcode %in% Sea.vic), ]$yrs_present)

vic <- lm(log(price) ~ sqft_above + lat + bedrooms + bathrooms + floors 
          + grade + yrs_present + I(yrs_present^2) + yrs_present*renovation
          + sqft_basement + sqft_lot15, 
          data = df[which(df$zipcode %in% Sea.vic), ])

summary(vic)

# ols regression of price in Bellevue
# Mean yrs_present in Bellevue
mean(df[which(df$zipcode %in% Bellevue), ]$yrs_present)

bel <- lm(log(price) ~ sqft_above + lat + bedrooms + bathrooms + floors 
          + grade + yrs_present + I(yrs_present^2) + yrs_present*renovation
          + sqft_basement + sqft_lot15, 
          data = df[which(df$zipcode %in% Bellevue), ])

summary(bel)


# Step3 put coefficients of three regression into one table for comparison
coef_regression_region<-data.frame(summary(cen)$coefficients[,1], 
                                   summary(vic)$coefficients[,1],
                                   summary(bel)$coefficients[,1])
names(coef_regression_region)<-c("Central Seattle", "Vicinity of Seattle",
                                 "Bellevue")
view(coef_regression_region)


##########################Quantile Regression##################################
# We have already finished the linear regression with quadratic and intersection
# terms inserted, noted as "lm2". We plan to further adopt quantile regression
# based on this model.

#we fit the model similar to that in linear regression once again
fit = lm(log(price) ~ sqft_above + lat + bedrooms + bathrooms + floors + grade 
         + I(yrs_present^2) + yrs_present*renovation +sqft_basement+sqft_lot15,
         data=df)

# quantile regression
rqfit = rq(fit, tau = seq(0.05, 0.95, by = 0.05), data = df)
plot(rqfit)
#findings in quantile regression
#bedrooms
#above median: bedrooms increasing comes with lower price, 
#              the higher the price is, the lower the coef is
#              (negative efficient increases)
#below median: bedrooms increasing comes with higher price, 
#              the higher the price is, the lower the coef is
#              (positive efficient decreases)

# yrs_present
#above 30%: years increasing comes with higher price, 
#           the higher the price is, the higher the coef is
#           (positive efficient increases)
#below 30%: years increasing comes with lower price, 
#           negative efficient decreases with the increasing of price

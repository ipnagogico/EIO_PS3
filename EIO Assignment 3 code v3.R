remove(list=ls())
rm()

#install.packages("tidyverse")
#install.packages("foreign")
#install.packages("fastDummies")
#install.packages("coefplot")

library(tidyverse)
library(foreign)
library(fastDummies)
library(coefplot)
library(stargazer)

#setwd("yourwd")

data <- read.dta("cars.dta")

######################################
### Data Cleaning & Transformation ###

data <- data %>% 
  rename(condition = impcond,
         brand = makename,
         model = modname,
         import_price = impprice,
         engine_power = engpow,
         engine_capacity = engcap,
         gross_weight = grosswgt,
         four_wheel_drive = fourwd,
         export_country = countrycode,
         quantity = unitsold,
         brand_model = makemod) %>% 
  mutate(size = width * length * height, # a proxy for the model's size
         engine_power = na_if(engine_power, 0),
         relative_engine_capacity = engine_capacity / size, # a proxy for the model's power
         real_price = import_price / (cpi * 0.01), 
         export_country = droplevels(export_country), # helps us to create country dummies
         brand_model_condition = paste(brand_model, condition))

data <- dummy_cols(data, select_column = "export_country")

# We identify and eliminate heavy outliers in real price
ggplot(data) + geom_histogram(aes(real_price))

price_quartiles <- quantile(data$real_price)
price_iqr <- IQR(data$real_price)
lower_border <- price_quartiles[2] - 3 * price_iqr
upper_border <- price_quartiles[4] + 3 * price_iqr

data <- data %>% 
  mutate(price_outlier = (real_price < lower_border) | (real_price > upper_border)) %>% 
  filter(!price_outlier)

# we introduce an index for joining of data frames down the line
index <- seq(nrow(data))

data <- data %>% 
  add_column(index)



##################
## Market Size ###

# Our market size is based on the number of households in Cyprus
# We need numbers for the following years:
unique(data$year)

# We have information on population and household size from 2004 to 2019.
# For the years in the data set (1988-2000), we only have population numbers.
# Thus, we use the data from 2004 to 2019 to estimate a household size we can use
# for the years in the data set
population04_19 <- c(1010410,
                     1027657,
                     1045508,
                     1063708,
                     1081568,
                     1098089,
                     1112617,
                     1124837,
                     1135046,
                     1143866,
                     1152297,
                     1160987,
                     1170189,
                     1179685,
                     1189262,
                     1198574)

households04_19 <- c(220695,
                     228390,
                     236921,
                     241518,
                     248034,
                     252727,
                     263966,
                     272407,
                     307305,
                     306304,
                     307072,
                     306788,
                     301800,
                     309253,
                     296253,
                     301472)

year04_19 <- seq(2004, 2019)

households04_19 <- tibble(year04_19,
                          population04_19,
                          households04_19) %>% 
  rename(year = year04_19,
         population = population04_19,
         households = households04_19) %>% 
  mutate(people_per_hh = population / households)

ggplot(households04_19, aes(year, people_per_hh)) + geom_point() + geom_smooth(method = "lm")

year88_00 <- seq(1988, 2000)
population88_00 <- c(736476,
                     751043,
                     766616,
                     783121,
                     800610,
                     818750,
                     837104,
                     855391,
                     873426,
                     891190,
                     908710,
                     926049,
                     943288)


# Since the household size seems to be descending over the years,
# we estimate the household size for our in-sample years by a linear model
household_model <- lm(people_per_hh ~ year, data = households04_19)
household_size88_00 <- predict.lm(household_model, tibble(year88_00) %>% rename(year = year88_00))
market_size88_00 <- population88_00 / household_size88_00

# Merge it all with the original data frame
market <- tibble(year88_00,
                 market_size88_00) %>% 
  rename(year = year88_00,
         market_size = market_size88_00)

data <- data %>% 
  left_join(market, by = "year")

#plot of our estimated market size, as proposed: linear trend
ggplot(data, aes(year, market_size)) + geom_point() + geom_smooth(method = "lm") +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  xlim(1988,2001)


##############
## Nesting ###


####create the two layered nests:


#actually first decision would be the budget, is it okay to use this because it is an endogenous variable?
# so we want to base our nesting structure on price clusters 
# Ad-hoc check for reasonable groups

ggplot (data, aes(x=rprice, y=condition))+geom_point()
summary(data$condition)
data %>% filter(condition=="used") %>% summary("rprice")

ggplot(data = data, aes(x = rprice, fill = suv)) +
  geom_histogram(bins= 150)+ 
  geom_vline(xintercept =7834,  color="aquamarine", size=1.5)+ #first border is the max price of used cars, so all used cars are inside nest 1
  geom_vline(xintercept =18000,  color="aquamarine", size=1.5)+
  geom_vline(xintercept =27400,  color="aquamarine", size=1.5)

table(data$engine_capacity)

# Cutoffs are based on descriptive check
cutoffs_1_price = c(7834, 18000, 27400)
cutoffs_inf_1_rprice = c(-Inf, cutoffs_1_price, Inf)


data <- data %>% 
  mutate(group_1_rprice = cut(rprice,
                              breaks = cutoffs_inf_1_rprice,
                              labels = c("cheap", "moderate", "expensive", "luxus")))

# Ad-hoc check if groups are reasonable
ggplot(data, aes(rprice, fill = group_1_rprice)) + geom_histogram(bins = 100) +
  labs(title = "distribution of rprice") + 
  ylab("frequency") + 
  xlab("real price")
table(data$group_1_rprice)


#second level nest is the car size proxied by the engine capacity

#decide on the cars size
summary(data$size) 
summary(data$basespan)
summary(data$width)
summary(data$length)
summary(data$height)#many NAs, so we use something else
summary(data$engine_capacity) # no NAs 
ggplot(data, aes(x=engine_capacity ,y=basespan)) +geom_point()+geom_smooth(method=lm)
ggplot(data, aes(x=engine_capacity ,y=size)) +geom_point()+geom_smooth(method=lm)# we see the engine capacity is in fact positivly correlated with size and basspan 



#which will be the same as in assigment 1 (we do this, to make the resulting cross price elasticities comparable)

# We want to base our nesting structure on engine capacity
# Ad-hoc check for reasonable groups
ggplot(data, aes(engine_capacity)) + geom_histogram(bins = 50)
table(data$engine_capacity)

# Cutoffs are based on descriptive check
cutoffs_2_engine = c(1000, 1600, 2400)
cutoffs_inf_2_engine = c(-Inf, cutoffs_2_engine, Inf)


data <- data %>% 
  mutate(group_2_engine = cut(engine_capacity,
                     breaks = cutoffs_inf_2_engine,
                     labels = c("tiny", "small", "middle", "big")))

# Ad-hoc check if groups are reasonable
ggplot(data, aes(engine_capacity, fill = group_2_engine)) + geom_histogram(bins = 100) +
  labs(title = "distribution of engine capacity") + 
  ylab("frequency") + 
  xlab("engine capacity")
table(data$group_2_engine)






##############################
### Market Share Variables ###



### 1 share: Product j market shares on total years quantity & outside good market shares each year

# Total quantity sold (all cars) in a year
total_quant <- data %>% 
  group_by(year) %>% 
  summarize(total_quantity = sum(quantity))
#add to data frame and calcualte the shares 
data <- data %>% 
  left_join(total_quant, by = "year") %>% 
  mutate(sharej = quantity / market_size, # a car's share in a year
         outside_quantity = market_size - total_quantity, #calculate outside goods amaount
         outside_share = outside_quantity / market_size) # outside share in a year

data[1,]
# i.e. Opel Omega made up 0.019% of all potentially sold cars in 1988 (market share in denominator). The outside share was 94.9%

# nest level 1: rprice 
# Group shares each year:


group_1_quant <- data %>% # summarize quantities per year and per group (level 1 toplevel price)
  group_by(group_1_rprice, year) %>% 
  summarize(group_1_quantity = sum(quantity))

#add to data

data <- data %>% 
  left_join(group_1_quant, by = c("group_1_rprice", "year")) 

#nest level 2: engine capacity 

group_2_engine_quant <- data %>% 
group_by(group_1_rprice,group_2_engine, year) %>% 
  summarize(subgroup_2_engine_quantity = sum(quantity)) # calculates the sum of shares of any subgroup (lowest level)
group_2_engine_quant$subgroup_2_engine_quantity <- as.numeric(group_2_engine_quant$subgroup_2_engine_quantity)



### 2share: share of product j on the subgroup:

#add to data
data <- data %>% 
  left_join(group_2_engine_quant, by = c("group_1_rprice", "group_2_engine", "year")) %>% mutate(sharej_subgroup = quantity/subgroup_2_engine_quantity)
#and calculate the share of product j on the subgroups sells 
data[1,] #Opel Omega has a share of 27 % of its subgroup expensive and middel 


###3 share: share of a subgroup on the group:

data <- data %>% mutate(sharesubgroup_group = subgroup_2_engine_quantity/group_1_quantity)
data[1,]
# i.e. the middel engine capacity cars (of which Opel Omega is one), make 68% of the expensive cars (of which Opel Omega is one) 



############################################
### Instruments: Variable Transformation ###

data$logsharej <- log(data$sharej) #adds share j on total market t to the data 



## instruments for price: 

# Number of products in a year in a sub group and its inverse 
#difference to assignment 1 is now, we use the subgroup, before we just had one level 
subgroup_size <- data %>% 
  group_by(group_1_rprice,group_2_engine, year) %>% 
  summarize(subgroup_size = n()) %>% 
  mutate(subgroup_inverse = 1/subgroup_size)
range(subgroup_size$subgroup_size)

# join with data set
data <-left_join(data, subgroup_size, by= c("group_1_rprice","group_2_engine", "year"))

cor(data$real_price, data$subgroup_size)
ggplot(data, aes(subgroup_size, real_price)) + geom_point() + geom_smooth(method = "lm") #still makes sence, with greater number of compeitors, the price declines

cor(data$real_price, data$subgroup_inverse)
ggplot(data, aes(subgroup_inverse, real_price)) + geom_point() + geom_smooth(method = "lm")


# products of the same brand in a year
brand_in_year <- data %>% 
  group_by(brand, year) %>% 
  summarize(brand_products_in_year = n())
data <- left_join(data, brand_in_year, by = c("brand", "year"))
cor(data$rprice, data$brand_products_in_year)
ggplot(data, aes(brand_products_in_year, real_price)) + geom_point() + geom_smooth(method = "lm")





# instruments for the subgroup shares of product j 

# sums of numerical characteristics of other cars in the same year and same group
characteristic_names <- c("real_price", "engine_capacity", "size", "cylinder", "suv", "diesel") #the same with assignment 1
selection <-  c("year", "brand_model", "group_1_rprice" , "group_2_engine", characteristic_names) #here now again subgroup, not only one level group

characteristics <- data %>% 
  select(all_of(selection))

characteristics_means <- characteristics
characteristics_sums <- characteristics

for (i in seq(nrow(characteristics))) {
  characteristics_temp <- characteristics[-i, ] %>% # drop the analyzed car
    filter(group_1_rprice == characteristics[i, "group_1_rprice"], 
           group_2_engine == characteristics[i, "group_2_engine"], 
           year == characteristics[i, "year"]) # only take cars from that subgroup in that year
  characteristics_means[i, characteristic_names] <- colMeans(characteristics_temp[, characteristic_names]) # calculate the means. Problem: not calculatable for cars that are the only ones in their group in a particular year (e.g. i = 44)
  characteristics_sums[i, characteristic_names] <- colSums(characteristics_temp[, characteristic_names]) # calculate the sums
}

# Problem with the means:
# If a car is the only one in a group, the mean is not calculatable. Thus, we
# only continue with the sums.
colSums(is.na(characteristics_means))

# join the sums to the base dataframe
# the indices are persistent throughout the original and sums dataframe
# we use this to merge them to the original dataframe

# only keep the characteristics
characteristics_sums <- characteristics_sums %>% 
  select(all_of(characteristic_names))

# give them a fitting suffix
colnames(characteristics_sums) <- paste(colnames(characteristics_sums), "sums_subgroup", sep = "_")

# add the index column
characteristics_sums <- characteristics_sums %>% 
  add_column(index)

# merge with original data frame
data <- data %>%
  left_join(characteristics_sums, by = "index") 

data$logsharej_subgroup <- log(data$sharej_subgroup) #adds log of share j product of the subgroup to the data 





# instruments for the group shares of subgroups h 

# sums of numerical characteristics of other cars in the same year and same group
characteristic_names <- c("real_price", "engine_capacity", "size", "cylinder", "suv", "diesel") #the same with assignment 1
selection <-  c("year", "brand_model", "group_1_rprice" , characteristic_names) #here now groupe

characteristics <- data %>% 
  select(all_of(selection))

characteristics_means <- characteristics
characteristics_sums <- characteristics

for (i in seq(nrow(characteristics))) {
  characteristics_temp <- characteristics[-i, ] %>% # drop the analyzed car
    filter(group_1_rprice == characteristics[i, "group_1_rprice"],
           year == characteristics[i, "year"]) # only take cars from that group in that year
  characteristics_means[i, characteristic_names] <- colMeans(characteristics_temp[, characteristic_names]) # calculate the means. Problem: not calculatable for cars that are the only ones in their group in a particular year (e.g. i = 44)
  characteristics_sums[i, characteristic_names] <- colSums(characteristics_temp[, characteristic_names]) # calculate the sums
}

# Problem with the means:
# If a car is the only one in a group, the mean is not calculatable. Thus, we
# only continue with the sums.
colSums(is.na(characteristics_means))

# join the sums to the base dataframe
# the indices are persistent throughout the original and sums dataframe
# we use this to merge them to the original dataframe

# only keep the characteristics
characteristics_sums <- characteristics_sums %>% 
  select(all_of(characteristic_names))

# give them a fitting suffix
colnames(characteristics_sums) <- paste(colnames(characteristics_sums), "sums_group", sep = "_")

# add the index column
characteristics_sums <- characteristics_sums %>% 
  add_column(index)

# merge with original data frame
data <- data %>%
  left_join(characteristics_sums, by = "index") 

data$logsharesubgroup_group <- log(data$sharesubgroup_group) #adds log of share subgroup of the group to the data 





##### first stage ols: 

#price 
summary(data$impduty)

first_stage_price <- lm(real_price ~ subgroup_size+  captariff , data) #big coefficient, because group size is of a range 1-36, prices are max 40.000
summary(first_stage_price)
stargazer(first_stage_price, type = "html", title = "First Stage Regression - Price", ci = T)
#add predicted ("fitted") values to dataframe :
data$price_fitted <- predict(first_stage_price, data)



#subgroupshare

first_stage_sharej_subgroup <- lm(logsharej_subgroup ~ engine_capacity_sums_subgroup + size_sums_subgroup + cylinder_sums_subgroup, data) #we choose characteristics sums as instrument for the within group share
summary(first_stage_sharej_subgroup)
stargazer(first_stage_sharej_subgroup, type = "html", title = "First Stage Share j Subgroup Regression", ci = T)
#add predicted ("fitted") values to dataframe :
data$logsharej_subgroup_fitted <- predict(first_stage_sharej_subgroup, data)




#subgroup of group share

first_stage_share_subgroup_group <- lm(logsharesubgroup_group ~ engine_capacity_sums_group + size_sums_group + cylinder_sums_group, data) #we choose characteristics sums as instrument for the within group share
summary(first_stage_share_subgroup_group)
stargazer(first_stage_share_subgroup_group, type = "html", title = "First Stage Share Subgroup Group Regression", ci = T)
#add predicted ("fitted") values to dataframe :
data$logsharesubgroup_group_fitted <- predict(first_stage_share_subgroup_group, data)




# Second stage
second_stage <- lm(logsharej ~ 
                     size + 
                     engine_capacity +
                     relative_engine_capacity +
                     suv + 
                     diesel + 
                     age +
                     export_country_England +
                     export_country_France + 
                     export_country_Germany + 
                     export_country_Russia + 
                     export_country_Italy + 
                     export_country_USA + 
                     export_country_CzechRep + 
                     export_country_Sweden + 
                     export_country_Spain + 
                     export_country_Korea + 
                     export_country_Malaysia +
                     price_fitted + 
                     logsharej_subgroup_fitted+
                     logsharesubgroup_group_fitted, 
                     data)

summary(second_stage)
  stargazer(second_stage, type = "html", title = "Second Stage Regression", ci = T)

 
  ###############################################
  ### Elasticity Calculation: Prepatory Steps ###


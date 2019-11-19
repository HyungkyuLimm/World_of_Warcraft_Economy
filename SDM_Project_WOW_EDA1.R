#########################################
## World of Warcraft Auction House Data
## Initial Exploratory Data Analysis
##
#########################################

    

# Clearing lists and setting working directory
#rm(list=ls())
#setwd("/Users/hyungkyulim/Desktop/wow_auction_history")

# Loading libraries
library(DAAG)
library(lattice)
library(tidyverse) #Set of useful packages when working with dataframes in r 
library(ggfortify) #Clustering
library(GGally)
library(RColorBrewer) #Colors
library(wesanderson) #More colors
library(glmnet)
set.seed(123) #setting seed for k-means clustering
######################################
## Loading Data
######################################

itemPrices <- read_csv("~/Desktop/wow/daily_prices.csv") #14,888,271 rows 11 columns
itemMapper<- read_csv("~/Desktop/wow/itemID_name_mapper.csv") # 111,165 rows, 27 columns


nrow(itemMapper[itemMapper$quality == "0",])
nrow(itemMapper[itemMapper$quality == "1",])
nrow(itemMapper[itemMapper$quality == "2",])
nrow(itemMapper[itemMapper$quality == "3",])
nrow(itemMapper[itemMapper$quality == "4",])
nrow(itemMapper[itemMapper$quality == "5",])


# merging item info
By = c("item" = "id")
drop = c("name_dede","name_eses","name_frfr", "name_itit","name_ptbr","name_ruru","name_kokr","name_zhtw")
itemData <- inner_join(itemPrices, itemMapper, by = By)
itemData <- itemData %>% select(-drop)

# Avaiable factors after the merge Factor Levels
qualityLevels <- c("Junk", "Common", "Uncommon", "Rare", "Epic", "Legendary")
classLevels <- c("Consumable", "Gem", "Armor", "Projectile", "Trade Goods", "Enchantment", "Recipe", "Quest", "Key", "Miscellaneous", "Glyph")
tradeType <- c("Gem", "Cloth", "Leather", "OreMetals", "Cooking", "Herb", "Elemental", "Enchanting", "Ink")
consumableType <- c("Potion", "Elixer", "Flask", "Food", "FirstAid")
miscellaneous <- c("Junk", "Pet", "Mount", "Other")
subTypes <- c(tradeType, consumableType, miscellaneous)
itemData <- itemData %>% 
	mutate(rarity = case_when(
	quality == 0 ~ "Junk",
	quality == 1 ~ "Common",
	quality == 2 ~ "Uncommon",
	quality == 3 ~ "Rare",
	quality == 4 ~ "Epic",
	quality == 5 ~ "Legendary"
	)) %>%
	mutate(rarity = factor(rarity,
							levels = qualityLevels))

itemData <- itemData %>% 
	mutate(className = case_when(
	 class == 0 ~ "Consumable", class == 3 ~ "Gem", class == 4 ~ "Armor",
	 class == 6 ~ "Projectile", class == 7 ~ "Trade Goods", class == 8 ~ "Enchantment",
	 class == 9 ~ "Recipe", class == 12 ~ "Quest", class == 13 ~ "Key",
	 class == 15 ~ "Miscellaneous", class == 16 ~ "Glyph")) %>%
	 mutate(className = factor(className,
	 							levels = classLevels))
	 									

itemData <- itemData %>% 
	mutate(subType = case_when(
	class == 7 & subclass == 4 ~ "Gem", class == 7 & subclass == 5 ~ "Cloth", class == 7 & subclass == 6 ~ "Leather",
	class == 7 & subclass == 7 ~ "OreMetals", class == 7 & subclass == 8 ~ "Cooking", class == 7 & subclass == 9 ~ "Herb",
	class == 7 & subclass == 10 ~ "Elemental", class == 7 & subclass == 12 ~ "Enchanting", class == 7 & subclass == 16 ~ "Ink",
	class == 0 & subclass == 1 ~ "Potion", class == 0 & subclass == 2 ~ "Elixer", class == 0 & subclass == 3 ~ "Flasks",
	class == 0 & subclass == 5 ~ "Food", class == 0 & subclass == 7 ~ "FirstAid", class == 15 & subclass == 0 ~ "Junk",
	class == 15 & subclass == 2 ~ "Pet", class == 15 & subclass == 5 ~ "Mount",
	TRUE ~ "Other")) %>%
	 mutate(subType = factor(subType,
	 							levels = subTypes))

itemData <- itemData %>% 
	mutate(resource = case_when(
	subType ==  "Gem" | subType ==  "Cloth" | subType ==  "Leather" | subType ==  "OreMetals" ~ "Resource",
	TRUE ~ "Other")) %>%
	mutate(rarity = factor(resource,
							levels = c("Gem", "Cloth", "Leather", "OreMetals")))

#smaller dataset for easier computations
itemData_smol <- sample_n(itemData, 100000)


numericalVariables_smol <- itemData_smol %>% select(pricemin, priceavg, pricemax, pricestart, priceend, quantitymin, quantityavg, quantitymax, quality, level, class, subclass, buyfromvendor, selltovendor, type, requiredskill, stacksize, requiredlevel)

numericalVariables_big <- itemData %>% select(pricemin, priceavg, pricemax, pricestart, priceend, quantitymin, quantityavg, quantitymax, quality, level, class, subclass, buyfromvendor, selltovendor, type, requiredskill, stacksize, requiredlevel)

#############################################################################################################
#########################################	RUN ONE AT A TIME	#############################################
#############################################################################################################


######################################
## Clustering Methods
######################################

# Need to convert catigorical variables into binary, to use

## PCA ##
data <- numericalVariables_smol
princComp <- prcomp(data)
PCA <- autoplot(princComp, data = data, colour = 'quality') + 
	scale_color_gradientn(colours = rainbow(5)) +
	scale_y_log10()
PCA

## PC variance explained ##
varExplained <- princComp$sdev^2 / sum(princComp$sdev^2)
PCNames <- colnames(summary(princComp)$importance)
PCA_points <- barplot(100* varExplained, las=2, xlab='PC', ylab='% Variance Explained', names = PCNames)
	


# K Means clustering of PCA
data <- numericalVariables_smol
set.seed(123)
kMeans_PCA <- autoplot(kmeans(data, 6), data = data) + #k choosen based off number of levels in quality
	scale_y_log10()
kMeans_PCA

## k Means clustering ##
# NOT FINISHED YET, having issues with locale and the parser.

data <- itemData_smol
dates <- data %>% select(when)
data <- data %>% mutate( weekday = wday(y,dates, )) #### Issue with date time convering ######

kMeansObj <- kmeans(data, 6)

######################################
## Box Plots
######################################


## Rarity ##
data <- itemData_smol
qualityBox <- ggplot(data, aes( x = rarity, y = priceavg)) + 
	geom_boxplot() + 
	scale_y_log10()	
qualityBox 

## Class Name ##
data <- itemData_smol
classBox <- ggplot(data, aes( x = className, y = priceavg)) + 
	geom_boxplot() + 
	scale_y_log10()	
classBox

## subClass ##
data <- itemData_smol
subClassBox <- ggplot(data, aes( x = subType, y = priceavg)) + 
	geom_boxplot() + 
	scale_y_log10()
subClassBox


######################################
## Correlation Matricies 
######################################

# Need to refine these
redundantColumns <- c("class", "quantitymin", "quantitymax", "name_enus", "pricemin", "pricemax", "pricestart", "priceend", "auctionable")
data <- itemData_smol %>% select(-redundantColumns)
corMatrix <- ggcorr(data)
corMatrix 


pairs <- ggpairs(data) #warning this one takes a bit
pairs

######################################
## Point Plots
######################################


#quality/quantity
data <- itemData_smol
qualityQuantity <- ggplot(data, aes(x= rarity, y = quantityavg), combo =  "box_no_facet") + 
	geom_point()
qualityQuantity
  


# - Note that the plots group them by class via color, feel free to change it if you want.
#  	I just defaulted to color = class

#level/Price
data <- itemData_smol
levelPrice <- ggplot(data, aes(x= level, y = priceavg, color = rarity)) + 
	geom_point() +
	scale_y_log10() 
	#scale_color_gradientn(colours = rainbow(1))
levelPrice

#class/price
classPrice <- ggplot(data, aes(x= class, y = priceavg, color = className)) + 
	geom_point() +
	scale_y_log10() #+
	scale_color_gradientn(colours = rainbow(5))
classPrice

#type/price
typePrice <- ggplot(data, aes(x= type, y = priceavg, color = class)) + 
	geom_point() +
	scale_y_log10() +
	scale_color_gradientn(colours = rainbow(5))
typePrice



######################################
## Heat Maps
######################################

 ## Rarity ##
data <- itemData_smol %>% group_by(rarity, when) %>% 
						  summarize(rarityPriceAverage = mean(priceavg))
 
rarityHeatMap <- ggplot(data = data, mapping = aes(x = when, y = rarity, fill = log(rarityPriceAverage)) ) + 
		   geom_tile() +
		   xlab( label = "Date") +
		   scale_fill_gradient(name = "Log(Average Price)",
                      low = "#FFFFFF",
                      high = "#012345")
rarityHeatMap
## Class ## 
data <- itemData_smol %>% group_by(className, when) %>% 
						  summarize(classPriceAverage = mean(priceavg))
                     
classHeatMap <- ggplot(data = data, mapping = aes(x = when, y = className, fill = log(classPriceAverage)) ) + 
		   geom_tile() +
		   xlab( label = "Date") +
		   scale_fill_gradient(name = "Log(Average Price)",
                      low = "#FFFFFF",
                      high = "#012345")
classHeatMap
                      
## Level ##
data <- itemData_smol %>% group_by(level, when) %>% 
						  summarize(levelPriceAverage = mean(priceavg))
                     
levelHeatMap <- ggplot(data = data, mapping = aes(x = when, y = level, fill = log(levelPriceAverage)) ) + 
		   geom_tile() +
		   xlab( label = "Date") +
		   scale_fill_gradient(name = "Log(Average Price)",
                      low = "#FFFFFF",
                      high = "#012345")
levelHeatMap
                      
## Required Skills ##
data <- itemData_smol %>% group_by(subType, when) %>% 
						  summarize(subTypePriceAverage = mean(priceavg))
                     
skillHeatMap <- ggplot(data = data, mapping = aes(x = when, y = subType, fill = log(subTypePriceAverage)) ) + 
		   geom_tile() +
		   xlab( label = "Date") +
		   scale_fill_gradient(name = "Log(Average Price)",
                      low = "#FFFFFF",
                      high = "#012345")
skillHeatMap

######################################
## Timer Series
######################################

# Since plotting a time series by item would get too unruly, 
# (item is a categorical vairable with >100,000 levels) The following were 
# Segregated for better visualization. 

## Segregated by quality ##
data <- itemData_smol
data <- data %>% group_by(quality, when)
data <- data %>% summarize(qualityPriceAverage = mean(priceavg))
qualitySeries <- ggplot(data = data, aes(x= when, y = qualityPriceAverage)) +
	geom_line(aes(color = factor(quality))) +
	scale_y_log10() 
qualitySeries

# closer look at the previous plot:	
min <- as.Date("2018-09-30")
max <- as.Date("2018-10-8")
qualitySeries_closer <- ggplot(data = data, aes(x= when, y = qualityPriceAverage)) +
	geom_line(aes(color = factor(quality))) +
	scale_y_log10() +
	scale_x_date(limits = c(min, max))
qualitySeries_closer

## Segregated by requiredskill ##
# This one is a bit much
data <- itemData_smol
data <- data %>% group_by(requiredskill, when)
data <- data %>% summarize(skillPriceAverage = mean(priceavg))
skillSeries <- ggplot(data = data, aes(x= when, y = skillPriceAverage)) +
	geom_line(aes(color = factor(requiredskill))) +
	scale_y_log10() 
skillSeries

# But ut might be work examining the plot again without the log-scaled axis
skillSeries_noLog <- ggplot(data = data, aes(x= when, y = skillPriceAverage)) +
	geom_line(aes(color = factor(requiredskill)))
skillSeries_noLog

## Segregated by type ##
data <- itemData_smol
data <- data %>% group_by(type, when)
data <- data %>% summarize(typePriceAverage = mean(priceavg))
typeSeries <- ggplot(data = data, aes(x= when, y = typePriceAverage)) +
	geom_line(aes(color = factor(type))) +
	scale_y_log10() 
typeSeries

# closer look at the previous plot: some values are missing 
# because items of a certain type weren't sold that day	
# testing the range might be worth some merit
min <- as.Date("2018-09-30")
max <- as.Date("2018-10-8")
typeSeries_closer <- ggplot(data = data, aes(x= when, y = typePriceAverage)) +
	geom_line(aes(color = factor(type))) +
	scale_y_log10() +
	scale_x_date(limits = c(min, max))
typeSeries_closer

## Segregated by subclass ##
data <- itemData_smol
data <- data %>% group_by(subclass, when)
data <- data %>% summarize(subclassPriceAverage = mean(priceavg))
subclassSeries <- ggplot(data = data, aes(x= when, y = subclassPriceAverage)) +
	geom_line(aes(color = factor(subclass))) +
	scale_y_log10() 
subclassSeries

# closer look at the previous plot:	
min <- as.Date("2018-09-30")
max <- as.Date("2018-10-8")
subclassSeries_closer <- ggplot(data = data, aes(x= when, y = subclassPriceAverage)) +
	geom_line(aes(color = factor(subclass))) +
	scale_y_log10() +
	scale_x_date(limits = c(min, max))
subclassSeries_closer




## Consumables ##
data <- itemData_smol
data <- data %>% filter(subType ==  "Elixer" | subType ==  "Flask" | subType ==  "Potion" | subType ==  "Food")
data <- data %>% group_by(subType, when)
data <- data %>% summarize(subTypePriceAverage = mean(priceavg))
subTypeSeries <- ggplot(data = data, aes(x= when, y = subTypePriceAverage)) +
	geom_line(aes(color = subType)) +
	scale_y_log10() 
subTypeSeries

# closer look at the previous plot:	
min <- as.Date("2018-09-30")
max <- as.Date("2018-10-8")
subTypeSeries_closer <- ggplot(data = data, aes(x= when, y = subTypePriceAverage)) +
	geom_line(aes(color = subType)) +
	scale_y_log10() +
	scale_x_date(limits = c(min, max))
subTypeSeries_closer

## Trade Goods/ Resources ##
data <- itemData_smol
data <- data %>% filter(subType ==  "Gem" | subType ==  "Cloth" | subType ==  "Leather" | subType ==  "OreMetals")
data <- data %>% group_by(subType, when)
data <- data %>% summarize(tradeGoodsPriceAverage = mean(priceavg))
tradeGoodsSeries <- ggplot(data = data, aes(x= when, y = tradeGoodsPriceAverage)) +
	geom_line(aes(color = subType)) +
	scale_y_log10() 
tradeGoodsSeries

# closer look at the previous plot:	
min <- as.Date("2018-09-30")
max <- as.Date("2018-10-8")
tradeGoodsSeries_closer <- ggplot(data = data, aes(x= when, y = tradeGoodsPriceAverage)) +
	geom_line(aes(color = subType)) +
	scale_y_log10() +
	scale_x_date(limits = c(min, max))
tradeGoodsSeries_closer


# Ridge

itemData_useful<-itemData %>%
  select(rarity,className,subType,level,buyfromvendor,selltovendor,resource,requiredlevel,house,priceavg)

apply(itemData_useful, 10 , as.numeric) 

X <- as.matrix(itemData_useful[,1:9])
Y <- itemData_useful[,10]


  
# Fit a Ridge Regression Model
ridge.mod = glmnet(X, Y, alpha=0)
names(ridge.mod)
coef(ridge.mod)
dim(coef(ridge.mod))

ridge.mod$lambda[10]
coef(ridge.mod)[,10]
l2_norm <- sqrt(sum(coef(ridge.mod)[2:9,10]^2))
l2_norm



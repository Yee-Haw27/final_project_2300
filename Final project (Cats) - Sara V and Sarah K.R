
dog <-read.csv("C:/Users/Sarah/AppData/Local/Temp/8ea47487-e04c-4513-a046-e887456ffdda_archive.zip.dda/dog.csv")

library(ggplot2)
library(dplyr)
library(GGally)

dim(dog)
head(dog)
help(dog)
summary(dog)
str(dog)


dog %>%
  summarise(
    mean_age = mean(Age..Years., na.rm = TRUE),
    median_age = median(Age..Years., na.rm = TRUE)
  )

# cleaning dataset

dog2 <- na.omit(dog)


boxplot(dog2$Age..Years., main = "Boxplot of Age", ylab = "Age")


# GOOGLED FACTOR TO FIX ERROR
dog2$Breed <- factor(dog2$Breed)
dog2$Color <- factor(dog2$Color)
dog2$Gender <- factor(dog2$Gender)

# Preparing data
dog2$Age..Year. <- as.numeric(dog2$Age..Years.)
dog2$Weight <- as.numeric(dog2$Weight..kg.)

# model
pred_weight <- lm(Weight..kg. ~ Age..Year., data = dog2)
summary(pred_weight)

par(mfrow = c(2, 2))
plot(pred_weight)

# Predicts weight via age
update_dog <- data.frame(Age..Year. = 18)
predicted_weight <- predict(pred_weight, update_dog)
print(predicted_weight)





# CLUSTER ANALYSIS


dog <-read.csv("C:/Users/Sarah/OneDrive/Documents/dog.csv")


# Remove Breed column
dog$Breed <- NULL
ggpairs(dog)



# (b)
set.seed(194)
mall.std <- data.frame(scale(mall))

# (c)

totwss <- c()
for (k in 1:10) {
  kmeans_result <- kmeans(mall.std, centers = k, nstart = 25)
  totwss <- c(totwss, kmeans_result$tot.withinss)
}

k_values <- 1:10
table <- data.frame(K = k_values, Total_within_cluster_sum_of_squares = totwss)
print(table)



# (d)
plot(1:10, wss, type = "b", xlab = "Number of Clusters (K)", ylab = "Total Within-Cluster Sum of Squares")


# (e)
set.seed(921)
kmeans_5 <- kmeans(mall.std, centers = 5, nstart = 20)



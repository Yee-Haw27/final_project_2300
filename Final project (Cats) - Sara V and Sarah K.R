
cat <-read.csv("C:/Users/Sarah/OneDrive/Documents/cat.csv")


library(dplyr)

dim(cat)
head(cat)
help(cat)
summary(cat)
str(cat)

summary(cat$Age..Years.)
summary(cat$Weight..kg.)

cat %>%
  summarise(
    mean_age = mean(Age..Years., na.rm = TRUE),
    median_age = median(Age..Years., na.rm = TRUE)
  )

# cleaning dataset

cat <- na.omit(cat)
cat <- cat %>% filter(!is.na(Breed), !is.na(Color), !is.na(Gender))

boxplot(cat$Age..Years., main = "Boxplot of Age", ylab = "Age")


# GOOGLED FACTOR TO FIX ERROR
cat$Breed <- factor(cat$Breed)
cat$Color <- factor(cat$Color)
cat$Gender <- factor(cat$Gender)

# Preparing data
cat$Age..Year. <- as.numeric(cat$Age..Years.)
cat$Weight <- as.numeric(cat$Weight..kg.)

# model
pred_weight <- lm(Weight..kg. ~ Age..Year., data = cat)
summary(pred_weight)

par(mfrow = c(2, 2))
plot(pred_weight)

# Predicts weight by year (not great model)
update_cat <- data.frame(Age..Year. = 18)
predicted_weight <- predict(pred_weight, update_cat)
print(predicted_weight)


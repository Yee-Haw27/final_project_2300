
cat <-read.csv("C:/Users/Sarah/Downloads/cats.csv")


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

# model
pred <- lm(as.numeric(Breed) ~ Age..Years. + Weight..kg. + Color + Gender, data = cat)
summary(pred)


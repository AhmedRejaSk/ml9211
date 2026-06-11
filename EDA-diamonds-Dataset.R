# ============================================
# COMPLETE EDA OF DIAMONDS DATASET IN R
# ============================================

# Install packages (run once if needed)
install.packages(c("ggplot2", "dplyr", "GGally",
                   "corrplot", "gridExtra"))

# Load libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)
library(gridExtra)

# ============================================
# 1. LOAD DATA
# ============================================

data("diamonds")

# View first rows
head(diamonds)

# Structure of dataset
str(diamonds)

# Dimensions
dim(diamonds)

# Column names
names(diamonds)

# Summary statistics
summary(diamonds)

# ============================================
# 2. CHECK MISSING VALUES
# ============================================

colSums(is.na(diamonds))

# ============================================
# 3. CHECK DUPLICATES
# ============================================

sum(duplicated(diamonds))

# ============================================
# 4. DATA TYPES
# ============================================

sapply(diamonds, class)

# ============================================
# 5. DESCRIPTIVE STATISTICS
# ============================================

# Numeric variables only
numeric_data <- diamonds %>%
  select(where(is.numeric))

# Mean
sapply(numeric_data, mean)

# Median
sapply(numeric_data, median)

# Standard deviation
sapply(numeric_data, sd)

# Variance
sapply(numeric_data, var)

# ============================================
# 6. HISTOGRAMS
# ============================================

par(mfrow = c(2, 3))

hist(diamonds$price,
     main = "Price",
     xlab = "Price",
     col = "skyblue")

hist(diamonds$carat,
     main = "Carat",
     xlab = "Carat",
     col = "lightgreen")

hist(diamonds$depth,
     main = "Depth",
     xlab = "Depth",
     col = "orange")

hist(diamonds$table,
     main = "Table",
     xlab = "Table",
     col = "pink")

hist(diamonds$x,
     main = "Length (x)",
     xlab = "x",
     col = "violet")

hist(diamonds$y,
     main = "Width (y)",
     xlab = "y",
     col = "gold")

par(mfrow = c(1,1))

# ============================================
# 7. BOXPLOTS
# ============================================

par(mfrow = c(2,3))

boxplot(diamonds$price,
        main = "Price",
        col = "skyblue")

boxplot(diamonds$carat,
        main = "Carat",
        col = "lightgreen")

boxplot(diamonds$depth,
        main = "Depth",
        col = "orange")

boxplot(diamonds$table,
        main = "Table",
        col = "pink")

boxplot(diamonds$x,
        main = "x",
        col = "violet")

boxplot(diamonds$y,
        main = "y",
        col = "gold")

par(mfrow = c(1,1))

# ============================================
# 8. CATEGORICAL VARIABLES
# ============================================

# Cut
table(diamonds$cut)

ggplot(diamonds, aes(x = cut)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Diamond Cut Distribution")

# Color
table(diamonds$color)

ggplot(diamonds, aes(x = color)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Diamond Color Distribution")

# Clarity
table(diamonds$clarity)

ggplot(diamonds, aes(x = clarity)) +
  geom_bar(fill = "purple") +
  labs(title = "Diamond Clarity Distribution")

# ============================================
# 9. CORRELATION ANALYSIS
# ============================================

# Correlation matrix
cor_matrix <- cor(numeric_data)

# Display correlation matrix
round(cor_matrix, 2)

# Correlation plot
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.8)

# ============================================
# 10. PAIR PLOTS
# ============================================

ggpairs(diamonds[, c("price", "carat",
                     "depth", "table", "x")])

# ============================================
# 11. PRICE ANALYSIS
# ============================================

# Price distribution
ggplot(diamonds, aes(x = price)) +
  geom_histogram(fill = "steelblue",
                 bins = 40) +
  labs(title = "Distribution of Price")

# Price vs Carat
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_smooth(method = "lm",
              color = "red") +
  labs(title = "Price vs Carat")

# ============================================
# 12. BOXPLOTS OF PRICE BY CATEGORY
# ============================================

# Price by Cut
ggplot(diamonds, aes(x = cut,
                     y = price,
                     fill = cut)) +
  geom_boxplot() +
  labs(title = "Price by Cut")

# Price by Color
ggplot(diamonds, aes(x = color,
                     y = price,
                     fill = color)) +
  geom_boxplot() +
  labs(title = "Price by Color")

# Price by Clarity
ggplot(diamonds, aes(x = clarity,
                     y = price,
                     fill = clarity)) +
  geom_boxplot() +
  labs(title = "Price by Clarity")

# ============================================
# 13. OUTLIER DETECTION
# ============================================

detect_outliers <- function(x){
  
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  which(x < lower | x > upper)
}

# Outliers in price
outliers_price <- detect_outliers(diamonds$price)

length(outliers_price)

# ============================================
# 14. NORMALITY CHECK
# ============================================

# Q-Q plot
qqnorm(diamonds$price)
qqline(diamonds$price,
       col = "red")

# Shapiro-Wilk Test
# Use subset because dataset is large
set.seed(123)

sample_price <- sample(diamonds$price, 5000)

shapiro.test(sample_price)

# ============================================
# 15. FEATURE SCALING
# ============================================

scaled_data <- as.data.frame(scale(numeric_data))

head(scaled_data)

# ============================================
# 16. CREATE SUBSETS BY CUT
# ============================================

diamond_subsets <- split(diamonds,
                         diamonds$cut)

# Example subsets
fair_data      <- diamond_subsets$Fair
good_data      <- diamond_subsets$Good
ideal_data     <- diamond_subsets$Ideal

# ============================================
# 17. TOP CORRELATIONS WITH PRICE
# ============================================

sort(cor(numeric_data$price,
         numeric_data))[ , drop = FALSE]

# ============================================
# 18. SAVE DATA
# ============================================

write.csv(diamonds,
          "diamonds_EDA.csv",
          row.names = FALSE)

# ============================================
# END OF EDA
# ============================================


# View first rows
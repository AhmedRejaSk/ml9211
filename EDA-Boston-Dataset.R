# ============================================
# COMPLETE EDA OF BOSTON HOUSING DATASET IN R
# ============================================

# Install required packages (run once if needed)
install.packages(c("MASS", "ggplot2", "dplyr", "corrplot",
                   "GGally", "caret", "gridExtra"))

# Load libraries
library(MASS)
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(caret)
library(gridExtra)

# ============================================
# 1. LOAD DATA
# ============================================

data("Boston")

# View first rows
head(Boston)

# Structure of dataset
str(Boston)

# Dimensions
dim(Boston)

# Column names
names(Boston)

# Summary statistics
summary(Boston)

# ============================================
# 2. CHECK MISSING VALUES
# ============================================

colSums(is.na(Boston))

# ============================================
# 3. CHECK DUPLICATES
# ============================================

sum(duplicated(Boston))

# ============================================
# 4. DESCRIPTIVE STATISTICS
# ============================================

# Mean
sapply(Boston, mean)

# Median
sapply(Boston, median)

# Standard deviation
sapply(Boston, sd)

# Variance
sapply(Boston, var)

# ============================================
# 5. DISTRIBUTION OF VARIABLES
# ============================================
# Plot histogram of 4 variables in one figure
# 2 rows and 2 columns

# Set plotting area: 2 rows × 2 columns
par(mfrow = c(2, 2))

# Histogram 1
hist(Boston$crim,
     main = "Histogram of CRIM",
     xlab = "CRIM",
     col = "skyblue",
     border = "white")

# Histogram 2
hist(Boston$rm,
     main = "Histogram of RM",
     xlab = "RM",
     col = "lightgreen",
     border = "white")

# Histogram 3
hist(Boston$lstat,
     main = "Histogram of LSTAT",
     xlab = "LSTAT",
     col = "orange",
     border = "white")

# Histogram 4
hist(Boston$medv,
     main = "Histogram of MEDV",
     xlab = "MEDV",
     col = "pink",
     border = "white")

# Reset plotting area
par(mfrow = c(1,1))

#=============================================
# Histogram of all variables
par(mfrow = c(4,4))

for(i in names(Boston)){
  hist(Boston[[i]],
       main = paste("Histogram of", i),
       xlab = i,
       col = "skyblue",
       border = "white")
}
par(mfrow = c(1,1))

# ============================================
# 6. BOXPLOTS
# ============================================

par(mfrow = c(4,4))

for(i in names(Boston)){
  boxplot(Boston[[i]],
          main = paste("Boxplot of", i),
          col = "lightgreen")
}

par(mfrow = c(1,1))

# ============================================
# 7. CORRELATION ANALYSIS
# ============================================

# Correlation matrix
cor_matrix <- cor(Boston)

# Display correlation matrix
round(cor_matrix, 2)

# Correlation plot
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.7)

# ============================================
# 8. PAIRWISE SCATTER PLOTS
# ============================================

# Scatterplot matrix
ggpairs(Boston[, c("medv", "rm", "lstat", "ptratio", "nox")])

# ============================================
# 9. TARGET VARIABLE ANALYSIS
# ============================================

# Distribution of MEDV
ggplot(Boston, aes(x = medv)) +
  geom_histogram(fill = "steelblue",
                 color = "white",
                 bins = 30) +
  labs(title = "Distribution of MEDV",
       x = "Median House Value",
       y = "Frequency")

# Boxplot of MEDV
ggplot(Boston, aes(y = medv)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of MEDV")

# ============================================
# 10. SCATTER PLOTS WITH TARGET VARIABLE
# ============================================

# RM vs MEDV
p1 <- ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "RM vs MEDV")

# LSTAT vs MEDV
p2 <- ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "LSTAT vs MEDV")

# PTRATIO vs MEDV
p3 <- ggplot(Boston, aes(x = ptratio, y = medv)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "PTRATIO vs MEDV")

# NOX vs MEDV
p4 <- ggplot(Boston, aes(x = nox, y = medv)) +
  geom_point(color = "brown") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "NOX vs MEDV")

grid.arrange(p1, p2, p3, p4, ncol = 2)

# ============================================
# 11. OUTLIER DETECTION
# ============================================

# Detect outliers using IQR method
detect_outliers <- function(x){
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  which(x < lower | x > upper)
}

# Example for CRIM
outliers_crim <- detect_outliers(Boston$crim)

length(outliers_crim)

# ============================================
# 12. NORMALITY CHECK
# ============================================

# Q-Q plot for MEDV
qqnorm(Boston$medv)
qqline(Boston$medv, col = "red")

# Shapiro-Wilk test
shapiro.test(Boston$medv)

# ============================================
# 13. FEATURE SCALING
# ============================================

# Standardization
Boston_scaled <- as.data.frame(scale(Boston))

head(Boston_scaled)

# ============================================
# 14. TRAIN-TEST SPLIT
# ============================================

set.seed(123)

train_index <- createDataPartition(Boston$medv,
                                   p = 0.8,
                                   list = FALSE)

train_data <- Boston[train_index, ]
test_data  <- Boston[-train_index, ]

dim(train_data)
dim(test_data)

# ============================================
# 15. BASIC INSIGHTS
# ============================================

# Top correlations with MEDV
sort(cor(Boston$medv, Boston))[ , drop = FALSE]

# ============================================
# 16. SAVE CLEANED DATA
# ============================================

write.csv(Boston, "Boston_EDA_Data.csv",
          row.names = FALSE)

# ============================================
# END OF EDA
# ============================================
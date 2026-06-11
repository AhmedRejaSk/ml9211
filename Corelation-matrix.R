#install.packages("corrplot")
library(corrplot)
data(mtcars)
head(mtcars)
# Compute the correlation matrix
cor_matrix <- cor(mtcars)

# Round the values for cleaner display (optional)
cor_matrix <- round(cor_matrix, 2)
corrplot(cor_matrix, method = "color")

#Change the colour of the matrix
corrplot(cor_matrix,
         method = "color",
         col = colorRampPalette(c("blue", "yellow", "green"))(200))

# Display the Correlation value
corrplot(cor_matrix, method = "number")


# A common advanced visualization:
corrplot(cor_matrix,
         method = "color",           # Use colors
         type = "upper",             # Display only the upper triangle
         order = "hclust",           # Reorder variables based on hierarchical clustering
         addCoef.col = "black",      # Add correlation coefficients as text
         tl.col = "red",             # Color of text labels
         tl.srt = 45,                # Text label rotation (diagonal)
         sig.level = 0.05,           # Add significance level (p-value) if available (requires Hmisc package)
         insig = "blank")            # Leave blank insignificant correlations


#Heatmap

#heatmap(cor(mtcars),
        col = colorRampPalette(c("yellow", "orange", "red"))(100),
        symm = TRUE)







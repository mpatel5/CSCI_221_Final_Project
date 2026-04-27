# Part 3: Association Rule Mining - Online Store Dataset

# Install packages if needed
# install.packages("readr")
# install.packages("dplyr")
# install.packages("arules")
# install.packages("arulesViz")

# Load libraries
library(readr)
library(dplyr)
library(arules)
library(arulesViz)

# Load dataset
df <- read_csv("online_store.csv")

# Preview the dataset
head(df)
str(df)
summary(df)

# Display histograms for numeric variables as exploratory data analysis only.
# These plots help preview Quantity and Price distributions, but the numeric
# columns are not used in the final transaction baskets.
numeric_cols_preview <- sapply(df, is.numeric)

if (any(numeric_cols_preview)) {
  df_numeric_preview <- df[, numeric_cols_preview]
  
  par(mfrow = c(2, 2))
  
  for (col in names(df_numeric_preview)) {
    hist(
      df_numeric_preview[[col]],
      main = paste("Histogram of", col),
      xlab = col,
      col = "lightblue",
      border = "black"
    )
  }
  
  par(mfrow = c(1, 1))
}

# Clean and prepare the dataset

# Check for missing values
cat("Missing Values:\n")
print(colSums(is.na(df)))

# Check for duplicate rows
cat("\nDuplicate Rows:", sum(duplicated(df)), "\n")

# Remove duplicate rows to avoid counting the same transaction multiple times.
# Keeping duplicates would inflate support values and distort association rules.
df <- distinct(df)

# Remove rows with missing values instead of imputing them
# because Customer ID and Description are key transaction fields.
# Imputing these values could create inaccurate customer or product relationships.
df <- na.omit(df)

# Check and remove numeric outliers using the Z-score method
numeric_cols <- sapply(df, is.numeric)

if (any(numeric_cols)) {
  
  df_numeric <- df[, numeric_cols]
  
  z_scores <- scale(df_numeric)
  
  outlier_mask <- apply(abs(z_scores) > 3, 1, any)
  
  cat("\nOriginal shape:", dim(df), "\n")
  cat("Number of outlier rows:", sum(outlier_mask), "\n")
  
  df <- df[!outlier_mask, ]
  
  cat("After outlier removal:", dim(df), "\n")
}

# Convert the cleaned dataframe into transactions
# Build transaction baskets grouped by Invoice
# Each invoice is one transaction; items are the product Descriptions purchased
basket <- df %>%
  group_by(Invoice) %>%
  summarise(items = list(unique(Description)), .groups = "drop")

transactions <- as(basket$items, "transactions")

# View transaction summary
summary(transactions)

# Visualize the most frequent items/categories
itemFrequencyPlot(
  transactions,
  topN = 20,
  type = "absolute",
  main = "Top 20 Most Frequent Items"
)

# Apply the Apriori algorithm with support and confidence thresholds
rules <- apriori(
  transactions,
  parameter = list(
    support = 0.01,
    confidence = 0.50,
    minlen = 2
  )
)

# Apply the lift threshold
# Lift greater than 1.20 means the relationship is stronger than random chance.
rules_lift <- subset(rules, lift > 1.20)

# Generate and display association rules
rules_sorted <- sort(rules_lift, by = "lift", decreasing = TRUE)

cat("\nTop Association Rules Sorted by Lift:\n")
inspect(head(rules_sorted, 15))

cat("\nNumber of rules generated:", length(rules), "\n")
cat("Number of rules after lift filter:", length(rules_lift), "\n")

# Generate a network visualization of the association rules
plot(
  head(rules_sorted, 20),
  method = "graph",
  engine = "htmlwidget"
)

# Generate a grouped visualization of the association rules
plot(
  head(rules_sorted, 20),
  method = "grouped"
)

# Convert rules to a dataframe for easier viewing
rules_df <- as(rules_sorted, "data.frame")

# Display top rules in dataframe form
head(rules_df, 15)

# Interpret at least five meaningful rules
cat("\nInterpretation:\n")
cat("Association rule mining was used to identify relationships between products purchased together in the online store dataset.\n")
cat("Transactions were grouped by Invoice, meaning each basket represents the set of products purchased in a single transaction.\n")
cat("The Apriori algorithm was applied using support = 0.01, confidence = 0.50, and lift > 1.20.\n")
cat("Support measures how frequently a rule appears in the dataset.\n")
cat("Confidence measures how often the consequent occurs when the antecedent occurs.\n")
cat("Lift measures whether the relationship is stronger than random chance.\n")

cat("\nFive Meaningful Rule Interpretations:\n")

cat("Rule 1: {POPPY'S PLAYHOUSE BEDROOM} → {POPPY'S PLAYHOUSE KITCHEN} has support 0.0107, confidence 0.8462, and lift 60.66. This indicates that customers who purchase the bedroom set are very likely to also purchase the matching kitchen set, showing a strong bundle relationship.\n")

cat("Rule 2: {SET/6 RED SPOTTY PAPER PLATES} → {SET/6 RED SPOTTY PAPER CUPS} has confidence 0.7055 and lift 50.17. This suggests that customers buying party plates often also buy matching cups, indicating a clear cross-selling opportunity.\n")

cat("Rule 3: {WOODEN STAR CHRISTMAS SCANDINAVIAN} → {WOODEN HEART CHRISTMAS SCANDINAVIAN} has lift 42.90. This shows that customers tend to purchase coordinated decorative items together, especially seasonal products.\n")

cat("Rule 4: {FELTCRAFT CUSHION BUTTERFLY} → {FELTCRAFT CUSHION RABBIT} has confidence 0.6535 and lift 42.46. This indicates that customers buying one cushion design are likely to buy another similar cushion, suggesting product style preferences.\n")

cat("Rule 5: {GREEN REGENCY TEACUP AND SAUCER} → {PINK REGENCY TEACUP AND SAUCER} has confidence 0.6498 and lift 36.37. This suggests that customers often purchase multiple variations of similar products, indicating bundle or collection purchasing behavior.\n")

cat("\nConclusion:\n")
cat("These results demonstrate how market basket analysis can uncover strong product relationships that can be leveraged for recommendations, bundling strategies, and increasing sales.\n")

cat("High-lift rules, such as POPPY'S PLAYHOUSE BEDROOM → POPPY'S PLAYHOUSE KITCHEN, are especially useful for identifying strong product bundles because customers who buy one item are much more likely to buy the matching item.\n")

cat("However, rules with slightly lower lift but higher support can also be valuable because they represent products purchased frequently across many transactions. For example, common staple items or frequently purchased products may be better for store layout decisions, aisle placement, homepage placement, or promotional displays.\n")

cat("Therefore, lift is useful for finding strong relationships, while support is useful for identifying product combinations that affect a larger number of customers.\n")
# ============================================================
# Association Rule Learning - Groceries Dataset
# Assignment: Unsupervised Learning Method
# ============================================================

# ---- 0. Setup ----
# install.packages(c("arules", "arulesViz", "RColorBrewer"))  # run once if needed
library(arules)
library(arulesViz)
library(RColorBrewer)

# ---- 1. Load the data ----
# Option A: use the built-in Groceries dataset from the 'arules' package
#data("Groceries")
#transactions <- Groceries

# Option B: load from your own groceries.csv (basket-format, no header,
# variable number of items per row). Uncomment to use your file instead:
transactions <- read.transactions("groceries.csv", format = "basket",
                                  sep = ",", rm.duplicates = TRUE)

summary(transactions)


# ============================================================
# Question 1: Understand the Data
# ============================================================
n_transactions <- length(transactions)
n_items <- ncol(transactions)

cat("Number of transactions:", n_transactions, "\n")
cat("Number of distinct items:", n_items, "\n")

# 10 most frequent items
itemFrequencyPlot(transactions, topN = 10, type = "relative",
                   main = "Top 10 Most Frequent Items",
                   col = brewer.pal(8, "Pastel2"))

top10 <- sort(itemFrequency(transactions, type = "relative"), decreasing = TRUE)[1:10]
print(top10)

# ============================================================
# Question 2: Frequent Itemsets
# ============================================================
itemsets <- apriori(transactions, parameter = list(
  target = "frequent itemsets",
  support = 0.01,
  minlen = 2
))

itemsets_sorted <- sort(itemsets, by = "support", decreasing = TRUE)
inspect(head(itemsets_sorted, 10))          # top 10 frequent itemsets
cat("\nHighest-support itemset:\n")
inspect(itemsets_sorted[1])

# ============================================================
# Question 3: Association Rules
# ============================================================
rules <- apriori(transactions, parameter = list(
  support = 0.01,
  confidence = 0.3,
  minlen = 2
))

cat("Number of rules generated:", length(rules), "\n")
rules_by_lift <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_by_lift, 10))

# Export full rule table (antecedent, consequent, support, confidence, lift)
rules_df <- as(rules, "data.frame")
write.csv(rules_df, "association_rules_output.csv", row.names = FALSE)
head(rules_df, 10)

# ============================================================
# Question 5: Find Interesting Rules
# ============================================================
top_confidence <- sort(rules, by = "confidence", decreasing = TRUE)[1]
top_lift       <- sort(rules, by = "lift",       decreasing = TRUE)[1]
top_support    <- sort(rules, by = "support",    decreasing = TRUE)[1]

cat("\nHighest confidence rule:\n"); inspect(top_confidence)
cat("\nHighest lift rule:\n");       inspect(top_lift)
cat("\nHighest support rule:\n");    inspect(top_support)

# ============================================================
# Visualization (for the report / Question 3 figures)
# ============================================================
plot(rules, measure = c("support", "confidence"), shading = "lift",
     main = "Association Rules: Support vs Confidence (shaded by Lift)")

plot(head(rules_by_lift, 10), method = "graph",
     main = "Top 10 Rules by Lift - Network Graph")

# ============================================================
# End of script
# ============================================================

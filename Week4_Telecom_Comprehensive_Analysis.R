# ============================================================
# YuvaIntern - Week 4
# Comprehensive Data Analysis, Reporting and Presentation
# Telecom Customer Churn Analysis
# ============================================================

# 1. Load required packages
library(dplyr)
library(ggplot2)
library(car)

# 2. Set working directory
setwd("C:/Users/USER/OneDrive/Desktop/YuvaIntern_Week4_Telecom_Comprehensive_Analysis")

# 3. Load dataset
data <- read.csv(
  "telecom_customer_churn.csv",
  stringsAsFactors = FALSE
)

# 4. Check dataset dimensions
dim(data)

# 5. Check dataset structure
str(data)
# ============================================================
# 6. Missing Value Check
# ============================================================

missing_values <- colSums(is.na(data))

missing_values
# ============================================================
# 7. Blank Value Check
# ============================================================

blank_values <- sapply(data, function(x) {
  sum(trimws(as.character(x)) == "", na.rm = TRUE)
})

blank_values
# ============================================================
# 8. Missing and Blank Value Treatment
# ============================================================

# Convert blank character values to NA
data[] <- lapply(data, function(x) {
  if (is.character(x)) {
    x <- trimws(x)
    x[x == ""] <- NA
  }
  x
})

# Customers without phone service
data$Multiple.Lines[
  data$Phone.Service == "No" &
    is.na(data$Multiple.Lines)
] <- "No Phone Service"

data$Avg.Monthly.Long.Distance.Charges[
  data$Phone.Service == "No" &
    is.na(data$Avg.Monthly.Long.Distance.Charges)
] <- 0

# Customers without internet service
internet_columns <- c(
  "Internet.Type",
  "Online.Security",
  "Online.Backup",
  "Device.Protection.Plan",
  "Premium.Tech.Support",
  "Streaming.TV",
  "Streaming.Movies",
  "Streaming.Music",
  "Unlimited.Data"
)

for (col in internet_columns) {
  data[[col]][
    data$Internet.Service == "No" &
      is.na(data[[col]])
  ] <- "No Internet Service"
}

data$Avg.Monthly.GB.Download[
  data$Internet.Service == "No" &
    is.na(data$Avg.Monthly.GB.Download)
] <- 0

# Churn information is not applicable to non-churned customers
data$Churn.Category[
  data$Customer.Status != "Churned" &
    is.na(data$Churn.Category)
] <- "Not Applicable"

data$Churn.Reason[
  data$Customer.Status != "Churned" &
    is.na(data$Churn.Reason)
] <- "Not Applicable"

# Replace any remaining numeric missing values with median
numeric_columns <- sapply(data, is.numeric)

data[numeric_columns] <- lapply(
  data[numeric_columns],
  function(x) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
    x
  }
)

# Check remaining missing values
colSums(is.na(data))
# ============================================================
# 9. Duplicate Check
# ============================================================

duplicate_count <- sum(duplicated(data))

duplicate_count
# ============================================================
# 10. Outlier Detection using IQR
# ============================================================

numeric_data <- data[sapply(data, is.numeric)]

outlier_summary <- sapply(numeric_data, function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  
  sum(x < lower | x > upper, na.rm = TRUE)
})

outlier_summary
# ============================================================
# 11. Invalid Monthly Charge Check
# ============================================================

negative_monthly_charge <- sum(data$Monthly.Charge < 0)

negative_monthly_charge
# ============================================================
# 12. Correct Invalid Monthly Charges
# ============================================================

# Replace negative Monthly Charge values with NA
data$Monthly.Charge[data$Monthly.Charge < 0] <- NA

# Replace missing Monthly Charge values with median
data$Monthly.Charge[is.na(data$Monthly.Charge)] <-
  median(data$Monthly.Charge, na.rm = TRUE)

# Verify correction
sum(data$Monthly.Charge < 0)
# ============================================================
# 13. Data Normalization
# ============================================================

normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data$Age_Normalized <- normalize(data$Age)

data$Monthly_Charge_Normalized <-
  normalize(data$Monthly.Charge)

data$Total_Revenue_Normalized <-
  normalize(data$Total.Revenue)

# Check normalized variables
summary(
  data[, c(
    "Age_Normalized",
    "Monthly_Charge_Normalized",
    "Total_Revenue_Normalized"
  )]
)
# ============================================================
# 14. Categorical Encoding
# ============================================================

data$Gender_Encoded <-
  ifelse(data$Gender == "Male", 1, 0)

data$Married_Encoded <-
  ifelse(data$Married == "Yes", 1, 0)

data$Phone_Service_Encoded <-
  ifelse(data$Phone.Service == "Yes", 1, 0)

data$Internet_Service_Encoded <-
  ifelse(data$Internet.Service == "Yes", 1, 0)

data$Paperless_Billing_Encoded <-
  ifelse(data$Paperless.Billing == "Yes", 1, 0)

# Check encoded variables
head(
  data[, c(
    "Gender",
    "Gender_Encoded",
    "Married",
    "Married_Encoded",
    "Phone.Service",
    "Phone_Service_Encoded",
    "Internet.Service",
    "Internet_Service_Encoded",
    "Paperless.Billing",
    "Paperless_Billing_Encoded"
  )]
)
# ============================================================
# 15. Customer Status Analysis
# ============================================================

customer_status <- table(data$Customer.Status)

customer_status

customer_status_percentage <-
  round(prop.table(customer_status) * 100, 2)

customer_status_percentage
# ============================================================

# ============================================================
# 17. Contract Type vs Customer Status
# ============================================================

contract_status <- table(
  data$Contract,
  data$Customer.Status
)

contract_status

contract_status_percentage <- round(
  prop.table(contract_status, margin = 1) * 100,
  2
)

contract_status_percentage
# ============================================================
# 18. Correlation Analysis
# ============================================================

correlation_data <- data[, c(
  "Age",
  "Number.of.Dependents",
  "Number.of.Referrals",
  "Tenure.in.Months",
  "Avg.Monthly.GB.Download",
  "Monthly.Charge",
  "Total.Charges",
  "Total.Revenue"
)]

correlation_matrix <- round(
  cor(correlation_data, use = "complete.obs"),
  2
)

correlation_matrix
# ============================================================
# 19. Hypothesis Testing
# ============================================================

model_data <- data[
  data$Customer.Status %in% c("Churned", "Stayed"),
]

model_data$Customer.Status <- factor(
  model_data$Customer.Status,
  levels = c("Stayed", "Churned")
)

# Monthly Charge t-test
monthly_charge_test <- t.test(
  Monthly.Charge ~ Customer.Status,
  data = model_data
)

monthly_charge_test

# Tenure t-test
tenure_test <- t.test(
  Tenure.in.Months ~ Customer.Status,
  data = model_data
)

tenure_test
# ============================================================
# 20. Normality Test
# ============================================================

set.seed(123)

monthly_charge_sample <- sample(
  model_data$Monthly.Charge,
  size = min(5000, nrow(model_data))
)

normality_test <- shapiro.test(
  monthly_charge_sample
)

normality_test
# ============================================================
# 21. Logistic Regression Predictive Model
# ============================================================

# Create binary churn outcome
model_data$Churn <- ifelse(
  model_data$Customer.Status == "Churned",
  1,
  0
)

# Check churn distribution
table(model_data$Churn)

# Split data into training and testing sets
set.seed(123)

train_index <- sample(
  1:nrow(model_data),
  size = 0.80 * nrow(model_data)
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

dim(train_data)
dim(test_data)
# ============================================================
# 22. Build Logistic Regression Model
# ============================================================

logistic_model <- glm(
  Churn ~ Age +
    Number.of.Dependents +
    Number.of.Referrals +
    Tenure.in.Months +
    Monthly.Charge +
    Contract +
    Internet.Service +
    Paperless.Billing +
    Payment.Method,
  family = binomial,
  data = train_data
)

summary(logistic_model)
# ============================================================
# 23. Prediction and Model Performance
# ============================================================

# Predict churn probabilities
predicted_probability <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

# Convert probabilities into 0/1 predictions
predicted_churn <- ifelse(
  predicted_probability >= 0.5,
  1,
  0
)

# Confusion matrix
confusion_matrix <- table(
  Actual = test_data$Churn,
  Predicted = predicted_churn
)

confusion_matrix

# Performance metrics
accuracy <- mean(
  predicted_churn == test_data$Churn
)

precision <- confusion_matrix[2, 2] /
  sum(confusion_matrix[, 2])

recall <- confusion_matrix[2, 2] /
  sum(confusion_matrix[2, ])

f1_score <- 2 * (precision * recall) /
  (precision + recall)

accuracy
precision
recall
f1_score
# ============================================================
# 24. Model Diagnostics, VIF and Odds Ratios
# ============================================================

# Residual diagnostic
plot(
  logistic_model,
  which = 1,
  main = "Logistic Regression Residual Diagnostic"
)

# Multicollinearity check
vif_values <- vif(logistic_model)

vif_values

# Odds ratios
odds_ratios <- exp(coef(logistic_model))

round(odds_ratios, 3)
# ============================================================
# 25. Final Analysis Summary
# ============================================================

cat("============================================================\n")
cat("WEEK 4 FINAL ANALYSIS SUMMARY\n")
cat("============================================================\n\n")

cat("Dataset:\n")
cat("Customers analyzed:", nrow(data), "\n")
cat("Variables:", ncol(data), "\n\n")

cat("Customer Status:\n")
print(customer_status)
cat("\n")

cat("Churn Rate:",
    round(mean(model_data$Churn) * 100, 2),
    "%\n\n")

cat("Model Performance:\n")
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
cat("Precision:", round(precision * 100, 2), "%\n")
cat("Recall:", round(recall * 100, 2), "%\n")
cat("F1 Score:", round(f1_score * 100, 2), "%\n\n")

cat("Important Findings:\n")
cat("- Competitor-related reasons were the largest churn category.\n")
cat("- Month-to-Month customers showed much higher churn than long-term contract customers.\n")
cat("- Churned customers had higher average Monthly Charges.\n")
cat("- Churned customers had substantially lower average Tenure.\n")
cat("- Total Charges and Total Revenue showed a very strong positive correlation.\n")
cat("- The logistic regression model achieved approximately 84% accuracy.\n")
cat("- VIF results indicated no serious multicollinearity.\n")

cat("\n============================================================\n")
cat("END OF WEEK 4 ANALYSIS\n")
cat("============================================================\n")
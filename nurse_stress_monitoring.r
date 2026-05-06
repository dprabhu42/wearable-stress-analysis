# STAT 355 Project: Analysis and Modeling of Stress Using Wearable Sensor Data
# Reproducible analysis script

# Load packages
library(ggplot2)
library(dplyr)
library(nnet)

# Load data
data <- read.csv("merged_data.csv")

# Create main random sample
set.seed(123)
data_sample <- data[sample(nrow(data), 20000), ]

# Convert stress label to factor for plots/classification
data_sample$label <- as.factor(data_sample$label)

# Create balanced sample for comparison plots and balanced classification
set.seed(123)
data_balanced <- data %>%
  group_by(label) %>%
  sample_n(2000, replace = TRUE) %>%
  ungroup()

data_balanced$label <- as.factor(data_balanced$label)

# -------------------------
# Part 1: Exploratory Data Analysis
# -------------------------

# Original stress level distribution
ggplot(data_sample, aes(x = label)) +
  geom_bar() +
  labs(
    title = "Original Stress Level Distribution",
    x = "Stress Level",
    y = "Count"
  )

# Heart rate across stress levels
ggplot(data_balanced, aes(x = label, y = HR)) +
  geom_boxplot() +
  labs(
    title = "Heart Rate Across Stress Levels",
    x = "Stress Level",
    y = "Heart Rate"
  )

# EDA across stress levels
ggplot(data_balanced, aes(x = label, y = EDA)) +
  geom_boxplot() +
  labs(
    title = "EDA Across Stress Levels",
    x = "Stress Level",
    y = "EDA"
  )

# Temperature across stress levels
ggplot(data_balanced, aes(x = label, y = TEMP)) +
  geom_boxplot() +
  labs(
    title = "Temperature Across Stress Levels",
    x = "Stress Level",
    y = "Temperature"
  )

# Correlation matrix for physiological variables
cor(data_sample[, c("HR", "EDA", "TEMP")])

# -------------------------
# Part 2, Q1: ANOVA
# -------------------------

# One-way ANOVA: Does EDA differ across stress levels?
anova_model <- aov(EDA ~ label, data = data_balanced)
summary(anova_model)

# Tukey post-hoc test
TukeyHSD(anova_model)

# -------------------------
# Part 2, Q2: Linear Regression
# -------------------------

# Convert label to numeric for regression
data_sample$label_numeric <- as.numeric(as.character(data_sample$label))

# Simple linear regression: Does EDA predict stress level?
lm_model <- lm(label_numeric ~ EDA, data = data_sample)
summary(lm_model)

# -------------------------
# Part 2, Q3: Classification
# -------------------------

# Classification on original imbalanced sample
set.seed(123)

train_index <- sample(1:nrow(data_sample), 0.7 * nrow(data_sample))

train <- data_sample[train_index, ]
test <- data_sample[-train_index, ]

model <- multinom(label ~ EDA + HR + TEMP, data = train)

predictions <- predict(model, newdata = test)

# Accuracy and confusion matrix
mean(predictions == test$label)
table(Predicted = predictions, Actual = test$label)

# Classification on balanced sample
set.seed(123)

train_index_b <- sample(1:nrow(data_balanced), 0.7 * nrow(data_balanced))

train_b <- data_balanced[train_index_b, ]
test_b <- data_balanced[-train_index_b, ]

model_b <- multinom(label ~ EDA + HR + TEMP, data = train_b)

pred_b <- predict(model_b, newdata = test_b)

# Accuracy and confusion matrix for balanced model
mean(pred_b == test_b$label)
table(Predicted = pred_b, Actual = test_b$label)
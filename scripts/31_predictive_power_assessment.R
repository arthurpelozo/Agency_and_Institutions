################################################################################
# Phase 31: Predictive Power Assessment
# Out-of-sample validation and predictive improvement quantification
################################################################################

library(tidyverse)
library(lfe)
library(caret)

cat("════════════════════════════════════════════════════════════════════════\n")
cat("PHASE 31: PREDICTIVE POWER ASSESSMENT\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

# Create output directories
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Set seed for reproducibility
set.seed(42)

################################################################################
# 1. Load panel data
################################################################################

cat("1. LOADING PANEL DATA (2017-2019)\n")
cat("────────────────────────────────────────────────────────────────────────\n")

panel <- read_csv("data_processed/panel/panel_2017_2019_balanced.csv",
                  show_col_types = FALSE)

# Complete cases
panel_complete <- panel %>%
  filter(!is.na(agency_index), !is.na(tea_binary), !is.na(age), 
         !is.na(female), !is.na(gemeduc)) %>%
  rename(agency = agency_index, educ = gemeduc) %>%
  mutate(
    age_sq = age^2,
    agency_sq = agency^2
  )

cat(sprintf("✓ Loaded: %d observations\n", nrow(panel_complete)))

################################################################################
# 2. Train-test split (temporal)
################################################################################

cat("\n2. TRAIN-TEST SPLIT (TEMPORAL)\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Train on 2017-2018, test on 2019
train_data <- panel_complete %>% filter(year %in% c(2017, 2018))
test_data <- panel_complete %>% filter(year == 2019)

cat(sprintf("✓ Training set: %d observations (2017-2018)\n", nrow(train_data)))
cat(sprintf("✓ Test set: %d observations (2019)\n", nrow(test_data)))

################################################################################
# 3. Baseline models (without agency)
################################################################################

cat("\n3. BASELINE MODELS (WITHOUT AGENCY)\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Null model (overall mean)
pred_null <- mean(train_data$tea_binary)
test_data$pred_null <- pred_null

# Demographics only
model_demo <- lm(tea_binary ~ age + age_sq + female + educ,
                 data = train_data)
test_data$pred_demo <- predict(model_demo, newdata = test_data)

cat("✓ Null model: Overall mean\n")
cat("✓ Demographic model: age + age² + female + educ\n")

################################################################################
# 4. Full models (with agency)
################################################################################

cat("\n4. FULL MODELS (WITH AGENCY)\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Linear agency
model_linear <- lm(tea_binary ~ agency + age + age_sq + female + educ,
                   data = train_data)
test_data$pred_linear <- predict(model_linear, newdata = test_data)

# Quadratic agency
model_quad <- lm(tea_binary ~ agency + agency_sq + age + age_sq + female + educ,
                 data = train_data)
test_data$pred_quad <- predict(model_quad, newdata = test_data)

# With interactions
model_interact <- lm(tea_binary ~ agency + age + age_sq + female + educ +
                       agency:female + agency:educ,
                     data = train_data)
test_data$pred_interact <- predict(model_interact, newdata = test_data)

cat("✓ Linear agency model\n")
cat("✓ Quadratic agency model\n")
cat("✓ Interactive agency model (agency × gender/education)\n")

################################################################################
# 5. Predictive performance metrics
################################################################################

cat("\n5. PREDICTIVE PERFORMANCE METRICS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Function to compute metrics
compute_metrics <- function(actual, predicted, model_name) {
  # Classification metrics (threshold at 0.5)
  pred_binary <- ifelse(predicted > 0.5, 1, 0)
  
  # Confusion matrix
  cm <- table(Predicted = pred_binary, Actual = actual)
  
  # Metrics
  accuracy <- mean(pred_binary == actual)
  
  # Handle division by zero
  precision <- if (sum(pred_binary == 1) > 0) {
    sum(pred_binary == 1 & actual == 1) / sum(pred_binary == 1)
  } else {
    NA
  }
  
  recall <- if (sum(actual == 1) > 0) {
    sum(pred_binary == 1 & actual == 1) / sum(actual == 1)
  } else {
    NA
  }
  
  f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA
  }
  
  # Regression metrics
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(actual - predicted))
  
  # R-squared
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  # AUC-ROC
  auc <- tryCatch({
    pred_obj <- ROCR::prediction(predicted, actual)
    perf <- ROCR::performance(pred_obj, "auc")
    as.numeric(perf@y.values)
  }, error = function(e) NA)
  
  data.frame(
    model = model_name,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1 = f1,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    auc = auc
  )
}

# Compute for all models
performance <- bind_rows(
  compute_metrics(test_data$tea_binary, test_data$pred_null, "Null (mean)"),
  compute_metrics(test_data$tea_binary, test_data$pred_demo, "Demographics only"),
  compute_metrics(test_data$tea_binary, test_data$pred_linear, "Agency (linear)"),
  compute_metrics(test_data$tea_binary, test_data$pred_quad, "Agency (quadratic)"),
  compute_metrics(test_data$tea_binary, test_data$pred_interact, "Agency (interactive)")
)

cat("\nOUT-OF-SAMPLE PERFORMANCE (2019 TEST SET):\n\n")
print(performance %>% 
        mutate(across(where(is.numeric), ~round(., 4))),
      row.names = FALSE)

################################################################################
# 6. Predictive improvement from agency
################################################################################

cat("\n6. PREDICTIVE IMPROVEMENT FROM AGENCY\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Calculate improvements
baseline_rmse <- performance$rmse[performance$model == "Demographics only"]
agency_rmse <- performance$rmse[performance$model == "Agency (linear)"]
rmse_improvement <- (baseline_rmse - agency_rmse) / baseline_rmse * 100

baseline_r2 <- performance$r_squared[performance$model == "Demographics only"]
agency_r2 <- performance$r_squared[performance$model == "Agency (linear)"]
r2_improvement <- agency_r2 - baseline_r2

cat(sprintf("\nPREDICTIVE GAINS:\n"))
cat(sprintf("  • RMSE improvement: %.2f%%\n", rmse_improvement))
cat(sprintf("  • R² improvement: %.4f → %.4f (+%.4f)\n",
            baseline_r2, agency_r2, r2_improvement))
cat(sprintf("  • Relative R² gain: %.1f%%\n",
            (r2_improvement / baseline_r2) * 100))

# Best model
best_model <- performance %>%
  arrange(desc(r_squared)) %>%
  slice(1)

cat(sprintf("\nBEST MODEL: %s\n", best_model$model))
cat(sprintf("  • R²: %.4f\n", best_model$r_squared))
cat(sprintf("  • RMSE: %.4f\n", best_model$rmse))
cat(sprintf("  • Accuracy: %.2f%%\n", best_model$accuracy * 100))

################################################################################
# 7. Cross-validation (k-fold)
################################################################################

cat("\n7. K-FOLD CROSS-VALIDATION\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# 5-fold CV on full dataset
k <- 5
folds <- createFolds(panel_complete$tea_binary, k = k, list = TRUE)

cv_results <- list()

for (i in 1:k) {
  train_idx <- unlist(folds[-i])
  test_idx <- folds[[i]]
  
  cv_train <- panel_complete[train_idx, ]
  cv_test <- panel_complete[test_idx, ]
  
  # Demographics model
  m_demo <- lm(tea_binary ~ age + age_sq + female + educ, data = cv_train)
  pred_demo <- predict(m_demo, newdata = cv_test)
  
  # Agency model
  m_agency <- lm(tea_binary ~ agency + age + age_sq + female + educ, 
                 data = cv_train)
  pred_agency <- predict(m_agency, newdata = cv_test)
  
  # Compute R²
  r2_demo <- 1 - sum((cv_test$tea_binary - pred_demo)^2) / 
    sum((cv_test$tea_binary - mean(cv_test$tea_binary))^2)
  r2_agency <- 1 - sum((cv_test$tea_binary - pred_agency)^2) / 
    sum((cv_test$tea_binary - mean(cv_test$tea_binary))^2)
  
  cv_results[[i]] <- data.frame(
    fold = i,
    r2_demo = r2_demo,
    r2_agency = r2_agency,
    improvement = r2_agency - r2_demo
  )
}

cv_summary <- bind_rows(cv_results)

cat(sprintf("\n5-FOLD CROSS-VALIDATION RESULTS:\n"))
cat(sprintf("  Demographics model R²: %.4f (SD=%.4f)\n",
            mean(cv_summary$r2_demo), sd(cv_summary$r2_demo)))
cat(sprintf("  Agency model R²: %.4f (SD=%.4f)\n",
            mean(cv_summary$r2_agency), sd(cv_summary$r2_agency)))
cat(sprintf("  Mean improvement: %.4f (SD=%.4f)\n",
            mean(cv_summary$improvement), sd(cv_summary$improvement)))

# Test improvement significance
improvement_test <- t.test(cv_summary$r2_agency, cv_summary$r2_demo, 
                           paired = TRUE)
cat(sprintf("  Paired t-test: t=%.2f, p=%.4f\n",
            improvement_test$statistic, improvement_test$p.value))

################################################################################
# 8. Variable importance
################################################################################

cat("\n8. VARIABLE IMPORTANCE (STANDARDIZED)\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Standardize variables
train_std <- train_data %>%
  mutate(
    agency_std = (agency - mean(agency)) / sd(agency),
    age_std = (age - mean(age)) / sd(age),
    age_sq_std = (age_sq - mean(age_sq)) / sd(age_sq),
    educ_std = (educ - mean(educ, na.rm=TRUE)) / sd(educ, na.rm=TRUE)
  )

model_std <- lm(tea_binary ~ agency_std + age_std + age_sq_std + female + educ_std,
                data = train_std)

# Extract standardized coefficients
coef_std <- coef(model_std)[-1]  # Exclude intercept
se_std <- coef(summary(model_std))[-1, "Std. Error"]
t_std <- coef(summary(model_std))[-1, "t value"]

importance <- data.frame(
  variable = names(coef_std),
  coef = coef_std,
  se = se_std,
  t_value = t_std,
  abs_t = abs(t_std)
) %>%
  arrange(desc(abs_t))

cat("\nStandardized coefficients (importance):\n")
print(importance %>% 
        mutate(across(where(is.numeric), ~round(., 4))),
      row.names = FALSE)

################################################################################
# 9. Visualizations
################################################################################

cat("\n9. CREATING VISUALIZATIONS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

# Plot 1: Model comparison
p1 <- performance %>%
  mutate(model = factor(model, levels = model)) %>%
  ggplot(aes(x = model, y = r_squared, fill = model)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.4f", r_squared)), vjust = -0.5) +
  labs(
    title = "Out-of-Sample Predictive Performance",
    subtitle = "R² on 2019 test set (trained on 2017-2018)",
    x = NULL,
    y = "R² (out-of-sample)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("output/figures/fig_predictive_performance.png", p1, 
       width = 10, height = 6, dpi = 300)
cat("✓ Saved: output/figures/fig_predictive_performance.png\n")

# Plot 2: Predicted vs actual (best model)
p2 <- test_data %>%
  ggplot(aes(x = pred_linear, y = tea_binary)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Predicted vs Actual TEA",
    subtitle = "Agency model on 2019 test set",
    x = "Predicted probability",
    y = "Actual TEA (0/1)"
  ) +
  theme_minimal()

ggsave("output/figures/fig_predicted_vs_actual.png", p2, 
       width = 8, height = 6, dpi = 300)
cat("✓ Saved: output/figures/fig_predicted_vs_actual.png\n")

# Plot 3: Variable importance
p3 <- importance %>%
  ggplot(aes(x = reorder(variable, abs_t), y = coef, fill = coef > 0)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se),
                width = 0.2) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "darkgreen")) +
  labs(
    title = "Variable Importance (Standardized Coefficients)",
    subtitle = "Effect on TEA probability",
    x = NULL,
    y = "Standardized coefficient ± 95% CI"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/figures/fig_variable_importance.png", p3, 
       width = 8, height = 6, dpi = 300)
cat("✓ Saved: output/figures/fig_variable_importance.png\n")

################################################################################
# 10. Save results
################################################################################

cat("\n10. SAVING RESULTS\n")
cat("────────────────────────────────────────────────────────────────────────\n")

write_csv(performance, "output/tables/table31_predictive_performance.csv")
cat("✓ Saved: output/tables/table31_predictive_performance.csv\n")

write_csv(cv_summary, "output/tables/table31_cv_results.csv")
cat("✓ Saved: output/tables/table31_cv_results.csv\n")

write_csv(importance, "output/tables/table31_variable_importance.csv")
cat("✓ Saved: output/tables/table31_variable_importance.csv\n")

cat("\n════════════════════════════════════════════════════════════════════════\n")
cat("PHASE 31 COMPLETE: Predictive Power Assessment\n")
cat("════════════════════════════════════════════════════════════════════════\n")

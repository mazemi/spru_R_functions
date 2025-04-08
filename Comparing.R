library(openxlsx)
library(dplyr)

df = read.xlsx("./mean_combined_data.xlsx", sheet = "both_data")

df <- na.omit(df)
clean_df <- df %>% select(-c(round, dist_name, dist_code))

jmmi <- clean_df %>% select(2:12)
vam <- clean_df %>% select(13:23)


t.test(jmmi$food_basket, vam$food_basket, paired = TRUE)

# test for all vars:

common_vars <- intersect(names(jmmi), names(vam))

results <- sapply(common_vars, function(var) {
  x <- jmmi[[var]]
  y <- vam[[var]]
  complete_idx <- complete.cases(x, y)
  test <- t.test(x[complete_idx], y[complete_idx], paired = TRUE)
  decision <- ifelse(test$p.value < 0.05, "Reject H0", "Do not reject H0")
  c(p_value = round(test$p.value, 4), decision = decision)
})

# Transpose and convert to data frame
results_df <- as.data.frame(t(results))
print(results_df)

# total var analysis:
# Get number of columns to compare
n <- min(ncol(jmmi), ncol(vam))

# Run the t-tests and collect results
results <- sapply(1:n, function(i) {
  x <- jmmi[[i]]
  y <- vam[[i]]
  complete_idx <- complete.cases(x, y)
  
  if (sum(complete_idx) < 2) return(c(p_value = NA, decision = NA, higher_source = NA))
  
  test <- t.test(x[complete_idx], y[complete_idx], paired = TRUE)
  decision <- ifelse(test$p.value < 0.05, "Reject H0", "Do not reject H0")
  
  higher <- NA
  if (test$p.value < 0.05) {
    mean_x <- mean(x[complete_idx])
    mean_y <- mean(y[complete_idx])
    higher <- ifelse(mean_x > mean_y, "JMMI", "VAM")
  }
  
  c(p_value = round(test$p.value, 4), decision = decision, higher_source = higher)
})

# Transpose and convert to data frame
results_df <- as.data.frame(t(results))

# Assign proper row names (from jmmi column names)
rownames(results_df) <- names(jmmi)[1:n]


write.xlsx(df, "jmmi_vam_data.xlsx")
write.xlsx(results_df, "result.xlsx")
print(results_df)

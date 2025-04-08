# Load required library for plotting
library(ggplot2)
library(openxlsx)
library(dplyr)

df = read.xlsx("./mean_combined_data.xlsx", sheet = "both_data")

df <- na.omit(df)
clean_df <- df %>% select(-c(round, dist_name, dist_code))

jmmi <- clean_df %>% select(2:12)
vam <- clean_df %>% select(13:23)

pdf("boxplots_comparison.pdf", width = 8, height = 6)

n <- min(ncol(jmmi), ncol(vam))

# Run the t-tests and collect results
results <- sapply(1:n, function(i) {
  x <- jmmi[[i]]
  y <- vam[[i]]
  complete_idx <- complete.cases(x, y)
  
  if (sum(complete_idx) < 2) return(c(p_value = NA, decision = NA, higher_source = NA))
  
  # Perform paired t-test
  test <- t.test(x[complete_idx], y[complete_idx], paired = TRUE)
  decision <- ifelse(test$p.value < 0.05, "Reject H0", "Do not reject H0")
  
  higher <- NA
  if (test$p.value < 0.05) {
    mean_x <- mean(x[complete_idx])
    mean_y <- mean(y[complete_idx])
    higher <- ifelse(mean_x > mean_y, "JMMI", "VAM")
  }
  
  # Boxplot for each variable comparison
  data <- data.frame(
    value = c(x[complete_idx], y[complete_idx]),
    source = rep(c("JMMI", "VAM"), each = sum(complete_idx)),
    variable = rep(names(jmmi)[i], 2 * sum(complete_idx))
  )
  
  p <- ggplot(data, aes(x = source, y = value, fill = source)) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", names(jmmi)[i])) +
    theme_minimal() +
    labs(x = "Source", y = "Price")
  
  print(p)
  
  return(c(p_value = round(test$p.value, 4), decision = decision, higher_source = higher))
})

dev.off()

results_df <- as.data.frame(t(results))

rownames(results_df) <- names(jmmi)[1:n]



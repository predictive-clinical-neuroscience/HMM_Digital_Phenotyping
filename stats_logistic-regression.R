library(broom)
library(data.table)
library(nnet)

df_testing <- read.csv('/directory/file_name.csv', header=TRUE, stringsAsFactors=TRUE, na.strings=c("","NA"))  
df_testing$LABEL[df_testing$LABEL == 'empty'] <- NA
df_testing <- subset(df_testing, !is.na(df_testing$LABEL))

# Assign LABELs
df_testing$AGE <- as.numeric(as.character(df_testing$AGE))
df_testing$LABEL <- as.character(df_testing$LABEL)
df_testing$LABEL[df_testing$LABEL == 'Control'] <- 0
df_testing$LABEL[df_testing$LABEL == 'Schizophrenia'] <- 1
df_testing$LABEL[df_testing$LABEL == 'Alzheimer\'s disease'] <- 2
df_testing$LABEL[df_testing$LABEL == 'Alzheimers disease'] <- 2
df_testing$LABEL[df_testing$LABEL == 'Dementia'] <- 2
df_testing$LABEL[df_testing$LABEL == 'Healthy / subjective cognitive complaints'] <- 3
df_testing$LABEL <- as.factor(df_testing$LABEL)

# Multinomial model
model <- multinom(LABEL ~ dwell_times_state1 + AGE, data = df_testing)

z <- summary(model)$coefficients/summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
odds <- exp(coef(model))

coefficients <- as.data.frame(summary(model)$coefficients)
colnames(coefficients) <- paste('coefficients', colnames(coefficients), sep = '_')
standard_errors <- as.data.frame(summary(model)$standard.errors)
colnames(standard_errors) <- paste('standard_errors', colnames(standard_errors), sep = '_')
z_values <- as.data.frame(z)
colnames(z_values) <- paste('z_values', colnames(z_values), sep = '_')
p_values <- as.data.frame(p)
colnames(p_values) <- paste('p_values', colnames(p_values), sep = '_')
log_reg_testing_models <- cbind(coefficients, standard_errors, z_values, p_values)
number_of_models <- 3
log_reg_testing_models$FDR.corrected.p.value_dwell_times_state1 <- lapply(log_reg_testing_models$p_values_dwell_times_state1, function(x) p.adjust(x, method = "fdr", n = number_of_models))
log_reg_testing_models$FDR.corrected.p.value_AGE <- lapply(log_reg_testing_models$p_values_AGE, function(x) p.adjust(x, method = "fdr", n = number_of_models))
log_reg_testing_models <- as.matrix(log_reg_testing_models)
write.table(log_reg_testing_models, file = "/directory/file_name.txt", sep = ",", quote = FALSE, row.names = F)

# Binomial models (for sensitivity analysis)
df_testing_HC_SZ <- subset(df_testing, LABEL==0 | LABEL==1)
df_testing_HC_SZ <- subset(df_testing_HC_SZ, AGE < 42)  # age matching
model_binomial_HC_SZ <- glm(LABEL ~ dwell_times_state1 + AGE, data = df_testing_HC_SZ, family = binomial)

df_testing_HC_AD <- subset(df_testing, LABEL==0 | LABEL==2)
df_testing_HC_AD <- subset(df_testing_HC_AD, AGE > 50)  # age matching
model_binomial_HC_AD <- glm(LABEL ~ dwell_times_state1 + AGE, data = df_testing_HC_AD, family = binomial)

df_testing_HC_SCC <- subset(df_testing, LABEL==0 | LABEL==3)
df_testing_HC_SCC <- subset(df_testing_HC_SCC, AGE > 43)  # age matching
model_binomial_HC_SCC <- glm(LABEL ~ dwell_times_state1 + AGE, data = df_testing_HC_SCC, family = binomial)

model_binomial_HC_SZ <- tidy(model_binomial_HC_SZ)
model_binomial_HC_AD <- tidy(model_binomial_HC_AD)
model_binomial_HC_SCC <- tidy(model_binomial_HC_SCC)
data_list <- list(model_binomial_HC_SZ, model_binomial_HC_AD, model_binomial_HC_SCC)
all_logreg_models <- rbindlist(data_list)
number_of_models <- 3
all_logreg_models$FDR.corrected.p.value <- as.numeric(as.character(lapply(all_logreg_models$p.value, function(x) p.adjust(x, method = "fdr", n = number_of_models))))
all_logreg_models$Odds <- as.numeric(as.character(lapply(all_logreg_models$estimate, function(x) exp(x))))
write.table(all_logreg_models, file = "/directory/file_name.txt", sep = ",", quote = FALSE, row.names = F)
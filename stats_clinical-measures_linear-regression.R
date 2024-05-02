library(broom)
library(data.table)

df <- read.csv('/directory/file_name.csv', header=TRUE, stringsAsFactors=TRUE, na.strings=c("","NA")) 

### Models for MMSE score
df$MMSE_TOT[df$MMSE_TOT == 'empty'] <- NA  # non AD or HC participants
df_MMSEsubset <- subset(df, !is.na(df$MMSE_TOT))
df_MMSEsubset$MMSE_TOT <- as.numeric(as.character(df_MMSEsubset$MMSE_TOT))
df_MMSEsubset$AGE <- as.numeric(as.character(df_MMSEsubset$AGE))
model <- lm(MMSE_TOT ~ dwell_times_state1 + AGE, data = df_MMSEsubset)

# For the different labels:
df_MMSEsubset$LABEL <- as.character(df_MMSEsubset$LABEL)
df_MMSEsubset$LABEL[df_MMSEsubset$LABEL == 'Control'] <- 0
df_MMSEsubset$LABEL[df_MMSEsubset$LABEL == 'Alzheimer\'s disease'] <- 1
df_MMSEsubset$LABEL <- as.factor(df_MMSEsubset$LABEL)

df_MMSEsubset_HC <- subset(df_MMSEsubset, LABEL==0)
df_MMSEsubset_AD <- subset(df_MMSEsubset, LABEL==1)

model_HC <- lm(MMSE_TOT ~ dwell_times_state1 + AGE, data = df_MMSEsubset_HC)
model_AD <- lm(MMSE_TOT ~ dwell_times_state1 + AGE, data = df_MMSEsubset_AD)

model_all_testing <- tidy(model)
model_HC <- tidy(model_HC)
model_AD <- tidy(model_AD)
data_list <- list(model_all_testing, model_HC, model_AD)
all_mmse_models <- rbindlist(data_list)
number_of_models <- 3
all_mmse_models$FDR.corrected.p.value <- as.numeric(as.character(lapply(all_mmse_models$p.value, function(x) p.adjust(x, method = "fdr", n = number_of_models))))
write.table(all_mmse_models, file = "/directory/file_name.txt", sep = ",", quote = FALSE, row.names = F)

### Models for PANSS scores
df$PANSS_PT[df$PANSS_PT == 'empty'] <- NA  # non SZ participants
df$PANSS_NT[df$PANSS_NT == 'empty'] <- NA
df$PANSS_GT[df$PANSS_GT == 'empty'] <- NA
df$PANSS_CS[df$PANSS_CS == 'empty'] <- NA
df_PANSSsubset <- subset(df, !is.na(df$PANSS_PT))
df_PANSSsubset$PANSS_PT <- as.numeric(as.character(df_PANSSsubset$PANSS_PT))
df_PANSSsubset$PANSS_NT <- as.numeric(as.character(df_PANSSsubset$PANSS_NT))
df_PANSSsubset$PANSS_GT <- as.numeric(as.character(df_PANSSsubset$PANSS_GT))
df_PANSSsubset$PANSS_CS <- as.numeric(as.character(df_PANSSsubset$PANSS_CS))
df_PANSSsubset$PANSS_TOTAL <- df_PANSSsubset$PANSS_PT + df_PANSSsubset$PANSS_NT + df_PANSSsubset$PANSS_GT # Calculate total score
df_PANSSsubset$AGE <- as.numeric(as.character(df_PANSSsubset$AGE))
model_PT <- lm(PANSS_PT ~ dwell_times_state1 + AGE, data = df_PANSSsubset)
model_NT <- lm(PANSS_NT ~ dwell_times_state1 + AGE, data = df_PANSSsubset)
model_GT <- lm(PANSS_GT ~ dwell_times_state1 + AGE, data = df_PANSSsubset)
model_CS <- lm(PANSS_CS ~ dwell_times_state1 + AGE, data = df_PANSSsubset)
model_TOTAL <- lm(PANSS_TOTAL ~ dwell_times_state1 + AGE, data = df_PANSSsubset)

model_PT <- tidy(model_PT)
model_NT <- tidy(model_NT)
model_GT <- tidy(model_GT)
model_CS <- tidy(model_CS)
model_TOTAL <- tidy(model_TOTAL)
data_list <- list(model_PT, model_NT, model_GT, model_CS, model_TOTAL)
all_panss_models <- rbindlist(data_list)
write.table(all_panss_models, file = "/directory/file_name.txt", sep = ",", quote = FALSE, row.names = F)


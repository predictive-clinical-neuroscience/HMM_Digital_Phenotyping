library(broom)
library(data.table)

df <- read.csv('/directory/file_name.csv', header=TRUE, stringsAsFactors=TRUE, na.strings=c("","NA"))

### Models for SFS score
df$SFS[df$SFS == 'empty'] <- NA
df_SFSsubset <- subset(df, !is.na(df$SFS))
df_SFSsubset <- subset(df_SFSsubset, !is.na(df_SFSsubset$LABEL))
df_SFSsubset$SFS <- as.numeric(as.character(df_SFSsubset$SFS))
df_SFSsubset$LONELINESS <- as.numeric(as.character(df_SFSsubset$LONELINESS))
df_SFSsubset$AGE <- as.numeric(as.character(df_SFSsubset$AGE))

# For the different labels
df_SFSsubset$LABEL <- as.character(df_SFSsubset$LABEL)
df_SFSsubset$LABEL[df_SFSsubset$LABEL == 'Control'] <- 0
df_SFSsubset$LABEL[df_SFSsubset$LABEL == 'Schizophrenia'] <- 1
df_SFSsubset$LABEL[df_SFSsubset$LABEL == 'Alzheimer\'s disease'] <- 2
df_SFSsubset$LABEL[df_SFSsubset$LABEL == 'Alzheimers disease'] <- 2
df_SFSsubset$LABEL[df_SFSsubset$LABEL == 'Dementia'] <- 2
df_SFSsubset$LABEL <- as.factor(df_SFSsubset$LABEL)

# Overall testing
model_all_testing <- lm(SFS ~ AGE + LABEL*dwell_times_state2, data = df_SFSsubset)
model_all_testing <- tidy(model_all_testing)
write.table(model_all_testing, file = "/directory/file_name.txt", sep = ",", quote = FALSE, row.names = F)

# For each label
df_SFSsubset_HC <- subset(df_SFSsubset, LABEL==0)
df_SFSsubset_SZ <- subset(df_SFSsubset, LABEL==1)
df_SFSsubset_AD <- subset(df_SFSsubset, LABEL==2)

model_HC <- lm(SFS ~ dwell_times_state2 + AGE, data = df_SFSsubset_HC)
model_SZ <- lm(SFS ~ dwell_times_state2 + AGE, data = df_SFSsubset_SZ)
model_AD <- lm(SFS ~ dwell_times_state2 + AGE, data = df_SFSsubset_AD)
model_HC <- tidy(model_HC)
model_SZ <- tidy(model_SZ)
model_AD <- tidy(model_AD)
data_list <- list(model_HC, model_SZ, model_AD)
all_SFS_models <- rbindlist(data_list)
number_of_models <- 3
all_SFS_models$FDR.corrected.p.value <- as.numeric(as.character(lapply(all_SFS_models$p.value, function(x) p.adjust(x, method = "fdr", n = number_of_models))))
write.table(all_SFS_models, file = "/directory/file_name.txt", sep = ",", quote = FALSE, row.names = F)

### Models for LONELINESS score
df$LONELINESS[df$LONELINESS == 'empty'] <- NA
df_LONELINESSsubset <- subset(df, !is.na(df$LONELINESS))
df_LONELINESSsubset <- subset(df_LONELINESSsubset, !is.na(df_LONELINESSsubset$LABEL))
df_LONELINESSsubset$LONELINESS <- as.numeric(as.character(df_LONELINESSsubset$LONELINESS))
df_LONELINESSsubset$AGE <- as.numeric(as.character(df_LONELINESSsubset$AGE))

# For the different labels
df_LONELINESSsubset$LABEL <- as.character(df_LONELINESSsubset$LABEL)
df_LONELINESSsubset$LABEL[df_LONELINESSsubset$LABEL == 'Control'] <- 0
df_LONELINESSsubset$LABEL[df_LONELINESSsubset$LABEL == 'Schizophrenia'] <- 1
df_LONELINESSsubset$LABEL[df_LONELINESSsubset$LABEL == 'Alzheimer\'s disease'] <- 2
df_LONELINESSsubset$LABEL[df_LONELINESSsubset$LABEL == 'Alzheimers disease'] <- 2
df_LONELINESSsubset$LABEL[df_LONELINESSsubset$LABEL == 'Dementia'] <- 2
df_LONELINESSsubset$LABEL <- as.factor(df_LONELINESSsubset$LABEL)
                                                                       
model_all_testing <- lm(LONELINESS ~ AGE + LABEL*dwell_times_state2, data = df_LONELINESSsubset)
model_all_testing <- tidy(model_all_testing)
write.table(model_all_testing, file = "/directory/file_name.txt", sep = ",", quote = FALSE, row.names = F)
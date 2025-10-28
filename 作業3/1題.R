# 1. 讀檔與檢視
df <- read.csv(file.choose(), stringsAsFactors = TRUE)

# 2. KM 與 log-rank
surv_obj <- Surv(df$Time, df$Status)
fit_km <- survfit(surv_obj ~ Treatment, data = df)
ggsurvplot(fit_km, data = df, pval = TRUE, conf.int = FALSE, risk.table=TRUE,
           legend.labs = c("Chemo only","Chemo+RT"))


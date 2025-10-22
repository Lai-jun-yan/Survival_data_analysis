library(survival)

# 建立 KM 曲線
fit_km <- survfit(Surv(Time, Status) ~ Treatment, data = df)

# 繪製 log-log plot
plot(fit_km, fun = "cloglog", col = c("orange","blue"),
     lty = 1:1, lwd = 2,
     xlab = "追蹤時間 (天)", ylab = "log(-log(Survival))",
     main = "Log-Log Plot for Proportional Hazards Check")
legend("bottomright", legend=c("化療 (0)", "化療加放療 (1)"),
       col=c("orange","blue"), lty=1, lwd=2)

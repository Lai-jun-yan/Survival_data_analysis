# 5. log-log plot
# -----------------------------
# 1️⃣ 取出 KM 曲線
fit_km <- survfit(Surv(Time, Status) ~ Treatment, data = df)

# 分組 KM 曲線
KM0 <- survfit(Surv(Time, Status) ~ 1, data = df, subset = (Treatment == 0))  # Chemo only
KM1 <- survfit(Surv(Time, Status) ~ 1, data = df, subset = (Treatment == 1))  # Chemo + RT

# -----------------------------
# 2️⃣ 計算 log(-log(S)) 的上下界（可選）
y_max <- max(c(log(-log(summary(KM0)$surv)), log(-log(summary(KM1)$surv))))
y_min <- min(c(log(-log(summary(KM0)$surv)), log(-log(summary(KM1)$surv))))

# -----------------------------
# 3️⃣ 畫圖
plot(summary(KM0)$time, log(-log(summary(KM0)$surv)),
     type="l", col="blue", lwd=2,
     xlab="Time", ylab="log(-log(S))",
     ylim=c(y_min, y_max),
     main="PH Assumption Check: log(-log(S) vs Time)")

lines(summary(KM1)$time, log(-log(summary(KM1)$surv)),
      type="l", col="red", lwd=2)

# -----------------------------
# 4️⃣ 加上圖例
legend("bottomleft",
       legend=c("Chemo only", "Chemo + RT"),
       col=c("blue", "red"),
       lty=1, lwd=2)

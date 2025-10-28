#畫出cox的生存曲線與KM比較
sf_cox0 <- survfit(cox1, newdata = data.frame(Treatment = 0))
sf_cox1 <- survfit(cox1, newdata = data.frame(Treatment = 1))


plot(fit_km,
     col = c("#1f77b4", "#d62728"),
     lty = 1, lwd = 3,
     xlab = "Time", ylab = "Survival Probability",
     main = "KM vs Cox Fitted Survival Curves")

lines(s0$time, s0$surv, col = "#1f77b4", lty = 2, lwd = 3)
lines(s1$time, s1$surv, col = "#d62728", lty = 2, lwd = 3)

legend("topright",
       legend = c("KM: Chemo only", "KM: Chemo+RT",
                  "Cox: Chemo only", "Cox: Chemo+RT"),
       col = c("#1f77b4", "#d62728", "#1f77b4", "#d62728"),
       lty = c(1, 1, 2, 2), lwd = 3, bg = "white")

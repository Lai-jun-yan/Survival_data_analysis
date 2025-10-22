library(survival)
library(ggplot2)

# Cox 模型
cox_model <- coxph(Surv(Time, Status) ~ Treatment, data = df)

# Schoenfeld 殘差檢驗
zph <- cox.zph(cox_model)

# 查看檢定結果
print(zph)

# 畫 β(t) 隨時間變化曲線
plot(zph, var="Treatment", main="Time-varying effect of Treatment (β(t))")
abline(h=0, col="red", lty=2)  # 加水平線作為參考

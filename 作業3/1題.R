data = read.csv(file.choose())

library(survival)
library(survminer)

df = data

# 建立生存物件
surv_obj <- Surv(time = df$Time, event = df$Status)

# 根據 Treatment 分組建立 KM 模型
fit <- survfit(surv_obj ~ Treatment, data = df)

# 繪製 KM 曲線
ggsurvplot(
  fit,
  data = df,
  pval = TRUE,              # 顯示 Log-rank 檢定 p 值
  conf.int = TRUE,          # 顯示信賴區間
  risk.table = TRUE,        # 顯示風險表
  legend.title = "治療組別",
  legend.labs = c("化療 (0)", "化療加放療 (1)"),
  xlab = "追蹤時間 (天)",
  ylab = "生存機率",
  palette = c("#E7B800", "#2E9FDF")  # 自訂顏色
)

# 建立 Cox 比例風險模型
cox_model <- coxph(Surv(Time, Status) ~ Treatment, data = df)

# 顯示結果
summary(cox_model)

# KM 曲線
fit_km <- survfit(Surv(Time, Status) ~ Treatment, data = df)

# Cox 模型
cox_model <- coxph(Surv(Time, Status) ~ Treatment, data = df)

# 擬合 Cox 曲線
newdata <- data.frame(Treatment = c(0,1))
fit_cox <- survfit(cox_model, newdata = newdata)

# 繪圖：KM + Cox 擬合曲線
ggsurvplot(
  fit_km,
  data = df,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  legend.title = "治療組別",
  legend.labs = c("化療 (0)", "化療加放療 (1)"),
  xlab = "追蹤時間 (天)",
  ylab = "生存機率",
  palette = c("#E7B800", "#2E9FDF")
) -> p

# 加上 Cox 擬合曲線（紅色虛線）
p$plot + 
  geom_step(data = as.data.frame(fit_cox[1]), aes(x = time, y = surv), color = "red", linetype = 2) +
  geom_step(data = as.data.frame(fit_cox[2]), aes(x = time, y = surv), color = "red", linetype = 2)










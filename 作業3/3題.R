library(survival)
library(survminer)
library(ggplot2)

# KM 曲線
fit_km <- survfit(Surv(Time, Status) ~ Treatment, data = df)

# Cox 模型
cox_model <- coxph(Surv(Time, Status) ~ Treatment, data = df)
newdata <- data.frame(Treatment = c(0,1))
fit_cox <- survfit(cox_model, newdata = newdata)

# 將 Cox 擬合曲線轉資料框
cox_df0 <- data.frame(time = summary(fit_cox[1])$time, surv = summary(fit_cox[1])$surv,
                      Treatment = "0", Type = "Cox")
cox_df1 <- data.frame(time = summary(fit_cox[2])$time, surv = summary(fit_cox[2])$surv,
                      Treatment = "1", Type = "Cox")
cox_df <- rbind(cox_df0, cox_df1)

# 將 KM 曲線轉資料框
km_df <- data.frame(
  time = c(fit_km$time),
  surv = c(fit_km$surv),
  Treatment = factor(rep(fit_km$strata %>% names() %>% gsub("Treatment=", "", .),
                         times = fit_km$strata)),
  Type = "KM"
)

# 指定顏色與線型
cols <- c("0"="#E7B800", "1"="#2E9FDF")
linetypes <- c("KM"=1, "Cox"=2)  # 1=實線, 2=虛線

# 繪圖
ggplot() +
  geom_step(data = km_df, aes(x = time, y = surv, color = Treatment, linetype = Type), size=1) +
  geom_step(data = cox_df, aes(x = time, y = surv, color = Treatment, linetype = Type), size=1) +
  scale_color_manual(values = cols, labels = c("化療 (0)", "化療加放療 (1)")) +
  scale_linetype_manual(values = linetypes, labels = c("KM", "Cox")) +
  labs(x = "追蹤時間 (天)", y = "生存機率",
       color = "治療組別", linetype = "曲線類型") +
  theme_minimal(base_size = 14)

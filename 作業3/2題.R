# 3. Cox PH 模型
cox1 <- coxph(surv_obj ~ Treatment, data = df)
summary(cox1)
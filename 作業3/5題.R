# 6. Schoenfeld 殘差檢定與繪圖

cox1 <- coxph(Surv(Time, Status) ~ Treatment, data = df)

residuals(cox1, type="scaledsch")

zph.cox = cox.zph(cox1 , transform="identity")

zph.cox

par(mfrow=c(1, length(cox1$coef)))
for (i in 1: length(cox1$coef)) {
  plot(zph.cox[i]); 
  abline(h=0, col=2)
  abline(h=coef(cox1)[i], col=3)
}


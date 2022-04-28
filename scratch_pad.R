

library(survival)
library(survminer)


df <- simulate.competing.data(n = 1500,
	par0 = c(shapeR = .3, scaleR = 15,
                  shapeM = 2, scaleM = 20.5,
                  shapeD = 1.51, scaleD = 12.23,
                  sigma = 0.1,
                  alphaM = 2,
                  alphaD = -2,
                  betaR = -.5,
                  betaM = -.5,
                  betaD = 0.5))%>%
group_by(id) %>%
filter(delirium==1 | day == losic ) %>%
filter(day == min(day)) %>%
mutate(delirium = ifelse(is.na(delirium), 0, delirium))

mod <-survival::survfit(data = df, formula = Surv(day, delirium) ~ trt)

survminer::ggsurvplot(mod)




coxph(Surv(day, delirium) ~ trt, data = df2)

summary(Model1)
summary(Model2)

survminer::ggsurvplot(Model2)

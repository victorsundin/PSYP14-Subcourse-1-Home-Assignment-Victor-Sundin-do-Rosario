library(tidyverse)
library(psych)	
library(gridExtra)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot) 
library(tidyverse) 

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

summary(data_sample_1)
describe(data_sample_1)
str(data_sample_1)


data_sample_1[data_sample_1$pain == 55,] 
data_sample_1[data_sample_1$STAI_trait == 4.2,]
data_sample_1[88, "pain"] <- 5
data_sample_1[34, "STAI_trait"] <- 42

data_sample_1 <- data_sample_1 %>% 	
  mutate(sex = factor(sex))


model_1 <- lm(pain ~ sex + age, data = data_sample_1)

model_2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_1)


model_1 %>%
  plot(which = 4)

model_2 %>%
  plot(which = 4)

data_sample_1 %>% 
  slice(c(8,23,47))



model_1 %>%
  plot(which = 2)

model_2 %>%
  plot(which = 2)

describe(residuals(model_1))
describe(residuals(model_2))


plot(x = model_1, which = 1) 

model_1 %>%
  residualPlots()


model_2 %>% 
  residualPlots()


model_1 %>%
  plot(which = 3)
model_2 %>%
  plot(which = 3)

model_1 %>%
  ncvTest()
model_2 %>%
  ncvTest() 

model_1 %>%
  bptest()
model_2 %>%
  bptest()


model_1 %>%
  vif()
model_2 %>%
  vif()

data_sample_1 %>%
  select(pain, age, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness) %>%
  pairs.panels(col = "red", lm = T)

cor.test(data_sample_1$cortisol_serum, data_sample_1$cortisol_saliva)



model_2_withoutsaliva %>%
  plot(which = 4)

model_2_withoutsaliva %>%
  plot(which = 3)
model_2_withoutsaliva %>%
  plot(which = 5)

data_sample_1 %>% 
  slice(c(47, 65, 86))

describe(residuals(model_2_withoutsaliva))


model_2_withoutsaliva %>%
  plot(which = 2)

describe(residuals(model_2_withoutsaliva))


model_2_withoutsaliva %>% 
  residualPlots()



model_2_withoutsaliva %>%
  plot(which = 3)

model_2_withoutsaliva %>%
  ncvTest() 

model_2_withoutsaliva %>%
  bptest()




model_2_withoutsaliva %>%
  vif()


AIC(model_1)
AIC(model_2_withoutsaliva)

anova(model_1, model_2_withoutsaliva)

summary(model_1)
summary(model_2_withoutsaliva)

model_2_withoutsaliva <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_1)
summary(model_2_withoutsaliva)


coef_table(model_1)
coef_table(model_2_withoutsaliva)

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}


library(tidyverse)
library(psych)	
library(gridExtra)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot) 
library(tidyverse) 

model_full <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = data_sample_1 )
summary(model_full)



model_full %>%
  plot(which = 4)

data_sample_1 %>% 
  slice(c(47,85,86))



model_full %>%
  plot(which = 2)

describe(residuals(model_full))



model_full %>%
  residualPlots()


model_full %>%
  plot(which = 3)

model_full %>%
  ncvTest()

model_full %>%
  bptest()

#Collinäritet

model_full %>%
  vif()



#library(olsrr)
#ols_step_backward_p(model_3, prem = 0.05)


model_backwards <-step(object = model_full, direction = "backward")

summary(model_2_withoutsaliva)
summary(model_backwards)
summary(model_full)

model_backwards
model_2_withoutsaliva
AIC(model_2_withoutsaliva)
AIC(model_backwards)
AIC(model_full)


anova(model_2_withoutsaliva, model_backwards)
anova(model_backwards, model_2_withoutsaliva)


data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")
str(data_sample_2)

data_sample_2 <- data_sample_2 %>% 	
  mutate(sex = factor(sex))

predict(model_2_withoutsaliva, newdata = data_sample_2 )



RSS_model_2_withoutsaliva_datasample2 <- sum((data_sample_2$pain - predict(model_2_withoutsaliva, newdata = data_sample_2 ))^2)
RSS_model_2_backwards_datasample2 <- sum((data_sample_2$pain - predict(model_backwards, newdata = data_sample_2 ))^2)


summary(model_backwards)
coef_table(model_backwards)

library(cAIC4)
library(r2glmm)
library(lme4) 
library(lmerTest) 
library(MuMIn) 
library(psych)
library(tidyverse)


stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")
str(data_sample_3)
summary(data_sample_3)
describe(data_sample_3)


str(data_sample_3)

data_sample_3 %>%
  filter(!grepl('female|male', sex))

data_sample_3$sex <- replace(data_sample_3$sex, data_sample_3$sex=="woman", "female")

data_sample_3 <- data_sample_3 %>% 	
  mutate(sex = factor(sex))

data_sample_3 <- data_sample_3 %>% 	
  mutate(hospital = factor(hospital))


model_4_randomintercept = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1 | hospital ), data = data_sample_3)
summary(model_4_randomintercept)
summary(model_2_withoutsaliva)


r2beta(model_4_randomintercept, method = "nsj", data = data_sample_3)
r.squaredGLMM(model_4_randomintercept)
confint(model_4_randomintercept)
stdCoef.merMod(model_4_randomintercept)


coef_table(model_2_withoutsaliva)



data_sample_4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")

str(data_sample_4)
summary(data_sample_4)

data_sample_4 <- data_sample_4 %>% 	
  mutate(sex = factor(sex))

data_sample_4 <- data_sample_4 %>% 	
  mutate(hospital = factor(hospital))


mod_mean <- lm(pain ~ 1, data= data_sample_4)

TSS = sum((data_sample_4$pain - predict(mod_mean))^2)

RSS_model_4_randomintercept_datasample4 = sum((data_sample_4$pain - predict(model_4_randomintercept, newdata = data_sample_4, allow.new.levels = TRUE))^2)

R2= 1-(RSS_model_4_randomintercept_datasample4/TSS)

R2
r.squaredGLMM(model_4_randomintercept)



model_5_random_slopeintercept = lmer(pain ~ cortisol_serum + ( cortisol_serum| hospital),
                                     data = data_sample_3)
model_5_random_slopeintercept
summary(model_5_random_slopeintercept)
r.squaredGLMM(model_5_random_slopeintercept)
confint(model_5_random_slopeintercept)
r2beta(model_5_random_slopeintercept, method = "nsj", data = data_sample_3)


?isSingular

data_sample_3 = data_sample_3 %>%
  mutate(pred_slope = predict(model_5_random_slopeintercept))


data_sample_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red", aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)




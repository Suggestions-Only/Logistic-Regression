library(psych)

surgery <- read.csv("C:/Users/Owner/Desktop/Surgery Timing.csv")

surgery$gender <- factor(surgery$gender,
                         levels = c(0,1),
                         labels = c("Male", "Female"))
surgery$race <- factor(surgery$race,
                       levels = c(1,2,3),
                       labels = c("White","Black","Other"))
surgery$baseline_cancer <- factor(surgery$baseline_cancer,
                                  levels = c(0,1),
                                  labels = c("No","Yes"))
surgery$baseline_cvd <- factor(surgery$baseline_cvd,
                               levels = c(0,1),
                               labels = c("No","Yes"))
surgery$baseline_dementia <- factor(surgery$baseline_dementia,
                               levels = c(0,1),
                               labels = c("No","Yes"))
surgery$baseline_diabetes <- factor(surgery$baseline_diabetes,
                                    levels = c(0,1),
                                    labels = c("No","Yes"))
surgery$baseline_digestive <- factor(surgery$baseline_digestive,
                                     levels = c(0,1),
                                     labels = c("No","Yes"))
surgery$baseline_osteoart <- factor(surgery$baseline_osteoart,
                                    levels = c(0,1),
                                    labels = c("No","Yes"))
surgery$baseline_psych <- factor(surgery$baseline_psych,
                                    levels = c(0,1),
                                    labels = c("No","Yes"))
surgery$baseline_pulmonary <- factor(surgery$baseline_pulmonary,
                                    levels = c(0,1),
                                    labels = c("No","Yes"))
surgery$baseline_charlson <- factor(surgery$baseline_charlson,
                                    levels = c(0,1),
                                    labels = c("No","Yes"))
surgery$month <- factor(surgery$month,
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                   "Sep","Oct","Nov","Dec"))
surgery$moonphase <- factor(surgery$moonphase,
                            levels = c(1,2,3,4),
                            labels = c("New Moon","First Q","Full Moon","Last Q"))

plot(surgery$dow,surgery$ccsComplicationRate)
plot(surgery$hour,surgery$ccsComplicationRate)
boxplot(surgery$ccsComplicationRate ~ surgery$month)
boxplot(surgery$ccsComplicationRate ~ surgery$moonphase)

boxplot(surgery$complication_rsi~surgery$dow)
plot(surgery$hour,surgery$complication_rsi)
boxplot(surgery$complication_rsi ~ surgery$month)
boxplot(surgery$complication_rsi ~ surgery$moonphase)

describeBy(surgery$hour, group = surgery$gender)
boxplot(surgery$hour ~ surgery$gender)

describeBy(surgery$dow, group = surgery$gender)
boxplot(surgery$dow ~ surgery$gender)

describeBy(surgery$hour, group = surgery$race)
boxplot(surgery$hour ~ surgery$race)

describeBy(surgery$dow, group = surgery$race)

check_model <- glm(complication ~ gender + hour + dow + month + moonphase,
                   family = binomial, data = surgery)
summary(check_model)

timing_model <- glm(complication ~ hour + dow + month + moonphase,
                    family = binomial, data = surgery)
summary(timing_model)

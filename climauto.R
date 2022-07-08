# renv::init()

# Install packages
# install.packages("jsonlite")
install.packages("ggplot2")
library(ggplot2)

# Read file
climauto_raw <- read.csv("Climauto.csv")
climauto_df <- data.frame(climauto_raw)

# Remove sundays from the data frame
climauto <- climauto_df[climauto_df$dia_da_semana != "dom",]

# Set variables
quantity <- climauto$quantidade
type <- climauto$modelo
week_day <- factor(climauto$dia_da_semana,
    levels = c("seg", "ter", "qua", "qui", "sex", "sab"),
    labels = c("seg", "ter", "qua", "qui", "sex", "sab"),
    ordered = TRUE)
month <- factor(climauto$mes,
    levels = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"),
    labels = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"),
    ordered = TRUE)
day <- factor(climauto$dia)

# Snoop data
summary(climauto)
hist(quantity)
plot(week_day, quantity)
plot(month, quantity)
plot(day, quantity)

model_test <- aov(quantity ~ week_day + month)
summary(model_test)
qqnorm(residuals(model_test))
qqline(residuals(model_test))
shapiro.test(residuals(model_test))

monday <- climauto[climauto$dia_da_semana == "seg",]$quantidade
tuesday <- climauto[climauto$dia_da_semana == "ter",]$quantidade
wednesday <- climauto[climauto$dia_da_semana == "qua",]$quantidade
thursday <- climauto[climauto$dia_da_semana == "qui",]$quantidade
friday <- climauto[climauto$dia_da_semana == "sex",]$quantidade
saturday <- climauto[climauto$dia_da_semana == "sab",]$quantidade
saturday <- saturday[-c(53)]

week_days <- data.frame(monday, tuesday, wednesday, thursday, friday, saturday)
week_days
week_days_mean <- colMeans(week_days, na.rm = TRUE)
week_days_mean
week_days_sum <- colSums(week_days, na.rm = TRUE)
week_days_sum

week_day <- data.frame(week_days_mean, week_days_sum)
plot(week_day$week_days_mean)
plot(week_day$week_days_sum)

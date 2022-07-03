# renv::init()

# Install packages
# install.packages("jsonlite")
install.packages("ggplot2")
library(ggplot2)

# Read file
climauto_raw <- read.csv("Climauto.csv")
climauto <- data.frame(climauto_raw)

# Set variables
quantity <- climauto$quantidade
model <- climauto$modelo
week_day <- factor(climauto$dia_da_semana,
    levels = c("dom", "seg", "ter", "qua", "qui", "sex", "sab"),
    labels = c("dom", "seg", "ter", "qua", "qui", "sex", "sab"),
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

model_test <- aov(quantity ~ week_day+month)
summary(model_test)
qqnorm(residuals(model_test))
qqline(residuals(model_test))
shapiro.test(residuals(model_test))

ggplot(climauto, aes(day, quantity)) +
    geom_point()

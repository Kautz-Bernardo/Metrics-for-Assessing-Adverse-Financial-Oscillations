#Work authored by Bernardo Kautz. Follow on LinkedIn: https://www.linkedin.com/in/bernardo-kautz/.
dev.off()
rm(list = ls())
cat("\014")

#Packages
install.packages("quantmod")
install.packages("purrr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("moments")
install.packages("metRology")
install.packages("MASS")
library("quantmod")
library("purrr")
library("dplyr")
library("ggplot2")
library("lubridate")
library("moments")
library("metRology")
library("MASS")

#Reading
chosen <- "EUO" #✎ Settable parameter(s)
getSymbols(chosen, from = Sys.Date() - years(10), to = Sys.Date()) #✎ Adjustable parameter(s)
log.returns <- map(chosen, ~ Ad(get(.x))) %>% #✎ Adjustable parameter(s)
  reduce(merge) %>%
  `colnames<-`(chosen) %>%
  {diff(log(.))[-1]}
tail(log.returns)

#Checking for normality
ggplot(data = data.frame(log.returns = as.numeric(log.returns)), aes(x = log.returns)) +
  geom_histogram(binwidth = 0.005, fill = "violet", color = "darkviolet", aes(y = after_stat(density))) +
  geom_density(alpha = 0.5, fill = "magenta", color = "darkmagenta") +
  labs(title = paste("Distribution of Log Returns for", chosen), x = "Log Returns", y = "Density") +
  theme_minimal()

cat("Skewness:", skewness(log.returns), "Kurtosis:", kurtosis(log.returns), sep = "\n")

if (jarque.test(as.vector(log.returns))$p.value < 0.05) {
  cat("The distribution is not normal according to the Jarque-Bera test")
} else {
  cat("The distribution appears to be normal according to the Jarque-Bera test")
}

#Adaptation for Student's t-Distribution
sample_t <- rt.scaled(10000, mean = fitdistr(as.vector(log.returns), "t")$estimate[1], #✎ Adjustable parameter(s)
                      sd = fitdistr(as.vector(log.returns), "t")$estimate[2],
                      df = fitdistr(as.vector(log.returns), "t")$estimate[3])

ggplot(data = data.frame(sample_t = as.numeric(sample_t)), aes(x = sample_t)) +
  geom_histogram(binwidth = 0.005, fill = "hotpink", color = "deeppink", aes(y = after_stat(density))) +
  geom_density(alpha = 0.5, fill = "violetred", color = "violetred4") +
  labs(title = paste("Student's t-Distribution Simulation for", chosen), x = "Log Returns", y = "Density") +
  theme_minimal()

cat("Value at Risk:", quantile(sample_t, 0.05), "Expected Shortfall:", mean(sample_t[sample_t < quantile(sample_t, 0.05)]), sep = "\n")
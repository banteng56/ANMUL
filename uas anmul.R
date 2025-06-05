# ======================
# 0. LOAD LIBRARY
# ======================
library(dplyr)
library(lavaan)
library(psych)
library(corrplot)
library(semPlot)
# ======================
# 1. LOAD DATA
# ======================
data <- read.csv("C:/dokumen/semester 4/uas-anmul/merged_student_sem.csv")

# ======================
# 2. CLEAN & TRANSFORMASI
# ======================
colnames(data) <- make.names(colnames(data)) 

# Recode variabel kategori jadi numerik
data$Motivation <- recode(data$Motivation, "Low" = 1, "Medium" = 2, "High" = 3)
data$Stress <- recode(data$Stress, "Low" = 1, "Medium" = 2, "High" = 3)
data$Gender <- ifelse(data$Gender == "M", 1, 0)  # M = 1, F = 0
data$Income <- recode(data$Income, "Low" = 1, "Medium" = 2, "High" = 3)
data$Parental_Edu <- recode(data$Parental_Edu,
                            "High School" = 1, "Bachelor's" = 2,
                            "Master's" = 3, "PhD" = 4)
data$Final_Grade <- recode(data$Final_Grade, "C" = 1, "B" = 2, "A" = 3)

# Pilih kolom yang relevan dan buang NA
data_cfa <- data %>%
  select(Motivation, Stress, Sleep, math.score, reading.score, writing.score) %>%
  na.omit()

# ======================
# 3. STANDARISASI
# ======================
data_scaled <- as.data.frame(scale(data_cfa))  # z-score scaling

# ======================
# 4. CEK KORELASI
# ======================
corrplot(cor(data_scaled), method = "circle", type = "lower", tl.cex = 0.8)

# ======================
# 5. MODEL CFA (Sederhana & Stabil)
# ======================
model_cfa <- '
  PsychologicalWellbeing =~ Motivation + Stress + Sleep
  AcademicPerformance =~ math.score + reading.score + writing.score
'

fit_cfa <- cfa(model_cfa, data = data_scaled, estimator = "MLR")
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

# ======================
# 6. EVALUASI FIT MODEL
# ======================
fitMeasures(fit_cfa, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))

# ======================
# 7. VISUALISASI
# ======================
semPaths(fit_cfa,
         what = "std",         
         layout = "tree",
         edge.label.cex = 1.1,
         sizeMan = 6,
         sizeLat = 8,
         fade = FALSE,
         color = list(lat = "lightblue", man = "lightyellow"))



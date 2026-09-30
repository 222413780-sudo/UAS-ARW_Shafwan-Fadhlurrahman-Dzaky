# Load package
library(readxl)
library(urca)

# 1. PERSIAPAN DATA
data <- read_excel("D:/STIS/Semester 4/ARW/New folder/Data No 3.xlsx")

colnames(data)[3] <- "Konsumsi"
colnames(data)[4] <- "PDB"

ts_konsumsi <- ts(
  data$Konsumsi,
  start = c(2010, 1),
  frequency = 4
)

ts_pdb <- ts(
  data$PDB,
  start = c(2010, 1),
  frequency = 4
)

plot(
  cbind(ts_konsumsi, ts_pdb),
  main = "Tren Konsumsi Rumah Tangga dan PDB Indonesia"
)

# 2. UJI STASIONERITAS ADF
summary(
  ur.df(ts_konsumsi, type = "none", lags = 1)
)

summary(
  ur.df(ts_pdb, type = "none", lags = 1)
)

# Uji setelah differencing
dkonsumsi <- diff(ts_konsumsi)
dpdb <- diff(ts_pdb)

summary(
  ur.df(dkonsumsi, type = "none", lags = 1)
)

summary(
  ur.df(dpdb, type = "none", lags = 1)
)

# 3. REGRESI JANGKA PANJANG
model_jp <- lm(ts_konsumsi ~ ts_pdb)

summary(model_jp)

# Uji kointegrasi Engle-Granger
resid_jp <- residuals(model_jp)

uji_kointegrasi <- ur.df(
  resid_jp,
  type = "none",
  lags = 1
)

summary(uji_kointegrasi)

# 4. ESTIMASI ERROR CORRECTION MODEL
ect_1 <- resid_jp[1:(length(resid_jp) - 1)]

model_ecm <- lm(
  dkonsumsi ~ dpdb + ect_1
)

summary(model_ecm)
# Load package
library(readxl)
library(urca)
library(vars)

# 1. PERSIAPAN DAN PEMBERSIHAN DATA
data <- read_excel("D:/STIS/Semester 4/ARW/New folder/Data NO 2.xlsx")

data$tukar_usd <- as.numeric(
  gsub(",", "", data$tukar_usd)
)

ts_data <- ts(
  data[, c("inflasi", "suku_bunga", "tukar_usd")],
  start = c(2011, 1),
  frequency = 12
)

# 2. UJI STASIONERITAS ADF
adf_inflasi <- ur.df(
  ts_data[, "inflasi"],
  type = "none",
  lags = 2
)

adf_suku_bunga <- ur.df(
  ts_data[, "suku_bunga"],
  type = "none",
  lags = 2
)

adf_kurs <- ur.df(
  ts_data[, "tukar_usd"],
  type = "none",
  lags = 2
)

summary(adf_inflasi)
summary(adf_suku_bunga)
summary(adf_kurs)

# Uji ADF setelah differencing
adf_diff_inflasi <- ur.df(
  diff(ts_data[, "inflasi"]),
  type = "none",
  lags = 2
)

adf_diff_suku_bunga <- ur.df(
  diff(ts_data[, "suku_bunga"]),
  type = "none",
  lags = 2
)

adf_diff_kurs <- ur.df(
  diff(ts_data[, "tukar_usd"]),
  type = "none",
  lags = 2
)

summary(adf_diff_inflasi)
summary(adf_diff_suku_bunga)
summary(adf_diff_kurs)

# 3. PENENTUAN LAG OPTIMAL
lag_opt <- VARselect(
  ts_data,
  lag.max = 10,
  type = "const"
)

print(lag_opt$selection)

p_opt <- lag_opt$selection["AIC(n)"]

cat(
  "Lag optimum berdasarkan AIC:",
  p_opt,
  "\n"
)

# 4. UJI KOINTEGRASI JOHANSEN
johansen_test <- ca.jo(
  ts_data,
  type = "trace",
  ecdet = "const",
  K = p_opt
)

summary(johansen_test)
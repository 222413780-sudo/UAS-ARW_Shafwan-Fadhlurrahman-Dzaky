# Load package
library(readxl)
library(urca)
library(forecast)
library(lmtest)
library(nortsTest)
library(rugarch)

# 1. PERSIAPAN DATA
PGAS <- read_excel("D:/STIS/Semester 4/ARW/New folder/DATA PGAS.xlsx")

pgas <- data.frame(PGAS)

pgas$Date <- as.Date(pgas$Date)
pgas <- pgas[order(pgas$Date), ]

# 2. MENGHITUNG RETURN
harga <- pgas$Close

ret <- diff(log(harga)) * 100
ret <- na.omit(ret)

# 3. PLOT RETURN HARIAN
plot(
  pgas$Date[-1],
  ret,
  type = "l",
  main = "Return Harian Saham PGAS",
  ylab = "Return (%)",
  xlab = "Tanggal"
)

abline(h = 0, col = "red", lty = 2)

# 4. UJI STASIONERITAS ADF
adf_ret <- ur.df(
  ret,
  type = "none",
  lags = 2
)

summary(adf_ret)

# 5. PEMILIHAN MODEL ARIMA
best_model <- auto.arima(
  ret,
  trace = TRUE
)

print(best_model)

# 6. MENGAMBIL ORDE ARIMA
p_order <- arimaorder(best_model)[1]
q_order <- arimaorder(best_model)[3]

# 7. RESIDUAL MODEL ARIMA
res_ret <- resid(best_model)

# 8. UJI EFEK ARCH
uji_arch <- arch.test(res_ret)

print(uji_arch)

p_value_arch <- uji_arch$p.value

# 9. PEMODELAN VOLATILITAS
if (p_value_arch < 0.05) {
  
  # ARCH(1)
  spec_arch1 <- ugarchspec(
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1, 0)
    ),
    mean.model = list(
      armaOrder = c(p_order, q_order),
      include.mean = TRUE
    )
  )
  
  fit_arch1 <- ugarchfit(
    spec_arch1,
    data = ret
  )
  
  # GARCH(1,1)
  spec_garch11 <- ugarchspec(
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1, 1)
    ),
    mean.model = list(
      armaOrder = c(p_order, q_order),
      include.mean = TRUE
    )
  )
  
  fit_garch11 <- ugarchfit(
    spec_garch11,
    data = ret
  )
  
  # EGARCH(1,1)
  spec_egarch11 <- ugarchspec(
    variance.model = list(
      model = "eGARCH",
      garchOrder = c(1, 1)
    ),
    mean.model = list(
      armaOrder = c(p_order, q_order),
      include.mean = TRUE
    ),
    distribution.model = "norm"
  )
  
  fit_egarch11 <- ugarchfit(
    spec_egarch11,
    data = ret,
    solver = "hybrid"
  )
  
  # TGARCH / GJR-GARCH(1,1)
  spec_tgarch11 <- ugarchspec(
    variance.model = list(
      model = "gjrGARCH",
      garchOrder = c(1, 1)
    ),
    mean.model = list(
      armaOrder = c(p_order, q_order),
      include.mean = TRUE
    ),
    distribution.model = "norm"
  )
  
  fit_tgarch11 <- ugarchfit(
    spec_tgarch11,
    data = ret
  )
  
  # 10. PERBANDINGAN AIC
  aic_arch1 <- infocriteria(fit_arch1)[1, 1]
  aic_garch11 <- infocriteria(fit_garch11)[1, 1]
  aic_egarch11 <- infocriteria(fit_egarch11)[1, 1]
  aic_tgarch11 <- infocriteria(fit_tgarch11)[1, 1]
  
  cat("========== PERBANDINGAN AIC ==========\n")
  
  cat("ARCH(1) : AIC =", aic_arch1, "\n")
  cat("GARCH(1,1) : AIC =", aic_garch11, "\n")
  cat("EGARCH(1,1) : AIC =", aic_egarch11, "\n")
  cat("TGARCH(1,1) : AIC =", aic_tgarch11, "\n")
  
  # 11. MENENTUKAN MODEL TERBAIK
  aic_values <- c(
    aic_arch1,
    aic_garch11,
    aic_egarch11,
    aic_tgarch11
  )
  
  model_names <- c(
    "ARCH(1)",
    "GARCH(1,1)",
    "EGARCH(1,1)",
    "TGARCH(1,1)"
  )
  
  model_fits <- list(
    fit_arch1,
    fit_garch11,
    fit_egarch11,
    fit_tgarch11
  )
  
  model_index <- which.min(aic_values)
  
  model_terbaik <- model_fits[[model_index]]
  nama_model_terbaik <- model_names[model_index]
  
  cat("========== MODEL TERBAIK ==========\n")
  
  cat("Model =", nama_model_terbaik, "\n")
  cat("AIC =", aic_values[model_index], "\n")
  
  # 12. OUTPUT MODEL TERBAIK
  show(model_terbaik)
  
  # 13. CONDITIONAL VOLATILITY
  conditional_volatility <- sigma(model_terbaik)
  
  volatility_data <- data.frame(
    Date = pgas$Date[-1],
    Volatility = as.numeric(conditional_volatility)
  )
  
  # 14. PLOT CONDITIONAL VOLATILITY
  plot(
    volatility_data$Date,
    volatility_data$Volatility,
    type = "l",
    main = paste(
      "Conditional Volatility Saham PGAS -",
      nama_model_terbaik
    ),
    ylab = "Volatilitas",
    xlab = "Tanggal"
  )
}

# install.packages(c("xts","urca","forecast","tseries","FinTS","rugarch"))  # sekali saja
library(xts)
library(urca)
library(forecast)
library(tseries)
library(FinTS)
library(rugarch)

# Ganti path sesuai lokasi file
df <- read.csv(
  "C:/Users/paska/Downloads/ptba.csv",
  fileEncoding = "UTF-8-BOM",     # file diawali BOM
  colClasses = "character"
)

df$Tanggal <- as.Date(df$Tanggal, format = "%d/%m/%Y")          # format dd/mm/yyyy
df$Harga   <- as.numeric(gsub(".", "", df$Terakhir, fixed = TRUE)) # "3.090" -> 3090
df <- df[order(df$Tanggal), ]

stopifnot(!is.unsorted(df$Tanggal), !anyNA(df$Tanggal), !anyNA(df$Harga))
range(df$Tanggal)

price <- xts(df$Harga, order.by = df$Tanggal)
ret   <- na.omit(100 * diff(log(price)))   # log-return harian (%)
colnames(ret) <- "ret"
r <- as.numeric(ret)

plot(zoo::as.zoo(price), main = "Harga Penutupan PTBA", xlab = "", ylab = "Rp")
plot(zoo::as.zoo(ret), main = "Return Harian Saham PTBA", xlab = "", ylab = "Return (%)")

#(r deskriptif)
m <- mean(r); s <- sd(r)
c(n = length(r), mean = m, sd = s,
  skewness = mean((r - m)^3) / s^3,
  kurtosis = mean((r - m)^4) / s^4)
jarque.bera.test(r)


#(r stasioner)
adf_price <- ur.df(log(as.numeric(price)), type = "drift", selectlags = "AIC")
summary(adf_price)

adf_ret <- ur.df(r, type = "drift", selectlags = "AIC")
summary(adf_ret)

pp.test(r)

#(r model-mean)
par(mfrow = c(1, 2))
acf(r, lag.max = 24, main = "ACF return")
pacf(r, lag.max = 24, main = "PACF return")
par(mfrow = c(1, 1))
Box.test(r, lag = 12, type = "Ljung-Box")

# Bandingkan ARMA sederhana, pilih dengan BIC (lebih tahan overfitting daripada AIC)
kandidat_arma <- expand.grid(p = 0:2, q = 0:2)
kandidat_arma$BIC <- apply(kandidat_arma, 1, function(o) {
  f <- tryCatch(Arima(r, order = c(o["p"], 0, o["q"]), include.mean = TRUE), error = function(e) NULL)
  if (is.null(f)) NA else BIC(f)
})
kandidat_arma[order(kandidat_arma$BIC), ]

best_arma <- kandidat_arma[which.min(kandidat_arma$BIC), ]
p <- best_arma$p
q <- best_arma$q
cat("Mean equation terpilih: ARMA(", p, ",", q, ")\n")

fit_mean <- Arima(r, order = c(p, 0, q), include.mean = TRUE)
checkresiduals(fit_mean)
res_mean <- residuals(fit_mean)

#(uji arch)
sq_res <- res_mean^2
par(mfrow = c(1, 2))
acf(sq_res, lag.max = 24, main = "ACF residual^2")
pacf(sq_res, lag.max = 24, main = "PACF residual^2")
par(mfrow = c(1, 1))

Box.test(sq_res, lag = 12, type = "Ljung-Box")
ArchTest(res_mean, lags = 5)
ArchTest(res_mean, lags = 12)


 #(estimasi}
buat_fit <- function(vmodel, order, dist, archm = FALSE) {
  spec <- ugarchspec(
    variance.model = list(model = vmodel, garchOrder = order),
    mean.model = list(armaOrder = c(p, q), include.mean = TRUE,
                      archm = archm, archpow = 1),
    distribution.model = dist
  )
  fit <- tryCatch(ugarchfit(spec, data = ret, solver = "hybrid"), error = function(e) NULL)
  if (is.null(fit) || fit@fit$convergence != 0) return(NULL)
  fit
}

daftar <- list(
  ARCH1   = list("sGARCH",   c(1, 0), FALSE),
  ARCH2   = list("sGARCH",   c(2, 0), FALSE),
  GARCH11 = list("sGARCH",   c(1, 1), FALSE),
  GARCH12 = list("sGARCH",   c(1, 2), FALSE),
  GARCH21 = list("sGARCH",   c(2, 1), FALSE),
  IGARCH  = list("iGARCH",   c(1, 1), FALSE),
  EGARCH  = list("eGARCH",   c(1, 1), FALSE),
  TGARCH  = list("gjrGARCH", c(1, 1), FALSE),
  APARCH  = list("apARCH",   c(1, 1), FALSE),
  ARCH_M  = list("sGARCH",   c(1, 0), TRUE),
  GARCH_M = list("sGARCH",   c(1, 1), TRUE)
)

models <- list()
for (dist in c("norm", "std")) {          # Normal vs Student-t (ekor tebal)
  for (nm in names(daftar)) {
    d <- daftar[[nm]]
    f <- buat_fit(d[[1]], d[[2]], dist, d[[3]])
    if (!is.null(f)) models[[paste(nm, dist, sep = "_")]] <- f
  }
}
names(models)

#(cek-diagnostik)
hasil <- do.call(rbind, lapply(names(models), function(nm) {
  f  <- models[[nm]]
  sr <- as.numeric(residuals(f, standardize = TRUE))
  ic <- infocriteria(f)
  cm <- f@fit$robust.matcoef
  # parameter selain mu/ar/ma/shape harus signifikan (varians, asimetri, archm)
  # (beta1 pada IGARCH tidak diestimasi bebas sehingga p-value-nya NA, diabaikan)
  par_uji <- setdiff(rownames(cm), c("mu", "shape", grep("^ar[0-9]|^ma[0-9]", rownames(cm), value = TRUE)))
  data.frame(
    model   = nm,
    AIC     = ic[1],
    BIC     = ic[2],
    LB_res  = Box.test(sr,   lag = 12, type = "Ljung-Box")$p.value,
    LB_res2 = Box.test(sr^2, lag = 12, type = "Ljung-Box")$p.value,
    ARCH_LM = unname(ArchTest(sr, lags = 12)$p.value),
    maks_p_param = max(cm[par_uji, 4], na.rm = TRUE),
    persistensi  = persistence(f)
  )
}))
hasil[, -1] <- round(hasil[, -1], 4)
hasil[order(hasil$AIC), ]

#(pilih-model-valid)
# Syarat model valid:
# 1) tidak ada autokorelasi & efek ARCH tersisa (p > 0,05)
# 2) semua parameter varians/asimetri/archm signifikan (p < 0,05)
# 3) varians stasioner: persistensi (alpha + beta) < 1
# lalu dipilih AIC terkecil
valid <- hasil[
  hasil$LB_res > 0.05 &
    hasil$LB_res2 > 0.05 &
    hasil$ARCH_LM > 0.05 &
    hasil$maks_p_param < 0.05 &
    hasil$persistensi < 1,
]
valid <- valid[order(valid$AIC), ]
valid

# Selisih AIC/BIC total (bukan per observasi) terhadap model terbaik.
# Selisih < 2 praktis setara.
n_obs <- length(r)
data.frame(model = valid$model,
           dAIC = round((valid$AIC - valid$AIC[1]) * n_obs, 2),
           dBIC = round((valid$BIC - min(valid$BIC)) * n_obs, 2))

best_name <- valid$model[1]
# best_name <- "GARCH11_std"   # aktifkan jika ingin memakai GARCH(1,1)-t (lihat catatan)
best_fit  <- models[[best_name]]
cat("Model terbaik:", best_name, "\n")
show(best_fit)
round(best_fit@fit$robust.matcoef, 4)

#(evaluasi-model)
std_res <- as.numeric(residuals(best_fit, standardize = TRUE))

par(mfrow = c(1, 2))
acf(std_res,   main = "ACF standardized residuals")
acf(std_res^2, main = "ACF squared standardized residuals")
par(mfrow = c(1, 1))

Box.test(std_res,   lag = 12, type = "Ljung-Box")
Box.test(std_res^2, lag = 12, type = "Ljung-Box")
ArchTest(std_res, lags = 12)

signbias(best_fit)     # uji leverage effect
nyblom(best_fit)       # stabilitas parameter

# Persistensi dan half-life guncangan
persistence(best_fit)
halflife(best_fit)

# Volatilitas bersyarat
sig <- sigma(best_fit)
plot(zoo::as.zoo(sig), main = paste("Volatilitas bersyarat -", best_name),
     xlab = "", ylab = "sigma (%)", col = "firebrick")
sig[which.max(sig)]
round(tapply(as.numeric(sig), format(index(sig), "%Y"), mean), 3)


#(forecast)
fc <- ugarchforecast(best_fit, n.ahead = 10)
data.frame(hari_ke = 1:10,
           sigma   = round(as.numeric(sigma(fc)), 4),
           varians = round(as.numeric(sigma(fc))^2, 4))

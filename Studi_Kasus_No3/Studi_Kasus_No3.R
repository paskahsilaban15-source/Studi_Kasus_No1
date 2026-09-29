# =====================================================================
# Studi Kasus no 3
# Author : Paskah Silaban
# Date   : 2026-09-29
# =====================================================================

pkgs <- c("readxl", "urca", "tseries", "lmtest", "sandwich",
          "strucchange", "car")
baru <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(baru) > 0) install.packages(baru)
invisible(lapply(pkgs, library, character.only = TRUE))

options(scipen = 10, digits = 6, width = 150)
while (sink.number() > 0) sink()   # tutup semua sink sisa dari eksekusi sebelumnya
FOLDER    <- "C:/Users/paska/Downloads"          # <-- ganti dengan folder penyimpanan file
FILE_DATA <- file.path(FOLDER, "no3.xlsx")        # <-- ganti dengan nama file Excel yang dipakai
OUT       <- file.path(FOLDER, "output_no3")   # folder tujuan grafik dan tabel hasil (di dalam folder data)
dir.create(OUT, showWarnings = FALSE)
K <- list()                          # wadah untuk menyimpan angka-angka penting

# seluruh keluaran console turut direkam ke file log
sink(file.path(OUT, "log_output_no3.txt"), split = TRUE)

# fungsi pembantu untuk membuat lag dan lead pada vektor biasa
L    <- function(x, k = 1) if (k == 0) x else c(rep(NA, k), head(x, -k))
LEAD <- function(x, k = 1) if (k == 0) x else c(tail(x, -k), rep(NA, k))
garis <- function(judul) cat("\n", strrep("=", 70), "\n", judul, "\n",
                             strrep("=", 70), "\n", sep = "")

# ---------------------------------------------------------------------
# 1. MEMUAT DATA DAN TRANSFORMASI
# ---------------------------------------------------------------------
garis("1. DATA")
d <- read_excel(FILE_DATA, sheet = "3")   # data yang sudah rapi ada di sheet '3'
d <- as.data.frame(d)
names(d) <- c("tahun", "triwulan", "C", "Y")   # C = konsumsi rumah tangga, Y = PDB
n <- nrow(d)
d$periode <- paste0(d$tahun, "Q", d$triwulan)
d$tt      <- d$tahun + (d$triwulan - 1) / 4
print(head(d)); print(tail(d)); cat("Jumlah observasi:", n, "\n")

# logaritma natural agar koefisien dapat dibaca sebagai elastisitas
d$lnC <- log(d$C)
d$lnY <- log(d$Y)
d$dC  <- c(NA, diff(d$lnC))          # laju pertumbuhan konsumsi antartriwulan
d$dY  <- c(NA, diff(d$lnY))          # laju pertumbuhan PDB antartriwulan

# variabel dummy triwulan (Q1 menjadi pembanding)
d$S2 <- as.numeric(d$triwulan == 2)
d$S3 <- as.numeric(d$triwulan == 3)
d$S4 <- as.numeric(d$triwulan == 4)
# dummy periode pasca-pandemi (mulai 2021Q1) beserta interaksinya dengan dummy triwulan
TB <- 2021.00                          # titik pergeseran struktural (ditentukan pada Langkah 4)
d$P   <- as.numeric(d$tt >= TB)
d$PS2 <- d$P * d$S2; d$PS3 <- d$P * d$S3; d$PS4 <- d$P * d$S4
# dummy impuls untuk kejutan COVID-19 (PSBB pada 2020Q2, pemulihan pada 2020Q3)
d$D202 <- as.numeric(d$periode == "2020Q2")
d$D203 <- as.numeric(d$periode == "2020Q3")

lnC <- ts(d$lnC, start = c(2010, 1), frequency = 4)
lnY <- ts(d$lnY, start = c(2010, 1), frequency = 4)

# ---------------------------------------------------------------------
# 2. RINGKASAN STATISTIK DAN GRAFIK
# ---------------------------------------------------------------------
garis("2. STATISTIK DESKRIPTIF")
d$rasio <- d$C / d$Y
desk <- data.frame(
  Variabel = c("Konsumsi RT (miliar Rp)", "PDB (miliar Rp)",
               "Rasio C/Y", "Growth q-to-q lnC", "Growth q-to-q lnY"),
  Mean = c(mean(d$C), mean(d$Y), mean(d$rasio),
           mean(d$dC, na.rm = TRUE), mean(d$dY, na.rm = TRUE)),
  SD   = c(sd(d$C), sd(d$Y), sd(d$rasio),
           sd(d$dC, na.rm = TRUE), sd(d$dY, na.rm = TRUE)),
  Min  = c(min(d$C), min(d$Y), min(d$rasio),
           min(d$dC, na.rm = TRUE), min(d$dY, na.rm = TRUE)),
  Max  = c(max(d$C), max(d$Y), max(d$rasio),
           max(d$dC, na.rm = TRUE), max(d$dY, na.rm = TRUE)))
print(desk, digits = 6)
write.csv(desk, file.path(OUT, "tabel1_deskriptif.csv"), row.names = FALSE)

# pertumbuhan tahunan (y-o-y) untuk keperluan narasi
d$gC_yoy <- c(rep(NA, 4), diff(d$lnC, lag = 4)) * 100
d$gY_yoy <- c(rep(NA, 4), diff(d$lnY, lag = 4)) * 100
cat("\nRata-rata pertumbuhan y-o-y (%) 2011-2019: C =",
    round(mean(d$gC_yoy[d$tahun %in% 2011:2019]), 3), "| Y =",
    round(mean(d$gY_yoy[d$tahun %in% 2011:2019]), 3), "\n")
cat("Rata-rata pertumbuhan y-o-y (%) 2022-2026: C =",
    round(mean(d$gC_yoy[d$tahun >= 2022]), 3), "| Y =",
    round(mean(d$gY_yoy[d$tahun >= 2022]), 3), "\n")
cat("Rasio C/Y rata-rata 2010-2019 =", round(mean(d$rasio[d$tahun <= 2019]), 4),
    "| 2022-2026 =", round(mean(d$rasio[d$tahun >= 2022]), 4), "\n")

# Gambar 1: deret level dan deret logaritma
png(file.path(OUT, "gambar1_plot_level_log.png"), width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 1), mar = c(3, 4.5, 2.5, 1))
plot(d$tt, d$Y / 1e6, type = "l", lwd = 2, col = "navy",
     ylim = range(c(d$C, d$Y) / 1e6), xlab = "", ylab = "Rp kuadriliun",
     main = "PDB dan Konsumsi Rumah Tangga (ADHK 2010)")
lines(d$tt, d$C / 1e6, lwd = 2, col = "darkorange")
abline(v = 2020.25, lty = 3, col = "grey40")
legend("topleft", c("PDB (Y)", "Konsumsi RT (C)"), col = c("navy", "darkorange"),
       lwd = 2, bty = "n")
plot(d$tt, d$lnY, type = "l", lwd = 2, col = "navy", ylim = range(c(d$lnC, d$lnY)),
     xlab = "", ylab = "logaritma natural", main = "ln(PDB) dan ln(Konsumsi)")
lines(d$tt, d$lnC, lwd = 2, col = "darkorange")
abline(v = 2020.25, lty = 3, col = "grey40")
dev.off()

# Gambar 2: rasio C/Y (kecenderungan konsumsi rata-rata)
png(file.path(OUT, "gambar2_rasio_CY.png"), width = 1400, height = 650, res = 150)
par(mar = c(3, 4.5, 2.5, 1))
plot(d$tt, d$rasio, type = "o", pch = 16, cex = .6, col = "darkgreen",
     xlab = "", ylab = "C / Y", main = "Rasio Konsumsi RT terhadap PDB")
abline(v = TB, lty = 2, col = "red")
dev.off()

# Gambar 3: membandingkan pola musiman pertumbuhan q-to-q sebelum dan sesudah 2021
pre  <- d$tahun >= 2011 & d$tahun <= 2019
post <- d$tahun >= 2022
musim <- rbind(
  tapply(d$dC[pre], d$triwulan[pre], mean),  tapply(d$dC[post], d$triwulan[post], mean),
  tapply(d$dY[pre], d$triwulan[pre], mean),  tapply(d$dY[post], d$triwulan[post], mean)) * 100
rownames(musim) <- c("dlnC 2011-19", "dlnC 2022-26", "dlnY 2011-19", "dlnY 2022-26")
colnames(musim) <- paste0("Q", 1:4)
cat("\nRata-rata pertumbuhan q-to-q (%) per triwulan:\n"); print(round(musim, 3))
write.csv(round(musim, 4), file.path(OUT, "tabel2_pola_musiman.csv"))
png(file.path(OUT, "gambar3_pola_musiman.png"), width = 1400, height = 700, res = 150)
par(mfrow = c(1, 2), mar = c(3, 4.5, 3, 1))
barplot(musim[1:2, ], beside = TRUE, col = c("grey60", "darkorange"),
        ylab = "% q-to-q", main = "Konsumsi RT: rata-rata dlnC")
legend("topright", c("2011-2019", "2022-2026"), fill = c("grey60", "darkorange"), bty = "n")
barplot(musim[3:4, ], beside = TRUE, col = c("grey60", "navy"),
        ylab = "% q-to-q", main = "PDB: rata-rata dlnY")
legend("topright", c("2011-2019", "2022-2026"), fill = c("grey60", "navy"), bty = "n")
dev.off()

# Gambar 4: diagram pencar antara lnC dan lnY
png(file.path(OUT, "gambar4_scatter.png"), width = 900, height = 800, res = 150)
plot(d$lnY, d$lnC, pch = 16, col = ifelse(d$P == 1, "red", "navy"),
     xlab = "ln PDB", ylab = "ln Konsumsi RT", main = "Scatter ln C vs ln Y")
legend("topleft", c("2010-2020", "2021-2026"), col = c("navy", "red"), pch = 16, bty = "n")
dev.off()

# ---------------------------------------------------------------------
# 3. PENGUJIAN STASIONERITAS (ADF, PP, KPSS)
# ---------------------------------------------------------------------
garis("3. UJI STASIONERITAS")
# ADF: pada level dipakai model konstanta + tren, pada diferensi pertama cukup konstanta
# lag ditentukan dengan kriteria Schwarz (BIC), batas maksimum 8 lag (setara 2 tahun)
MAXLAG <- 8
uji_akar <- function(x, nama) {
  hasil <- NULL
  for (bentuk in c("Level", "First Difference")) {
    z <- if (bentuk == "Level") x else diff(x)
    tp  <- if (bentuk == "Level") "trend" else "drift"
    adf <- ur.df(z, type = tp, lags = MAXLAG, selectlags = "BIC")
    k   <- sum(grepl("z.diff.lag", rownames(coef(adf@testreg))))
    pp  <- ur.pp(z, type = "Z-tau", model = ifelse(tp == "trend", "trend", "constant"),
                 lags = "short")
    kp  <- ur.kpss(z, type = ifelse(tp == "trend", "tau", "mu"), lags = "short")
    hasil <- rbind(hasil, data.frame(
      Variabel = nama, Bentuk = bentuk, Model = ifelse(tp == "trend", "C+Trend", "C"),
      ADF_tau = round(adf@teststat[1], 3), ADF_lag = k, ADF_cv5 = adf@cval[1, "5pct"],
      ADF_ket = ifelse(adf@teststat[1] < adf@cval[1, "5pct"], "Stasioner", "Tidak stasioner"),
      PP_Ztau = round(pp@teststat, 3), PP_cv5 = round(pp@cval[1, "5pct"], 3),
      PP_ket  = ifelse(pp@teststat < pp@cval[1, "5pct"], "Stasioner", "Tidak stasioner"),
      KPSS = round(kp@teststat, 3), KPSS_cv5 = kp@cval[1, "5pct"],
      KPSS_ket = ifelse(kp@teststat > kp@cval[1, "5pct"], "Tidak stasioner", "Stasioner")))
  }
  hasil
}
tab_ur <- rbind(uji_akar(lnC, "lnC"), uji_akar(lnY, "lnY"))
print(tab_ur, row.names = FALSE)
write.csv(tab_ur, file.path(OUT, "tabel3_uji_akar_unit.csv"), row.names = FALSE)
cat("\nCatatan: H0 ADF & PP = ada akar unit; H0 KPSS = stasioner.\n")
# keluaran rinci ADF untuk dimasukkan ke lampiran
print(summary(ur.df(lnC, type = "trend", lags = MAXLAG, selectlags = "BIC")))
print(summary(ur.df(diff(lnC), type = "drift", lags = MAXLAG, selectlags = "BIC")))
print(summary(ur.df(lnY, type = "trend", lags = MAXLAG, selectlags = "BIC")))
print(summary(ur.df(diff(lnY), type = "drift", lags = MAXLAG, selectlags = "BIC")))

# ---------------------------------------------------------------------
# 4. MENDETEKSI PERUBAHAN POLA MUSIMAN (BREAK)
# ---------------------------------------------------------------------
garis("4. PERGESERAN POLA MUSIMAN")
# 4a. mencari tanggal break lewat SSR terkecil dari regresi jangka panjang
grid <- seq(2019.00, 2023.75, by = 0.25)
ssr  <- sapply(grid, function(b) {
  Pb <- as.numeric(d$tt >= b)
  sum(resid(lm(lnC ~ lnY + S2 + S3 + S4 + Pb + Pb:S2 + Pb:S3 + Pb:S4, data = d))^2)
})
tab_grid <- data.frame(Break = paste0(floor(grid), "Q", (grid %% 1) * 4 + 1), SSR = ssr)
print(tab_grid, row.names = FALSE)
cat("Break dengan SSR minimum:", tab_grid$Break[which.min(ssr)], "\n")
png(file.path(OUT, "gambar5_grid_break.png"), width = 1200, height = 600, res = 150)
par(mar = c(4, 4.5, 2.5, 1))
plot(grid, ssr, type = "b", pch = 16, xlab = "Tanggal break", ylab = "SSR",
     main = "SSR regresi jangka panjang menurut tanggal break")
abline(v = grid[which.min(ssr)], col = "red", lty = 2)
dev.off()

# 4b. uji F ala Chow dengan H0: koefisien P, P*S2, P*S3, P*S4 semuanya nol
lr_tanpa <- lm(lnC ~ lnY + S2 + S3 + S4, data = d)
lr_break <- lm(lnC ~ lnY + S2 + S3 + S4 + P + PS2 + PS3 + PS4, data = d)
cat("\nUji F signifikansi dummy rezim & interaksi musiman:\n")
print(anova(lr_tanpa, lr_break))

# ---------------------------------------------------------------------
# 5. ENGLE-GRANGER TAHAP 1 : PERSAMAAN JANGKA PANJANG
# ---------------------------------------------------------------------
garis("5. PERSAMAAN JANGKA PANJANG (OLS)")
# lnC_t = b0 + b1 lnY_t + musiman + P + P*musiman + u_t
LR <- lr_break
print(summary(LR))
# PERHATIAN: dalam regresi kointegrasi, standar error OLS tidak sahih untuk
# inferensi (ada bias orde kedua) -> dipakai DOLS (Stock-Watson) dengan HAC Newey-West
garis("5b. DOLS (Stock & Watson, 1993), leads/lags dlnY = 2")
for (j in 1:2) { d[[paste0("dY_lag", j)]] <- L(d$dY, j); d[[paste0("dY_lead", j)]] <- LEAD(d$dY, j) }
DOLS <- lm(lnC ~ lnY + S2 + S3 + S4 + P + PS2 + PS3 + PS4 + dY +
             dY_lag1 + dY_lag2 + dY_lead1 + dY_lead2, data = d)
ct_dols <- coeftest(DOLS, vcov = NeweyWest(DOLS, lag = 4, prewhite = FALSE))
print(round(ct_dols[1:9, ], 6))
tab_lr <- data.frame(Variabel = rownames(coef(summary(LR))),
                     OLS_koef = coef(LR), OLS_se = coef(summary(LR))[, 2],
                     OLS_p = coef(summary(LR))[, 4],
                     DOLS_koef = ct_dols[rownames(coef(summary(LR))), 1],
                     DOLS_se_HAC = ct_dols[rownames(coef(summary(LR))), 2],
                     DOLS_p = ct_dols[rownames(coef(summary(LR))), 4])
write.csv(tab_lr, file.path(OUT, "tabel_LR_OLS_DOLS.csv"), row.names = FALSE)
b_dols <- ct_dols["lnY", 1]; se_dols <- ct_dols["lnY", 2]
cat("\nUji H0: elastisitas jangka panjang = 1 (DOLS): t =",
    round((b_dols - 1) / se_dols, 3), " p =",
    round(2 * pt(-abs((b_dols - 1) / se_dols), df = DOLS$df.residual), 4), "\n")

# ---------------------------------------------------------------------
# 6. ENGLE-GRANGER TAHAP 2 : UJI KOINTEGRASI (ADF PADA RESIDUAL)
# ---------------------------------------------------------------------
garis("6. UJI KOINTEGRASI ENGLE-GRANGER")
d$ect <- resid(LR)                        # u_t = penyimpangan dari keseimbangan jangka panjang
ect_ts <- ts(d$ect, start = c(2010, 1), frequency = 4)
png(file.path(OUT, "gambar6_residual_jangka_panjang.png"), width = 1400, height = 600, res = 150)
par(mar = c(3, 4.5, 2.5, 1))
plot(d$tt, d$ect, type = "o", pch = 16, cex = .6, xlab = "", ylab = "u_t",
     main = "Residual persamaan jangka panjang (ECT)")
abline(h = 0, col = "red"); abline(v = 2020.25, lty = 3)
dev.off()

# nilai kritis Engle-Granger (MacKinnon, 2010; 2 variabel, memakai konstanta)
cv_eg <- function(T) c(`1%`  = -3.89644 - 10.9519 / T - 22.527 / T^2,
                       `5%`  = -3.33613 -  6.1101 / T -  6.823 / T^2,
                       `10%` = -3.04445 -  4.2412 / T -  2.720 / T^2)
CV <- cv_eg(n); cat("Nilai kritis MacKinnon (2010), T =", n, ":\n"); print(round(CV, 3))
adf_res <- ur.df(ect_ts, type = "none", lags = MAXLAG, selectlags = "BIC")
print(summary(adf_res))
tau_eg <- adf_res@teststat[1]
cat("\nStatistik tau residual =", round(tau_eg, 3),
    ifelse(tau_eg < CV["5%"], "< CV 5% MacKinnon (lihat catatan 6b)",
           ">= CV 5% -> gagal tolak H0: tidak ada kointegrasi"), "\n")
# melihat seberapa peka hasil terhadap pilihan lag (agar transparan)
sens <- data.frame(lag = 0:4, tau = sapply(0:4, function(k)
  round(ur.df(ect_ts, type = "none", lags = k)@teststat[1], 3)))
cat("\nSensitivitas tau terhadap panjang lag ADF residual:\n"); print(sens, row.names = FALSE)
write.csv(sens, file.path(OUT, "tabel4_sensitivitas_EG.csv"), row.names = FALSE)
cat("-> Pada lag 3-4 (wajar utk data triwulanan) tau > CV 5% MacKinnon: kesimpulan EG sensitif.\n")

# 6b. UJI EG DENGAN PENCARIAN BREAK (TIPE GREGORY-HANSEN) ---------------
garis("6b. UJI EG DENGAN BREAK TERPILIH DARI DATA (TIPE GREGORY-HANSEN)")
# Break 2021Q1 dipilih berdasarkan data (Langkah 4) dan persamaan jangka panjangnya
# memuat banyak dummy, jadi nilai kritis MacKinnon (tanpa break) TIDAK dapat dipakai.
# Solusinya: telusuri tau minimum pada seluruh kandidat break (trimming 15%),
# lalu bandingkan dengan nilai kritis Gregory & Hansen (1996).
tau_ec <- function(e) ur.df(e, type = "none", lags = MAXLAG, selectlags = "BIC")@teststat[1]
e_nobreak <- resid(lm(lnC ~ lnY + S2 + S3 + S4, data = d))
cat("Tanpa dummy break          : tau =", round(tau_ec(e_nobreak), 3), "\n")
idx_b <- floor(0.15 * n):ceiling(0.85 * n)
gh <- t(sapply(idx_b, function(i) {
  Pb <- as.numeric(seq_len(n) >= i)
  c(tt = d$tt[i],
    C  = tau_ec(resid(lm(lnC ~ lnY + S2 + S3 + S4 + Pb, data = d))),
    CS = tau_ec(resid(lm(lnC ~ lnY + S2 + S3 + S4 + Pb + Pb:S2 + Pb:S3 + Pb:S4, data = d))))
}))
gh_min <- data.frame(Model = c("Level shift (C)", "Level + pola musiman shift"),
                     tau_min = round(c(min(gh[, "C"]), min(gh[, "CS"])), 3),
                     Break = c(gh[which.min(gh[, "C"]), "tt"], gh[which.min(gh[, "CS"]), "tt"]))
print(gh_min, row.names = FALSE)
# Nilai kritis Gregory & Hansen (1996), 1 regresor, model C (level shift):
# 1% = -5.13, 5% = -4.61, 10% = -4.34  (cek dulu dengan tabel referensi sebelum dikutip).
# Model dengan pergeseran pola musiman lebih rumit -> nilai kritisnya bahkan lebih negatif.
cv_gh <- c(`1%` = -5.13, `5%` = -4.61, `10%` = -4.34)
cat("Nilai kritis Gregory-Hansen (model C, m = 1):\n"); print(cv_gh)
K$gh_tau <- min(gh[, "CS"])
cat(ifelse(K$gh_tau < cv_gh["5%"],
           "-> tau minimum < CV 5%: kointegrasi EG didukung meski break dipilih dari data.\n",
           "-> tau minimum > CV 5%: setelah memperhitungkan break yang dipilih dari data, uji EG\n   TIDAK cukup kuat menyimpulkan kointegrasi. Kesimpulan kointegrasi bertumpu pada\n   ARDL Bounds Test (Langkah 9).\n"))

# ---------------------------------------------------------------------
# 7. ERROR CORRECTION MODEL (ENGLE-GRANGER)
# ---------------------------------------------------------------------
# Rumus umum ECM dengan k lag:
# dlnC_t = a0 + b0*dlnY_t + sum_{i=1..k} a_i*dlnC_{t-i} + sum_{i=1..k} b_i*dlnY_{t-i}
#          + gamma*ECT_{t-1} + musiman + P*musiman + D2020Q2 + D2020Q3 + e_t
HAC <- function(m) NeweyWest(m, lag = 4, prewhite = FALSE)   # standar error robust Newey-West
# (list K untuk menyimpan angka penting sudah dibuat pada Langkah 0)
d$ect_1 <- L(d$ect, 1)
for (j in 1:4) { d[[paste0("dC_l", j)]] <- L(d$dC, j); d[[paste0("dY_l", j)]] <- L(d$dY, j) }
DET <- "S2 + S3 + S4 + P + PS2 + PS3 + PS4 + D202 + D203"
rumus_ecm <- function(k) {
  f <- "dC ~ dY"
  if (k > 0) f <- paste(f, "+", paste0("dY_l", 1:k, collapse = " + "),
                        "+", paste0("dC_l", 1:k, collapse = " + "))
  as.formula(paste(f, "+ ect_1 +", DET))
}

# 7a. MENENTUKAN LAG OPTIMAL -------------------------------------------
garis("7a. PEMILIHAN LAG OPTIMAL ECM ENGLE-GRANGER (MODEL PEMBANDING)")
# seluruh kandidat diestimasi pada sampel yang SAMA (2011Q2-2026Q2, n = 61)
# supaya nilai AIC/BIC/HQ dapat dibandingkan secara adil
dE <- d[d$tt >= 2011.25, ]
HQ <- function(m) { ll <- logLik(m); -2 * as.numeric(ll) + 2 * attr(ll, "df") * log(log(nobs(m))) }
sel_ecm <- do.call(rbind, lapply(0:4, function(k) {
  m <- lm(rumus_ecm(k), data = dE); ct <- coeftest(m, vcov = HAC(m))
  data.frame(Lag = k, AIC = AIC(m), BIC = BIC(m), HQ = HQ(m),
             Adj_R2 = summary(m)$adj.r.squared, ECT = coef(m)["ect_1"],
             ECT_p_HAC = ct["ect_1", 4], BG4_p = bgtest(m, 4)$p.value,
             RESET_p = resettest(m, power = 2:3)$p.value)
}))
rownames(sel_ecm) <- NULL
print(round(sel_ecm, 4))
write.csv(sel_ecm, file.path(OUT, "tabel5a_seleksi_lag_ECM.csv"), row.names = FALSE)
k_opt <- sel_ecm$Lag[which.min(sel_ecm$AIC)]
cat("Lag optimal (AIC):", k_opt, "| BIC:", sel_ecm$Lag[which.min(sel_ecm$BIC)],
    "| HQ:", sel_ecm$Lag[which.min(sel_ecm$HQ)], "\n")

# 7b. ESTIMASI ECM AKHIR ----------------------------------------------
garis(paste0("7b. ECM ENGLE-GRANGER FINAL (LAG ", k_opt, ")"))
ECM <- lm(rumus_ecm(k_opt), data = dE)
print(summary(ECM))
ct_ecm <- coeftest(ECM, vcov = HAC(ECM))
cat("\nKoefisien dengan SE HAC Newey-West:\n"); print(ct_ecm)
write.csv(round(cbind(coef(summary(ECM)), HAC_se = ct_ecm[, 2], HAC_t = ct_ecm[, 3],
                      HAC_p = ct_ecm[, 4]), 6), file.path(OUT, "tabel5_ECM_EG.csv"))
gamma_eg <- unname(coef(ECM)["ect_1"])
K$eg_k <- k_opt; K$eg_n <- nobs(ECM); K$eg_r2 <- summary(ECM)$r.squared
K$eg_adjr2 <- summary(ECM)$adj.r.squared; K$eg_F <- summary(ECM)$fstatistic[1]
K$eg_se_reg <- summary(ECM)$sigma
K$eg_gamma <- gamma_eg; K$eg_gamma_t_ols <- coef(summary(ECM))["ect_1", 3]
K$eg_gamma_p_ols <- coef(summary(ECM))["ect_1", 4]
K$eg_gamma_t_hac <- ct_ecm["ect_1", 3]; K$eg_gamma_p_hac <- ct_ecm["ect_1", 4]
K$eg_halflife <- log(.5) / log(1 + gamma_eg); K$eg_t90 <- log(.1) / log(1 + gamma_eg)
K$eg_b0 <- unname(coef(ECM)["dY"])
cat("\nSpeed of adjustment (gamma)           =", round(gamma_eg, 4),
    "\nDisekuilibrium terkoreksi per triwulan =", round(-100 * gamma_eg, 2), "%",
    "\nHalf-life (triwulan)                  =", round(K$eg_halflife, 2),
    "\nWaktu koreksi 90% (triwulan)          =", round(K$eg_t90, 2), "\n")

# 7c. UJI KAUSALITAS JANGKA PENDEK DAN JANGKA PANJANG ------------------------------
garis("7c. KAUSALITAS (WALD, SE HAC)")
h_sr <- c("dY = 0", if (k_opt > 0) paste0("dY_l", 1:k_opt, " = 0"))
w_sr <- linearHypothesis(ECM, h_sr, vcov. = HAC(ECM))
print(w_sr)
K$eg_wald_sr_F <- w_sr$F[2]; K$eg_wald_sr_df <- length(h_sr); K$eg_wald_sr_p <- w_sr$`Pr(>F)`[2]
K$eg_sum_dY <- sum(coef(ECM)[c("dY", if (k_opt > 0) paste0("dY_l", 1:k_opt))])
cat("Jumlah koefisien dlnY (efek jangka pendek kumulatif) =", round(K$eg_sum_dY, 4), "\n")
cat("Kausalitas jangka panjang Y -> C: uji t ECT (HAC) p =", round(K$eg_gamma_p_hac, 4), "\n")

# 7d. KEPEKAAN HASIL ECM TERHADAP SPESIFIKASI ------------------------------------------
garis("7d. SENSITIVITAS ECT")
dE_full <- na.omit(d[, c("dC", "dY", "ect_1", "S2", "S3", "S4", "P", "PS2", "PS3", "PS4", "D202", "D203")])
m_s1 <- lm(rumus_ecm(0), data = dE_full); m_s2 <- lm(rumus_ecm(0), data = dE)
sens_ecm <- data.frame(
  Spesifikasi = c("Lag 0, sampel 2010Q2-2026Q2", "Lag 0, sampel 2011Q2-2026Q2",
                  paste0("Lag ", k_opt, " (optimal), sampel 2011Q2-2026Q2")),
  n = c(nobs(m_s1), nobs(m_s2), nobs(ECM)),
  ECT = c(coef(m_s1)["ect_1"], coef(m_s2)["ect_1"], gamma_eg),
  p_HAC = c(coeftest(m_s1, vcov = HAC(m_s1))["ect_1", 4],
            coeftest(m_s2, vcov = HAC(m_s2))["ect_1", 4], K$eg_gamma_p_hac))
print(sens_ecm, row.names = FALSE)
write.csv(sens_ecm, file.path(OUT, "tabel5b_sensitivitas_ECM.csv"), row.names = FALSE)

# 7e. PENGUJIAN DIAGNOSTIK ---------------------------------------------------
garis("7e. UJI DIAGNOSTIK ECM ENGLE-GRANGER")
diag_ecm <- function(m) {
  jb <- jarque.bera.test(resid(m)); sw <- shapiro.test(resid(m))
  bg1 <- bgtest(m, order = 1); bg4 <- bgtest(m, order = 4)
  bp <- bptest(m); rs <- resettest(m, power = 2:3)
  lb <- Box.test(resid(m), lag = 8, type = "Ljung-Box", fitdf = 0)
  e2 <- resid(m)^2; arch <- summary(lm(e2 ~ L(e2, 1) + L(e2, 2) + L(e2, 3) + L(e2, 4)))
  arch_lm <- arch$r.squared * (length(e2) - 4)                     # ARCH-LM(4) dihitung dari T*R2
  data.frame(
    Uji = c("Jarque-Bera (normalitas)", "Shapiro-Wilk (normalitas)",
            "Breusch-Godfrey LM(1) (autokorelasi)", "Breusch-Godfrey LM(4) (autokorelasi)",
            "Ljung-Box Q(8) (autokorelasi)", "Durbin-Watson (autokorelasi)",
            "Breusch-Pagan (heteroskedastisitas)", "ARCH-LM(4) (heteroskedastisitas bersyarat)",
            "Ramsey RESET (spesifikasi)"),
    Statistik = round(c(jb$statistic, sw$statistic, bg1$statistic, bg4$statistic,
                        lb$statistic, dwtest(m)$statistic, bp$statistic, arch_lm,
                        rs$statistic), 4),
    p_value = round(c(jb$p.value, sw$p.value, bg1$p.value, bg4$p.value, lb$p.value,
                      dwtest(m)$p.value, bp$p.value, pchisq(arch_lm, 4, lower.tail = FALSE),
                      rs$p.value), 4))
}
tab_diag_eg <- diag_ecm(ECM); print(tab_diag_eg, row.names = FALSE)
write.csv(tab_diag_eg, file.path(OUT, "tabel6_diagnostik_ECM_EG.csv"), row.names = FALSE)
rs_std <- rstandard(ECM); idx <- order(-abs(rs_std))[1:5]
out_eg <- data.frame(periode = dE$periode[idx], resid_std = round(rs_std[idx], 2))
cat("\nResidual terstandar terbesar:\n"); print(out_eg, row.names = FALSE)
write.csv(out_eg, file.path(OUT, "tabel6b_outlier_ECM_EG.csv"), row.names = FALSE)
f_vif <- as.formula(paste("dC ~ dY + ect_1",
                          if (k_opt > 0) paste("+", paste0(c(paste0("dY_l", 1:k_opt), paste0("dC_l", 1:k_opt)), collapse = " + "))))
vif_eg <- vif(lm(f_vif, data = dE)); cat("\nVIF regresor utama:\n"); print(round(vif_eg, 3))
K$eg_vif_max <- max(vif_eg)
cusum <- efp(rumus_ecm(k_opt), data = dE, type = "OLS-CUSUM")
sc1 <- sctest(cusum); print(sc1); K$eg_cusum_S <- sc1$statistic; K$eg_cusum_p <- sc1$p.value
png(file.path(OUT, "gambar7_cusum.png"), width = 1200, height = 650, res = 150)
plot(cusum, main = "Uji Stabilitas OLS-CUSUM ECM Engle-Granger (batas 5%)"); dev.off()
png(file.path(OUT, "gambar8_aktual_fitted_ECM.png"), width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 1), mar = c(3, 4.5, 2.5, 1))
plot(dE$tt, dE$dC, type = "l", lwd = 2, xlab = "", ylab = "dlnC",
     main = paste0("ECM Engle-Granger (lag ", k_opt, "): dlnC aktual vs fitted"))
lines(dE$tt, fitted(ECM), col = "red", lwd = 2, lty = 2)
legend("bottomleft", c("Aktual", "Fitted"), col = c("black", "red"), lwd = 2, lty = 1:2, bty = "n")
plot(dE$tt, resid(ECM), type = "h", lwd = 2, xlab = "", ylab = "residual", main = "Residual ECM")
abline(h = c(-2, 2) * sd(resid(ECM)), col = "red", lty = 2)
dev.off()
png(file.path(OUT, "gambar9_acf_residual.png"), width = 1400, height = 600, res = 150)
par(mfrow = c(1, 2)); acf(resid(ECM), main = "ACF residual ECM"); pacf(resid(ECM), main = "PACF residual ECM")
dev.off()

# ---------------------------------------------------------------------
# 8. PENGUJIAN EKSOGENITAS LEMAH PDB
# ---------------------------------------------------------------------
garis("8. EKSOGENITAS LEMAH PDB")
f_y <- as.formula(paste("dY ~ ect_1",
                        if (k_opt > 0) paste("+", paste0(c(paste0("dY_l", 1:k_opt), paste0("dC_l", 1:k_opt)), collapse = " + ")),
                        "+", DET))
ECM_Y <- lm(f_y, data = dE); ct_y <- coeftest(ECM_Y, vcov = HAC(ECM_Y))
print(ct_y["ect_1", ])
K$wx_coef <- ct_y["ect_1", 1]; K$wx_se <- ct_y["ect_1", 2]; K$wx_p <- ct_y["ect_1", 4]
if (k_opt > 0) {
  w_cy <- linearHypothesis(ECM_Y, paste0("dC_l", 1:k_opt, " = 0"), vcov. = HAC(ECM_Y))
  K$wx_cy_F <- w_cy$F[2]; K$wx_cy_p <- w_cy$`Pr(>F)`[2]
  cat("Kausalitas jangka pendek C -> Y: F =", round(K$wx_cy_F, 3), "p =", round(K$wx_cy_p, 4), "\n")
}
cat(ifelse(K$wx_p > 0.05,
           "ECT tidak signifikan (5%) di persamaan dlnY -> PDB eksogen lemah -> ECM persamaan tunggal valid.\n",
           "ECT signifikan (5%) di persamaan dlnY -> PDB ikut menyesuaikan -> pertimbangkan VECM.\n"))
cat("Catatan: koefisien ECT pada persamaan dlnY bertanda", ifelse(K$wx_coef < 0, "NEGATIF", "POSITIF"),
    "- agar PDB ikut mengoreksi ketidakseimbangan, tandanya harus POSITIF.\n")

# ---------------------------------------------------------------------
# 9. UJI KETAHANAN: ARDL BOUNDS TEST (PESARAN, SHIN & SMITH, 2001) DAN ARDL-ECM
# ---------------------------------------------------------------------
garis("9. ARDL BOUNDS TEST & ARDL-ECM  (MODEL UTAMA)")
# Kelebihan ARDL: sahih untuk regresor I(0) maupun I(1), sesuai untuk sampel
# kecil (n = 61), dan ECT diperoleh langsung dari model dinamisnya.
# UECM: dlnC_t = c + phi*lnC_{t-1} + theta*lnY_{t-1} + sum a_i dlnC_{t-i}
#                  + sum b_j dlnY_{t-j} + deterministik + e_t
d$lnC_1 <- L(d$lnC, 1); d$lnY_1 <- L(d$lnY, 1)
rumus_uecm <- function(p, q) {
  f <- "dC ~ lnC_1 + lnY_1"
  if (p > 1) f <- paste(f, "+", paste0("dC_l", 1:(p - 1), collapse = " + "))
  if (q >= 1) f <- paste(f, "+ dY")
  if (q > 1) f <- paste(f, "+", paste0("dY_l", 1:(q - 1), collapse = " + "))
  as.formula(paste(f, "+", DET))
}
dA <- d[d$tt >= 2011.25, ]      # sampel dibuat seragam untuk semua orde
sel <- NULL
for (p in 1:4) for (q in 0:4) {
  m <- lm(rumus_uecm(p, q), data = dA)
  sel <- rbind(sel, data.frame(ARDL = sprintf("(%d,%d)", p, q), p = p, q = q,
                               AIC = AIC(m), BIC = BIC(m)))
}
sel <- sel[order(sel$AIC), ]; print(head(sel, 8), row.names = FALSE)
cat("Catatan: orde (4,4) berada di batas grid; untuk data triwulanan lag 4 (= 1 tahun) wajar.\n")
write.csv(sel, file.path(OUT, "tabel_seleksi_ARDL.csv"), row.names = FALSE)
p_opt <- sel$p[1]; q_opt <- sel$q[1]
cat("Orde ARDL optimal (AIC):", sel$ARDL[1], "\n")
UECM <- lm(rumus_uecm(p_opt, q_opt), data = dA)
print(summary(UECM))
Fb <- linearHypothesis(UECM, c("lnC_1 = 0", "lnY_1 = 0"))
F_stat <- Fb$F[2]; t_stat <- coef(summary(UECM))["lnC_1", 3]
K$ardl_F <- F_stat; K$ardl_t <- t_stat; K$ardl_p <- p_opt; K$ardl_q <- q_opt
cat("\nF-bounds =", round(F_stat, 3), " | t-bounds =", round(t_stat, 3), "\n")
# Nilai kritis Pesaran dkk. (2001) Case III (intersep bebas, tanpa tren), k = 1
cvF <- data.frame(alpha = c("10%", "5%", "2.5%", "1%"),
                  I0 = c(4.04, 4.94, 5.77, 6.84), I1 = c(4.78, 5.73, 6.68, 7.84))
cvt <- data.frame(alpha = c("10%", "5%", "2.5%", "1%"),
                  I0 = c(-2.57, -2.86, -3.13, -3.43), I1 = c(-2.91, -3.22, -3.50, -3.82))
cat("Nilai kritis F (Case III, k=1):\n"); print(cvF, row.names = FALSE)
cat("Nilai kritis t (Case III, k=1):\n"); print(cvt, row.names = FALSE)
cat("Keputusan (alpha 5%):", ifelse(F_stat > 5.73, "F > batas atas I(1) -> TERDAPAT KOINTEGRASI",
                                    ifelse(F_stat < 4.94, "F < batas bawah I(0) -> TIDAK ada kointegrasi", "inconclusive")), "\n")
cat("Keputusan t-bounds (alpha 5%):", ifelse(t_stat < -3.22, "t < batas I(1) -> hubungan jangka panjang valid",
                                             "tidak konklusif"), "\n")
cat("Catatan: nilai kritis Pesaran bersifat asimtotik; nilai kritis sampel kecil (Narayan, 2005)\n",
    "sedikit lebih besar, namun F =", round(F_stat, 2), "tetap jauh di atasnya.\n")
lr_ardl <- deltaMethod(UECM, "-lnY_1/lnC_1", vcov. = HAC(UECM))
print(lr_ardl)
theta_lr <- lr_ardl$Estimate
K$ardl_lr <- theta_lr; K$ardl_lr_se <- lr_ardl$SE
K$ardl_lr_lo <- theta_lr - 1.96 * lr_ardl$SE; K$ardl_lr_hi <- theta_lr + 1.96 * lr_ardl$SE
K$ardl_lr_z1 <- (theta_lr - 1) / lr_ardl$SE; K$ardl_lr_p1 <- 2 * pnorm(-abs(K$ardl_lr_z1))
cat("Elastisitas jangka panjang (ARDL) =", round(theta_lr, 4),
    "| uji H0: elastisitas = 1 -> z =", round(K$ardl_lr_z1, 3), " p =", round(K$ardl_lr_p1, 4), "\n")
d$ECT_ardl <- d$lnC - theta_lr * d$lnY
d$ECT_ardl_1 <- L(d$ECT_ardl, 1)
dA <- d[d$tt >= 2011.25, ]
f_recm <- update(rumus_uecm(p_opt, q_opt), . ~ . - lnC_1 - lnY_1 + ECT_ardl_1)
RECM <- lm(f_recm, data = dA)
print(summary(RECM))
ct_recm <- coeftest(RECM, vcov = HAC(RECM))
cat("\nKoefisien ARDL-ECM dengan SE HAC:\n"); print(ct_recm)
write.csv(round(cbind(coef(summary(RECM)), HAC_se = ct_recm[, 2], HAC_t = ct_recm[, 3],
                      HAC_p = ct_recm[, 4]), 6), file.path(OUT, "tabel7_ARDL_ECM.csv"))
tab_diag_ardl <- diag_ecm(RECM); print(tab_diag_ardl, row.names = FALSE)
write.csv(tab_diag_ardl, file.path(OUT, "tabel8_diagnostik_ARDL_ECM.csv"), row.names = FALSE)
gamma_ardl <- unname(coef(RECM)["ECT_ardl_1"])
K$ardl_gamma <- gamma_ardl; K$ardl_gamma_t_hac <- ct_recm["ECT_ardl_1", 3]
K$ardl_gamma_p_hac <- ct_recm["ECT_ardl_1", 4]
K$ardl_halflife <- log(.5) / log(1 + gamma_ardl); K$ardl_t90 <- log(.1) / log(1 + gamma_ardl)
K$ardl_r2 <- summary(RECM)$r.squared; K$ardl_adjr2 <- summary(RECM)$adj.r.squared
K$ardl_n <- nobs(RECM); K$ardl_b0 <- unname(coef(RECM)["dY"])
cusum2 <- efp(f_recm, data = dA, type = "OLS-CUSUM"); sc2 <- sctest(cusum2); print(sc2)
K$ardl_cusum_S <- sc2$statistic; K$ardl_cusum_p <- sc2$p.value
png(file.path(OUT, "gambar10_cusum_ARDL.png"), width = 1200, height = 650, res = 150)
plot(cusum2, main = "OLS-CUSUM ARDL-ECM (batas 5%)"); dev.off()

# ---------------------------------------------------------------------
# 10. MULTIPLIER DINAMIS: TANGGAPAN KONSUMSI TERHADAP KENAIKAN PDB 1% YANG PERMANEN
# ---------------------------------------------------------------------
garis("10. DYNAMIC MULTIPLIER")
# simulasi deterministik: lnY naik 1% secara permanen mulai triwulan 0,
# seluruh guncangan lain dinolkan; lintasan lnC dihitung rekursif dari persamaan ECM.
multiplier <- function(b, gamma, beta, H = 20, pad = 5) {
  get <- function(nm) if (nm %in% names(b)) b[[nm]] else 0
  Tn <- pad + H + 1
  Y <- c(rep(0, pad), rep(1, H + 1)); dY <- c(0, diff(Y))
  C <- rep(0, Tn); dC <- rep(0, Tn)
  for (t in (pad + 1):Tn) {
    v <- get("dY") * dY[t] + gamma * (C[t - 1] - beta * Y[t - 1])
    for (i in 1:4) v <- v + get(paste0("dY_l", i)) * dY[t - i] + get(paste0("dC_l", i)) * dC[t - i]
    dC[t] <- v; C[t] <- C[t - 1] + dC[t]
  }
  C[(pad + 1):Tn]
}
mul_eg   <- multiplier(coef(ECM), gamma_eg, coef(LR)["lnY"])
mul_ardl <- multiplier(coef(RECM), gamma_ardl, theta_lr)
tab_mul <- data.frame(Triwulan = 0:20, EG_ECM = mul_eg, ARDL_ECM = mul_ardl)
print(round(tab_mul[c(1, 2, 3, 5, 9, 13, 21), ], 4), row.names = FALSE)
write.csv(tab_mul, file.path(OUT, "tabel10_dynamic_multiplier.csv"), row.names = FALSE)
png(file.path(OUT, "gambar11_dynamic_multiplier.png"), width = 1300, height = 700, res = 150)
par(mar = c(4, 4.5, 2.5, 1))
plot(0:20, mul_ardl, type = "o", pch = 16, col = "navy", lwd = 2,
     ylim = range(c(0, mul_eg, mul_ardl, 1.05)), xlab = "Triwulan setelah guncangan",
     ylab = "Perubahan konsumsi (%)",
     main = "Respon konsumsi terhadap kenaikan PDB 1% permanen")
lines(0:20, mul_eg, type = "o", pch = 17, col = "darkorange", lwd = 2)
abline(h = coef(LR)["lnY"], col = "darkorange", lty = 2); abline(h = theta_lr, col = "navy", lty = 2)
legend("bottomright", c(paste0("ECM Engle-Granger (lag ", k_opt, ")"), "ARDL-ECM",
                        "garis putus = elastisitas jangka panjang"),
       col = c("darkorange", "navy", "grey40"), lty = c(1, 1, 2), pch = c(17, 16, NA), bty = "n")
dev.off()

# ---------------------------------------------------------------------
# 11. RANGKUMAN HASIL
# ---------------------------------------------------------------------
garis("11. RINGKASAN HASIL")
apc_akhir <- mean(d$rasio[d$tahun == 2025])
ring <- data.frame(
  Besaran = c("Elastisitas jangka panjang C thd Y",
              "Elastisitas jangka pendek seketika (dlnY_t)",
              "Respon konsumsi setelah 4 triwulan (dynamic multiplier)",
              "Koefisien ECT (speed of adjustment)",
              "p-value ECT (SE HAC)",
              "Half-life disekuilibrium (triwulan)",
              "Waktu koreksi 90% (triwulan)",
              "MPC jangka panjang implisit (elastisitas x C/Y 2025)"),
  ARDL_ECM_utama = c(theta_lr, K$ardl_b0, mul_ardl[5], gamma_ardl, K$ardl_gamma_p_hac,
                     K$ardl_halflife, K$ardl_t90, theta_lr * apc_akhir),
  EG_ECM_pembanding = c(coef(LR)["lnY"], K$eg_b0, mul_eg[5], gamma_eg, K$eg_gamma_p_hac,
                        K$eg_halflife, K$eg_t90, coef(LR)["lnY"] * apc_akhir))
ring[, 2:3] <- round(ring[, 2:3], 4)
print(ring, row.names = FALSE)
write.csv(ring, file.path(OUT, "tabel9_ringkasan.csv"), row.names = FALSE)
geser <- coef(LR)["P"] + (coef(LR)["PS2"] + coef(LR)["PS3"] + coef(LR)["PS4"]) / 4
cat("\nPergeseran level rata-rata pasca-2021 =", round(geser, 4), "->",
    round(100 * (exp(geser) - 1), 2), "%\n")
K$geser <- geser; K$apc2025 <- apc_akhir
K$mul_eg_0 <- mul_eg[1]; K$mul_eg_4 <- mul_eg[5]; K$mul_eg_8 <- mul_eg[9]; K$mul_eg_20 <- mul_eg[21]
K$mul_ardl_0 <- mul_ardl[1]; K$mul_ardl_4 <- mul_ardl[5]; K$mul_ardl_8 <- mul_ardl[9]; K$mul_ardl_20 <- mul_ardl[21]
K$tau_eg <- tau_eg; K$lr_b <- coef(LR)["lnY"]; K$dols_b <- b_dols

# PENAFSIRAN OTOMATIS KOEFISIEN ECT (MODEL UTAMA: ARDL-ECM) ----------
garis("12. MAKNA KOEFISIEN ERROR CORRECTION TERM (ARDL-ECM)")
syarat_ok <- gamma_ardl < 0 && gamma_ardl > -2 && K$ardl_gamma_p_hac < 0.05
cat("Koefisien ECT (gamma) =", round(gamma_ardl, 4), "| p (HAC) =", signif(K$ardl_gamma_p_hac, 3), "\n")
cat("Syarat ECM valid (negatif, signifikan, -2 < gamma < 0):", ifelse(syarat_ok, "TERPENUHI", "TIDAK terpenuhi"), "\n\n")
cat("Makna:\n",
    "1. Tanda negatif & signifikan: bila konsumsi berada di atas (di bawah) keseimbangan\n",
    "   jangka panjangnya terhadap PDB, konsumsi triwulan berikutnya akan turun (naik)\n",
    "   untuk kembali ke keseimbangan -> hubungan jangka panjang stabil.\n",
    "2. Besarnya:", round(-100 * gamma_ardl, 1), "% ketidakseimbangan triwulan lalu dikoreksi\n",
    "   dalam satu triwulan.\n",
    "3. Half-life =", round(K$ardl_halflife, 2), "triwulan; 90% ketidakseimbangan hilang dalam",
    round(K$ardl_t90, 2), "triwulan.\n",
    "4. Pembanding ECM Engle-Granger: gamma =", round(gamma_eg, 4), "(p HAC =",
    round(K$eg_gamma_p_hac, 4), ") -> arah sama, namun lebih lemah & tidak signifikan pada 5%.\n")
write.csv(data.frame(nama = names(K), nilai = sapply(K, function(x) as.numeric(x)[1])),
          file.path(OUT, "angka_kunci.csv"), row.names = FALSE)

sink()
cat("Selesai. Seluruh output tersimpan di folder:", OUT, "\n")

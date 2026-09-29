
# UAS_ARW_No 2
# Author : Paskah Silaban
# Date   : 2026-09-29


pkg  <- c("readxl", "dplyr", "tidyr", "ggplot2", "vars", "urca")
baru <- pkg[!pkg %in% installed.packages()[, "Package"]]
if (length(baru) > 0) install.packages(baru)
invisible(lapply(pkg, library, character.only = TRUE))
options(width = 150)   # lebarkan output console agar tabel tampil utuh

# ---------------------------------------------------------------------
# 1. Memuat data, menggabungkan, dan membuat variabel turunan
# ---------------------------------------------------------------------
folder <- "C:/Users/paska/Downloads"     # <- ganti dengan folder tempat file disimpan

kurs <- read_excel(file.path(folder, "nilai tukar.xlsx"), sheet = 1)
names(kurs) <- c("tanggal", "kurs")
kurs <- kurs %>%
  filter(!is.na(tanggal)) %>%
  mutate(tanggal = as.Date(tanggal),
         kurs    = as.numeric(gsub(",", "", kurs)))           # ubah teks "9,057.00" jadi angka 9057

infl <- read_excel(file.path(folder, "inflasi (1).xlsx"),    sheet = "inflasi") %>%
  filter(!is.na(tanggal)) %>% mutate(tanggal = as.Date(tanggal))
bi   <- read_excel(file.path(folder, "suku bunga (1).xlsx"), sheet = "sukuBunga") %>%
  filter(!is.na(tanggal)) %>% mutate(tanggal = as.Date(tanggal))

data <- infl %>%
  inner_join(bi,   by = "tanggal") %>%
  inner_join(kurs, by = "tanggal") %>%
  arrange(tanggal) %>%
  mutate(ln_kurs = log(kurs),
         bulan   = factor(format(tanggal, "%m")))

# Validasi data: harus 188 observasi, tidak ada NA, dan deret bulanan tidak putus
stopifnot(nrow(data) == 188, !anyNA(data[, c("inflasi", "suku_bunga", "kurs")]),
          all(diff(as.numeric(format(data$tanggal, "%Y")) * 12 +
                     as.numeric(format(data$tanggal, "%m"))) == 1))
range(data$tanggal)

# Deret waktu dalam bentuk level, dipakai untuk uji kointegrasi
Y_lvl <- ts(data[, c("inflasi", "suku_bunga", "ln_kurs")],
            start = c(2011, 1), frequency = 12)

# Variabel hasil diferensiasi, dipakai untuk model VAR
data_d <- data %>%
  mutate(d_bi   = c(NA, diff(suku_bunga)),          # selisih BI Rate antarbulan (poin persentase)
         d_kurs = c(NA, 100 * diff(ln_kurs)))       # depresiasi kurs bulanan dalam persen

# ---------------------------------------------------------------------
# 2. Ringkasan statistik dan visualisasi awal
# ---------------------------------------------------------------------
data_d %>%
  summarise(across(c(inflasi, suku_bunga, kurs, d_bi, d_kurs),
                   list(mean = ~mean(.x, na.rm = TRUE), sd  = ~sd(.x, na.rm = TRUE),
                        min  = ~min(.x, na.rm = TRUE),  max = ~max(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything()) %>%
  print(n = 30)

print(
  data %>%
    dplyr::select(tanggal, inflasi, suku_bunga, kurs) %>%
    pivot_longer(-tanggal) %>%
    ggplot(aes(tanggal, value)) + geom_line(colour = "steelblue4") +
    facet_wrap(~ name, ncol = 1, scales = "free_y") +
    labs(x = NULL, y = NULL, title = "Data level") + theme_minimal()
)

# Melihat pola musiman inflasi lewat rata-rata tiap bulan kalender
round(tapply(data$inflasi, data$bulan, mean), 2)

# ---------------------------------------------------------------------
# 3. Pengujian stasioneritas
#    ADF  : hipotesis nol = terdapat akar unit (data tidak stasioner)
#    KPSS : hipotesis nol = data stasioner
# ---------------------------------------------------------------------
adf_stat <- function(x, type) {
  u <- ur.df(x, type = type, lags = 12, selectlags = "AIC")   # lag awal 12, lag optimal dipilih via AIC
  c(stat = round(u@teststat[1], 3), cv5 = u@cval[1, 2])
}
kpss_stat <- function(x, type) {
  u <- ur.kpss(x, type = type, lags = "short")
  c(stat = round(u@teststat, 3), cv5 = u@cval[1, "5pct"])
}

# 3a. Inflasi bulanan (m-to-m)
infl_ds <- residuals(lm(inflasi ~ bulan, data = data))   # sisaan regresi dummy bulan = inflasi tanpa efek musiman
print(rbind(
  ADF_drift          = adf_stat(data$inflasi, "drift"),
  ADF_trend          = adf_stat(data$inflasi, "trend"),
  ADF_tanpa_musiman  = adf_stat(infl_ds, "drift"),
  KPSS_level         = kpss_stat(data$inflasi, "mu"),
  KPSS_trend         = kpss_stat(data$inflasi, "tau")
))
# Catatan: ADF dengan drift kerap tidak menolak H0 sebab banyaknya lag
# ikut menyerap komponen musiman. Sebaliknya, ADF dengan tren dan ADF pada
# data tanpa musiman menolak H0, dan KPSS dengan tren tidak menolak
# kestasioneran. Jadi inflasi m-to-m adalah I(0), stasioner di sekitar tren turun.

# 3b. BI Rate dan ln Kurs: bandingkan level dengan diferensi pertama
for (v in c("suku_bunga", "ln_kurs")) {
  x <- data[[v]]
  cat("\n==", v, "==\n")
  print(rbind(ADF_level_drift = adf_stat(x, "drift"),
              ADF_level_trend = adf_stat(x, "trend"),
              KPSS_level      = kpss_stat(x, "mu"),
              ADF_diff        = adf_stat(diff(x), "drift"),
              KPSS_diff       = kpss_stat(diff(x), "mu")))
}
# Simpulan: keduanya tidak stasioner pada level tetapi stasioner setelah
# didiferensiasi sekali, sehingga BI Rate dan ln Kurs berintegrasi orde satu, I(1).

# ---------------------------------------------------------------------
# 4. Pengujian kointegrasi
# ---------------------------------------------------------------------
# Penentuan rank Johansen: uji bertahap mulai r = 0 dan berhenti
# ketika H0 pertama kali tidak ditolak
rank_johansen <- function(jo, alpha = "5pct") {
  stat <- rev(jo@teststat); cv <- rev(jo@cval[, alpha])
  sum(cumprod(stat > cv))
}

# 4a. Johansen untuk tiga variabel, tanpa tren, dengan dummy musiman
jo3 <- ca.jo(Y_lvl, type = "trace", ecdet = "none", K = 2,
             spec = "transitory", season = 12)
summary(jo3)
cat("Rank (trace, 5%):", rank_johansen(jo3), "\n")
# Uji restriksi: apakah vektor kointegrasi hanya berisi inflasi, beta = (1, 0, 0)'
summary(blrtest(jo3, H = matrix(c(1, 0, 0), 3, 1), r = 1))
# H0 hampir ditolak pada 5%. Tanpa komponen tren, ln kurs yang naik terus
# tampaknya mengambil alih peran tren turun pada inflasi. Oleh karena itu
# pengujian diulang dengan tren terestriksi pada bagian 4b.

# 4b. Johansen dengan tren terestriksi
jo3t <- ca.jo(Y_lvl, type = "trace", ecdet = "trend", K = 2,
              spec = "transitory", season = 12)
summary(jo3t)
cat("Rank (trace, 5%):", rank_johansen(jo3t), "\n")
# Uji restriksi: vektor kointegrasi hanya memuat inflasi dan tren
# (koefisien BI Rate dan kurs sama dengan nol)
H_t <- matrix(c(1, 0, 0, 0,
                0, 0, 0, 1), 4, 2)
summary(blrtest(jo3t, H = H_t, r = 1))
# H0 tidak ditolak, artinya r = 1 sekadar menggambarkan inflasi yang stasioner
# di sekitar tren. Ini BUKAN bukti adanya hubungan jangka panjang antara
# inflasi, BI Rate, dan kurs.

# 4c. Kointegrasi khusus variabel I(1): BI Rate dan ln Kurs (Johansen)
for (k in 2:4) {
  j <- ca.jo(Y_lvl[, c("suku_bunga", "ln_kurs")], type = "trace",
             ecdet = "none", K = k, spec = "transitory", season = 12)
  cat("K =", k, "| trace (r<=1, r=0):", round(j@teststat, 2),
      "| CV5% (r<=1, r=0):", j@cval[, 2], "| rank:", rank_johansen(j), "\n")
}

# 4d. Uji Engle-Granger untuk BI Rate dan ln Kurs
# Nilai kritis berbasis residual (MacKinnon, dua variabel, ada konstanta).
# Mohon dicek kembali dengan tabel pada buku atau modul sebelum dikutip.
cv_eg2 <- c("1pct" = -3.96, "5pct" = -3.37, "10pct" = -3.07)
eg1 <- lm(suku_bunga ~ ln_kurs, data = data)
eg2 <- lm(ln_kurs ~ suku_bunga, data = data)
print(c(EG_bi_on_kurs = ur.df(residuals(eg1), type = "none", lags = 12, selectlags = "AIC")@teststat[1],
        EG_kurs_on_bi = ur.df(residuals(eg2), type = "none", lags = 12, selectlags = "AIC")@teststat[1]))
print(cv_eg2)
# Statistik uji lebih besar daripada nilai kritis, jadi kedua variabel I(1)
# ini tidak berkointegrasi.
# SIMPULAN: tanpa kointegrasi, VECM tidak dibutuhkan. Model yang dipakai
# adalah VAR dengan seluruh variabel dalam bentuk stasioner.

# ---------------------------------------------------------------------
# 5. Menyiapkan data untuk VAR (seluruhnya stasioner)
#    Urutan kolom menentukan urutan Cholesky: inflasi -> d_bi -> d_kurs
#    (dari yang paling lambat merespons ke yang paling cepat: harga paling
#     lambat menyesuaikan, BI Rate ditetapkan setelah melihat inflasi,
#     dan kurs bereaksi paling cepat)
# ---------------------------------------------------------------------
Y <- ts(na.omit(data_d[, c("inflasi", "d_bi", "d_kurs")]),
        start = c(2011, 2), frequency = 12)
plot(Y, main = "Variabel model VAR (stasioner)")

# ---------------------------------------------------------------------
# 6. Menentukan panjang lag (konstanta + tren + dummy musiman)
# ---------------------------------------------------------------------
sel <- VARselect(Y, lag.max = 12, type = "both", season = 12)
print(sel$selection)
print(round(sel$criteria, 4))
p <- as.numeric(sel$selection["HQ(n)"])   # hasilnya 2, sama dengan pilihan AIC dan FPE
p

# ---------------------------------------------------------------------
# 7. Mengestimasi VAR(p)
# ---------------------------------------------------------------------
var2 <- VAR(Y, p = p, type = "both", season = 12)
summary(var2)

# Fungsi uji-F (Wald) untuk menguji sekelompok koefisien sekaligus dalam satu persamaan
wald_F <- function(m, pola) {
  b <- coef(m); V <- vcov(m); idx <- grep(pola, names(b))
  F <- as.numeric(t(b[idx]) %*% solve(V[idx, idx]) %*% b[idx]) / length(idx)
  c(F = round(F, 3), df1 = length(idx), df2 = df.residual(m),
    p = round(pf(F, length(idx), df.residual(m), lower.tail = FALSE), 4))
}
cat("\nSignifikansi dummy musiman tiap persamaan:\n")
print(sapply(var2$varresult, wald_F, pola = "^sd"))
cat("\nKoefisien tren tiap persamaan:\n")
print(round(sapply(var2$varresult, function(m) summary(m)$coef["trend", c(1, 4)]), 5))

# ---------------------------------------------------------------------
# 8. Uji kausalitas Granger
# ---------------------------------------------------------------------
# (a) Uji simultan: apakah suatu variabel menyebabkan kedua variabel lainnya sekaligus
for (cz in colnames(Y)) {
  g <- causality(var2, cause = cz)$Granger
  cat(cz, "-> lainnya : F =", round(g$statistic, 3), " p =", round(g$p.value, 4), "\n")
}
# (b) Uji per pasangan: signifikansi lag X dalam persamaan Y
cat("\n")
for (y in colnames(Y)) for (x in setdiff(colnames(Y), y)) {
  w <- wald_F(var2$varresult[[y]], paste0("^", x, "\\.l"))
  cat(x, "->", y, ": F =", w["F"], " p =", w["p"], "\n")
}

# ---------------------------------------------------------------------
# 9. Uji diagnostik residual
# ---------------------------------------------------------------------
print(serial.test(var2, lags.pt = 16, type = "PT.asymptotic"))   # uji autokorelasi (Portmanteau)
print(serial.test(var2, lags.bg = 5,  type = "BG"))              # uji autokorelasi (Breusch-Godfrey)
print(arch.test(var2, lags.multi = 5))                           # uji efek ARCH / heteroskedastisitas
print(normality.test(var2)$jb.mul)                               # uji normalitas (Jarque-Bera)
print(roots(var2))                                               # uji stabilitas: semua akar harus < 1

# ---------------------------------------------------------------------
# 10. Impulse Response Function (Cholesky, selang kepercayaan bootstrap 95%)
#     - inflasi: respons biasa, sebab variabelnya sudah dalam level
#     - d_bi dan d_kurs: respons KUMULATIF, sehingga terbaca sebagai
#       pengaruh terhadap LEVEL BI Rate dan kurs
# ---------------------------------------------------------------------
set.seed(123)
ir  <- irf(var2, n.ahead = 24, ortho = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
set.seed(123)
irc <- irf(var2, n.ahead = 24, ortho = TRUE, cumulative = TRUE,
           boot = TRUE, ci = 0.95, runs = 1000)

ambil <- function(obj, res) {
  do.call(rbind, lapply(names(obj$irf), function(imp)
    data.frame(h = 0:24, impuls = imp, respons = res,
               irf = obj$irf[[imp]][, res],
               lo  = obj$Lower[[imp]][, res],
               hi  = obj$Upper[[imp]][, res])))
}
irf_df <- rbind(ambil(ir,  "inflasi"),
                ambil(irc, "d_bi"),
                ambil(irc, "d_kurs"))
lab_imp <- c(inflasi = "Guncangan Inflasi", d_bi = "Guncangan BI Rate", d_kurs = "Guncangan Kurs")
lab_res <- c(inflasi = "Inflasi m-to-m (pp)", d_bi = "Level BI Rate (pp, kumulatif)",
             d_kurs  = "Level kurs (%, kumulatif)")
irf_df$impuls  <- factor(lab_imp[irf_df$impuls],  levels = lab_imp)
irf_df$respons <- factor(lab_res[irf_df$respons], levels = lab_res)

print(
  ggplot(irf_df, aes(h, irf)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "lightblue", alpha = .6) +
    geom_line(colour = "navy", linewidth = .9) +
    geom_hline(yintercept = 0, colour = "red", linetype = 2) +
    facet_grid(respons ~ impuls, scales = "free_y") +
    labs(x = "Bulan setelah guncangan", y = NULL,
         title = "IRF ortogonal VAR(2), CI bootstrap 95%") +
    theme_bw()
)

# Tabel IRF: format nilai [batas bawah, batas atas]; tanda * berarti
# selang kepercayaan tidak mencakup nol (respons signifikan)
hz <- c(0, 1, 2, 3, 6, 12, 24)
tabel_irf <- irf_df %>%
  filter(h %in% hz) %>%
  mutate(nilai = sprintf("%.3f [%.3f, %.3f]", irf, lo, hi),
         signif = ifelse(lo > 0 | hi < 0, "*", "")) %>%
  dplyr::select(impuls, respons, h, nilai, signif) %>%
  arrange(impuls, respons, h)
print(tabel_irf, row.names = FALSE)

# ---------------------------------------------------------------------
# 11. Dekomposisi varians galat peramalan (FEVD)
# ---------------------------------------------------------------------
fv <- fevd(var2, n.ahead = 24)
print(lapply(fv, function(m) {
  x <- round(100 * m[c(1, 6, 12, 24), ], 2)
  rownames(x) <- paste("Bulan", c(1, 6, 12, 24)); x
}))

# ---------------------------------------------------------------------
# 12. Uji ketahanan hasil (robustness)
# ---------------------------------------------------------------------
# (a) Membalik urutan Cholesky menjadi kurs -> BI Rate -> inflasi
#     (dilaporkan respons kumulatif pada h = 24)
var2_rev <- VAR(Y[, c("d_kurs", "d_bi", "inflasi")], p = p, type = "both", season = 12)
irc_rev  <- irf(var2_rev, n.ahead = 24, cumulative = TRUE, boot = FALSE)
print(lapply(irc_rev$irf, function(m) round(m[25, ], 3)))

# (b) VAR dengan data level (pendekatan Sims, Stock & Watson, 1990),
#     dilaporkan respons pada h = 3 dan h = 24
var_lvl <- VAR(Y_lvl, p = p, type = "both", season = 12)
ir_lvl  <- irf(var_lvl, n.ahead = 24, boot = FALSE)
print(lapply(ir_lvl$irf, function(m) round(m[c(4, 25), ], 4)))

# ---------------------------------------------------------------------
# 13. Model pembanding: VECM dengan inflasi tahunan (year-on-year)
#     (inflasi yoy termasuk I(1), sehingga kointegrasinya bisa diuji
#      bersama BI Rate dan kurs)
# ---------------------------------------------------------------------
data_yoy <- data %>%
  mutate(infl_yoy = sapply(seq_along(inflasi), function(t)
    if (t < 12) NA else (prod(1 + inflasi[(t - 11):t] / 100) - 1) * 100)) %>%
  filter(!is.na(infl_yoy))

Y_yoy <- data.frame(infl  = data_yoy$infl_yoy,
                    bi    = data_yoy$suku_bunga,
                    lkurs = data_yoy$ln_kurs)
print(adf_stat(Y_yoy$infl, "drift")); print(adf_stat(diff(Y_yoy$infl), "drift"))

jo_yoy <- ca.jo(Y_yoy, type = "trace", ecdet = "const", K = 2, spec = "transitory")
summary(jo_yoy)
cat("Rank (trace, 5%):", rank_johansen(jo_yoy), "\n")
# Uji restriksi: apakah vektor kointegrasi hanya memuat inflasi yoy dan konstanta? (H0)
summary(blrtest(jo_yoy, H = matrix(c(1, 0, 0, 0,
                                     0, 0, 0, 1), 4, 2), r = 1))
vecm_yoy <- cajorls(jo_yoy, r = 1)
print(vecm_yoy$beta)                                # persamaan hubungan jangka panjang
print(lapply(summary(vecm_yoy$rlm), function(s) round(coef(s)["ect1", ], 4)))  # koefisien kecepatan penyesuaian (ECT)

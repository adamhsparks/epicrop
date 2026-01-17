# ---- Fix 1: wetness branch semantics ----
# If wetness == 1 => simple (binary) wetness
# If wetness == 0 => derived wetness hours + RcW (needs leafWet(simple = FALSE))
if (wetness == 1) {
  RHCoef[day + 1] <- as.numeric(
    wth@w$rhmax[day + 1] >= rhlim | wth@w$prcp[day + 1] >= rainlim
  )
} else {
  W <- leafWet(wth, simple = FALSE) # requires TMIN/TMAX etc. (not in posted script)
  RHCoef[day + 1] <- AFGen(rhRc, W[day + 1]) # rhRc hours should be 4..24, not 0..10
}

# ---- Fix 2: rhRc (RcW) domain ----
RHCoefRc <- cbind(
  c(4L, 6L, 8L, 10L, 12L, 14L, 16L, 18L, 20L, 22L, 24L),
  c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1, 1, 1, 1, 1)
)

# ---- Fix 3: define removal index before use ----
infday <- max(1, day - inftrans + 2) # keep indices >= 1
removedToday <- if (day > inftrans) infectious[infday] else 0

# ---- Fix 4: consistent diseased/removal order ----
Removed[day + 1] <- sum(infectious) - now_infectious[day + 1]
Diseased[day + 1] <- now_latent[day + 1] +
  now_infectious[day + 1] +
  Removed[day + 1]

# ---- Fix 5: senescence update with today's flow ----
Senesced[day + 1] <- Senesced[day] + RSenesced[day + 1]

# ---- Fix 6: no index 0 in slices ----
latday <- max(1, day - latrans + 2)
now_latent[day + 1] <- sum(latency[latday:(day + 1)])

infday <- max(1, day - inftrans + 2)
now_infectious[day + 1] <- sum(infectious[infday:(day + 1)])

# ---- Fix 7: SB age curve tail ----
AgeCoefRc_SB <- cbind(0:9 * 10, c(0.43, 0.5, 0.73, 0.81, 0.94, 1, 1, 1, 1, 1))


# AUDPC Helper Differences
# Use available length and trapezoidal rule on proportion, not percent
audpc_trapz <- function(dates, sev_pct) {
  y <- sev_pct / 100
  sum(diff(dates) * (head(y, -1) + tail(y, 1)) / 2)
}

Cal.LB.audpc <- function(wth, tpd) {
  o <- leafBlast.EPIRICE(wth, tpd)
  if (!inherits(o, "try-error")) {
    n <- nrow(o@d)
    audpc_trapz(o@d$date[1:n], o@d$severity[1:n])
  } else -999
}
``

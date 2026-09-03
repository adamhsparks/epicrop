#' Benchmark: Legacy cropsim SEIR() vs. Modern seir()
#'
#' Performance comparison of the two implementations using microbenchmark.
#' Run with: source("tests/benchmarks/benchmark_seir_implementations.R")

library(microbenchmark)
library(data.table)
library(ggplot2)
library(cropsim) # Legacy SEIR models
library(epicrop) # Modern seir() functions
library(meteor) # Weather data retrieval

# ==============================================================================
# SETUP: Prepare weather data in both formats
# ==============================================================================

if (!isClass("WeatherData")) {
  setClass("WeatherData", representation(w = "data.frame", lat = "numeric"))
}

#' Convert modern epicrop.wth to legacy cropsim WeatherData S4 object
to_legacy_wth <- function(wth) {
  w <- as.data.frame(wth)
  w$date <- as.Date(w$YYYYMMDD)
  w$tavg <- w$TEMP
  w$tmax <- w$TMAX
  w$tmin <- w$TMIN
  w$rhmax <- w$RHUM
  w$rhmin <- w$RHUM
  w$prec <- w$RAIN
  w$doy <- w$DOY

  keep <- c("date", "doy", "tavg", "tmax", "tmin", "rhmax", "rhmin", "prec")
  w <- w[, keep]

  new("WeatherData", w = w, lat = wth$LAT[1L])
}

# Get weather data
WTH_MODERN <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)

WTH_LEGACY <- to_legacy_wth(WTH_MODERN)
EMERGENCE <- "2000-07-01"


# ==============================================================================
# Helper: Extract and normalize results from both implementations
# ==============================================================================

#' Extract results from legacy S4 SEIR object
extract_legacy_result <- function(seir_obj) {
  dt <- as.data.table(seir_obj@d)

  if ("severity" %in% names(dt)) {
    setnames(dt, "severity", "intensity")
    dt[, intensity := intensity / 100]
  }

  return(dt)
}

#' Extract results from modern data.table
extract_modern_result <- function(dt) {
  dt <- data.table::copy(dt)

  if ("severity" %in% names(dt)) {
    setnames(dt, "severity", "intensity")
    dt[, intensity := intensity / 100]
  }

  if ("dates" %in% names(dt) && !"date" %in% names(dt)) {
    setnames(dt, "dates", "date")
  }

  return(dt)
}


# ==============================================================================
# BENCHMARK 1: Leaf Blast
# ==============================================================================

bm_leafblast <- microbenchmark(
  legacy = {
    result <- cropsim::leafBlast(WTH_LEGACY, emergence = EMERGENCE)
    extract_legacy_result(result)
  },
  modern = {
    result <- leaf_blast(WTH_MODERN, emergence = EMERGENCE)
    extract_modern_result(result)
  },
  times = 100,
  unit = "ms"
)

legacy_times_lb <- bm_leafblast[bm_leafblast$expr == "legacy", "time"] / 1e6
modern_times_lb <- bm_leafblast[bm_leafblast$expr == "modern", "time"] / 1e6

stats_lb <- data.table(
  disease = "Leaf Blast",
  implementation = c("legacy", "modern"),
  min = c(min(legacy_times_lb), min(modern_times_lb)),
  max = c(max(legacy_times_lb), max(modern_times_lb)),
  mean = c(mean(legacy_times_lb), mean(modern_times_lb)),
  median = c(median(legacy_times_lb), median(modern_times_lb)),
  sd = c(sd(legacy_times_lb), sd(modern_times_lb))
)

print(autoplot(bm_leafblast) + ggtitle("Leaf Blast: Legacy vs. Modern"))


# ==============================================================================
# BENCHMARK 2: Bacterial Blight
# ==============================================================================

bm_bactblight <- microbenchmark(
  legacy = {
    result <- cropsim::bactBlight(WTH_LEGACY, emergence = EMERGENCE)
    extract_legacy_result(result)
  },
  modern = {
    result <- bacterial_blight(WTH_MODERN, emergence = EMERGENCE)
    extract_modern_result(result)
  },
  times = 100,
  unit = "ms"
)

legacy_times_bb <- bm_bactblight[bm_bactblight$expr == "legacy", "time"] / 1e6
modern_times_bb <- bm_bactblight[bm_bactblight$expr == "modern", "time"] / 1e6

stats_bb <- data.table(
  disease = "Bacterial Blight",
  implementation = c("legacy", "modern"),
  min = c(min(legacy_times_bb), min(modern_times_bb)),
  max = c(max(legacy_times_bb), max(modern_times_bb)),
  mean = c(mean(legacy_times_bb), mean(modern_times_bb)),
  median = c(median(legacy_times_bb), median(modern_times_bb)),
  sd = c(sd(legacy_times_bb), sd(modern_times_bb))
)

print(autoplot(bm_bactblight) + ggtitle("Bacterial Blight: Legacy vs. Modern"))


# ==============================================================================
# BENCHMARK 3: Brown Spot
# ==============================================================================

bm_brownspot <- microbenchmark(
  legacy = {
    result <- cropsim::brownSpot(WTH_LEGACY, emergence = EMERGENCE)
    extract_legacy_result(result)
  },
  modern = {
    result <- brown_spot(WTH_MODERN, emergence = EMERGENCE)
    extract_modern_result(result)
  },
  times = 100,
  unit = "ms"
)

legacy_times_bs <- bm_brownspot[bm_brownspot$expr == "legacy", "time"] / 1e6
modern_times_bs <- bm_brownspot[bm_brownspot$expr == "modern", "time"] / 1e6

stats_bs <- data.table(
  disease = "Brown Spot",
  implementation = c("legacy", "modern"),
  min = c(min(legacy_times_bs), min(modern_times_bs)),
  max = c(max(legacy_times_bs), max(modern_times_bs)),
  mean = c(mean(legacy_times_bs), mean(modern_times_bs)),
  median = c(median(legacy_times_bs), median(modern_times_bs)),
  sd = c(sd(legacy_times_bs), sd(modern_times_bs))
)

print(autoplot(bm_brownspot) + ggtitle("Brown Spot: Legacy vs. Modern"))


# ==============================================================================
# BENCHMARK 4: Sheath Blight
# ==============================================================================

bm_sheathblight <- microbenchmark(
  legacy = {
    result <- cropsim::sheathBlight(WTH_LEGACY, emergence = EMERGENCE)
    extract_legacy_result(result)
  },
  modern = {
    result <- sheath_blight(WTH_MODERN, emergence = EMERGENCE)
    extract_modern_result(result)
  },
  times = 100,
  unit = "ms"
)

legacy_times_sb <- bm_sheathblight[bm_sheathblight$expr == "legacy", "time"] /
  1e6
modern_times_sb <- bm_sheathblight[bm_sheathblight$expr == "modern", "time"] /
  1e6

stats_sb <- data.table(
  disease = "Sheath Blight",
  implementation = c("legacy", "modern"),
  min = c(min(legacy_times_sb), min(modern_times_sb)),
  max = c(max(legacy_times_sb), max(modern_times_sb)),
  mean = c(mean(legacy_times_sb), mean(modern_times_sb)),
  median = c(median(legacy_times_sb), median(modern_times_sb)),
  sd = c(sd(legacy_times_sb), sd(modern_times_sb))
)

print(autoplot(bm_sheathblight) + ggtitle("Sheath Blight: Legacy vs. Modern"))


# ==============================================================================
# BENCHMARK 5: Tungro
# ==============================================================================

bm_tungro <- microbenchmark(
  legacy = {
    result <- cropsim::tungro(WTH_LEGACY, emergence = EMERGENCE)
    extract_legacy_result(result)
  },
  modern = {
    result <- epicrop::tungro(WTH_MODERN, emergence = EMERGENCE)
    extract_modern_result(result)
  },
  times = 100,
  unit = "ms"
)

legacy_times_tg <- bm_tungro[bm_tungro$expr == "legacy", "time"] / 1e6
modern_times_tg <- bm_tungro[bm_tungro$expr == "modern", "time"] / 1e6

stats_tg <- data.table(
  disease = "Tungro",
  implementation = c("legacy", "modern"),
  min = c(min(legacy_times_tg), min(modern_times_tg)),
  max = c(max(legacy_times_tg), max(modern_times_tg)),
  mean = c(mean(legacy_times_tg), mean(modern_times_tg)),
  median = c(median(legacy_times_tg), median(modern_times_tg)),
  sd = c(sd(legacy_times_tg), sd(modern_times_tg))
)

print(autoplot(bm_tungro) + ggtitle("Tungro: Legacy vs. Modern"))


# ==============================================================================
# COMBINED BENCHMARK: All 5 Diseases
# ==============================================================================

bm_all_diseases <- microbenchmark(
  "Legacy: Leaf Blast" = cropsim::leafBlast(WTH_LEGACY, emergence = EMERGENCE),
  "Legacy: Bacterial Blight" = cropsim::bactBlight(
    WTH_LEGACY,
    emergence = EMERGENCE
  ),
  "Legacy: Brown Spot" = cropsim::brownSpot(WTH_LEGACY, emergence = EMERGENCE),
  "Legacy: Sheath Blight" = cropsim::sheathBlight(
    WTH_LEGACY,
    emergence = EMERGENCE
  ),
  "Legacy: Tungro" = cropsim::tungro(WTH_LEGACY, emergence = EMERGENCE),
  "Modern: Leaf Blast" = leaf_blast(WTH_MODERN, emergence = EMERGENCE),
  "Modern: Bacterial Blight" = bacterial_blight(
    WTH_MODERN,
    emergence = EMERGENCE
  ),
  "Modern: Brown Spot" = brown_spot(WTH_MODERN, emergence = EMERGENCE),
  "Modern: Sheath Blight" = sheath_blight(WTH_MODERN, emergence = EMERGENCE),
  "Modern: Tungro" = epicrop::tungro(WTH_MODERN, emergence = EMERGENCE),
  times = 50,
  unit = "ms"
)

print(autoplot(bm_all_diseases) + ggtitle("Combined Benchmark: All Diseases"))


# ==============================================================================
# COMBINED STATISTICS
# ==============================================================================

all_stats <- rbind(stats_lb, stats_bb, stats_bs, stats_sb, stats_tg)

print(all_stats)


# ==============================================================================
# COMPARISON TABLE
# ==============================================================================

comparison <- data.table(
  Disease = c(
    "Leaf Blast",
    "Bacterial Blight",
    "Brown Spot",
    "Sheath Blight",
    "Tungro"
  ),
  Legacy_ms = c(
    stats_lb[implementation == "legacy", median],
    stats_bb[implementation == "legacy", median],
    stats_bs[implementation == "legacy", median],
    stats_sb[implementation == "legacy", median],
    stats_tg[implementation == "legacy", median]
  ),
  Modern_ms = c(
    stats_lb[implementation == "modern", median],
    stats_bb[implementation == "modern", median],
    stats_bs[implementation == "modern", median],
    stats_sb[implementation == "modern", median],
    stats_tg[implementation == "modern", median]
  )
)

comparison[, `:=`(
  Speedup_pct = (Legacy_ms - Modern_ms) / Modern_ms * 100,
  Ratio = Legacy_ms / Modern_ms
)]

print(comparison)


# ==============================================================================
# SPEEDUP VISUALIZATION
# ==============================================================================

speedup_data <- comparison[, .(Disease, Speedup_pct)]

p_speedup <- ggplot(
  speedup_data,
  aes(
    x = reorder(Disease, -Speedup_pct),
    y = Speedup_pct,
    fill = Speedup_pct > 0
  )
) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%.1f%%", Speedup_pct)),
    vjust = ifelse(speedup_data$Speedup_pct > 0, -0.5, 1.5),
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("FALSE" = "#E41A1C", "TRUE" = "#377EB8"),
    guide = "none"
  ) +
  labs(
    title = "Performance Speedup: Modern vs. Legacy",
    x = "Disease",
    y = "Speedup (%)"
  ) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_speedup)


# ==============================================================================
# MEDIAN COMPARISON VISUALIZATION
# ==============================================================================

median_comparison <- data.table(
  Disease = rep(comparison$Disease, 2),
  Implementation = rep(c("Legacy", "Modern"), each = 5),
  Median_ms = c(comparison$Legacy_ms, comparison$Modern_ms)
)

p_median <- ggplot(
  median_comparison,
  aes(x = Disease, y = Median_ms, fill = Implementation)
) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(
    values = c("Legacy" = "#E41A1C", "Modern" = "#377EB8")
  ) +
  labs(
    title = "Median Execution Time: Legacy vs. Modern",
    x = "Disease",
    y = "Median Time (ms)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_median)


# ==============================================================================
# RETURN RESULTS FOR INTERACTIVE USE
# ==============================================================================

invisible(list(
  comparison = comparison,
  stats = all_stats,
  benchmarks = list(
    leafblast = bm_leafblast,
    bactblight = bm_bactblight,
    brownspot = bm_brownspot,
    sheathblight = bm_sheathblight,
    tungro = bm_tungro,
    all_diseases = bm_all_diseases
  )
))

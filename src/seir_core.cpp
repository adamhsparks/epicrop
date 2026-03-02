// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

// Notes:
// - Internal arrays remain 1..n to mirror R logic.
// - Inputs must be length == duration (validated).
// - onset_idx is 1-based (validated).
// - All numeric values are double (as in R).
// - Fails fast on invalid inputs (length, NA/NaN/Inf, invalid parameters).

using Rcpp::List;
using Rcpp::Named;
using Rcpp::NumericVector;

// ---------- Helpers: validation & utilities ----------

inline void ensure_true(bool cond, const char *msg) {
  if (!cond)
    Rcpp::stop("%s", msg);
}

inline void check_no_missing(const NumericVector &v, const char *name) {
  const R_xlen_t n = v.size();
  for (R_xlen_t i = 0; i < n; ++i) {
    if (!std::isfinite(v[i]))
      Rcpp::stop("Vector '%s' contains non-finite at index %ld (1-based %ld).",
                 name, static_cast<long>(i), static_cast<long>(i + 1));
  }
}

inline void check_length(const NumericVector &v, int expected,
                         const char *name) {
  ensure_true(v.size() == expected,
              (std::string("Vector '") + name +
               "' must have length = duration").c_str());
}

inline double clamp01(double x) {
  if (x < 0.0) return 0.0;
  if (x > 1.0) return 1.0;
  return x;
}

static std::vector<int> floor_nonneg(const NumericVector &v) {
  const int n = static_cast<int>(v.size());
  std::vector<int> out(n, 0);
  for (int i = 0; i < n; ++i) {
    const double fv = std::floor(v[i]);
    if (!std::isfinite(fv) || fv < 0.0) {
      out[i] = 0;
    } else if (fv > static_cast<double>(std::numeric_limits<int>::max())) {
      out[i] = std::numeric_limits<int>::max();
    } else {
      out[i] = static_cast<int>(fv);
    }
  }
  return out;
}

// ---------- Core worker ----------

template <bool withLex>
static List seir_core_worker(
    int duration, int onset_idx, double H0, const NumericVector &Iprim,
    const NumericVector &Rc, const NumericVector &p_days,
    const NumericVector &i_days, const NumericVector &RRG_vals,
    const NumericVector &RRS_vals, const NumericVector &a_vals, double Sx,
    const NumericVector &RRLEX_vals) {
  const int n = duration;

  // ----------- Validate scalars -----------
  ensure_true(n > 0,              "duration must be > 0");
  ensure_true(onset_idx >= 1 && onset_idx <= n,
                                  "onset_idx must be in [1..duration]");
  ensure_true(Sx  >  0.0,        "Sx must be > 0");
  ensure_true(H0  >= 0.0,        "H0 must be >= 0");
  ensure_true(H0  <= Sx,         "H0 must be <= Sx");

  // ----------- Validate vector lengths -----------
  check_length(Iprim,    n, "Iprim");
  check_length(Rc,       n, "Rc");
  check_length(p_days,   n, "p_days");
  check_length(i_days,   n, "i_days");
  check_length(RRG_vals, n, "RRG_vals");
  check_length(RRS_vals, n, "RRS_vals");
  check_length(a_vals,   n, "a_vals");
  if (withLex) check_length(RRLEX_vals, n, "RRLEX_vals");

  // ----------- Validate contents (finite) -----------
  check_no_missing(Iprim,    "Iprim");
  check_no_missing(Rc,       "Rc");
  check_no_missing(p_days,   "p_days");
  check_no_missing(i_days,   "i_days");
  check_no_missing(RRG_vals, "RRG_vals");
  check_no_missing(RRS_vals, "RRS_vals");
  check_no_missing(a_vals,   "a_vals");
  if (withLex) check_no_missing(RRLEX_vals, "RRLEX_vals");

  // ----------- Precompute floored window sizes -----------
  const std::vector<int> p_int = floor_nonneg(p_days);
  const std::vector<int> i_int = floor_nonneg(i_days);

  // ----------- Internal 1..n arrays -----------
  std::vector<double> sites(n + 1, 0.0);
  std::vector<double> latency(n + 1, 0.0);
  std::vector<double> infectious_flow(n + 1, 0.0);
  std::vector<double> cs_lat(n + 1, 0.0);
  std::vector<double> cs_inf(n + 1, 0.0);
  std::vector<double> rsenesced(n + 1, 0.0);
  std::vector<double> senesced(n + 1, 0.0);
  std::vector<double> rgrowth(n + 1, 0.0);
  std::vector<double> rateinf(n + 1, 0.0);
  std::vector<double> rlex(n + 1, 0.0);
  std::vector<double> rtransfer(n + 1, 0.0);
  std::vector<double> removed(n + 1, 0.0);
  std::vector<double> now_latent(n + 1, 0.0);
  std::vector<double> now_infectious(n + 1, 0.0);
  std::vector<double> diseased(n + 1, 0.0);

  // ----------- Day 1 -----------
  sites[1]    = H0;
  rsenesced[1] = RRS_vals[0] * H0;
  senesced[1]  = rsenesced[1];
  rgrowth[1]   = RRG_vals[0] * H0 * (1.0 - H0 / Sx);
  rateinf[1]   = (1 == onset_idx) ? Iprim[0] : 0.0;

  // ----------- Loop t = 2..n -----------
  for (int t = 2; t <= n; ++t) {

    if ((t & 0x7FF) == 0)
      Rcpp::checkUserInterrupt();

    const int pback   = p_int[t - 1];
    const int iback   = i_int[t - 1];
    const int lat_src = t - pback + 2;
    const int inf_src = t - iback;

    // Removed today: infectious leaving the infectious window
    double removed_today = 0.0;
    {
      const int idx = inf_src - 1;
      if (idx >= 1 && idx <= n)
        removed_today = infectious_flow[idx];
    }

    // Sites
    {
      const double subtract_lex = withLex ? rlex[t - 1] : 0.0;
      sites[t] = std::max(0.0, sites[t-1] + rgrowth[t-1] - rateinf[t-1] -
                               rsenesced[t-1] - subtract_lex);
    }

    rsenesced[t] = removed_today + RRS_vals[t - 1] * sites[t];
    senesced[t]  = senesced[t - 1] + rsenesced[t];

    // Latency
    latency[t]  = rateinf[t - 1];
    cs_lat[t]   = cs_lat[t - 1] + latency[t];

    double win_lat0 = 0.0;
    if (lat_src > 2) {
      const int idx = lat_src - 2;
      if (idx <= n) win_lat0 = cs_lat[idx];  // idx >= 1 guaranteed by lat_src > 2
    }
    now_latent[t] = cs_lat[t] - win_lat0;

    rtransfer[t] = (lat_src > 1 && (lat_src - 1) <= n)
                       ? latency[lat_src - 1] : 0.0;

    if (!withLex) {
      // ---------- No lesion expansion ----------
      infectious_flow[t] = rtransfer[t];
      cs_inf[t] = cs_inf[t - 1] + infectious_flow[t];

      double win_inf0 = 0.0;
      if (inf_src > 2) {
        const int idx = inf_src - 2;
        if (idx <= n) win_inf0 = cs_inf[idx];
      }
      now_infectious[t] = cs_inf[t] - win_inf0;

      removed[t]  = cs_inf[t] - now_infectious[t];
      diseased[t] = now_latent[t] + now_infectious[t] + removed[t];

      const double total = sites[t] + diseased[t];
      const double COFR  = (total > 0.0)
                               ? clamp01(1.0 - diseased[t] / total) : 1.0;
      const double a     = a_vals[t - 1];

      rateinf[t] = (t == onset_idx)
          ? Iprim[t - 1]
          : now_infectious[t] * Rc[t-1] * (a == 1.0 ? COFR : std::pow(COFR, a));

      rgrowth[t] = RRG_vals[t - 1] * sites[t] * (1.0 - total / Sx);

    } else {
      // ---------- With lesion expansion ----------
      const double cs_pre = cs_inf[t - 1] + rtransfer[t];

      double base_pre = 0.0;
      if (inf_src > 2) {
        const int idx = inf_src - 2;
        if (idx <= n) base_pre = cs_inf[idx];
      }
      const double now_inf_pre  = cs_pre - base_pre;
      const double removed_pre  = cs_pre - now_inf_pre;
      const double diseased_pre = now_latent[t] + now_inf_pre + removed_pre;

      const double total_pre = sites[t] + diseased_pre;
      const double COFR = (total_pre > 0.0)
                              ? clamp01(1.0 - diseased_pre / total_pre) : 1.0;

      rlex[t] = RRLEX_vals[t - 1] * now_inf_pre * std::max(0.0, COFR);

      infectious_flow[t] = rtransfer[t] + rlex[t];
      cs_inf[t] = cs_inf[t - 1] + infectious_flow[t];

      double win_inf0 = 0.0;
      if (inf_src > 2) {
        const int idx = inf_src - 2;
        if (idx <= n) win_inf0 = cs_inf[idx];
      }
      now_infectious[t] = cs_inf[t] - win_inf0;

      removed[t]  = cs_inf[t] - now_infectious[t];
      diseased[t] = now_latent[t] + now_infectious[t] + removed[t];

      const double total = sites[t] + diseased[t];
      const double a     = a_vals[t - 1];

      rateinf[t] = (t == onset_idx)
          ? Iprim[t - 1]
          : now_infectious[t] * Rc[t-1] * (a == 1.0 ? COFR : std::pow(COFR, a));

      rgrowth[t] = RRG_vals[t - 1] * sites[t] * (1.0 - total / Sx);
    }
  }

  // ----------- Assemble outputs -----------
  NumericVector sites_out(n), latent_out(n), infectious_out(n), removed_out(n),
      senesced_out(n), rateinf_out(n), rlex_out(n), rtransfer_out(n),
      rremoved_out(n), rgrowth_out(n), rsenesced_out(n), diseased_out(n),
      intensity_out(n);

  for (int t = 1; t <= n; ++t) {
    const int    idx    = t - 1;
    const double nl     = now_latent[t];
    const double ni     = now_infectious[t];
    const double s      = sites[t];
    const double active = nl + ni;
    const double denom  = s + active;

    sites_out[idx]      = s;
    latent_out[idx]     = nl;
    infectious_out[idx] = ni;
    removed_out[idx]    = removed[t];
    senesced_out[idx]   = senesced[t];
    rateinf_out[idx]    = rateinf[t];
    rlex_out[idx]       = withLex ? rlex[t] : 0.0;
    rtransfer_out[idx]  = rtransfer[t];
    rremoved_out[idx]   = (t == 1) ? 0.0 : (removed[t] - removed[t - 1]);
    rgrowth_out[idx]    = rgrowth[t];
    rsenesced_out[idx]  = rsenesced[t];
    diseased_out[idx]   = diseased[t];
    intensity_out[idx]  = (denom > 0.0) ? active / denom : 0.0;
  }

  return List::create(
      Named("sites")      = sites_out,
      Named("latent")     = latent_out,
      Named("infectious") = infectious_out,
      Named("removed")    = removed_out,
      Named("senesced")   = senesced_out,
      Named("rateinf")    = rateinf_out,
      Named("rlex")       = rlex_out,
      Named("rtransfer")  = rtransfer_out,
      Named("rremoved")   = rremoved_out,
      Named("rgrowth")    = rgrowth_out,
      Named("rsenesced")  = rsenesced_out,
      Named("diseased")   = diseased_out,
      Named("intensity")  = intensity_out);
}

// ---------- Exported wrappers ----------

// [[Rcpp::export]]
List seir_core_no_lex_cpp(int duration, int onset_idx, double H0,
                          NumericVector Iprim, NumericVector Rc,
                          NumericVector p_days, NumericVector i_days,
                          NumericVector RRG_vals, NumericVector RRS_vals,
                          NumericVector a_vals, double Sx) {
  NumericVector empty;
  return seir_core_worker<false>(duration, onset_idx, H0, Iprim, Rc, p_days,
                                 i_days, RRG_vals, RRS_vals, a_vals, Sx, empty);
}

// [[Rcpp::export]]
List seir_core_with_lex_cpp(int duration, int onset_idx, double H0,
                            NumericVector Iprim, NumericVector Rc,
                            NumericVector p_days, NumericVector i_days,
                            NumericVector RRG_vals, NumericVector RRS_vals,
                            NumericVector RRLEX_vals, NumericVector a_vals,
                            double Sx) {
  return seir_core_worker<true>(duration, onset_idx, H0, Iprim, Rc, p_days,
                                i_days, RRG_vals, RRS_vals, a_vals, Sx,
                                RRLEX_vals);
}

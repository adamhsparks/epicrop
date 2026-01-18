// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
using namespace Rcpp;

// Constants
const double EPSILON = 1e-15;

// Fractional back-reference with proper bounds checking
inline double backref(const NumericVector& hist, int d, double delay) {
  if (delay <= 0.0) return 0.0;
  
  const double exact_day = (double)d - delay;
  if (exact_day < 0.0) return 0.0;
  
  const int j_lo = (int)std::floor(exact_day);
  const int j_hi = (int)std::ceil(exact_day);
  
  if (j_lo < 0 || j_hi >= d) return 0.0;
  
  const double frac = exact_day - (double)j_lo;
  
  double v = (1.0 - frac) * hist[j_lo];
  if (j_hi != j_lo && j_hi < d) {
    v += frac * hist[j_hi];
  }
  
  return std::max(0.0, v);
}

// [[Rcpp::export]]
Rcpp::List epicrop_core_cpp(const NumericVector& Rc,
                            const NumericVector& RRLEX,
                            const NumericVector& RRG,
                            const NumericVector& RRS,
                            const NumericVector& p_days,
                            const NumericVector& i_days,
                            const NumericVector& agg,
                            const NumericVector& Iprim,
                            const double H0,
                            const double SMax,
                            const int steps_per_day = 1) {

  const int n = Rc.size();
  
  // Input validation
  if (RRLEX.size() != n || RRG.size() != n || RRS.size() != n ||
      p_days.size() != n || i_days.size() != n || agg.size() != n ||
      Iprim.size() != n) {
    stop("All daily input vectors must have the same length.");
  }
  if (SMax <= 0.0) stop("SMax must be > 0.");
  if (steps_per_day < 1) stop("steps_per_day must be >= 1.");
  if ((steps_per_day & (steps_per_day - 1)) != 0) {
    Rcpp::warning("steps_per_day should be a power of 2 for best accuracy.");
  }

  // Output vectors
  NumericVector sites(n), latent(n), infectious(n), removed(n), senesced(n);
  NumericVector rateinf(n), rlex(n), rtransfer(n), rremoved(n), rgrowth(n), rsenesced(n);
  NumericVector diseased(n), intensity(n), cofr(n);

  // Delay histories
  NumericVector RI_hist(n);
  NumericVector BecInf_hist(n);

  // State variables
  double HS_prev = std::max(0.0, H0);
  double LAT_prev = 0.0;
  double INF_prev = 0.0;
  double REM_prev = 0.0;
  double SEN_prev = 0.0;

  const double dt = 1.0 / (double)steps_per_day;

  for (int d = 0; d < n; ++d) {

    // Cache daily parameters (ONCE per day)
    const double a_d = std::max(1.0, (double)agg[d]);
    const double Rc_d = std::max(0.0, (double)Rc[d]);
    const double rrlex_d = std::max(0.0, (double)RRLEX[d]);
    const double rrs_d = std:: max(0.0, (double)RRS[d]);
    const double rrg_d = std::max(0.0, (double)RRG[d]);
    const double p_d = std::max(0.0, (double)p_days[d]);
    const double i_d = std::max(0.0, (double)i_days[d]);
    const double Iprim_d = std::max(0.0, (double)Iprim[d]);

    // Pre-compute daily delays (ONCE per day)
    const double RT_day = backref(RI_hist, d, p_d);
    const double RREM_day = backref(BecInf_hist, d, i_d);

    // Daily accumulators
    double RI_today = 0.0;
    double RLEX_today = 0.0;
    double RT_today = 0.0;
    double RREM_today = 0.0;
    double RG_today = 0.0;
    double RSN_today = 0.0;

    for (int s = 0; s < steps_per_day; ++s) {

      // COFR calculation
      double total_prev = HS_prev + LAT_prev + INF_prev + REM_prev;
      if (total_prev < EPSILON) total_prev = EPSILON;
      double cofr_d = 1.0 - ((LAT_prev + INF_prev + REM_prev) / total_prev);
      cofr_d = std::max(0.0, std::min(1.0, cofr_d));

      // Aggregation power
      double cofr_pow;
      if (a_d == 1.0)      cofr_pow = cofr_d;
      else if (a_d == 2.0) cofr_pow = cofr_d * cofr_d;
      else if (a_d == 3.0) cofr_pow = cofr_d * cofr_d * cofr_d;
      else                 cofr_pow = std::pow(cofr_d, a_d);

      // Flows (distributed over sub-steps)
      double ri_prim = Iprim_d * dt;
      double ri_sec = INF_prev * Rc_d * cofr_pow * dt;
      double ri_step = ri_prim + ri_sec;
      
      double rlex_step = rrlex_d * INF_prev * cofr_d * dt;
      double rt_step = RT_day * dt;
      double rrem_step = RREM_day * dt;
      double rsn_step = rrs_d * HS_prev * dt;
      double rg_step = rrg_d * HS_prev * (1.0 - (total_prev / SMax)) * dt;

      // Flow limiting (priority order:  RLEX > RI > RSN)
      double HS_avail = HS_prev + rg_step;
      if (HS_avail < 0.0) HS_avail = 0.0;
      
      rlex_step = std::min(rlex_step, HS_avail);
      HS_avail -= rlex_step;
      
      ri_step = std::min(ri_step, HS_avail);
      HS_avail -= ri_step;
      
      rsn_step = std:: min(rsn_step, HS_avail);
      
      double INF_avail = INF_prev + rt_step + rlex_step;
      if (INF_avail < 0.0) INF_avail = 0.0;
      rrem_step = std::min(rrem_step, INF_avail);

      // State updates
      HS_prev  = HS_prev + rg_step - ri_step - rlex_step - rsn_step;
      LAT_prev = LAT_prev + ri_step - rt_step;
      INF_prev = INF_prev + rt_step + rlex_step - rrem_step;
      REM_prev = REM_prev + rrem_step;
      SEN_prev = SEN_prev + rsn_step;

      // Accumulate daily totals
      RI_today   += ri_step;
      RLEX_today += rlex_step;
      RT_today   += rt_step;
      RREM_today += rrem_step;
      RG_today   += rg_step;
      RSN_today  += rsn_step;
    }

    // Store daily outputs
    sites[d]     = HS_prev;
    latent[d]    = LAT_prev;
    infectious[d] = INF_prev;
    removed[d]   = REM_prev;
    senesced[d]  = SEN_prev;

    rateinf[d]   = RI_today;
    rlex[d]      = RLEX_today;
    rtransfer[d] = RT_today;
    rremoved[d]  = RREM_today;
    rgrowth[d]   = RG_today;
    rsenesced[d] = RSN_today;

    diseased[d] = latent[d] + infectious[d] + removed[d];
    double denom_alive = sites[d] + latent[d] + infectious[d];
    intensity[d] = (denom_alive > EPSILON) ? ((latent[d] + infectious[d]) / denom_alive) : 0.0;
    
    double total_cur = sites[d] + latent[d] + infectious[d] + removed[d];
    cofr[d] = (total_cur > EPSILON) ? (1.0 - (diseased[d] / total_cur)) : 0.0;

    // Update histories
    RI_hist[d] = rateinf[d];
    BecInf_hist[d] = rtransfer[d] + rlex[d];
  }

  return Rcpp:: List::create(
    _["sites"]      = sites,
    _["latent"]     = latent,
    _["infectious"] = infectious,
    _["removed"]    = removed,
    _["senesced"]   = senesced,
    _["rateinf"]    = rateinf,
    _["rlex"]       = rlex,
    _["rtransfer"]  = rtransfer,
    _["rremoved"]   = rremoved,
    _["rgrowth"]    = rgrowth,
    _["rsenesced"]  = rsenesced,
    _["diseased"]   = diseased,
    _["intensity"]  = intensity,
    _["cofr"]       = cofr
  );
}

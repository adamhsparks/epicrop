// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

constexpr double EPSILON = 1e-15;

// Inline helper functions
inline double fast_max(const double a, const double b) {
  return (a > b) ? a : b;
}

inline double fast_min(const double a, const double b) {
  return (a < b) ? a : b;
}

inline double fast_pow(const double base, const double exp) {
  if (exp == 1.0)
    return base;
  if (exp == 2.0)
    return base * base;
  if (exp == 3.0)
    return base * base * base;
  if (exp == 4.0) {
    const double base2 = base * base;
    return base2 * base2;
  }
  return std::pow(base, exp);
}

inline double clamp(const double val, const double min_val,
                    const double max_val) {
  return fast_max(min_val, fast_min(val, max_val));
}

// Optimized backref
inline double backref_opt(const double *hist, const int d, const double delay) {
  if (delay <= 0.0)
    return 0.0;

  const double exact_day = static_cast<double>(d) - delay;
  if (exact_day < 0.0)
    return 0.0;

  const int j_lo = static_cast<int>(std::floor(exact_day));
  const int j_hi = j_lo + 1;

  if (j_lo < 0 || j_hi >= d)
    return 0.0;

  const double frac = exact_day - static_cast<double>(j_lo);
  double v = (1.0 - frac) * hist[j_lo];

  if (j_hi < d) {
    v += frac * hist[j_hi];
  }

  return fast_max(0.0, v);
}

//' Optimized Epicrop Core SEIR Model
//'
//' @description
//'
//' A model framework based on the medical idea of a Susceptible-Exposed-
//' Infectious-Removed (\acronym{SEIR}) model. This framework is designed for
//' plant diseases. This function is originally used by specific disease models
//' in \sQuote{EPIWHEAT} and \sQuote{EPIRICE} to model disease intensity of
//  several diseases. Given proper values it can be used with other pathosystems
//  as well.
//'
//' @param Rc Numeric vector: reproduction rate
//' @param RRLEX Numeric vector: rate of lesion expansion
//' @param RRG Numeric vector: rate of host growth
//' @param RRS Numeric vector: rate of senescence
//' @param p_days Numeric vector: latent period (days)
//' @param i_days Numeric vector: infectious period (days)
//' @param agg Numeric vector: aggregation parameter
//' @param Iprim Numeric vector: primary infection rate
//' @param H0 Numeric scalar: initial healthy sites
//' @param SMax Numeric scalar: maximum sites
//' @param steps_per_day Integer: sub-daily time steps (default 1)
//'
//' @references
//
//'
//' Madden, L. V., G. Hughes, and F. van den Bosch. 2007. The Study of Plant
//' Disease Epidemics. American Phytopathological Society, St. Paul, MN.
//' \doi{10.1094/9780890545058}.
//'
//' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. Modeling
//' and mapping potential epidemics of rice diseases globally. _Crop
//' Protection_, ' Volume 34, 2012, Pages 6-17, ISSN 0261-2194
//' \doi{10.1016/j.cropro.2011.11.009}.
//'
//'  Savary, S., Stetkiewicz, S., Brun, R., and Willocquet, L. Modelling and
//'  Mapping Potential Epidemics of Wheat Diseases—Examples on Leaf Rust and
//'  Septoria Tritici Blotch Using EPIWHEAT. _European Journal of Plant
//'  Pathology_ Volume 142, No. 4, 2015, Pages 771–90, DOI:
//' \doi{10.1007/s10658-015-0650-7}.
//'
//' Sparks, A.H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia, S.D.
//' Silwal, S. Tolos, and K.A. Garrett, 2008. Ecology and Epidemiology in R:
//' Disease Progress over Time. *The Plant Health Instructor*.
//' \doi{10.1094/PHI-A-2008-0129-02}.
//'
//' @return Named list with 14 numeric vectors containing simulation results
//'
//' @export
// [[Rcpp::export]]
Rcpp::List epicrop_core_cpp(const NumericVector &Rc, const NumericVector &RRLEX,
                            const NumericVector &RRG, const NumericVector &RRS,
                            const NumericVector &p_days,
                            const NumericVector &i_days,
                            const NumericVector &agg,
                            const NumericVector &Iprim, const double H0,
                            const double SMax, const int steps_per_day = 1) {

  const int n = Rc.size();
  const double dt = 1.0 / static_cast<double>(steps_per_day);
  const double inv_SMax = 1.0 / SMax;

  // Pre-allocate output vectors
  NumericVector sites(n);
  NumericVector latent(n);
  NumericVector infectious(n);
  NumericVector removed(n);
  NumericVector senesced(n);
  NumericVector rateinf(n);
  NumericVector rlex(n);
  NumericVector rtransfer(n);
  NumericVector rremoved(n);
  NumericVector rgrowth(n);
  NumericVector rsenesced(n);
  NumericVector diseased(n);
  NumericVector intensity(n);
  NumericVector cofr(n);

  std::vector<double> RI_hist(n);
  std::vector<double> BecInf_hist(n);

  // Get raw pointers for faster access (SAFE - standard Rcpp)
  double *sites_ptr = REAL(sites);
  double *latent_ptr = REAL(latent);
  double *infectious_ptr = REAL(infectious);
  double *removed_ptr = REAL(removed);
  double *senesced_ptr = REAL(senesced);
  double *rateinf_ptr = REAL(rateinf);
  double *rlex_ptr = REAL(rlex);
  double *rtransfer_ptr = REAL(rtransfer);
  double *rremoved_ptr = REAL(rremoved);
  double *rgrowth_ptr = REAL(rgrowth);
  double *rsenesced_ptr = REAL(rsenesced);
  double *diseased_ptr = REAL(diseased);
  double *intensity_ptr = REAL(intensity);
  double *cofr_ptr = REAL(cofr);

  const double *Rc_ptr = REAL(Rc);
  const double *RRLEX_ptr = REAL(RRLEX);
  const double *RRG_ptr = REAL(RRG);
  const double *RRS_ptr = REAL(RRS);
  const double *p_days_ptr = REAL(p_days);
  const double *i_days_ptr = REAL(i_days);
  const double *agg_ptr = REAL(agg);
  const double *Iprim_ptr = REAL(Iprim);

  double *RI_hist_ptr = RI_hist.data();
  double *BecInf_hist_ptr = BecInf_hist.data();

  // State variables
  double HS = fast_max(0.0, H0);
  double LAT = 0.0;
  double INF = 0.0;
  double REM = 0.0;
  double SEN = 0.0;

  // Main loop
  for (int d = 0; d < n; ++d) {

    // Cache and clamp parameters
    const double a_d = fast_max(1.0, agg_ptr[d]);
    const double Rc_d = fast_max(0.0, Rc_ptr[d]);
    const double rrlex_d = fast_max(0.0, RRLEX_ptr[d]);
    const double rrs_d = fast_max(0.0, RRS_ptr[d]);
    const double rrg_d = fast_max(0.0, RRG_ptr[d]);
    const double p_d = fast_max(0.0, p_days_ptr[d]);
    const double i_d = fast_max(0.0, i_days_ptr[d]);
    const double Iprim_d = fast_max(0.0, Iprim_ptr[d]);

    // Compute delays using optimized backref
    const double RT_day = backref_opt(RI_hist_ptr, d, p_d);
    const double RREM_day = backref_opt(BecInf_hist_ptr, d, i_d);

    double RI_acc = 0.0;
    double RLEX_acc = 0.0;
    double RT_acc = 0.0;
    double RREM_acc = 0.0;
    double RG_acc = 0.0;
    double RSN_acc = 0.0;

    // Unroll for common case (steps_per_day == 1)
    if (steps_per_day == 1) {

      const double total = fast_max(EPSILON, HS + LAT + INF + REM);
      const double inv_total = 1.0 / total; // One division
      const double cofr_d =
          clamp(1.0 - (LAT + INF + REM) * inv_total, 0.0, 1.0);

      // Fast power computation
      const double cofr_pow = fast_pow(cofr_d, a_d);

      // Compute flows
      const double ri_prim = Iprim_d;
      const double ri_sec = INF * Rc_d * cofr_pow;
      double ri = ri_prim + ri_sec;

      double rlex_val = rrlex_d * INF * cofr_d;
      const double rt = RT_day;
      double rrem = RREM_day;
      double rsn = rrs_d * HS;
      const double rg = rrg_d * HS * (1.0 - total * inv_SMax);

      // Flow limiting
      double HS_avail = fast_max(0.0, HS + rg);
      rlex_val = fast_min(rlex_val, HS_avail);
      HS_avail -= rlex_val;
      ri = fast_min(ri, HS_avail);
      HS_avail -= ri;
      rsn = fast_min(rsn, HS_avail);

      const double INF_avail = fast_max(0.0, INF + rt + rlex_val);
      rrem = fast_min(rrem, INF_avail);

      // State updates
      HS = fast_max(0.0, HS + rg - ri - rlex_val - rsn);
      LAT = fast_max(0.0, LAT + ri - rt);
      INF = fast_max(0.0, INF + rt + rlex_val - rrem);
      REM = fast_max(0.0, REM + rrem);
      SEN = fast_max(0.0, SEN + rsn);

      RI_acc = ri;
      RLEX_acc = rlex_val;
      RT_acc = rt;
      RREM_acc = rrem;
      RG_acc = rg;
      RSN_acc = rsn;

    } else {
      // Multiple steps per day
      for (int s = 0; s < steps_per_day; ++s) {

        const double total = fast_max(EPSILON, HS + LAT + INF + REM);
        const double inv_total = 1.0 / total;
        const double cofr_d =
            clamp(1.0 - (LAT + INF + REM) * inv_total, 0.0, 1.0);
        const double cofr_pow = fast_pow(cofr_d, a_d);

        double ri = (Iprim_d + INF * Rc_d * cofr_pow) * dt;
        double rlex_val = rrlex_d * INF * cofr_d * dt;
        const double rt = RT_day * dt;
        double rrem = RREM_day * dt;
        double rsn = rrs_d * HS * dt;
        const double rg = rrg_d * HS * (1.0 - total * inv_SMax) * dt;

        double HS_avail = fast_max(0.0, HS + rg);
        rlex_val = fast_min(rlex_val, HS_avail);
        HS_avail -= rlex_val;
        ri = fast_min(ri, HS_avail);
        HS_avail -= ri;
        rsn = fast_min(rsn, HS_avail);

        const double INF_avail = fast_max(0.0, INF + rt + rlex_val);
        rrem = fast_min(rrem, INF_avail);

        HS = fast_max(0.0, HS + rg - ri - rlex_val - rsn);
        LAT = fast_max(0.0, LAT + ri - rt);
        INF = fast_max(0.0, INF + rt + rlex_val - rrem);
        REM = fast_max(0.0, REM + rrem);
        SEN = fast_max(0.0, SEN + rsn);

        RI_acc += ri;
        RLEX_acc += rlex_val;
        RT_acc += rt;
        RREM_acc += rrem;
        RG_acc += rg;
        RSN_acc += rsn;
      }
    }

    // Store outputs
    sites_ptr[d] = HS;
    latent_ptr[d] = LAT;
    infectious_ptr[d] = INF;
    removed_ptr[d] = REM;
    senesced_ptr[d] = SEN;
    rateinf_ptr[d] = RI_acc;
    rlex_ptr[d] = RLEX_acc;
    rtransfer_ptr[d] = RT_acc;
    rremoved_ptr[d] = RREM_acc;
    rgrowth_ptr[d] = RG_acc;
    rsenesced_ptr[d] = RSN_acc;

    // Derived quantities
    const double dis = LAT + INF + REM;
    diseased_ptr[d] = dis;

    const double denom = HS + LAT + INF;
    intensity_ptr[d] = (denom > EPSILON) ? ((LAT + INF) / denom) : 0.0;

    const double total_cur = HS + LAT + INF + REM;
    cofr_ptr[d] = (total_cur > EPSILON) ? (1.0 - dis / total_cur) : 0.0;

    // Update history
    RI_hist_ptr[d] = RI_acc;
    BecInf_hist_ptr[d] = RT_acc + RLEX_acc;
  }

  return List::create(
      Named("sites") = sites, Named("latent") = latent,
      Named("infectious") = infectious, Named("removed") = removed,
      Named("senesced") = senesced, Named("rateinf") = rateinf,
      Named("rlex") = rlex, Named("rtransfer") = rtransfer,
      Named("rremoved") = rremoved, Named("rgrowth") = rgrowth,
      Named("rsenesced") = rsenesced, Named("diseased") = diseased,
      Named("intensity") = intensity, Named("cofr") = cofr);
}

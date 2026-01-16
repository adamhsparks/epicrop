#include <Rcpp.h>
using namespace Rcpp;
// SEIR core simulator - called by seir() wrapper function
// This is an internal function not intended for direct use
// [[Rcpp::export]]
Rcpp::List seir_core_cpp(const NumericVector& Rc_age,
                         const NumericVector& Rc_temp,
                         const NumericVector& Rc_wet,
                         const NumericVector& RRG,
                         const double RcOpt,
                         const int onset,
                         const int duration,
                         const int p,
                         const int i,
                         const double H0,
                         const double I0,
                         const double Sx,
                         const double a,
                         const double RRS) {

  // Constants
  const double EPSILON = 1e-12;

  // Input validation
  if (duration <= 0) stop("duration must be > 0");
  if (Rc_age.size() < duration || Rc_temp. size() < duration || 
      Rc_wet.size() < duration || RRG.size() < duration) {
    stop("Rc_age, Rc_temp, Rc_wet, and RRG must be length >= duration");
  }
  if (onset < 0 || onset >= duration) stop("onset must be in [0, duration-1]");
  if (p < 0 || i < 0) stop("p and i must be >= 0");
  if (H0 < 0.0 || I0 < 0.0) stop("H0 and I0 must be >= 0");
  if (Sx <= 0.0) stop("Sx must be > 0");
  if (RcOpt < 0.0) stop("RcOpt must be >= 0");
  if (a < 0.0) stop("aggregation coefficient 'a' must be >= 0");
  if (RRS < 0.0) stop("RRS must be >= 0");

  // Output vectors (Savary names)
  NumericVector rc(duration), cofr(duration);
  NumericVector sites(duration), latency(duration), infectious(duration), 
                removed(duration), senesced(duration);
  NumericVector infection(duration), rtransfer(duration), rgrowth(duration), 
                rsenesced(duration);
  NumericVector diseased(duration), total_sites(duration), intensity(duration);

  // State variables (stocks)
  double H_prev = H0;
  double L_prev = 0.0;
  double I_prev = 0.0;
  double R_prev = 0.0;
  double Sen_prev = 0.0;

  // Main simulation loop (0-indexed:  d = 0 is day 1, d = 1 is day 2, etc.)
  for (int d = 0; d < duration; ++d) {

    // Calculate total sites (including removed for cofr calculation)
    double total_prev = H_prev + L_prev + I_prev + R_prev;
    if (total_prev <= 0.0) total_prev = EPSILON;

    // Correction factor (proportion of healthy sites)
    cofr[d] = H_prev / total_prev;
    // Ensure cofr is in valid range [0, 1]
    if (cofr[d] < 0.0) cofr[d] = 0.0;
    if (cofr[d] > 1.0) cofr[d] = 1.0;

    // Daily infection rate modifier
    rc[d] = RcOpt * Rc_age[d] * Rc_temp[d] * Rc_wet[d];

    // Growth and physiological senescence of healthy sites
    double alive_prev = H_prev + L_prev + I_prev;
    // Ensure growth is non-negative (logistic growth with carrying capacity)
    rgrowth[d] = RRG[d] * H_prev * std::max(0.0, 1.0 - (alive_prev / Sx));
    rsenesced[d] = RRS * H_prev;

    // New infections (H -> L) with onset day semantics
    double inf;
    if (d < onset) {
      inf = 0.0;
    } else if (d == onset) {
      inf = I0;  // Initial infection introduced on onset day
    } else {
      // Optimize pow() for common values of aggregation coefficient
      double cofr_powered;
      if (a == 0.0) {
        cofr_powered = 1.0;
      } else if (a == 1.0) {
        cofr_powered = cofr[d];
      } else if (a == 2.0) {
        cofr_powered = cofr[d] * cofr[d];
      } else {
        cofr_powered = std::pow(cofr[d], a);
      }
      inf = I_prev * rc[d] * cofr_powered;
    }
    inf = std:: max(0.0, inf);  // Ensure non-negative
    infection[d] = inf;

    // Transfers from latent to infectious (L -> I) after p days
    double tr = 0.0;
    if (p > 0) {
      if (d >= p) tr = infection[d - p];
    } else {
      // p == 0 => immediate transfer same day
      tr = infection[d];
    }
    tr = std::max(0.0, tr);
    rtransfer[d] = tr;

    // Removals after i days in infectious state (I -> R)
    double removed_today = 0.0;
    if (i > 0) {
      if (d >= i) removed_today = rtransfer[d - i];
    } else {
      // i == 0 => immediate removal same day
      removed_today = rtransfer[d];
    }
    removed_today = std::max(0.0, removed_today);

    // Update state variables (end-of-day stocks)
    double H = H_prev + rgrowth[d] - inf - rsenesced[d];
    double L = L_prev + inf - tr;
    double I = I_prev + tr - removed_today;
    double R = R_prev + removed_today;
    double Sen = Sen_prev + rsenesced[d];

    // Ensure non-negative states (numerical safety)
    H = std::max(0.0, H);
    L = std::max(0.0, L);
    I = std::max(0.0, I);
    R = std::max(0.0, R);
    Sen = std::max(0.0, Sen);

    // Store state variables (Savary names)
    sites[d] = H;
    latency[d] = L;
    infectious[d] = I;
    removed[d] = R;
    senesced[d] = Sen;

    // Derived outputs
    diseased[d] = L + I + R;
    total_sites[d] = H + L + I + R;

    double denom_alive = H + L + I;
    intensity[d] = (denom_alive > EPSILON) ? ((L + I) / denom_alive) : 0.0;

    // Advance to next time step
    H_prev = H;
    L_prev = L;
    I_prev = I;
    R_prev = R;
    Sen_prev = Sen;
  }

  return List::create(
    _["rc"] = rc,
    _["cofr"] = cofr,
    _["sites"] = sites,
    _["latency"] = latency,
    _["infectious"] = infectious,
    _["removed"] = removed,
    _["senesced"] = senesced,
    _["infection"] = infection,
    _["rtransfer"] = rtransfer,
    _["rgrowth"] = rgrowth,
    _["rsenesced"] = rsenesced,
    _["diseased"] = diseased,
    _["total_sites"] = total_sites,
    _["intensity"] = intensity
  );
}

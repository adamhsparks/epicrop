// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
using namespace Rcpp;

// Implementation notes:
// - Internal arrays use 1..n indexing (vectors sized n+1) to match the original R loops.
// - Inputs expected to be length = duration (NumericVector).
// - onset_idx is 1-indexed (same semantics as your R function).
// - All numeric values are double (R numeric -> double).
//
// Exports:
//   seir_core_no_lex_cpp(...)
//   seir_core_with_lex_cpp(...)

// [[Rcpp::export]]
List seir_core_no_lex_cpp(int duration,
                         int onset_idx,
                         double H0,
                         NumericVector Iprim, // length duration
                         NumericVector Rc,
                         NumericVector p_days,
                         NumericVector i_days,
                         NumericVector RRG_vals,
                         NumericVector RRS_vals,
                         NumericVector a_vals,
                         double Sx) {
  int n = duration;
  // internal arrays 1..n
  std::vector<double> sites(n+1, 0.0);
  std::vector<double> latency(n+1, 0.0);
  std::vector<double> infectious_flow(n+1, 0.0);
  std::vector<double> cs_lat(n+1, 0.0);
  std::vector<double> cs_inf(n+1, 0.0);

  std::vector<double> rsenesced(n+1, 0.0);
  std::vector<double> senesced(n+1, 0.0);
  std::vector<double> rgrowth(n+1, 0.0);
  std::vector<double> rateinf(n+1, 0.0);
  std::vector<double> rlex(n+1, 0.0);
  std::vector<double> rtransfer(n+1, 0.0);
  std::vector<double> removed(n+1, 0.0);
  std::vector<double> now_latent(n+1, 0.0);
  std::vector<double> now_infectious(n+1, 0.0);
  std::vector<double> diseased(n+1, 0.0);

  // Day 1
  sites[1] = H0;
  rsenesced[1] = RRS_vals[0] * sites[1];
  senesced[1] = rsenesced[1];
  rgrowth[1] = RRG_vals[0] * sites[1] * (1.0 - (sites[1] / Sx));
  rateinf[1] = (1 == onset_idx) ? Iprim[0] : 0.0;

  // Loop
  for (int t = 2; t <= n; ++t) {
    int lat_src = t - (int) std::floor(p_days[t-1]);
    int inf_src = t - (int) std::floor(i_days[t-1]);

    double removed_today = 0.0;
    if (inf_src > 1 && (inf_src-1) >= 1 && (inf_src-1) <= n) {
      removed_today = infectious_flow[inf_src - 1];
    }

    sites[t] = std::max(0.0, sites[t-1] + rgrowth[t-1] - rateinf[t-1] - rsenesced[t-1]);
    rsenesced[t] = removed_today + RRS_vals[t-1] * sites[t];
    senesced[t] = senesced[t-1] + rsenesced[t];

    latency[t] = rateinf[t-1];
    cs_lat[t] = cs_lat[t-1] + latency[t];

    double win_lat0 = (lat_src > 2) ? cs_lat[lat_src - 2] : 0.0;
    now_latent[t] = cs_lat[t] - win_lat0;
    rtransfer[t] = (lat_src > 1) ? latency[lat_src - 1] : 0.0;

    // Infectious (no lesion expansion)
    infectious_flow[t] = rtransfer[t];
    cs_inf[t] = cs_inf[t-1] + infectious_flow[t];
    double win_inf0 = (inf_src > 2) ? cs_inf[inf_src - 2] : 0.0;
    now_infectious[t] = cs_inf[t] - win_inf0;

    removed[t] = cs_inf[t] - now_infectious[t];

    diseased[t] = now_latent[t] + now_infectious[t] + removed[t];

    double total_cur = sites[t] + diseased[t];
    double COFR = (total_cur > 0.0) ? 1.0 - (diseased[t] / total_cur) : 1.0;
    COFR = std::min(1.0, std::max(0.0, COFR));

    if (t == onset_idx) {
      rateinf[t] = Iprim[t-1];
    } else {
      rateinf[t] = now_infectious[t] * Rc[t-1] * std::pow(COFR, a_vals[t-1]);
    }

    double total_sites = sites[t] + diseased[t];
    rgrowth[t] = RRG_vals[t-1] * sites[t] * (1.0 - (total_sites / Sx));
  }

  // Assemble outputs (1..n -> 0..n-1)
  NumericVector sites_out(n), latent_out(n), infectious_out(n), removed_out(n),
                senesced_out(n), rateinf_out(n), rlex_out(n), rtransfer_out(n),
                rremoved_out(n), rgrowth_out(n), rsenesced_out(n),
                diseased_out(n), intensity_out(n);

  for (int t = 1; t <= n; ++t) {
    int idx = t - 1;
    sites_out[idx] = sites[t];
    latent_out[idx] = now_latent[t];
    infectious_out[idx] = now_infectious[t];
    removed_out[idx] = removed[t];
    senesced_out[idx] = senesced[t];
    rateinf_out[idx] = rateinf[t];
    rlex_out[idx] = 0.0; // identically zero for this core
    rtransfer_out[idx] = rtransfer[t];
    // rremoved = c(0, diff(removed))
    if (t == 1) rremoved_out[idx] = 0.0;
    else rremoved_out[idx] = removed[t] - removed[t-1];
    rgrowth_out[idx] = rgrowth[t];
    rsenesced_out[idx] = rsenesced[t];
    diseased_out[idx] = diseased[t];
    if ((sites[t] + now_latent[t] + now_infectious[t]) > 0.0) {
      intensity_out[idx] = (now_latent[t] + now_infectious[t]) / (sites[t] + now_latent[t] + now_infectious[t]);
    } else {
      intensity_out[idx] = 0.0;
    }
  }

  return List::create(
    Named("sites") = sites_out,
    Named("latent") = latent_out,
    Named("infectious") = infectious_out,
    Named("removed") = removed_out,
    Named("senesced") = senesced_out,
    Named("rateinf") = rateinf_out,
    Named("rlex") = rlex_out,
    Named("rtransfer") = rtransfer_out,
    Named("rremoved") = rremoved_out,
    Named("rgrowth") = rgrowth_out,
    Named("rsenesced") = rsenesced_out,
    Named("diseased") = diseased_out,
    Named("intensity") = intensity_out
  );
}


// [[Rcpp::export]]
List seir_core_with_lex_cpp(int duration,
                           int onset_idx,
                           double H0,
                           NumericVector Iprim,
                           NumericVector Rc,
                           NumericVector p_days,
                           NumericVector i_days,
                           NumericVector RRG_vals,
                           NumericVector RRS_vals,
                           NumericVector RRLEX_vals,
                           NumericVector a_vals,
                           double Sx) {
  int n = duration;
  std::vector<double> sites(n+1,0.0);
  std::vector<double> latency(n+1,0.0);
  std::vector<double> infectious_flow(n+1,0.0);
  std::vector<double> cs_lat(n+1,0.0);
  std::vector<double> cs_inf(n+1,0.0);

  std::vector<double> rsenesced(n+1, 0.0);
  std::vector<double> senesced(n+1, 0.0);
  std::vector<double> rgrowth(n+1, 0.0);
  std::vector<double> rateinf(n+1, 0.0);
  std::vector<double> rlex(n+1, 0.0);
  std::vector<double> rtransfer(n+1, 0.0);
  std::vector<double> removed(n+1, 0.0);
  std::vector<double> now_latent(n+1, 0.0);
  std::vector<double> now_infectious(n+1, 0.0);
  std::vector<double> diseased(n+1, 0.0);

  // Day 1
  sites[1] = H0;
  rsenesced[1] = RRS_vals[0] * sites[1];
  senesced[1] = rsenesced[1];
  rgrowth[1] = RRG_vals[0] * sites[1] * (1.0 - (sites[1] / Sx));
  rateinf[1] = (1 == onset_idx) ? Iprim[0] : 0.0;

  // Loop
  for (int t = 2; t <= n; ++t) {
    int lat_src = t - (int) std::floor(p_days[t-1]);
    int inf_src = t - (int) std::floor(i_days[t-1]);

    double removed_today = 0.0;
    if (inf_src > 1 && (inf_src-1) >= 1 && (inf_src-1) <= n) {
      removed_today = infectious_flow[inf_src - 1];
    }

    sites[t] = std::max(0.0, sites[t-1] + rgrowth[t-1] - rateinf[t-1] - rsenesced[t-1] - rlex[t-1]);
    rsenesced[t] = removed_today + RRS_vals[t-1] * sites[t];
    senesced[t] = senesced[t-1] + rsenesced[t];

    latency[t] = rateinf[t-1];
    cs_lat[t] = cs_lat[t-1] + latency[t];

    double win_lat0 = (lat_src > 2) ? cs_lat[lat_src - 2] : 0.0;
    now_latent[t] = cs_lat[t] - win_lat0;
    rtransfer[t] = (lat_src > 1) ? latency[lat_src - 1] : 0.0;

    // Stage A: infectious window before today's lesion expansion
    double inf_wo_lex = rtransfer[t];
    double cs_pre = cs_inf[t-1] + inf_wo_lex;
    double now_inf_pre = (inf_src > 2) ? cs_pre - cs_inf[inf_src - 2] : cs_pre;

    double removed_pre = cs_pre - now_inf_pre;
    double diseased_pre = cs_pre + now_latent[t] + removed_pre;

    double total_cur = sites[t] + diseased_pre;
    double COFR = (total_cur > 0.0) ? 1.0 - (diseased_pre / total_cur) : 1.0;
    COFR = std::min(1.0, std::max(0.0, COFR));

    rlex[t] = RRLEX_vals[t-1] * now_inf_pre * std::max(0.0, COFR);

    // Finalise infectious window including lesion expansion
    infectious_flow[t] = rtransfer[t] + rlex[t];
    cs_inf[t] = cs_inf[t-1] + infectious_flow[t];
    double win_inf0 = (inf_src > 2) ? cs_inf[inf_src - 2] : 0.0;
    now_infectious[t] = cs_inf[t] - win_inf0;

    removed[t] = cs_inf[t] - now_infectious[t];
    diseased[t] = now_latent[t] + now_infectious[t] + removed[t];

    double COFR_use = COFR; // same COFR from pre-lex in original R
    if (t == onset_idx) {
      rateinf[t] = Iprim[t-1];
    } else {
      rateinf[t] = now_infectious[t] * Rc[t-1] * std::pow(COFR_use, a_vals[t-1]);
    }

    double total_sites = sites[t] + diseased[t];
    rgrowth[t] = RRG_vals[t-1] * sites[t] * (1.0 - (total_sites / Sx));
  }

  // Assemble outputs (1..n -> 0..n-1)
  NumericVector sites_out(n), latent_out(n), infectious_out(n), removed_out(n),
                senesced_out(n), rateinf_out(n), rlex_out(n), rtransfer_out(n),
                rremoved_out(n), rgrowth_out(n), rsenesced_out(n),
                diseased_out(n), intensity_out(n);

  for (int t = 1; t <= n; ++t) {
    int idx = t - 1;
    sites_out[idx] = sites[t];
    latent_out[idx] = now_latent[t];
    infectious_out[idx] = now_infectious[t];
    removed_out[idx] = removed[t];
    senesced_out[idx] = senesced[t];
    rateinf_out[idx] = rateinf[t];
    rlex_out[idx] = rlex[t];
    rtransfer_out[idx] = rtransfer[t];
    if (t == 1) rremoved_out[idx] = 0.0;
    else rremoved_out[idx] = removed[t] - removed[t-1];
    rgrowth_out[idx] = rgrowth[t];
    rsenesced_out[idx] = rsenesced[t];
    diseased_out[idx] = diseased[t];
    if ((sites[t] + now_latent[t] + now_infectious[t]) > 0.0) {
      intensity_out[idx] = (now_latent[t] + now_infectious[t]) / (sites[t] + now_latent[t] + now_infectious[t]);
    } else {
      intensity_out[idx] = 0.0;
    }
  }

  return List::create(
    Named("sites") = sites_out,
    Named("latent") = latent_out,
    Named("infectious") = infectious_out,
    Named("removed") = removed_out,
    Named("senesced") = senesced_out,
    Named("rateinf") = rateinf_out,
    Named("rlex") = rlex_out,
    Named("rtransfer") = rtransfer_out,
    Named("rremoved") = rremoved_out,
    Named("rgrowth") = rgrowth_out,
    Named("rsenesced") = rsenesced_out,
    Named("diseased") = diseased_out,
    Named("intensity") = intensity_out
  );
}

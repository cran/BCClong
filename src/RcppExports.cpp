// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// BCC
Rcpp::List BCC(Rcpp::List& dat, int R, Rcpp::List id, arma::umat n_obs, int N, int num_cluster, std::vector<std::string> dist, bool alpha_common, bool sigma_sq_e_common, arma::vec p, arma::vec q, arma::vec ppi, arma::vec alpha, arma::vec zz, arma::mat zz_local, std::vector<arma::mat> gamma, std::vector<arma::vec> sigma_sq_e, std::vector<arma::vec> phi, std::vector<arma::cube> sigma_sq_u, std::vector<arma::mat> beta, arma::vec delta, double a_star, double b_star, double aa0, double bb0, arma::mat a0, arma::mat b0, std::vector<arma::mat> v0, std::vector<arma::cube> V0, double cc0, double dd0, arma::mat c0, arma::mat d0, double rr0, double RR0, double ww0, double vv0, arma::mat lambda0, std::vector<arma::cube> Lambda0, Rcpp::RObject LOG_LIK_ITER, Rcpp::RObject PPI, Rcpp::RObject ZZ, Rcpp::RObject ALPHA, std::vector<arma::mat> ZZ_LOCAL, std::vector<Rcpp::RObject> GA, std::vector<arma::mat> GA_ACCEPT, std::vector<arma::cube> THETA, std::vector<arma::mat> THETA_ACCEPT, std::vector<Rcpp::RObject> SIGMA_SQ_U, std::vector<arma::mat> SIGMA_SQ_E, std::vector<arma::cube> T_LOCAL, Rcpp::RObject T, bool adaptive_tunning, double tunning_freq, arma::mat c_gamma_tunning, arma::vec c_beta_tunning, int burn_in, int thin, int per, int max_iter, int seed_initial);
RcppExport SEXP _BCClong_BCC(SEXP datSEXP, SEXP RSEXP, SEXP idSEXP, SEXP n_obsSEXP, SEXP NSEXP, SEXP num_clusterSEXP, SEXP distSEXP, SEXP alpha_commonSEXP, SEXP sigma_sq_e_commonSEXP, SEXP pSEXP, SEXP qSEXP, SEXP ppiSEXP, SEXP alphaSEXP, SEXP zzSEXP, SEXP zz_localSEXP, SEXP gammaSEXP, SEXP sigma_sq_eSEXP, SEXP phiSEXP, SEXP sigma_sq_uSEXP, SEXP betaSEXP, SEXP deltaSEXP, SEXP a_starSEXP, SEXP b_starSEXP, SEXP aa0SEXP, SEXP bb0SEXP, SEXP a0SEXP, SEXP b0SEXP, SEXP v0SEXP, SEXP V0SEXP, SEXP cc0SEXP, SEXP dd0SEXP, SEXP c0SEXP, SEXP d0SEXP, SEXP rr0SEXP, SEXP RR0SEXP, SEXP ww0SEXP, SEXP vv0SEXP, SEXP lambda0SEXP, SEXP Lambda0SEXP, SEXP LOG_LIK_ITERSEXP, SEXP PPISEXP, SEXP ZZSEXP, SEXP ALPHASEXP, SEXP ZZ_LOCALSEXP, SEXP GASEXP, SEXP GA_ACCEPTSEXP, SEXP THETASEXP, SEXP THETA_ACCEPTSEXP, SEXP SIGMA_SQ_USEXP, SEXP SIGMA_SQ_ESEXP, SEXP T_LOCALSEXP, SEXP TSEXP, SEXP adaptive_tunningSEXP, SEXP tunning_freqSEXP, SEXP c_gamma_tunningSEXP, SEXP c_beta_tunningSEXP, SEXP burn_inSEXP, SEXP thinSEXP, SEXP perSEXP, SEXP max_iterSEXP, SEXP seed_initialSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List& >::type dat(datSEXP);
    Rcpp::traits::input_parameter< int >::type R(RSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type id(idSEXP);
    Rcpp::traits::input_parameter< arma::umat >::type n_obs(n_obsSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type num_cluster(num_clusterSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type dist(distSEXP);
    Rcpp::traits::input_parameter< bool >::type alpha_common(alpha_commonSEXP);
    Rcpp::traits::input_parameter< bool >::type sigma_sq_e_common(sigma_sq_e_commonSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type p(pSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type q(qSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type ppi(ppiSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type zz(zzSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type zz_local(zz_localSEXP);
    Rcpp::traits::input_parameter< std::vector<arma::mat> >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< std::vector<arma::vec> >::type sigma_sq_e(sigma_sq_eSEXP);
    Rcpp::traits::input_parameter< std::vector<arma::vec> >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< std::vector<arma::cube> >::type sigma_sq_u(sigma_sq_uSEXP);
    Rcpp::traits::input_parameter< std::vector<arma::mat> >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< double >::type a_star(a_starSEXP);
    Rcpp::traits::input_parameter< double >::type b_star(b_starSEXP);
    Rcpp::traits::input_parameter< double >::type aa0(aa0SEXP);
    Rcpp::traits::input_parameter< double >::type bb0(bb0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type a0(a0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type b0(b0SEXP);
    Rcpp::traits::input_parameter< std::vector<arma::mat> >::type v0(v0SEXP);
    Rcpp::traits::input_parameter< std::vector<arma::cube> >::type V0(V0SEXP);
    Rcpp::traits::input_parameter< double >::type cc0(cc0SEXP);
    Rcpp::traits::input_parameter< double >::type dd0(dd0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type c0(c0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type d0(d0SEXP);
    Rcpp::traits::input_parameter< double >::type rr0(rr0SEXP);
    Rcpp::traits::input_parameter< double >::type RR0(RR0SEXP);
    Rcpp::traits::input_parameter< double >::type ww0(ww0SEXP);
    Rcpp::traits::input_parameter< double >::type vv0(vv0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type lambda0(lambda0SEXP);
    Rcpp::traits::input_parameter< std::vector<arma::cube> >::type Lambda0(Lambda0SEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type LOG_LIK_ITER(LOG_LIK_ITERSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type PPI(PPISEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type ZZ(ZZSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type ALPHA(ALPHASEXP);
    Rcpp::traits::input_parameter< std::vector<arma::mat> >::type ZZ_LOCAL(ZZ_LOCALSEXP);
    Rcpp::traits::input_parameter< std::vector<Rcpp::RObject> >::type GA(GASEXP);
    Rcpp::traits::input_parameter< std::vector<arma::mat> >::type GA_ACCEPT(GA_ACCEPTSEXP);
    Rcpp::traits::input_parameter< std::vector<arma::cube> >::type THETA(THETASEXP);
    Rcpp::traits::input_parameter< std::vector<arma::mat> >::type THETA_ACCEPT(THETA_ACCEPTSEXP);
    Rcpp::traits::input_parameter< std::vector<Rcpp::RObject> >::type SIGMA_SQ_U(SIGMA_SQ_USEXP);
    Rcpp::traits::input_parameter< std::vector<arma::mat> >::type SIGMA_SQ_E(SIGMA_SQ_ESEXP);
    Rcpp::traits::input_parameter< std::vector<arma::cube> >::type T_LOCAL(T_LOCALSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type T(TSEXP);
    Rcpp::traits::input_parameter< bool >::type adaptive_tunning(adaptive_tunningSEXP);
    Rcpp::traits::input_parameter< double >::type tunning_freq(tunning_freqSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type c_gamma_tunning(c_gamma_tunningSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type c_beta_tunning(c_beta_tunningSEXP);
    Rcpp::traits::input_parameter< int >::type burn_in(burn_inSEXP);
    Rcpp::traits::input_parameter< int >::type thin(thinSEXP);
    Rcpp::traits::input_parameter< int >::type per(perSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< int >::type seed_initial(seed_initialSEXP);
    rcpp_result_gen = Rcpp::wrap(BCC(dat, R, id, n_obs, N, num_cluster, dist, alpha_common, sigma_sq_e_common, p, q, ppi, alpha, zz, zz_local, gamma, sigma_sq_e, phi, sigma_sq_u, beta, delta, a_star, b_star, aa0, bb0, a0, b0, v0, V0, cc0, dd0, c0, d0, rr0, RR0, ww0, vv0, lambda0, Lambda0, LOG_LIK_ITER, PPI, ZZ, ALPHA, ZZ_LOCAL, GA, GA_ACCEPT, THETA, THETA_ACCEPT, SIGMA_SQ_U, SIGMA_SQ_E, T_LOCAL, T, adaptive_tunning, tunning_freq, c_gamma_tunning, c_beta_tunning, burn_in, thin, per, max_iter, seed_initial));
    return rcpp_result_gen;
END_RCPP
}
// LL
arma:: mat LL(const Rcpp::List& fit, int fast_version);
RcppExport SEXP _BCClong_LL(SEXP fitSEXP, SEXP fast_versionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List& >::type fit(fitSEXP);
    Rcpp::traits::input_parameter< int >::type fast_version(fast_versionSEXP);
    rcpp_result_gen = Rcpp::wrap(LL(fit, fast_version));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_BCClong_BCC", (DL_FUNC) &_BCClong_BCC, 61},
    {"_BCClong_LL", (DL_FUNC) &_BCClong_LL, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_BCClong(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

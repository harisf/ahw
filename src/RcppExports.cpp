// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// calculateCumHazard
DataFrame calculateCumHazard(DataFrame Times);
RcppExport SEXP _ahw_calculateCumHazard(SEXP TimesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type Times(TimesSEXP);
    rcpp_result_gen = Rcpp::wrap(calculateCumHazard(Times));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ahw_calculateCumHazard", (DL_FUNC) &_ahw_calculateCumHazard, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_ahw(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double rcpp_QuantileSearch(NumericVector x, NumericVector w, double q, double sw, int n) {

  // Variable declaration
  bool searching = {true};
  IntegerVector etendue = IntegerVector::create(0, n);
  double resultat = NA_REAL;
  double p1 = NA_REAL;
  double p2 = NA_REAL;
  int pivot = NA_INTEGER;

  // Searching algorithm based on wikipedia page (selection sort like algorithm)
  while(searching == true) {
    if (etendue[0] == etendue[1]) {
      resultat = x[etendue[0]];
      searching = false;
    } else if ((etendue[1] - etendue[0]) == 1) {
      resultat = (w[etendue[0]] * x[etendue[0]] + w[etendue[1]] * x[etendue[1]]) / (w[etendue[0]] + w[etendue[1]]);
      searching = false;
    } else {
      pivot = floor((etendue[0] + etendue[1]) / 2);
      p1 = sum(w[seq(0, pivot - 1)]) / sw;
      p2 = sum(w[seq(pivot + 1, n)]) / sw;
      if ((p1 < q) & (p2 < (1 - q))) {
        resultat = x[pivot];
        searching = false;
      } else if (p1 >= q) {
        etendue[1] = pivot;
      } else {
        etendue[0] = pivot;
      }
    }
  }

  return resultat;

}






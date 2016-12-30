#include <RcppArmadillo.h>
#include <math.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//[[Rcpp::export]]
arma::cube Price_Predictor(arma::mat prices, NumericVector array2)
{
  IntegerVector dim2 = array2.attr("dim");
  arma::cube hedge_prices(array2.begin(),dim2[0], dim2[1], dim2[2], false);
  
  int hedge_scenarios = dim2[0];
  int days = dim2[1];
  int assets = dim2[2];

  arma::cube hedge_prices_new = arma::zeros(dim2[0],dim2[1], dim2[2]);
  for ( int i = 0; i < hedge_scenarios; i++)
  {
    for ( int j = 0; j< days; j++)
    {
      for ( int k =0; k< assets; k++)
      {
        hedge_prices_new(i,j,k) = prices(j,k) * hedge_prices(i,days - 1,k)/hedge_prices(i,j,k);
      }
    }
  }
  return hedge_prices_new;
}
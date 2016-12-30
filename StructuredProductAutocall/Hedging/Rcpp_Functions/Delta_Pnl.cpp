/**
Input - Two 3D arrays, each having dimension - 'simulations' * 'days' * 'assets'
  a) The first input contains the PRICES obtained from actual/realized Monte Carlo simulation
  b) The second input contains the DELTA values calculated for all simulations on all days
  
Output - 2D array storing DELTA PNL, simulations along rows and days across columns

Output[i][j] = ( Input1[i][j+1][k] - Input1[i][j][k] ) * Input2[i][j][k], 0 < i < simulations, 0 < j < days-1, 0< k < assets
*/

#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
//[[Rcpp::export]]
arma::mat Delta_Pnl_Function(NumericVector array1, NumericVector array2)
{
  IntegerVector dim = array1.attr("dim"); 
  arma::cube my_array1(array1.begin(),dim[0], dim[1], dim[2], false);
  arma::cube my_array2(array2.begin(),dim[0], dim[1], dim[2], false);
  arma::mat new_mat = arma::zeros(dim[0],dim[1] - 1);
  for(int i = 0; i < dim[0]; ++i)
  {
    for(int j = 0; j <  dim[1] - 1; ++j)
    {
      double pnl = 0;
      for(int k = 0; k <  dim[2]; ++k)
      {pnl += ( my_array1(i,j+1,k) - my_array1(i,j,k) ) * my_array2(i,j,k);}
      new_mat(i,j) = pnl;
    }
    
  }
  return new_mat;
}
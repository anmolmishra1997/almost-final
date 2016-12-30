/**
Input - 2D array storing DELTA PNL, with simulations along rows and days across columns
Output - 2D array storing CUMULATIVE DELTA PNL, simulations along rows and days across columns

Output[i][j] = Input[i][1] + Input[i][2] + .... Input[i][j], 0 < i < simulations, 0 < j < days
*/

#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
//[[Rcpp::export]]
arma::mat Cumulative_Hedge_Pnl( arma::mat array1 )
{
  int num_rows = array1.n_rows;
  int num_cols = array1.n_cols;
  arma::mat new_mat = arma::zeros(num_rows, num_cols + 1);
  new_mat.col(0) = array1.col(0);
  for(int i = 0; i < num_rows; ++i)
  {
    for(int j = 1; j <  num_cols; ++j)
    {
       new_mat(i,j) = new_mat(i,j-1) + array1(i,j);
    }
  }
  return new_mat;
}

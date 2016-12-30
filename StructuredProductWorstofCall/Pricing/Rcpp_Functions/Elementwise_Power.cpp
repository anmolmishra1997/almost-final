/**
 Input - Two 2D matrices of same order
 Output - 2D matrix of same order as input
 
 Output[i][j] = Input1[i][j] ^ Input2[i][j]
*/



#include <RcppArmadillo.h>
#include <math.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
//[[Rcpp::export]]
arma::mat Elementwise_Power( arma::mat array1, arma::mat array2 )
{
  int num_rows = array1.n_rows;
  int num_cols = array1.n_cols;
  arma::mat new_mat = arma::zeros(num_rows, num_cols);
  for(int i = 0; i < num_rows; ++i)
  {
    for(int j = 0; j <  num_cols; ++j)
    {
      new_mat(i,j) = pow(array1(i,j),array2(i,j));
    }
  }
  return new_mat;
}

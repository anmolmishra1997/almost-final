/**
 Input - 2D array
 Output - 1D array of same number of elements as input's columns
 
 Output[j] = (Input[1][j] + Input[2][j] + .... Input[simulations][j])/simulations , 0 < i < simulations, 0 < j < days
*/

#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
//[[Rcpp::export]]
arma::vec Mean_2D_Scenarios(arma::mat array)
{
  int num_rows = array.n_rows;
  int num_cols = array.n_cols;
  arma::vec new_mat = arma::zeros(num_cols);
  for(int j = 0; j < num_cols; ++j)
  {
    double sum = 0;
    for(int i = 0; i <  num_rows; ++i)
    {sum += array(i,j);}
    new_mat(j) = sum/num_rows;
  }
  return new_mat;
}
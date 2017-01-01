/**
 Input - Two arrays, first 3D with dimensions - 'simulations' * 'days' * 'assets' and second 1D of length 'assets'
 a) The first input contains the DELTA values calculated for all simulations on all days
 b) The second input contains the MU( from normal distribution) values of all assets
 
 Output - 2D array storing ROLL COST, simulations along rows and days across columns
 
 Output[i][j] = Input1[i][j][1]* Input2[1] + Input1[i][j][2]* Input2[2] + ... + Input1[i][j][assets]* Input2[assets], 0 < i < simulations, 0 < j < days-1, 0< k < assets
 */

#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
//[[Rcpp::export]]
arma::mat Roll_Cost(NumericVector array, arma::mat array2)
{
  IntegerVector dim = array.attr("dim"); 
  arma::cube my_array(array.begin(),dim[0], dim[1], dim[2], false);
  arma::mat new_mat = arma::zeros(dim[0],dim[1]);
  for(int i = 0; i < dim[0]; ++i)
  {
    for(int j = 0; j <  dim[1]; ++j)
    {
      double term = 0;
      for(int k = 0; k <  dim[2]; ++k)
      {term += my_array(i,j,k) * array2(j,k) * 1.0/365;}
      new_mat(i,j) = term;
    }
  }
  return new_mat;
}
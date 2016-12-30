/**
 Input - 3D array
 Output - 2D array storing the mean of input array across all simulations
 
 Output[j][k] = (Input[1][j][k] + Input[2][j][k] + .... Input[simulations][j][k])/simulations , 0 < i < simulations, 0 < j < days, 0< k < assets
*/

#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
//[[Rcpp::export]]
arma::mat Mean_Scenarios(NumericVector array)
{
  IntegerVector dim = array.attr("dim"); 
  arma::cube my_array(array.begin(),dim[0], dim[1], dim[2], false);
  arma::mat new_mat = arma::zeros(dim[1],dim[2]);
  for(int i = 0; i < dim[1]; ++i)
  {
    for(int j = 0; j <  dim[2]; ++j)
    {
      double sum = 0;
      for(int k = 0; k <  dim[0]; ++k)
      {sum += my_array(k,i,j);}
      new_mat(i,j) = sum/dim[0];
    }
    
  }
  return new_mat;
}
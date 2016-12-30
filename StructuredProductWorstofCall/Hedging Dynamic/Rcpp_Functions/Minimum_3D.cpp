/**
 Input - 3D array
 Output - 2D array storing minimum of input array along 3rd dimension
 
 Output[i][j] = min(Input[i][j][1], Input[i][j][2], .... Input[i][j][assets]) , 0 < i < simulations, 0 < j < days, 0< k < assets
*/

#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
//[[Rcpp::export]]
arma::mat Minimum_3D(NumericVector array)
{
  IntegerVector dim = array.attr("dim"); 
  arma::cube my_array(array.begin(),dim[0], dim[1], dim[2], false);
  arma::mat new_mat = arma::zeros(dim[0],dim[1]);
  for(int i = 0; i < dim[0]; ++i)
  {
    for(int j = 0; j <  dim[1]; ++j)
    {
      double minimum = my_array(i,j,0);
      for(int k = 1; k <  dim[2]; ++k)
      {minimum = (minimum < my_array(i,j,k))?minimum:my_array(i,j,k);}
      new_mat(i,j) = minimum;
    }
  }
  return new_mat;
}
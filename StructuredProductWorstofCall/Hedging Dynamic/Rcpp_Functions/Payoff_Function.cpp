#include <RcppArmadillo.h>
#include <math.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

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
      {
      minimum = (minimum < my_array(i,j,k))?minimum:my_array(i,j,k);}
      new_mat(i,j) = minimum;
    }
  }
  return new_mat;
}

arma::mat Minimum_Position_3D(NumericVector array)
{
  IntegerVector dim = array.attr("dim"); 
  arma::cube my_array(array.begin(),dim[0], dim[1], dim[2], false);
  arma::mat new_mat = arma::zeros(dim[0],dim[1]);
  for(int i = 0; i < dim[0]; ++i)
  {
    for(int j = 0; j <  dim[1]; ++j)
    {
      double minimum = 0;
      for(int k = 0; k <  dim[2]; ++k)
      {minimum = (my_array(i,j,minimum) < my_array(i,j,k))?minimum:k;}
      new_mat(i,j) = minimum;
    }
    
  }
  return new_mat;
}

//[[Rcpp::export]]
arma::mat Payoff_Function(NumericVector array1, arma::mat array2,double value, double participation)
{
  IntegerVector dim = array1.attr("dim"); 
  arma::cube my_array1(array1.begin(),dim[0], dim[1], dim[2], false);
  arma::mat min_stock = Minimum_3D(array1);
  arma::mat min_stock_id = Minimum_Position_3D(array1);
  double powerraised = 0.0;
  arma::mat new_mat = arma::zeros(dim[0],dim[1]);
  int simulations = dim[0];
  int time = dim[1];
  for(int j = 0; j <  time; ++j)
  {
    for(int i = 0; i < simulations; ++i)
    {
      min_stock_id(i,j) = array2(j, min_stock_id(i,j)); 
    }
  }
  
  
  for(int j = 0; j <  time; ++j)
  {
    powerraised = (time - j - 1)/365.0;
    for(int i = 0; i < simulations; ++i)
    {
      new_mat(i,j) = participation * value * std::max( std::min(min_stock(i,j)/value - 1, 1.0),0.0)/pow(1+min_stock_id(i,j), powerraised); 
    }
  }
  
  return new_mat;
}
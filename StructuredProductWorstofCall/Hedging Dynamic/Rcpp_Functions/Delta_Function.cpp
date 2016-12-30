#include <RcppArmadillo.h>
#include <math.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

arma::mat Minimum_3D(arma::cube array)
{
  int dim1 = array.n_rows;
  int dim2 = array.n_cols;
  int dim3 = array.n_slices;
  arma::mat new_mat = arma::zeros(dim1,dim2);
  for(int i = 0; i < dim1; ++i)
  {
    for(int j = 0; j <  dim2; ++j)
    {
      double minimum = array(i,j,0);
      for(int k = 1; k <  dim3; ++k)
      {
        minimum = (minimum < array(i,j,k))?minimum:array(i,j,k);}
      new_mat(i,j) = minimum;
    }
  }
  return new_mat;
}

arma::mat Mean_2D_Scenarios(arma::mat array)
{
  int num_rows = array.n_rows;
  int num_cols = array.n_cols;
  arma::mat new_mat = arma::zeros(1,num_cols);
  for(int j = 0; j < num_cols; ++j)
  {
    double sum = 0;
    for(int i = 0; i <  num_rows; ++i)
    {sum += array(i,j);}
    new_mat(0,j) = sum/num_rows;
  }
  return new_mat;
}

arma::mat Mean_Scenarios(arma::cube array)
{
  int dim1 = array.n_rows;
  int dim2 = array.n_cols;
  int dim3 = array.n_slices;
  arma::mat new_mat = arma::zeros(dim2,dim3);
  for(int i = 0; i < dim2; ++i)
  {
    for(int j = 0; j <  dim3; ++j)
    {
      double sum = 0;
      for(int k = 0; k <  dim1; ++k)
      {sum += array(k,i,j);}
      new_mat(i,j) = sum/dim1;
    }
    
  }
  return new_mat;
}

arma::mat Minimum_Position_3D(arma::cube array)
{
  int dim1 = array.n_rows;
  int dim2 = array.n_cols;
  int dim3 = array.n_slices;
  arma::mat new_mat = arma::zeros(dim1,dim2);
  for(int i = 0; i < dim1; ++i)
  {
    for(int j = 0; j <  dim2; ++j)
    {
      double minimum = 0;
      for(int k = 0; k <  dim3; ++k)
      {minimum = (array(i,j,minimum) < array(i,j,k))?minimum:k;}
      new_mat(i,j) = minimum;
    }
    
  }
  return new_mat;
}

arma::mat Payoff_Function(arma::cube array1, NumericVector array2,int value)
{
  int simulations = array1.n_rows;
  int days = array1.n_cols;
  arma::mat min_stock = Minimum_3D(array1);
  arma::mat min_stock_id = Minimum_Position_3D(array1);
  double powerraised = 0.0;
  arma::mat new_mat = arma::zeros(simulations,days);
  
  for(int j = 0; j <  days; ++j)
  {
    for(int i = 0; i < simulations; ++i)
    {
      min_stock_id(i,j) = array2(min_stock_id(i,j)); 
    }
  }
  
  for(int j = 0; j <  days; ++j)
  {
    powerraised = (days - j - 1)/365.0;
    for(int i = 0; i < simulations; ++i)
    {
      new_mat(i,j) = std::max(std::min(min_stock(i,j)/value - 1, 1.0),0.0)/pow(1+min_stock_id(i,j), powerraised); 
    }
  }
  
  return new_mat;
}

//[[Rcpp::export]]
arma::cube Delta_Function(NumericVector array1, NumericVector array2,NumericVector array3, int value)
{
  IntegerVector dim1 = array1.attr("dim"); 
  arma::cube prices(array1.begin(),dim1[0], dim1[1], dim1[2], false);
  IntegerVector dim2 = array2.attr("dim");
  arma::cube hedge_prices(array2.begin(),dim2[0], dim2[1], dim2[2], false);
  arma::mat actual_payoff = arma::zeros(dim1[0],dim1[1]);
  arma::cube delta_all = arma::zeros(dim1[0],dim1[1], dim1[2]);
  
  // int scenarios = dim1[0];
  int hedge_scenarios = dim2[0];
  int days = dim1[1];
  int assets = dim1[2];
  int a = 20;
  // for ( int a = 0; a< scenarios; a++)
  // {
    arma::cube hedge_prices_new = arma::zeros(dim1[0],dim1[1], dim1[2]);
    for ( int i = 0; i < hedge_scenarios; i++)
    {
      for ( int j = 0; j< days; j++)
      {
        for ( int k =0; k< assets; k++)
        {
          hedge_prices_new(i,j,k) = prices(a,j,k) * hedge_prices(i,days - 1,k)/hedge_prices(i,j,k);
        }
      }
    }

    
    arma::mat min_stock = Minimum_3D(hedge_prices_new);
    arma::mat min_stock_id = Minimum_Position_3D(hedge_prices_new);
    double powerraised = 0.0;
    arma::mat payoff_a = arma::zeros(hedge_scenarios,days);
    
    for(int j = 0; j <  days; ++j)
    {
      for(int i = 0; i < hedge_scenarios; ++i)
      {
        min_stock_id(i,j) = array2(min_stock_id(i,j)); 
      }
    }
    
    for(int j = 0; j <  days; ++j)
    {
      powerraised = (days - j - 1)/365.0;
      for(int i = 0; i < hedge_scenarios; ++i)
      {
        payoff_a(i,j) = std::max(std::min(min_stock(i,j)/value - 1, 1.0),0.0)/pow(1+min_stock_id(i,j), powerraised); 
      }
    }
    
    actual_payoff.row(a) = Mean_2D_Scenarios(payoff_a);
    
    arma::mat payoff_increment = arma::mat(dim2[0],dim2[1]);
    arma::cube delta_hedge = arma::zeros(dim2[0],dim2[1],dim2[2]);
    
    for ( int k = 0; k< assets; k++)
    {
      arma::cube hedge_prices_increment = hedge_prices_new;
      hedge_prices_increment.slice(k) = hedge_prices_new.slice(k) * 1.01;
      arma::mat payoff_increment= Payoff_Function(hedge_prices_increment, array3, value);
      for ( int i = 0; i< hedge_scenarios; i++)
      {
        for ( int j =0; j<days; j++)
        {
          delta_hedge(i,j,k) = (payoff_increment(i,j) - payoff_a(i,j) )/(prices(a,j,k) * 0.01);  
        }
      }
    }

    delta_all(arma::span(a),arma::span(),arma::span()) = Mean_Scenarios(delta_hedge);
  // }
  return delta_all;
}


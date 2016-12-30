public class DeltaCalc43Misc {

public static void main(String[] args) {
	
}
	
public double[][] delta(double[][] CS1,double[][] CS2,double[][] CS3,double[][] CS4,
			double[][] CShz1,double[][] CShz2,double[][] CShz3,double[][] CShz4, int stk, int observation_dates_tenor[],
			double coupon_accrual_ER[], double coupon_barrier, double autocall_barrier, double barrier, double discount_factors[],
			double bond_deficiency[], double rates, double rate1, double rate2, double rate3)
{	double[][] deltacalculated=new double[CS1.length][(observation_dates_tenor[observation_dates_tenor.length-1]-1)];
    	double deltasum=0;
		
	double b1=0,b2=0,b3=0;
	if	(stk==1) b1=.01; else if(stk==2) b2=.01; else b3=.01;
	for(int i=0;i<CS1.length;i++){
		for(int j=0;j<(observation_dates_tenor[observation_dates_tenor.length-1]-1);j++){
			deltasum=0;
			double min[]=new double[observation_dates_tenor.length];
			double min_b[]=new double[observation_dates_tenor.length];
			for(int k=0;k<CShz1.length;k++){
				//Resetting parameters
				
				
					if(j<(observation_dates_tenor[0]-2)) //-1 for adjustment for array access b/w R and Java
					{
					min[0]=Math.min(CS1[i][j]*(1-b1)*CShz1[k][(observation_dates_tenor[0]-2)]/CShz1[k][j],
                				Math.min(CS2[i][j]*(1-b2)*CShz2[k][(observation_dates_tenor[0]-2)]/CShz2[k][j],
                         			CS3[i][j]*(1-b3)*CShz3[k][(observation_dates_tenor[0]-2)]/CShz3[k][j]));
					
					min_b[0]=Math.min(CS1[i][j]*(1+b1)*CShz1[k][(observation_dates_tenor[0]-2)]/CShz1[k][j],
                 				 Math.min(CS2[i][j]*(1+b2)*CShz2[k][(observation_dates_tenor[0]-2)]/CShz2[k][j],
                          			 CS3[i][j]*(1+b3)*CShz3[k][(observation_dates_tenor[0]-2)]/CShz3[k][j]));
					}
					 
					else {
						min[0]=Math.min(CS1[i][(observation_dates_tenor[0]-2)],Math.min(CS2[i][(observation_dates_tenor[0]-2)],
                                                                CS3[i][(observation_dates_tenor[0]-2)]));

						min_b[0]=Math.min(CS1[i][(observation_dates_tenor[0]-2)],Math.min(CS2[i][(observation_dates_tenor[0]-2)],
                                                                  CS3[i][(observation_dates_tenor[0]-2)]));

						}

					
				
				
				
			deltasum+=(pricer(min_b,observation_dates_tenor,coupon_accrual_ER,coupon_barrier,autocall_barrier, barrier, discount_factors,bond_deficiency,j,rates,rate1,rate2,rate3,b1,b2,b3)-
					  pricer(min,observation_dates_tenor,coupon_accrual_ER,coupon_barrier,autocall_barrier, barrier, discount_factors,bond_deficiency,j,rates,rate1,rate2,rate3,b1,b2,b3))/(2*(b1*CS1[i][j]+b2*CS2[i][j]+b3*CS3[i][j]));
			}
			deltacalculated[i][j]=deltasum/CShz1.length;
			
						}
				      }	
 	return deltacalculated; 
}
	
	
public double pricer(double min[], int observation_dates_tenor[], double coupon_accrual_ER[], double coupon_barrier, double autocall_barrier, double barrier, double discount_factors[],
double bond_deficiency[], int j, double rates, double rate1, double rate2, double rate3, double b1, double b2, double b3){
		
	double ratefactor = 0;
	if (b1>0) {ratefactor=1+rate1;}
	if (b2>0) {ratefactor=1+rate2;}
	if (b3>0) {ratefactor=1+rate3;}
    	  	double price=0;
	double coupon_accrued=0;
	coupon_accrued=Math.max(0,min[0]-1)/Math.pow(ratefactor,((1095-(double)j)/365));				
	price=coupon_accrued;
		
	return price;
		
	}


public double[][] price(double[][] CS1,double[][] CS2,double[][] CS3,double[][] CS4,
double[][] CShz1,double[][] CShz2,double[][] CShz3,double[][] CShz4, int observation_dates_tenor[],
double coupon_accrual_ER[], double coupon_barrier, double autocall_barrier, double barrier, double discount_factors[],
double bond_deficiency[], double rate1, double rate2, double rate3)
    {	double[][] pricecalculated=new double[CS1.length][(observation_dates_tenor[observation_dates_tenor.length-1])-1];
    	double pricesum=0;
		
	
	for(int i=0;i<CS1.length;i++){
		for(int j=0;j<(observation_dates_tenor[observation_dates_tenor.length-1]-1);j++){
			pricesum=0;
			double min[]=new double[observation_dates_tenor.length];
			for(int k=0;k<CShz1.length;k++){
							
				
					if(j<(observation_dates_tenor[0]-2))
					{
					min[0]=Math.min(2,Math.min(CS1[i][j]*CShz1[k][(observation_dates_tenor[0]-2)]/CShz1[k][j],
                				Math.min(CS2[i][j]*CShz2[k][(observation_dates_tenor[0]-2)]/CShz2[k][j],
                         			CS3[i][j]*CShz3[k][(observation_dates_tenor[0]-2)]/CShz3[k][j])));
					pricesum+=Math.max(0,(min[0]-1));

					}
					 
					else
					{
					min[0]=Math.min(2,Math.min(CS1[i][(observation_dates_tenor[0]-2)],Math.min(CS2[i][(observation_dates_tenor[0]-2)],
                                                                CS3[i][(observation_dates_tenor[0]-2)])));
					pricesum+=Math.max(0,(min[0]-1));
					}
		
				
			
					  
			}
			
			pricecalculated[i][j]=pricesum/CShz1.length;
					
						}
				      }	
	
 	return pricecalculated; 
    }


}
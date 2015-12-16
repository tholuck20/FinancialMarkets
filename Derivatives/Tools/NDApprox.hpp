//
// Created by mneri092115 on 16/12/2015.
//



#ifndef C_NDAPPROX_H
#define C_NDAPPROX_H
class NDApprox
{

public:
    //distributions
    double PDF(double x); // Probability Density Function (= Normal Distribution Function)
    double CND(double x); // Marsaglia (2004)
    double CND2(double x); // From volopta.com
        double Boole(double StartPoint, double EndPoint, int n);
    double CND3(double x); // Abramowitz and Stegun approximation
    double CND4(double x); // Shore  (2005)
    double CND5(double x); // Double precision univariate normal function

    double BND(double X, double y, double rho); // Bivariate Normal Distribution
    double CBND(double X, double y, double rho); // Cumulative Bivariate Normal Distribution
    double CBND2(double A,double b, double rho);
    double CBND3(double A, double b, double rho);
    double CBND4(double A, double b, double rho);
    double CBNDGeneral(int TypeFlag, double X, double y, double rho);
    double CNDEV(double p);




};



#endif //C_NDAPPROX_H

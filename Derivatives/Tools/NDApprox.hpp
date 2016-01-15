//
// Created by mneri092115 on 16/12/2015.
//



#ifndef C_NDAPPROX_H
#define C_NDAPPROX_H

class NDApprox
{

public:
    //distributions
    static double PDF(double x); // Probability Density Function (= Normal Distribution Function)
    static double ND(double x); // Normal Distribution Function (=PDF)
    static double CND(double x); // Marsaglia (2004)
    static double CND2(double x); // From volopta.com
    static double Boole(double StartPoint, double EndPoint, int n);
    static double CND3(double x); // Abramowitz and Stegun approximation
    static double CND4(double x); // Shore  (2005)
    static double CND5(double x); // Double precision univariate normal function

    static double BND(double X, double y, double rho); // Bivariate Normal Distribution
    static double CBND(double X, double y, double rho); // Cumulative Bivariate Normal Distribution
    static double CBND2(double A,double b, double rho);
    static double CBND3(double A, double b, double rho);
    static double CBND4(double A, double b, double rho);
    static double CBNDGeneral(int TypeFlag, double X, double y, double rho);
    static double CNDEV(double p);

    static double Sign(double x)
    {
        return (x<0.0) ? -1.0 : 1.0;
    }

private:

    // Boole's Rule
    static double Boole(double StartPoint, double EndPoint, int n)
    {
        vector<double> X(n+1, 0.0);
        vector<double> Y(n+1, 0.0);
        double delta_x = (EndPoint - StartPoint)/double(n);
        for (int i=0; i<=n; ++i) {
            X[i] = StartPoint + i*delta_x;
            Y[i] = NDApprox::PDF(X[i]);
        }
        double sum = 0;
        for (int t=0; t<=(n-1)/4; ++t) {
            int ind = 4*t;
            sum += (1/45.0)*(14*Y[ind] + 64*Y[ind+1] + 24*Y[ind+2] + 64*Y[ind+3] + 14*Y[ind+4])*delta_x;
        }
        return sum;
    }

    static double Factorial(double n)
    {
        if(n<=0)return 1.0;
        double res=1.0;
        for (double i = 2.0; i < (n + 1); i++)
        {
            res *= i;
        }
        return res;
    }

    static double Combination(double n, double r) {
        double nfloor = floor(n);
        double rfloor = floor(r);

        if ((n < 1) || (r < 0) || (r > n) || (r != rfloor) || (n != nfloor)) {
            return 0.0;
        }
        return Factorial(n) / (Factorial(r) * Factorial(n - r));
    }


};

#endif //C_NDAPPROX_H

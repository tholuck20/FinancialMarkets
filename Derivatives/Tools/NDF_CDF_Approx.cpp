// Approximations for Normal Distribution Function (NDF)
// Also known as Cumulative Distribution Function (CDF)
// Created by mneri092115 on 11/12/2015.


/* As NDF is a density (integral), we can use several methods to approximate the function:
 *
 - Méthode des rectangles / Rectangle method
 - Méthode des trapèzes / Trapezoidal rule
 - Méthode de Simpson / Simpson's rule
 - Méthode de Gauss-Legendre /
 - Midpoint method
 - Heun's method
 - Euler method
 - Verlet integration
 *
 Which one is the most appropriate to approximate the Standard normal Cumulative distribution function (CDF)?
 *
*/

#define PI (3.141592653589793238462643383279)

#include <cmath>



////////////////////////////////////////////////// Marsaglia  (2004) //////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Marsaglia (2004) approximation for NDF with own C++ code
double N1(double x){
    double sum = x;
    double temp = x;

    for(int i=1; i<=1000; ++i){
        temp = (temp * x * x / (2 * i + 1));
        sum += temp;
    }
    return 0.5 + (sum / sqrt(2*PI)) * exp(-(x * x)/2);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Another method (not from me and in 3 parts)
double f(double x) {
    double pi =  4.0*atan(1.0);
    return exp(-x*x*0.5)/sqrt(2*pi);
}

// Boole's Rule
double Boole(double StartPoint, double EndPoint, int n) {
    vector<double> X(n+1, 0.0);
    vector<double> Y(n+1, 0.0);
    double delta_x = (EndPoint - StartPoint)/double(n);
    for (int i=0; i<=n; ++i) {
        X[i] = StartPoint + i*delta_x;
        Y[i] = f(X[i]);
    }
    double sum = 0;
    for (int t=0; t<=(n-1)/4; t++) {
        int ind = 4*t;
        sum += (1/45.0)*(14*Y[ind] + 64*Y[ind+1] + 24*Y[ind+2] + 64*Y[ind+3] + 14*Y[ind+4])*delta_x;
    }
    return sum;
}

// N(0,1) cdf by Boole's Rule
double N2(double x) {
    return Boole(-10.0, x, 240);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////// Abramowitz and Stegun ////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Abramowitz and Stegun approximation for cumulative normal distribution
double N3(double x){
    const double b = 0.2316419;
    const double a1 = 0.319381530; const double a2 = 0.356563782;
    const double a3 = 1.781477937; const double a4 = 1.821255978;
    const double a5 = 1.330274429;

    double Q = 1.00 / (1.00 + b * fabs(x));
    double R = (1.00 / sqrt(2 * PI) *exp(-0.50 * x * x));

    double CND = R * (Q * (a1 + Q * (-a2 + Q * (a3 + Q * (-a4 + Q * a5)))));

    if (x < 0)
        return CND;
    else
        return 1-CND;
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////// Shore  (2005) ////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
double N4(double z){
    const double L = -0.61228883; const double A = -6.37309208;
    const double S1 = -0.11105481; const double S2 = 0.44334159;

    double tempA = A/(L/S1);
    double tempB = (pow(1 + S1 * z, L / S1) - 1);
    double tempB_ = (pow(1 + S1 * (-z), L / S1) - 1);

    double g = exp(-log(2) * exp(tempA * tempB + S2 * z)); // g(z)
    double g_ = exp(-log(2) * exp(tempA * tempB_ + S2 * (-z))); // g(-z)

    return 0.5 * (1 + g_ - g);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////






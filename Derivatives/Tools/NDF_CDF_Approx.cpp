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
};

// C function provided by Marsaglia for computing Phi (NDF approximation)
double Phi(double z){
    double z, t = 0, b = z, q = z * z, i = 1;
    while(s != t)
        s = (t = s) + (b *= q / (i += 2));
    return 0.5 + s * exp(-0.5 * q - 0.91893853320467274178L);
};
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////// Abramowitz and Stegun ////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// N2 - Second method: Abramowitz and Stegun approximation for cumulative normal distribution
double N2(double x){
    const double b = 0.2316419;
    const double a1 = 0.319381530; const double a2 = 0.356563782;
    const double a3 = 1.781477937; const double a4 = 1.821255978;
    const double a5 = 1.330274429;

    double Q = 1.00 / (1.00 + b * abs(x));
    double R = (1.00 / sqrt(2 * PI) *exp(-0.50 * x * x));

    double CND = R * (Q * (a1 + Q * (-a2 + Q * (a3 + Q * (-a4 + Q * a5)))));

    if (x < 0)
        return temp;
    else
        return 1-temp;
};
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*
Shore (2005) :
g(z) = exp{− log(2) exp{[α/(λ/S1)][(1 + S1z)
            (λ/S1) − 1] + S2z}}
where λ = −0.61228883; S1 = −0.11105481; S2 = 0.44334159; α =
−6.37309208
 */

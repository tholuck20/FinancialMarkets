//
// Created by mneri092115 on 10/12/2015.
//


#include <vector>
#include <cmath>
using namespace std;

#ifndef DERIVATIVES_BLACK_SCHOLES_H
#define DERIVATIVES_BLACK_SCHOLES_H

// Tools
/* As N is a density (integral), we can use several methods to approximate the function:
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
// Standard normal Cumulative distribution function (CDF) = Fonction de répartition
// Also known as NDF: Normal Distribution Function
double N1(double x);
double N2(double x);
double N3(double x); // N1 = 34 at 9 numbers after the decimal point
double Boole(double StartPoint, double EndPoint, int n);

// Standard normal Probability Density Function (PDF) = Densité de probabilité
double pdf(double x);

class BS {

public:
    // Black and Scholes valuation
    double BSPrice(double S, double K, double T, double r, double v, char optType, double q = 0);

    // Black and Scholes implied volatility
    double BSImplVol(double S, double K, double T, double r, double v, char optType, double q = 0);
    double BSImplVol2(double S, double K, double T, double r, double v, char optType, double q = 0);
    double BSImplVol3(double S, double K, double T, double r, double v, char optType, double q = 0);
    double BSImplVol4(double S, double K, double T, double r, char optType, double b, double cm);

    // Black and Scholes Greeks
    double BSDelta(double S, double K, double T, double r, double v, char optType, double q = 0);
    double BSGamma(double S, double K, double T, double r, double v, double q = 0);
    double BSVega(double S, double K, double T, double r, double v, double q = 0);
    double BSRho(double S, double K, double T, double r, double v, char optType, double q = 0);
    double BSTheta(double S, double K, double T, double r, double v, char optType, double q = 0);
    double BSVanna(double S, double K, double T, double r, double v, double q = 0, double b = 0); //////
    double BSCharm(double S, double K, double T, double r, double v, char optType, double q = 0, double b = 0); //////
    double BSSpeed(double S, double K, double T, double r, double v, char optType);
    double BSZomma(double S, double K, double T, double r, double v, char optType);
    double BSColor(double S, double K, double T, double r, double v, char optType);
    double BSdVega_dTime(double S, double K, double T, double r, double v, char optType);
    double BSVomma(double S, double K, double T, double r, double v, char optType);
    double BSDualDelta(double S, double K, double T, double r, double v, char optType);
    double BSDualGamma(double S, double K, double T, double r, double v, char optType);



};






#endif //DERIVATIVES_BLACK_SCHOLES_H

//
// Created by mneri092115 on 10/12/2015.
//

#ifndef DERIVATIVES_BLACK_SCHOLES_H
#define DERIVATIVES_BLACK_SCHOLES_H
#include <vector>
#include <cmath>
using namespace std;


// Tools
/* As N is a density (integral), we can use several method to approximate the function:
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
 Which one is the most appropriate to approximate the Standard normal Probability Density Function?
 *
*/
// Standard normal Cumulative distribution function (CDF) = Fonction de répartition
double N1(double x);
double N2(double x);
double N3(double x); // N1 = 34 at 9 numbers after the decimal point
double Boole(double StartPoint, double EndPoint, int n);

// Standard normal Probability Density Function (PDF) = Densité de probabilité
double pdf(double x);

// Black and Scholes valuation
double BSPrice(double S, double K, double r, double T, double v, char optType);
double BSImpliedVol(double S, double K, double r, double T, double v, char optType);

// Black and Scholes Greeks
double BSDelta(double S, double K, double r, double T, double v, char optType, double q = 0);
double BSGamma(double S, double K, double T, double r, double v, double q = 0);
double BSVega(double S, double K, double T, double r, double v, double q = 0);
double BSRho(double S, double K, double T, double r, double v, char OpType);
double BSTheta(double S, double K, double T, double r, double v, char OpType, double q = 0);

double BSVanna(double S, double K, double T, double r, double v, char OpType);
double BSCharm(double S, double K, double T, double r, double v, char OpType);
double BSSpeed(double S, double K, double T, double r, double v, char OpType);
double BSZomma(double S, double K, double T, double r, double v, char OpType);
double BSColor(double S, double K, double T, double r, double v, char OpType);
double BSdVega_dTime(double S, double K, double T, double r, double v, char OpType);
double BSVomma(double S, double K, double T, double r, double v, char OpType);
double BSDualDelta(double S, double K, double T, double r, double v, char OpType);
double BSDualGamma(double S, double K, double T, double r, double v, char OpType);







#endif //DERIVATIVES_BLACK_SCHOLES_H

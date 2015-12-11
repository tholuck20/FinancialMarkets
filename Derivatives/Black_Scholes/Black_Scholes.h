//
// Created by mneri092115 on 10/12/2015.
//

#ifndef DERIVATIVES_BLACK_SCHOLES_H
#define DERIVATIVES_BLACK_SCHOLES_H
#include <vector>
#include <cmath>
using namespace std;



// Standard normal Cumulative distribution function (CDF) = Fonction de répartition
double N1(double x); // done
// Standard normal Probability Density Function (PDF) = Densité de probabilité
double pdf(double x); // done

// Black and Scholes valuation
double BSPrice(double S, double K, double T, double r, double v, char optType); // done
double BSImpliedVol(double S, double K, double T, double r, double v, char optType); //////////////////////////////////////////////

// Black and Scholes Greeks
double BSDelta(double S, double K, double T, double r, double v, char optType, double q = 0); // done
double BSGamma(double S, double K, double T, double r, double v, double q = 0); // done
double BSVega(double S, double K, double T, double r, double v, double q = 0); // done
double BSRho(double S, double K, double T, double r, double v, char OpType); // done
double BSTheta(double S, double K, double T, double r, double v, char OpType, double q = 0); // done

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

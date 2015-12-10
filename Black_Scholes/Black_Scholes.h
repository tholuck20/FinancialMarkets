//
// Created by mneri092115 on 10/12/2015.
//

#ifndef FINANCE_RECEIPTS_BLACK_SCHOLES_H
#define FINANCE_RECEIPTS_BLACK_SCHOLES_H
#include <vector>
#include <cmath>
using namespace std;


// Tools

double N1(double x); // Standard normal Cumulative distribution function (CDF) = Fonction de répartition
double N2(double x);

double N3(double x); // Standard normal Probability Density Function (PDF) // N1 = N3 at 4 numbers after decimal point

double N4(double x); // N1 = N4 at 9 numbers after the decimal point
double Boole(double StartPoint, double EndPoint, int n);


// Black and Scholes valuation
double BSPrice(double S, double K, double r, double T, double v, char optType);
double BSImpliedVol(double S, double K, double r, double T, double v, char optType);

// Black and Scholes Greeks
double BSDelta(double S, double K, double r, double T, double v, char optType, double q = 0);
double BSGamma(double S, double K, double T, double r, double v);
double BSVega(double S, double K, double T, double r, double v);
double BSRho(double S, double K, double T, double r, double v, char OpType);
double BSTheta(double S, double K, double T, double r, double v, char OpType);

double BSVanna(double S, double K, double T, double r, double v, char OpType);
double BSCharm(double S, double K, double T, double r, double v, char OpType);
double BSSpeed(double S, double K, double T, double r, double v, char OpType);
double BSZomma(double S, double K, double T, double r, double v, char OpType);
double BSColor(double S, double K, double T, double r, double v, char OpType);
double BSdVega_dTime(double S, double K, double T, double r, double v, char OpType);
double BSVomma(double S, double K, double T, double r, double v, char OpType);
double BSDualDelta(double S, double K, double T, double r, double v, char OpType);
double BSDualGamma(double S, double K, double T, double r, double v, char OpType);







#endif //FINANCE_RECEIPTS_BLACK_SCHOLES_H

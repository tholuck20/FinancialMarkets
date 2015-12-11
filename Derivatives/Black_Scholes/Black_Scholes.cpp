// Black & Scholes
// Created by mneri092115 on 10/12/2015.

// S = Stock price
// K = Strike price
// r = Risk free rate
// T = Maturity
// v = Yearly Implied volatility
// optType = Option type (Call or Put)

// d1 = (ln(S/K)+(r+(v^2)/2)*T)/v*sqrt(T)
// d2 = d1 - v*sqrt(T)

// Call = S*N(d1)-K*exp(-r*T)*N(d2)
//      = S*N(d1)-K*exp(-r*T)*N(d1-v*sqrt(T))
// Put = K*exp(-r*T)*N(-d2)-S*N(-d1)
//     = K*exp(-r*T)*N(-d1+v*sqrt(T))-S*N(-d1)

#define PI (3.141592653589793238462643383279)

#include "Black_Scholes.h"

#include <cmath>
#include <iomanip>
#include <iostream>
#include <vector>

using namespace std;

// Probability Density Function
double pdf(double x){
    return exp(-0.5 * x * x) / sqrt(2 * PI);
}

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

// Black and Scholes Price
double BSPrice(double S, double K, double T, double r, double v, char optType){
    double d1 = (log(S / K) + (r + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    if (optType == 'C')
        return S * N1(d1) - K * exp(-r * T) * N1(d2); // change N type for different CDF valuation method
    else
        return K * exp(-r * T) * N1(-d2) - S * N1(-d1);
}

// Black and Scholes Delta
double BSDelta(double S, double K, double T, double r, double v, char optType, double q = 0){
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    if (optType == 'C')
        return exp(-q * T) * N1(d1);
    else
        return exp(-r * q) * (N1(d1) - 1); // if q == 0 -->> exp (-r * 0) = 1
}

// Black and Scholes Gamma
double BSGamma(double S, double K, double T, double r, double v, double q = 0){
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    return pdf(d1) / (S * v * sqrt(T));
}

// Black and Scholes Vega
double BSVega(double S, double K, double T, double r, double v, double q = 0){
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    return 0.01 * pdf(d1) * S * exp(-q * T); // 0.01 because /100 to be in %
}

// Black and Scholes Rho
double BSRho(double S, double K, double T, double r, double v, char OpType){
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);
    if (optType == 'C')
        return K * T * exp(-r * T) * N1(d2);
    else
        return -K * T * exp(-r * T) * N1(-d2);
}

// Black and Scholes Theta
double BSTheta(double S, double K, double T, double r, double v, char OpType, double q = 0){
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);
    if (optType == 'C')
        if (q == 0) // without dividend
            return -K * exp(-r * T) * (r * N1(d2) + v * pdf(d2) / 2 * sqrt(T)); // Theta call no dividend
        else // q != 0 -> with dividend
            return q* S * exp(-q * T) * N1(d1) - K * exp(-r * T) * (r * N1(d2) + v * pdf(d2) / 2 * sqrt(T));
    else // optType == P
        if (q == 0) // without dividend
            return K * exp(-r * T) * (r * N1(-d2) - v * pdf(d2) / 2 * sqrt(T)); // Theta put no dividend
        else // with dividend
            return -q * S * exp(-q * T) * N1(-d1) + K * exp(-r * T) * (r * N1(-d2) - v * pdf(-d2) / 2 * sqrt(T));
}



int main()
{
    double S = 30;
    double K = 35;
    double r = 0.05;
    double T = 1;
    double v = 0.25;
    char optType = 'C';
    char optType2 = 'P';

    cout << "Call B&S Price = " << fixed << setprecision(10) << BSPrice(S,K,T,r,v,optType) << endl;
    cout << "Put  B&S Price = " << fixed << setprecision(10) << BSPrice(S,K,T,r,v,optType2) << endl << endl;

    cout << "Delta Call B&S = " << fixed << setprecision(10) << BSDelta(S,K,T,r,v,optType) << endl;
    cout << "Delta Put B&S = " << fixed << setprecision(10) << BSDelta(S,K,T,r,v,optType2) << endl << endl;

    cout << "Theta Call B&S = " << fixed << setprecision(10) << BSTheta(S,K,T,r,v,optType) << endl;
    cout << "Theta Put B&S = " << fixed << setprecision(10) << BSTheta(S,K,T,r,v,optType2) << endl << endl;
}
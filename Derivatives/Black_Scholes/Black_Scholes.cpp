//
// Created by mneri092115 on 10/12/2015.
//

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

#include "Black_Scholes.hpp"
#include <../Tools/NDApprox.hpp>

#include <cmath>
#include <iomanip>
#include <iostream>

using namespace std;

// Probability Density Function
double PDF(double x)
{
    return exp(-0.5 * x * x) / sqrt(2 * PI);
}

// Marsaglia (2004) approximation for NDF with own C++ code
double CND(double x)
{
    double sum = x;
    double temp = x;

    for(int i=1; i<=1000; i++){
        temp = (temp * x * x / (2 * i + 1));
        sum += temp;
    }
    return 0.5 + (sum / sqrt(2*PI)) * exp(-(x * x)/2);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Black and Scholes Price
double BSPrice(double S, double K, double T, double r, double v, char optType, double q, double b)
{
    double b = r;
    double d1 = (log(S / K) + (b - q + 0.5 * v * v) * T) / (v * sqrt(T));
    // b not r in case of underlying is a commodity (and b is defined so != r), if not b = r
    double d2 = d1 - v * sqrt(T);

    if (optType == 'C')
        // When commodity: b != r and q = 0 so only exp((b-r)*T) left
        // When !commodity: b = r and q may be != 0 so only exp(-q*T) left
        return  S * exp((b - r) * T) * exp(-q * T) * CND(d1) - K * exp(-r * T) * CND(d2);
    else
        return -S * exp((b - r) * exp(-q * T) * CND(-d1) + K * exp(-r * T) * CND(-d2));
}

// Black and Scholes Delta
double BSDelta(double S, double K, double T, double r, double v, char optType, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    if (optType == 'C')
        return exp(-q * T) * CND(d1);
    else
        return exp(-r * q) * (CND(d1) - 1); // if q == 0 -->> exp (-r * 0) = 1
}

// Black and Scholes Gamma
double BSGamma(double S, double K, double T, double r, double v, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    return PDF(d1) / (S * v * sqrt(T));
}

// Black and Scholes Vega
double BSVega(double S, double K, double T, double r, double v, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    return 0.01 * PDF(d1) * S * exp(-q * T); // 0.01 because /100 to be in %
}

// Black and Scholes Rho
double BSRho(double S, double K, double T, double r, double v, char optType, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);
    if (optType == 'C')
        return K * T * exp(-r * T) * CND(d2);
    else
        return -K * T * exp(-r * T) * CND(-d2);
}

// Black and Scholes Theta
double BSTheta(double S, double K, double T, double r, double v, char optType, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);
    if (optType == 'C')
    if (q == 0) // without dividend
        return -K * exp(-r * T) * (r * CND(d2) + v * PDF(d2) / 2 * sqrt(T)); // Theta call no dividend
    else // q != 0 -> with dividend
        return q* S * exp(-q * T) * CND(d1) - K * exp(-r * T) * (r * CND(d2) + v * PDF(d2) / 2 * sqrt(T));
    else // optType == P
    if (q == 0) // without dividend
        return K * exp(-r * T) * (r * CND(-d2) - v * PDF(d2) / 2 * sqrt(T)); // Theta put no dividend
    else // with dividend
        return -q * S * exp(-q * T) * CND(-d1) + K * exp(-r * T) * (r * CND(-d2) - v * PDF(-d2) / 2 * sqrt(T));
}

// Implied Volatility using the Newton-Raphson method
double BSImplVol(double S, double K, double T, double r, double v, char optType, double q)
{
    const double epsilon = 0.00000001;
    // Manaster and Koehler seed value
    double vi = sqrt(fabs(log(S / K) + r * T) * 2 / T);
    double ci = BSPrice(S,K,T,r,vi,optType,q);
    double vegai = 100 * BSVega(S,K,T,r,vi,q);
    double minDiff = fabs(v - ci); // cm <-> v

    while (minDiff >= epsilon){
        vi -= (ci - v) / vegai;
        ci = BSPrice(S,K,T,r,vi,optType,q);
        vegai = 100 * BSVega(S,K,T,r,vi,q);
        minDiff = fabs(v - ci);
    }

    if (minDiff < epsilon)
        return vi;
    else
        return 0;
}

double BSImplVol2(double S, double K, double T, double r, double v, char optType, double q)
{
    double cpTest = 0;
    double IV = 50;
    double upper = 50;
    double lower = 0;
    double range;
    double price = BSPrice(S,K,T,r,v,optType,q);

    while(1){
        cpTest = BSPrice(S,K,T,r,IV,optType,q);

        if(cpTest > price){
            upper = IV;
            IV = (lower + upper) / 2;
        }
        else{
            lower = IV;
            IV = (lower + upper) / 2;
        }
        range = fabs(lower - upper);
        if(range < 0.00001)
            break;
    }
    return IV;
}

double BSImplVol3(double S, double K, double T, double r, double v, char optType, double q)
{
    const double epsilon = 0.00000001;
    const double dVol = 0.00000001;
    const int maxIter = 10000;
    double vol_1 = v, price_1, vol_2, price_2, dx;

    for (int i = 1; i < maxIter; i++) {
        price_1 = BSPrice(S, K, T, r, vol_1, optType,q);
        vol_2 = vol_1 - dVol;
        price_2 = BSPrice(S,K,T,r,vol_2, optType,q);
        dx = (price_2 - price_1) / dVol;
        if (fabs(dx) < epsilon || i == maxIter)
            return vol_1;
        else
            vol_1 = vol_1 - (BSPrice(S,K,T,r,v,optType,q) - price_1) / dx;
    };
    return vol_1;
}

double BSImplVol4(double S, double K, double T, double r, char optType, double b, double cm)
{
    double vLow, vHigh, vi;
    double cLow, cHigh, epsilon;
    int counter;

    vLow = 0.005;
    vHigh = 4;
    // epsilon = 0.00000001;
    epsilon = 0.0001;

    cLow = BSPrice(S, K, T, r, vLow, optType, 0, b);
    cHigh = BSPrice(S, K, T, r, vHigh, optType, 0, b);
    counter = 0;
    vi = vLow + (cm - cLow) * (vHigh - vLow) / (cHigh - cLow);
    while (fabs(cm - BSPrice(S, K, T, r, vi, optType, 0, b)) > epsilon)
    {
        counter = counter + 1;
        if (counter == 500) return 0.0;// return -99999.99 ;
        if (BSPrice(S, K, T, r, vi, optType, 0, b) < cm)
        {
            vLow = vi;
        }
        else
        {
            vHigh = vi;
        }
        cLow = BSPrice(S, K, T, r,vLow, optType,0,b);
        cHigh = BSPrice(S, K, T, r, vHigh,optType,0,b);
        vi = vLow + (cm - cLow) * (vHigh - vLow) / (cHigh - cLow);
    }
    return vi;

}


double BSVanna(double S, double K, double T, double r, double v, double q, double b)
{
    double d1 = (log(S / K) + (b - q + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    return -exp(-q * T) * PDF(d1) * d2 / v;
}

double BSCharm(double S, double K, double T, double r, double v, char optType, double q, double b)
{
    double d1 = (log(S / K) + (b - q + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    if(optType == 'C'){
        return -q * exp(-q * T) * CND(d1) + exp(-q * T) * PDF(d1) * \
        (2 * (r - q) * T - d2 * v * sqrt(T))/(2 * T * v * sqrt(T)); }
    else {
        return q * exp(-q * T) * CND(-d1) + exp(-q * T) * PDF(d1) * \
        (2 * (r - q) * T - d2 * v * sqrt(T)) / (2 * T * v * sqrt(T));
    };
}






//double BSSpeed(double S, double K, double T, double r, double v, char optType){}
//double BSZomma(double S, double K, double T, double r, double v, char optType){}
//double BSColor(double S, double K, double T, double r, double v, char optType){}
//double BSdVega_dTime(double S, double K, double T, double r, double v, char optType){}
//double BSVomma(double S, double K, double T, double r, double v, char optType){}
//double BSDualDelta(double S, double K, double T, double r, double v, char optType){}
//double BSDualGamma(double S, double K, double T, double r, double v, char optType){}





int main()
{
    double S = 30;
    double K = 35;
    double r = 0.05;
    double T = 1;
    double v = 0.25;
    //double q = 0.00;
    char optType = 'C';
    char optType2 = 'P';

    cout << "Call B&S Price = " << fixed << setprecision(10) << BSPrice(S,K,T,r,v,optType) << endl;
    cout << "Put  B&S Price = " << fixed << setprecision(10) << BSPrice(S,K,T,r,v,optType2) << endl << endl;

    cout << "Delta Call B&S = " << fixed << setprecision(10) << BSDelta(S,K,T,r,v,optType) << endl;
    cout << "Delta Put B&S = " << fixed << setprecision(10) << BSDelta(S,K,T,r,v,optType2) << endl << endl;

    cout << "Theta Call B&S = " << fixed << setprecision(10) << BSTheta(S,K,T,r,v,optType) << endl;
    cout << "Theta Put B&S = " << fixed << setprecision(10) << BSTheta(S,K,T,r,v,optType2) << endl << endl;

    cout << "Implied Volatility = " << fixed << setprecision(10) << BSImplVol(S,K,T,r,v,optType);
}

//
// Created by mneri092115 on 10/12/2015.
//

#define PI (3.141592653589793238462643383279)

#include "Black_Scholes.hpp"
#include "../Tools/NDApprox.hpp"

#include <cmath>
#include <iomanip>
#include <iostream>

using namespace std;

//////////////////////////////////////////////////////// Price ////////////////////////////////////////////////////////
// Cr�er plusieurs fonctions prenant en compte q (dividend), b (cost of carry)
// Black and Scholes Price + Merton for dividend
double BSPrice(double S, double K, double T, double r, double v, char optType, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5 * v * v) * T) / (v * sqrt(T));
    // b not r in case of underlying is a commodity (and b is defined so != r), if not b = r
    double d2 = d1 - v * sqrt(T);

    if (optType == 'C')
        // When commodity: b != r and q = 0 so only exp((b-r)*T) left
        // When !commodity: b = r and q may be != 0 so only exp(-q*T) left
        return  S * exp(-q * T) * NDApprox::CND(d1) - K * exp(-r * T) * NDApprox::CND(d2);
    else
        return -S * exp(-q * T) * NDApprox::CND(-d1) + K * exp(-r * T) * NDApprox::CND(-d2);
}

// ++ c >= max(S*exp(-q*T)-K*exp(-r*T),0)
// ++ p >= max(-S*exp(-q*T)+K*exp(-r*T),0)
// Parit� Call/Put c+K*exp(-r*T) = p+S*exp(-q*T)

// BSPrice for FX Options with Garman-Kohlhagen model
double BSGKPrice(double S, double K, double T, double rd, double rf, double v, double optType)
{
    double d1 = (log(S / K) + (rd - rf + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    if (optType == 'C')
        return S * exp(-rf * T) * NDApprox::CND(d1) - K * exp(-rd * T) * NDApprox::CND(d2);
    else
        return -S * exp(-rf * T) * NDApprox::CND(-d1) + K * exp(-rd * T) * NDApprox::CND(-d2);
}

// BSPrice for options on futures with Black-76 model
double Black76(double F, double K, double T, double r, double v, char optType)
{
    double d1 = (log(F / K) + (0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    if (optType == 'C')
        return exp(-r * T) * (F * NDApprox::CND(d1) - K * NDApprox::CND(d2));
    else
        return exp(-r * T) * (-F * NDApprox::CND(-d1) - K * NDApprox::CND(-d2));
}

// BSPrice for commodity options
double CBSPRice(double S, double K, double T, double r, double v, char optType, double b)
{
    double d1 = (log(S / K) + (b + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    if (optType == 'C')
        return S * exp((b - r) * T) * NDApprox::CND(d1) - K * exp(-r * T) * NDApprox::CND(d2);
    else
        return -S * exp((b - r) * T) * NDApprox::CND(-d1) + K * exp(-r * T) * NDApprox::CND(-d2);
}

//////////////////////////////////////////////////////// Greeks ////////////////////////////////////////////////////////

// Black and Scholes Delta
double BSDelta(double S, double K, double T, double r, double v, char optType, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    if (optType == 'C')
        return exp(-q * T) * NDApprox::CND(d1);
    else
        return -exp(-q * T) * NDApprox::CND(-d1); // * (NDApprox::CND(d1)-1
}

// Black and Scholes Gamma
double BSGamma(double S, double K, double T, double r, double v, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    return exp(-q * T) * NDApprox::PDF(d1) / (S * v * sqrt(T)); // N'(d1) = PDF(d1)
}

// Black and Scholes Vega
double BSVega(double S, double K, double T, double r, double v, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    return 0.01 * NDApprox::PDF(d1) * S * exp(-q * T); // 0.01 because /100 to be in %
}

// Black and Scholes Rho
double BSRho(double S, double K, double T, double r, double v, char optType, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);
    if (optType == 'C')
        return K * T * exp(-r * T) * NDApprox::CND(d2);
    else
        return -K * T * exp(-r * T) * NDApprox::CND(-d2);
}

// Black and Scholes Theta
double BSTheta(double S, double K, double T, double r, double v, char optType, double q) // Annual Theta
{
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);
    if (optType == 'C')
        if (q == 0) // Theta Call no dividend
            return -K * exp(-r * T) * (r * NDApprox::CND(d2) + v * NDApprox::PDF(d2) / 2 * sqrt(T));
        else // q != 0 -> Theta Call dividend
            return q * S * exp(-q * T) * NDApprox::CND(d1) - K * exp(-r * T) * (r * NDApprox::CND(d2) + \
                    v * NDApprox::PDF(d2) / 2 * sqrt(T));
    else // optType == P
        if (q == 0) // Theta Put no dividend
            return K * exp(-r * T) * (r * NDApprox::CND(-d2) - v * NDApprox::PDF(d2) / 2 * sqrt(T));
        else // Theta Put dividend
            return -q * S * exp(-q * T) * NDApprox::CND(-d1) + K * exp(-r * T) * (r * NDApprox::CND(-d2) - \
                    v * NDApprox::PDF(-d2) / 2 * sqrt(T));
}

// Implied Volatility using the Newton-Raphson method for European options
double BSImplVol(double S, double K, double T, double r, char optType, double q, double cm)
{
    const double epsilon = 0.00000001;
    // Manaster and Koehler seed value
    double vi = sqrt(fabs(log(S / K) + r * T) * 2 / T);
    double ci = BSPrice(S,K,T,r,vi,optType,q);
    double vegai = 100 * BSVega(S,K,T,r,vi,q);
    double minDiff = fabs(cm - ci); // cm <-> v

    while (minDiff >= epsilon)
    {
        vi -= (ci - cm) / vegai; // cm <-> v
        ci = BSPrice(S,K,T,r,vi,optType,q);
        vegai = 100 * BSVega(S,K,T,r,vi,q);
        minDiff = fabs(cm - ci); // cm <-> v
    }

    if (minDiff < epsilon)
        return vi;
    else
        return 0;
}

double BSImplVol2(double S, double K, double T, double r, char optType, double q, double cm)
{
    double cpTest = 0;
    double IV = 50;
    double upper = 50;
    double lower = 0;
    double range;
    //double price = BSPrice(S,K,T,r,v,optType,q);

    while(1){
        cpTest = BSPrice(S,K,T,r,IV,optType,q);

        if(cpTest > cm)
        {
            upper = IV;
            IV = (lower + upper) / 2;
        }
        else
        {
            lower = IV;
            IV = (lower + upper) / 2;
        }
        range = fabs(lower - upper);
        if(range < 0.00001)
            break;
    }
    return IV;
}

double BSImplVol3(double S, double K, double T, double r, double v, char optType, double q, double cm)
{
    const double epsilon = 0.00000001;
    const double dVol = 0.00000001;
    const int maxIter = 10000;
    double vol_1 = v, price_1, vol_2, price_2, dx;

    for (int i = 1; i < maxIter; ++i)
    {
        price_1 = BSPrice(S, K, T, r, vol_1, optType,q);
        vol_2 = vol_1 - dVol;
        price_2 = BSPrice(S,K,T,r,vol_2, optType,q);
        dx = (price_2 - price_1) / dVol;
        if (fabs(dx) < epsilon || i == maxIter)
            return vol_1;
        else
            vol_1 = vol_1 - (cm - price_1) / dx;
    };
    return vol_1;
}

//Implied Volatility using Bisection for European options
double BSImplVol4(double S, double K, double T, double r, char optType, double q, double cm)
{
    double vLow, vHigh, vi;
    double cLow, cHigh, epsilon;
    int counter;

    vLow = 0.005;
    vHigh = 4;
    epsilon = 0.00000001;

    cLow = BSPrice(S, K, T, r, vLow, optType, 0);
    cHigh = BSPrice(S, K, T, r, vHigh, optType, 0);
    counter = 0;
    vi = vLow + (cm - cLow) * (vHigh - vLow) / (cHigh - cLow);
    while (fabs(cm - BSPrice(S, K, T, r, vi, optType, q)) > epsilon)
    {
        counter = counter + 1;
        if (counter == 500) return 0.0;

        if (BSPrice(S, K, T, r, vi, optType, q) < cm)
            {
                vLow = vi;
            }
        else
            {
                vHigh = vi;
            }
            cLow = BSPrice(S, K, T, r,vLow, optType,q);
            cHigh = BSPrice(S, K, T, r, vHigh,optType,q);
            vi = vLow + (cm - cLow) * (vHigh - vLow) / (cHigh - cLow);
    }
    return vi;

}

double BSVanna(double S, double K, double T, double r, double v, double q, double b)
{
    double d1 = (log(S / K) + (b - q + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    return -exp(-q * T) * NDApprox::PDF(d1) * d2 / v;
}

double BSCharm(double S, double K, double T, double r, double v, char optType, double q, double b)
{
    double d1 = (log(S / K) + (b - q + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    if(optType == 'C'){
        return -q * exp(-q * T) * NDApprox::CND(d1) + exp(-q * T) * NDApprox::PDF(d1) * \
        (2 * (r - q) * T - d2 * v * sqrt(T))/(2 * T * v * sqrt(T)); }
    else {
        return q * exp(-q * T) * NDApprox::CND(-d1) + exp(-q * T) * NDApprox::PDF(d1) * \
        (2 * (r - q) * T - d2 * v * sqrt(T)) / (2 * T * v * sqrt(T));
    };
}

double BSSpeed(double S, double K, double T, double r, double v, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5 * v * v) * T) / (v * sqrt(T));

    return -exp(-q * T) * (NDApprox::PDF(d1) / (S * S * v * sqrt(T))) * ((d1 / (v * sqrt(T))) + 1);
    //return -BSGamma(S,K,T,r,v,q)/S * ((d1 / (v * sqrt(T))) + 1);
}

double BSZomma(double S, double K, double T, double r, double v, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    return BSGamma(S,K,T,r,v,q) * ((d1 * d2 - 1) / v);
    //return exp(-q * T) * NDApprox::PDF(d1) * (d1 * d2 -1) / (S * v * v * sqrt(T));
}

double BSColor(double S, double K, double T, double r, double v, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    return (-exp(-q * T) * NDApprox::PDF(d1) / (2 * S * T * v * sqrt(T))) * \
        (2 * q * T + 1 + (2 * (r - q) * T  - d1 * d2 * v * sqrt(T)) / v * sqrt(T));
}

double BSdVega_dTime(double S, double K, double T, double r, double v, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    return S * exp(-q * T) * NDApprox::PDF(d1) * sqrt(T) *
           (q + ((r - q) * d1) / (v * sqrt(T)) - (1 + d1 * d2) / (2 * T));
}

double BSVomma(double S, double K, double T, double r, double v, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    return S * exp(-q * T) * NDApprox::PDF(d1) * sqrt(T) * d1 * d2 / v;
}

double BSDualDelta(double S, double K, double T, double r, double v, char optType, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    if (optType == 'C')
    {
        return -exp(-q * T) * NDApprox::CND(d2);
    }
    else
    {
        return exp(-q * T) * NDApprox::CND(-d2);
    }
}

double BSDualGamma(double S, double K, double T, double r, double v, double q)
{
    double d1 = (log(S / K) + (r - q + 0.5 * v * v) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    return exp(-r * T) * NDApprox::PDF(d2) / (K * v * sqrt(T));
}



int main()
{
    double S = 30;
    double F = 30;
    double K = 35;
    double T = 1;
    double r = 0.05;
    double rf = 0.02;
    double rd = 0.05;
    double v = 0.25;
    double q = 0.00;
    double b = 0.00;
    double cm = 2.76;
    char optType = 'C';
    char optType2 = 'P';

    cout << "EC Black&Scholes Price = " << fixed << setprecision(5) << BSPrice(S,K,T,r,v,optType,q) << endl;
    cout << "EP Black&Scholes Price = " << BSPrice(S,K,T,r,v,optType2,q) << endl;
    cout << "EC FX Price = " << BSGKPrice(S,K,T,rd,rf,v,optType) << endl;
    cout << "EP FX Price = " << BSGKPrice(S,K,T,rd,rf,v,optType2) << endl;
    cout << "EC Futures Price = " << Black76(F,K,T,r,v,optType) << endl;
    cout << "EP Futures Price = " << Black76(F,K,T,r,v,optType2) << endl;
    cout << "EC CTY Price = " << CBSPRice(S,K,T,r,v,optType,b) << endl;
    cout << "EP CTY Price = " << CBSPRice(S,K,T,r,v,optType2,b) << endl << endl;

    cout << "Implied Volatility = " << BSImplVol(S,K,T,r,optType,q,cm) << endl;
    cout << "Implied Volatility 2 = " << BSImplVol2(S,K,T,r,optType,q,cm) << endl;
    cout << "Implied Volatility 3 = " << BSImplVol3(S,K,T,r,v,optType,q,cm) << endl;
    cout << "Implied Volatility 4 = " << BSImplVol4(S,K,T,r,optType,q,cm) << endl << endl;

    cout << "Delta Call B&S = " << BSDelta(S,K,T,r,v,optType,q) << endl;
    cout << "Delta Put B&S = " << BSDelta(S,K,T,r,v,optType2,q) << endl << endl;

    cout << "Gamma B&S = " << BSGamma(S,K,T,r,v,q) << endl;
    cout << "Vega B&S = " << BSVega(S,K,T,r,v,q) << endl<< endl;

    cout << "Rho Call B&S = " << BSRho(S,K,T,r,v,optType,q) << endl;
    cout << "Rho Put B&S = " << BSRho(S,K,T,r,v,optType2,q) << endl << endl;

    cout << "Theta Call B&S = " << BSTheta(S,K,T,r,v,optType,q) << " per year" << endl;
    cout << "Theta Call B&S = " << BSTheta(S,K,T,r,v,optType,q)/365 << " per day" << endl;
    cout << "Theta Put B&S = " << BSTheta(S,K,T,r,v,optType2,q) << " per year" << endl;
    cout << "Theta Put B&S = " << BSTheta(S,K,T,r,v,optType2,q)/365 << " per day" << endl << endl;

    cout << "Vanna B&S = " << BSVanna(S,K,T,r,v,q,b) << endl<< endl;
    cout << "Charm Call B&S = " << BSCharm(S,K,T,r,v,optType,q,b) << endl;
    cout << "Charm Put B&S = " << BSCharm(S,K,T,r,v,optType2,q,b) << endl<< endl;
    cout << "Speed B&S = " << BSSpeed(S,K,T,r,v,q) << endl;
    cout << "Zomma B&S = " << BSZomma(S,K,T,r,v,q) << endl;
    cout << "Color B&S = " << BSColor(S,K,T,r,v,q) << endl;
    cout << "dVega_dTime B&S = " << BSdVega_dTime(S,K,T,r,v,q) << endl;
    cout << "Vomma B&S = " << BSVomma(S,K,T,r,v,q) << endl << endl;

    cout << "DualDelta Call B&S = " << BSDualDelta(S,K,T,r,v,optType,q) << endl;
    cout << "DualDelta Put B&S = " << BSDualDelta(S,K,T,r,v,optType2,q) << endl << endl;

    cout << "DualGamma B&S = " << BSDualGamma(S,K,T,r,v,q) << endl;

}

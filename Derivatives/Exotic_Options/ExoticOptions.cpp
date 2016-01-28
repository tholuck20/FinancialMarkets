// Exotic Options
// Created by mneri092115 on 20/01/2016.


#include "ExoticOptions.hpp"
#include "../Tools/NDApprox.hpp"
#include <cmath>


// BinaryOption(S, K, r, q, vol, T, IsCall, IsFut, Divs, IsCash, Result)
// Calculations for binary options on stocks, stock indices,currencies and futures
double BinaryOptions(double S, double K, double T, double r, double v,
                     char optType, bool isCash, bool isFut, double q, int result) // default value for isFut, q, result
{
    // Implied volatility using Newton-Raphson method
    double test_price = v;
    double vol = 0.1;
    int iter = 0;
    double price;

    if (result == 6)
        do {
            double p_vol = Binary(S, K, T, r, v + 0.01, optType, isCash, q);
            double price = Binary(S, K, T, r, v, optType, isCash, q);
            double vega = p_vol - price;

            // Update vol to level necessary to fit price assuming linear price-vol relationship
            double d_vol = (test_price - price) / (vega * 100.0 + 0.0001);

            // Limit permissible change to avoid shooting off to infinity
            if (d_vol > 0.1)
                d_vol = 0.1;
            else if (d_vol < -0.1)
                d_vol = -0.1;

            vol += d_vol;

            // Don't let volatility get down to zero
            if (vol < 0.00001) {vol = 0.0001;}
            ++iter;

        } while (abs(price - test_price) > 0.000001 && iter < 25);


    // If only price to be calculated don't wast time and memory
    price = Binary(S,K,T,r,v,optType,isCash,q);
    if (result == 0) {return price;}

    // Calculate Greeks by perturbing parameters (shocking)
    double p_delta_u = Binary(S*1.0001,K,T,r,v,optType,isCash,q); // shock Spot
    double p_delta_d = Binary(S*0.9999,K,T,r,v,optType,isCash,q);

    double delta1 = (p_delta_u - price) / (0.0001 * S); // real delta definition: dV/dS
    double delta2 = (price - p_delta_d) / (0.0001 * S);

    double delta = 0.5 * (delta1 + delta2);
    double gamma = (delta1 - delta2) / (0.0001 * S);

    double p_vega = Binary(S,K,T,r,v+0.01,optType,isCash,q); // shock volatility
    double vega = p_vega - price;

    double p_theta = Binary(S,K,T-0.5/365.0,r,v,optType,isCash,q); // shock time
    double theta = 2.0 * (p_theta - price);

    double p_rho;
    if (isFut)
        p_rho = Binary(S,K,T,r+0.01,v,optType,isCash,q+0.01); // shock interest rate and dividend
    else
        p_rho = Binary(S,K,T,r+0.01,v,optType,isCash,q); // shock interest rate

    double rho = p_rho - price;

    switch (result)
    {
        case 1:
            return delta;
        case 2:
            return gamma;
        case 3:
            return vega;
        case 4:
            return theta;
        case 5:
            return rho;
        case 6: // implyVol
            return vol;
        default:
            return price;
    }
}


double Binary(double S, double K, double T, double r, double v, char optType, bool isCash, double q)
{
    // Time correction if NULL
    if (T<=0.0) {T = 0.00001;}

    double cash;
    if (isCash)  // CashOrNothing
        cash = 1.0;
    else // AssetOrNothing
        cash = 0.0;

    //double d1 = (log(S / K) + (r - q + 0.5 * v * v) * T) / (v * sqrt(T));
    //double d2 = d1 - v * sqrt(T);
    double pv_S = S * exp(-q * T);
    double pv_K = K * exp(-r * T);
    double d1 = log(pv_S/pv_K) / (v * sqrt(T)) + 0.5 * v * sqrt(T);
    double d2 = d1 - v * sqrt(T);

    if (isCash /*== true*/)
        if (optType == 'C')
            return cash * exp(-r * T) * NDApprox::CND(d2);
        else
            return cash * exp(-r * T) * NDApprox::CND(-d2);
    else /*if (isCash == false)*/
        if (optType == 'C')
            return S * NDApprox::CND(d1);
        else // (optType == 'P')
            return S * NDApprox::CND(-d1);
}

/*int main(){
    double S = 50; double K = 55; double T = 1; double r = 0.05; double v = 0.25;
    char optType = 'C'; bool isCash = true; bool isFut = false; double q = 0.02;

    cout << "Price = " << BinaryOptions(S,K,T,r,v,optType,isCash,isFut,q,0) << endl;
    cout << "Delta = " << BinaryOptions(S,K,T,r,v,optType,isCash,isFut,q,1) << endl;
    cout << "Gamma = " << BinaryOptions(S,K,T,r,v,optType,isCash,isFut,q,2) << endl;
    cout << "Vega = " << BinaryOptions(S,K,T,r,v,optType,isCash,isFut,q,3) << endl;
    cout << "Theta = " << BinaryOptions(S,K,T,r,v,optType,isCash,isFut,q,4) << endl;
    cout << "Rho = " << BinaryOptions(S,K,T,r,v,optType,isCash,isFut,q,5) << endl;
    cout << "Implied Vol = " << BinaryOptions(S,K,T,r,v,optType,isCash,isFut,q,6) << endl;
}*/


// BarrierOption(S, K, r, q, vol, T, IsCall, IsFut, H, IsUp, IsIn, Result)
// Calculations for barrier options on non-dividend-paying stocks, stock indices, currencies and futures

// AverageOption(S, K, r, q, vol, T, IsCall, IsFut, CurrAve, TimeSoFar, Result)
// Calculations for Asian options on non-dividend-paying stocks, stock indices, currencies and futures

// ChooserOption(S, K, r, q, vol, T, IsFut, TimeToChoice, Result)
// Calculations for chooser options on non-dividend-paying stocks, stock indices,currencies and futures

// CompoundOption(S, K1, r, q, vol, T1, IsCall, IsFut, K2, T2, IsOptionOnCall, Result)
// Calculations for compound options on non-dividend-paying stocks, stock indices,currencies and futures

// LookbackOption(S, r, q, vol, T, IsCall, IsFut, IsFixedLookback, Smax, Smin, K, Result)
// Calculations for lookback options on non-dividend-paying stocks, stock indices,currencies and futures

//
// Created by mneri092115 on 30/12/2015.
//

#include "Bond_Options.hpp"
#include "../Tools/NDApprox.hpp"
#include "../Black_Scholes/Black_Scholes.hpp"
#include <cmath>
#include <vector>

using namespace std;

// Zero coupon bond price using Black & Scholes
double BSZeroCouponBondOptionPrice(double B, double K,double T, double r, double v, char optType)
{
    return BSPrice(B,K,T,r,v,optType,0);
    // B * NDApprox::CND(d1) - K * exp(-r * T) * NDApprox::CND(d2) --> for call
}

// Coupon bond price using Black & Scholes with adjusted bond price
double BSCouponBondOptionPrice(double B, double K, double T, double r, double v, char optType,
                       vector<double> coupon_times,
                       vector<double> coupon_amounts)
{
    double adjusted_B = B;
    for (int i = 0; i < coupon_times.size(); ++i)
    {
        if (coupon_times[i] <= time)
            adjusted_B -= coupon_amounts[i] * exp (-r * coupon_times[i]);
    };
    return BSPrice(adjusted_B,K,T,r,v,optType,0);
}

// http://orlin.hgs.name/Modeling%20Derivatives%20in%20C%2B%2B.pdf
// ZeroCouponBondVasicekPrice --> bond price
// ZeroCouponBondEuropeanOptionVasicekPrice --> bond option price

// HullWhite Extended Vasicek model

// European Options on Coupon-Bearing Bonds

// Cox-Ingersoll-Ross Model

// HullWhiteBondOpt(BondLife,Coupon,Princ,Frequ,K,T,sigma,a, IsCall,IsQuoted,Zeros,Result)
// Calculations for bond options using Hull-White model

// TreeBondOpt(BondLife,Coupon,Princ,Frequ,K,T,sigma,a, IsCall,IsQuoted,IsAmerican,Model,nSteps,Zeros,Result)
// Calculations for bond options usng a trinomial tree

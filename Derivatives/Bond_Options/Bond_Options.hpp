//
// Created by mneri092115 on 11/12/2015.
//

#ifndef DERIVATIVES_BOND_OPTIONS_H
#define DERIVATIVES_BOND_OPTIONS_H
#include <vector>
#include <cmath>
using namespace std;


// BlackBondOpt(BondLife,Coupon,Princ,Frequ,K,T,vol,IsCall,IsQuoted,Zeros,Result)
// Calculations for bond options usng Black's model

// HullWhiteBondOpt(BondLife,Coupon,Princ,Frequ,K,T,sigma,a, IsCall,IsQuoted,Zeros,Result)
// Calculations for bond options using Hull-White model

// TreeBondOpt(BondLife,Coupon,Princ,Frequ,K,T,sigma,a, IsCall,IsQuoted,IsAmerican,Model,nSteps,Zeros,Result)
// Calculations for bond options usng a trinomial tree

//BondPricing(BondLife, Coupon, Princ, Frequ, Zeros, IsClean, Result)


#endif //DERIVATIVES_BOND_OPTIONS_H

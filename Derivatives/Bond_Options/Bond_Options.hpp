//
// Created by mneri092115 on 30/12/2015.
//

#ifndef C_BOND_OPTIONS_HPP
#define C_BOND_OPTIONS_HPP

#include <vector>

// Zero coupon bond price using Black & Scholes
double BSZeroCouponBondOptionPrice(double B, double K,double T, double r, double v, char optType);

// Coupon bond price using Black & Scholes with adjusted bond price
double BSCouponBondOptionPrice(double B, double K, double T, double r, double v, char optType,
                         std::vector<double> coupon_times,
                         std::vector<double> coupon_amounts);






#endif //C_BOND_OPTIONS_HPP

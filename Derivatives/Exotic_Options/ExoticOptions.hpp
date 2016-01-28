//
// Created by mneri092115 on 10/12/2015.
//

#ifndef EXOTIC_OPTIONS_H
#define EXOTIC_OPTIONS_H



// Price and Greeks computation for Binary Options
double BinaryOptions(double S, double K, double T, double r, double v, char optType, bool isCash,
                     bool isFut = false, double q = 0, int result = 0);
double Binary(double S, double K, double T, double r, double v, char optType, bool isCash, double q = 0);


// Price and Greeks computation for Barrier Options
double BarrierOptions(double S, double K, double T, double r, double v, char optType, double B,
                      bool isUp = true, bool isIn = true, bool isFut = false, double q = 0, int result = 0);
                      // Default: price of Up and In, no Future
                      // B = barrier level
double Barrier(double S, double K, double T, double r, double v, char optType, double B, int type, double q);
                      // type : Up and In (1), Up and Out (2), Down and In (3), Down and Out (4)
double rubin_A(double pv_S, double x1, double pv_K, double T, double v, double phi);
double rubin_B(double pv_S, double pv_K, double B, double lambda, double y, double v, double T, double phi, double eta);


// AverageOption(S, K, r, q, vol, T, IsCall, IsFut, CurrAve, TimeSoFar, Result)
// Calculations for Asian options on non-dividend-paying stocks, stock indices, currencies and futures

// ChooserOption(S, K, r, q, vol, T, IsFut, TimeToChoice, Result)
// Calculations for chooser options on non-dividend-paying stocks, stock indices,currencies and futures

// CompoundOption(S, K1, r, q, vol, T1, IsCall, IsFut, K2, T2, IsOptionOnCall, Result)
// Calculations for compound options on non-dividend-paying stocks, stock indices,currencies and futures

// LookbackOption(S, r, q, vol, T, IsCall, IsFut, IsFixedLookback, Smax, Smin, K, Result)
// Calculations for lookback options on non-dividend-paying stocks, stock indices,currencies and futures



#endif //C_EXOTICOPTIONS_HPP

//
// Created by mneri092115 on 10/12/2015.
//

#ifndef EXOTIC_OPTIONS_H
#define EXOTIC_OPTIONS_H
#include <vector>
#include <cmath>

using namespace std;


// BinaryOption(S, K, r, q, vol, T, IsCall, IsFut, Divs, IsCash, Result)
// Calculations for binary options on stocks, stock indices,currencies and futures

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



#endif //C_EXOTICOPTIONS_HPP

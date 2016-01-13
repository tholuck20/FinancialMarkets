//
// Created by mneri092115 on 10/12/2015.
//

#ifndef DERIVATIVES_BLACK_SCHOLES_HPP
#define DERIVATIVES_BLACK_SCHOLES_HPP

// Tools
/* As N is a density (integral), we can use several methods to approximate the function:
 *
 - Méthode des rectangles / Rectangle method
 - Méthode des trapèzes / Trapezoidal rule
 - Méthode de Simpson / Simpson's rule
 - Méthode de Gauss-Legendre /
 - Midpoint method
 - Heun's method
 - Euler method
 - Verlet integration
 *
 Which one is the most appropriate to approximate the Standard normal Cumulative distribution function (CDF)?
 *
*/

    // Black and Scholes valuation
    double BSPrice(double S, double K, double T, double r, double v, char optType, double q = 0);
    // BSPrice for FX Options with Garman-Kohlhagen moodel
    double BSGarmanKohlhagen(double S, double K, double T, double rd, double rf, double v, double optType);
            // Foreign risk free rate can be interpreted as a dividend rate
    // BSPrice for options on futures with Black-76 model
    double Black76Price(double F, double K, double T, double r, double v, char optType);
    // BSPrice for commodity options
    double CBSPRice(double S, double K, double T, double r, double v, char optType, double b = 0);




    // Black and Scholes implied volatility
    double BSImplVol(double S, double K, double T, double r, char optType, double q = 0, double cm = 0);
    double BSImplVol2(double S, double K, double T, double r, char optType, double q = 0, double cm = 0);
    double BSImplVol3(double S, double K, double T, double r, double v, char optType, double q = 0, double cm = 0);
    double BSImplVol4(double S, double K, double T, double r, char optType, double q = 0, double cm = 0);

    // Black and Scholes Greeks
    double BSDelta(double S, double K, double T, double r, double v, char optType, double q = 0);
    double BSGamma(double S, double K, double T, double r, double v, double q = 0);
    double BSVega(double S, double K, double T, double r, double v, double q = 0);
    double BSRho(double S, double K, double T, double r, double v, char optType, double q = 0);
    double BSTheta(double S, double K, double T, double r, double v, char optType, double q = 0);
    double BSVanna(double S, double K, double T, double r, double v, double q = 0, double b = 0); //////
    double BSCharm(double S, double K, double T, double r, double v, char optType, double q = 0, double b = 0); //////
    double BSSpeed(double S, double K, double T, double r, double v, double q = 0);
    double BSZomma(double S, double K, double T, double r, double v, double q = 0);
    double BSColor(double S, double K, double T, double r, double v, double q = 0);
    double BSdVega_dTime(double S, double K, double T, double r, double v, double q = 0);
    double BSVomma(double S, double K, double T, double r, double v, double q = 0);
    double BSDualDelta(double S, double K, double T, double r, double v, char optType, double q = 0);
    double BSDualGamma(double S, double K, double T, double r, double v, double q = 0);

#endif //DERIVATIVES_BLACK_SCHOLES_HPP

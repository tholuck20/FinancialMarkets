//
// Created by mneri092115 on 12/01/2016.
//

#ifndef C_AMERICAN_HPP
#define C_AMERICAN_HPP

    // Barone-Adesi and Whaley Approximation
    double BAWApprox(double S, double K, double T, double r, double v, char optType, double b);
    // Newton-Raphson algorithm to solve critical commodity price
    double CCTYPrice(double K, double T, double r, double v, char optType, double b);

    // Binomial valuation for American options
    double Binomial(double S, double K, double T, double r, double v, char optType, double q, int n = 500);

    // Leisen Reimer Binomial Tree (optStyle = European/American)
    double LRBinomial(double S, double K, double T, double r, double v, char optType, char optStyle, int Method = 1);
        // Method: Peizer-Pratt Inversion method (1 or 2) (does not change anything)
        // Results: OK for European Option (both Call & Put)
        //          nOK for American Call Option (same value as European Call)
        //          OK for American Put Option

#endif //C_AMERICAN_HPP

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
    double Binomial(double S, double K, double T, double r, double v, char optType, double q);

    // Leisen Reimer Binomial Tree (optStyle = European/American)
    double LRBinomial(double S, double K, double T, double r, double v, char optType, char optStyle, int Method = 1);
        // Method: Peizer-Pratt Inversion method (1 or 2)

    // Extrapolated Flexible Binomial Tree
    double FlexBinomial(double S, double K, double T, double r, double v, char optType, char optStyle); // n

    // Flexible Binomial Tree from Tian (1991)
    double TianFlexBinomial(double S, double K, double T, double r, double v, char optType, char optStyle); // n + lambda

    // Cox Ross Rubinstein Binomial Tree
    double CRRBinomial(double S, double K, double T, double r, double v, char optType, char optStyle); // int n


#endif //C_AMERICAN_HPP

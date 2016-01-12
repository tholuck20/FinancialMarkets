//
// Created by mneri092115 on 12/01/2016.
//

#include "Binomial.hpp"
#include "../Black_Scholes/Black_Scholes.hpp"
#include "../Tools/NDApprox.hpp"
#include <cmath>
#include <vector>

using namespace std;

double BAWApprox(double S, double K, double T, double r, double v, char optType, double b)
{
    double s;
    if (optType == 'C')
        s = 1;
    else
        s = -1;

    if ((s == 1) && (b >= r))
        return CBSPRice(S,K,T,r,v,optType,b);
    else
        double Sk = CCTYPrice(K, T, r, v, optType, b);
        double N = 2 * b / (v * v);
        double k = 2 * r / (v * v * (1 - exp(-r * T)));
        double d1 = (log(Sk / X) + (b + v * v / 2) * T) / (v * sqrt(T));
        double Q = (-(N - 1) + s * sqrt(pow(N - 1, 2) + 4 * k)) / 2;
        double a = s * (Sk / Q) * (1 - exp((b - r) * T) * NDApprox::CND(s * d1));

        if (s * S < s * Sk)
        {
            return CBSPRice(S,K,T,r,v,optType,b) + a * pow(S / Sk, Q);
        }
        else
        {
            return s * (S - X);
        }
}

double CCTYPrice(double K, double T, double r, double v, char optType, double b) // Critical CTY Price
{
    double s; // toggles positive for call and negative for put
    if (optType == 'C')
        s = 1;
    else
        s = -1;

    double N = 2 * b / (v * v);
    double m = 2 * r / (v * v);
    double qu = (-(N - 1) + s * sqrt(pow(N - 1, 2) + 4 * m)) / 2;
    double su = K / (1 - 1 / qu);
    double h = -s * (b * T + s * 2 * v * sqrt(T)) * K / (s * (su - K));
    double Si = su + K * exp(h) - su * exp(h);

    double k = 2 * r / (v * v * (1 - exp(-r * T)));
    double d1 = (log(Si / K) + (b + v * v / 2) * T) / (v * sqrt(T));
    double Q = (-(N - 1) + s * sqrt(pow(N - 1, 2) + 4 * k)) / 2;
    double LHS = s * (Si - K);
    double RHS = CBSPRice(S,K,T,r,v,optType,b) + s * (1 - exp((b - r) * T) * NDApprox::PDF(s * d1)) * Si / Q;

    double bi = s * exp((b - r) * T) * NDApprox::CND(s * d1) * (1 - 1 / Q) + \
                s * (1 - s * exp((b - r) * T) * NDApprox::CND(s * d1) / (v * sqrt(T))) / Q;
    double E = 0.000001;

    // Newton-Raphson algorithm for finding critical price Si
    while (fabs(LHS - RHS) / K > E)
    {
        Si = (K + s * (RHS - bi * Si)) / (1 - s * bi);
        d1 = (log(Si / K) + (b + v * v / 2) * T) / (v * sqrt(T));
        LHS = s * (Si - K);
        RHS = CBSPRice(S,K,T,r,v,optType,b) + s * (1 - exp((b - r) * T) * NDApprox::CND(s * d1)) * Si / Q;
        bi = s * exp((b - r) * T) * NDApprox::CND(s * d1) * (1 - 1 / Q) + \
             s * (1 - s * exp((b - r) * T) * NDApprox::CND(s * d1) / (v * sqrt(T))) / Q;
    }
    return Si;
}

double Binomial(double S, double K, double T, double r, double v, char optType, double q)
{
    int iter = 500;
    double delta_t = T / iter;
    double R = exp(r * delta_t);
    double Rinv = 1.0 / R;
    double u = exp(v * sqrt(delta_t));
    double uu = u * u;
    double d = 1.0 / u;
    double p_up = (exp((r - q) * (delta_t)) - d) / (u - d);
    double p_down = 1.0 - p_up;
    double prices[iter+1];
    double values[iter+1];

    prices[0] = S * pow(d, iter);  // fill in the endnodes.
    for (int i = 1; i <= iter; ++i) prices[i] = uu * prices[i - 1];
    if (optType == 'P')
        for (int i = 0; i <= iter; ++i) values[i] = max(0, (K - prices[i]));
    else
        for (int i = 0; i <= iter; ++i) values[i] = max(0, (prices[i] - K));
    for (int step = iter - 1; step >= 0; --step)
    {
        for (int i = 0; i <= step; ++i)
        {
            values[i] = (p_up * values[i + 1] + p_down * values[i]) * Rinv;
            prices[i] = d * prices[i + 1];
            if (optType == 'P')
            {
                values[i] = max(values[i], K - prices[i]);
            }
            else
            {
                values[i] = max(values[i], prices[i] - K);
            }
        }
    }
    return values[0];
}

double LRBinomial(double S, double K, double T, double r, double v, char optType, char optStyle, int Method) {
    NDApprox::Sign(x);

    int n = 100;                    // Number of steps
    if (n % 2 == 0)                    // Use only odd number of steps
        n = n + 1;

    vector <vector<double>> S(n + 1, vector<double>(n + 1, 0));        // Vectors for calls and puts
    vector <vector<double>> EC(n + 1, vector<double>(n + 1, 0));     // European Call
    vector <vector<double>> EP(n + 1, vector<double>(n + 1, 0));     // European Put
    vector <vector<double>> AC(n + 1, vector<double>(n + 1, 0));     // American Call
    vector <vector<double>> AP(n + 1, vector<double>(n + 1, 0));     // American Put

    double d1 = (log(S / K) + (r + 0.5 * v * v) * T) / v / sqrt(T);
    double d2 = d2 = d1 - v * sqrt(T);
    double u_n = exp(v * sqrt(T / n));
    double d_n = exp(-v * sqrt(T / n));
    double r_n = exp(r * T / n);

    double Term1 = pow((d1 / (n + 1.0 / 3.0 - (1 - Method) * 0.1 / (n + 1))), 2) * (n + 1.0 / 6.0);
    double pp = 0.5 + NDApprox::Sign(d1) * 0.5 * sqrt(1 - exp(-Term1));

    double Term2 = pow((d2 / (n + 1.0 / 3.0 - (1 - Method) * 0.1 / (n + 1))), 2) * (n + 1.0 / 6.0);
    double p = 0.5 + NDApprox::Sign(d2) * 0.5 * sqrt(1 - exp(-Term2));

    double u = r_n * pp / p;
    double d = (r_n - p * u) / (1 - p);

    // Build the binomial tree
    for (int j = 0; j <= n; ++j) {
        for (int i = 0; i <= j; ++i) {
            S[i][j] = Spot * pow(u, j - i) * pow(d, i);
        }
    }
    // Compute terminal payoffs
    for (int i = 0; i <= n; ++i) {
        EC[i][n] = max(S[i][n] - K, 0.0);
        AC[i][n] = max(S[i][n] - K, 0.0);
        EP[i][n] = max(K - S[i][n], 0.0);
        AP[i][n] = max(K - S[i][n], 0.0);
    }
    // Backward recursion through the tree
    for (int j = n - 1; j >= 0; --j) {
        for (int i = 0; i <= j; ++i) {
            EC[i][j] = exp(-r * T / n) * (p * EC[i][j + 1] + (1 - p) * EC[i + 1][j + 1]);
            EP[i][j] = exp(-r * T / n) * (p * EP[i][j + 1] + (1 - p) * EP[i + 1][j + 1]);
            AC[i][j] = max(S[i][j] - K, exp(-r * T / n) * (p * AC[i][j + 1] + (1 - p) * AC[i + 1][j + 1]));
            AP[i][j] = max(K - S[i][j], exp(-r * T / n) * (p * AP[i][j + 1] + (1 - p) * AP[i + 1][j + 1]));
        }
    }


    // optType, optStyle

    if (optStyle == 'E')
        if (optType == 'C')
            return EC[0][0]; // European Call
        else
            return EP[0][0]; // European Put
    else if (optStyl == 'A')
        if (optType == 'C')
            return AC[0][0]; // American Call
        else
            return AP[0][0]; // American Put
}






double FlexBinomial(double S, double K, double T, double r, double v, char optType, char optStyle)
{

}

double TianFlexBinomial(double S, double K, double T, double r, double v, char optType, char optStyle)
{

}

double CRRBinomial(double S, double K, double T, double r, double v, char optType, char optStyle)
{

}

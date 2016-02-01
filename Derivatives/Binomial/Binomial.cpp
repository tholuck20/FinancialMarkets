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
        double Sk = CCTYPrice(K,T,r,v,optType,b);
        double N = 2 * b / (v * v);
        double k = 2 * r / (v * v * (1 - exp(-r * T)));
        double d1 = (log(Sk / K) + (b + v * v / 2) * T) / (v * sqrt(T));
        double Q = (-(N - 1) + s * sqrt(pow(N - 1, 2) + 4 * k)) / 2;
        double a = s * (Sk / Q) * (1 - exp((b - r) * T) * NDApprox::CND(s * d1));

        if (s * S < s * Sk)
            return CBSPRice(S,K,T,r,v,optType,b) + a * pow(S / Sk, Q);
        else
            return s * (S - K);
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
    double RHS = CBSPRice(Si,K,T,r,v,optType,b) + s * (1 - exp((b - r) * T) * NDApprox::PDF(s * d1)) * Si / Q;

    double bi = s * exp((b - r) * T) * NDApprox::CND(s * d1) * (1 - 1 / Q) + \
                s * (1 - s * exp((b - r) * T) * NDApprox::CND(s * d1) / (v * sqrt(T))) / Q;
    double E = 0.000001;

    // Newton-Raphson algorithm for finding critical price Si
    while (fabs(LHS - RHS) / K > E)
    {
        Si = (K + s * (RHS - bi * Si)) / (1 - s * bi);
        d1 = (log(Si / K) + (b + v * v / 2) * T) / (v * sqrt(T));
        LHS = s * (Si - K);
        RHS = CBSPRice(Si,K,T,r,v,optType,b) + s * (1 - exp((b - r) * T) * NDApprox::CND(s * d1)) * Si / Q;
        bi = s * exp((b - r) * T) * NDApprox::CND(s * d1) * (1 - 1 / Q) + \
             s * (1 - s * exp((b - r) * T) * NDApprox::CND(s * d1) / (v * sqrt(T))) / Q;
    }
    return Si;
}

double Binomial(double S, double K, double T, double r, double v, char optType, double q, int n)
{
    double delta_t = T / n;
    double R = exp(r * delta_t);
    double Rinv = 1.0 / R;
    double u = exp(v * sqrt(delta_t));
    double uu = u * u;
    double d = 1.0 / u;
    double p_up = (exp((r - q) * (delta_t)) - d) / (u - d);
    double p_down = 1.0 - p_up;
    double prices[n+1];
    double values[n+1];

    prices[0] = S * pow(d, n);  // fill in the endnodes.
    for (int i = 1; i <= n; ++i) prices[i] = uu * prices[i - 1];
    if (optType == 'P')
        for (int i = 0; i <= n; ++i) values[i] = max(0.0, (K - prices[i]));
    else
        for (int i = 0; i <= n; ++i) values[i] = max(0.0, (prices[i] - K));
    for (int step = n - 1; step >= 0; --step)
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

double LRBinomial(double S, double K, double T, double r, double v, char optType, char optStyle, int Method)
{
    int n = 500;                    // Number of steps
    if (n % 2 == 0) n = n + 1;      // Use only odd number of steps

    vector <vector<double>> V(n+1, vector<double>(n+1, 0));
    vector <vector<double>> Op(n+1, vector<double>(n+1, 0));

    double d1 = (log(S / K) + (r + 0.5 * v * v) * T) / v / sqrt(T); // Black & Scholes d1
    double d2 = d1 - v * sqrt(T);
    double dt = T/n;
    double r_n = exp(r * dt);

    double Term1 = pow((d1 / (n + 1.0 / 3.0 - (1 - Method) * 0.1 / (n + 1))), 2) * (n + 1.0 / 6.0);
    double pp = 0.5 + NDApprox::Sign(d1) * 0.5 * sqrt(1 - exp(-Term1));

    double Term2 = pow((d2 / (n + 1.0 / 3.0 - (1 - Method) * 0.1 / (n + 1))), 2) * (n + 1.0 / 6.0);
    double p = 0.5 + NDApprox::Sign(d2) * 0.5 * sqrt(1 - exp(-Term2));

    double u = r_n * pp / p;
    double d = (r_n - p * u) / (1 - p);

    // Build the binomial tree
    for (int j=0; j<=n; ++j) {
        for (int i=0; i<=j; ++i) {
            V[i][j] = S * pow(u, j - i) * pow(d, i);
        }
    }
    // Calculate Terminal Price for Calls and Puts (option value at each final node)
    for (int i=0; i<=n; ++i) {
        if (optType == 'C')
            Op[i][n] = max(V[i][n] - K, 0.0); // AC & EC
        else // optType == 'P'
            Op[i][n] = max(K - V[i][n], 0.0); // AP & EP
    }
    // Backward recursion through the tree (option value at earlier nodes)
    for (int j=n-1; j>=0; --j) {
        for (int i=0; i<=j; ++i) {
            if (optStyle == 'A')
                if (optType == 'C')
                    Op[i][j] = max(V[i][j] - K, exp(-r * dt) * (p * Op[i][j + 1] + (1 - p) * Op[i + 1][j + 1]));
                else
                    Op[i][j] = max(K - V[i][j], exp(-r * dt) * (p * Op[i][j + 1] + (1 - p) * Op[i + 1][j + 1]));
            else
                Op[i][j] = exp(-r * dt) * (p * Op[i][j + 1] + (1 - p) * Op[i + 1][j + 1]);
        }
    }
    return Op[0][0];
}

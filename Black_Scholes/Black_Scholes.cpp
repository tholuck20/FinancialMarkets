//
// Created by mneri092115 on 10/12/2015.
//

// S = Stock price
// K = Strike price
// r = Risk free rate
// T = Maturity
// v = Yearly Implied volatility
// optType = Option type (Call or Put)

// d1 = (ln(S/K)+(r+(v^2)/2)*T)/v*sqrt(T)
// d2 = d1 - v*sqrt(T)

// Call = S*N(d1)-K*exp(-r*T)*N(d2)
//      = S*N(d1)-K*exp(-r*T)*N(d1-v*sqrt(T))
// Put = K*exp(-r*T)*N(-d2)-S*N(-d1)
//     = K*exp(-r*T)*N(-d1+v*sqrt(T))-S*N(-d1)

#define PI (3.141592653589793238462643383279)

#include <cmath>
#include <iomanip>
#include <iostream>

using namespace std;

// N1 - First method: an approximation of the cumulative distribution function (CDF)
double N1(double x){
    double sum = x;
    double temp = x;

    for(int i=1; i<=1000; ++i){
        temp = (temp * x * x / (2 * i + 1));
        sum += temp;
    }
    return 0.5 + (sum / sqrt(2*PI)) * exp(-(x * x)/2);
};

// N2 - Second method
double N2(double x){
    double Q = 1 / (1 + 0.2316419 * abs(x));
    double R = (1 / sqrt(2 * PI) *exp(-0.5 * x * x));
    double temp = (1.781477937 + Q * (-1.821255978 + 1.330274429 * Q));
    temp = R * (Q * (0.31938153 + Q * (-0.356563782 + Q * temp)));

    if (x < 0)
        return temp;
    else
        return 1-temp;
};

// N3 - Third method
double N3(double x){
    return 1 / sqrt(2 * PI) * exp(-0.5 * x * x);
};

// N4 - Fourth method (not from me and in 2 parts)

    // Boole's Rule
    double Boole(double StartPoint, double EndPoint, int n) {
        vector<double> X(n+1, 0.0);
        vector<double> Y(n+1, 0.0);
        double delta_x = (EndPoint - StartPoint)/double(n);
        for (int i=0; i<=n; ++i) {
            X[i] = StartPoint + i*delta_x;
            Y[i] = f(X[i]);
        }
        double sum = 0;
        for (int t=0; t<=(n-1)/4; t++) {
            int ind = 4*t;
            sum += (1/45.0)*(14*Y[ind] + 64*Y[ind+1] + 24*Y[ind+2] + 64*Y[ind+3] + 14*Y[ind+4])*delta_x;
        }
        return sum;
    }

    // N(0,1) cdf by Boole's Rule
    double N4(double x) {
        return Boole(-10.0, x, 240);
    };



// Black and Scholes Price
double BSPrice(double S, double K, double r, double T, double v, char optType){
    double d1 = (log(S / K) + (r + (v * v) / 2) * T) / (v * sqrt(T));
    double d2 = d1 - v * sqrt(T);

    if (optType == 'C')
        return S * N1(d1) - K * exp(-r * T) * N1(d2); // change N type for different CDF valuation method
    else
        return K * exp(-r * T) * N1(-d2) - S * N1(-d1);
};

// Black and Scholes Delta
double BSDelta(double S, double K, double r, double T, double v, char optType, double q = 0){
    double d1 = (log(S / K) + (r - q + 0.5* v * v) * T) / (v * sqrt(T));
    if (optType == 'C')
        return exp(-q * T) * N1(d1);
    else
        return exp(-r * q) * (N1(d1) - 1); // if q == 0 -->> exp (-r * 0) = 1
};

// Black and Scholes Gamma
double BDGamma(double S, double K, double r, double T, double v){

};


// Black-Scholes Gamma
double BSGamma(double S, double K, double T, double r, double v)
{
    double d = (log(S/K) + T*(r + 0.5*v*v)) / (v*sqrt(T));
    return f(d) / S / v / sqrt(T);
}

double f(double x) {
    double pi =  4.0*atan(1.0);
    return exp(-x*x*0.5)/sqrt(2*pi);
}

/*
Call B&S Price = 1.7691700522
Put  B&S Price = 5.0621999098

d1 B&S = -0.29160272
d2 B&S = -0.54160272

Delta B&S = 0.3852951980
Delta B&S = -0.6147048020
*/

int main()
{
    double S = 30;
    double K = 35;
    double r = 0.05;
    double T = 1;
    double v = 0.25;
    char optType = 'C';
    char optType2 = 'P';
    int dNb = 1;
    int dNb2 = 2;

    cout << "Call B&S Price = " << fixed << setprecision(10) << BSPrice(S,K,r,T,v,optType) << endl;
    cout << "Put  B&S Price = " << fixed << setprecision(10) << BSPrice(S,K,r,T,v,optType2) << endl << endl;

    cout << "d1 B&S = " << fixed << setprecision(8) << BSd(S,K,r,T,v,dNb) << endl;
    cout << "d2 B&S = " << fixed << setprecision(8) <<  BSd(S,K,r,T,v,dNb2) << endl << endl;

    cout << "Delta B&S = " << fixed << setprecision(10) << BSDelta(S,K,r,T,v,optType) << endl;
    cout << "Delta B&S = " << fixed << setprecision(10) << BSDelta(S,K,r,T,v,optType2) << endl << endl;
}

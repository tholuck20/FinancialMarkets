
// Longstaff-Schwartz (2000) algorithm for valuing American options under Black-Scholes

#include "stdafx.h"
#include <iostream>
#include <iomanip>
#include <vector>
#include <math.h>

#include "BlackScholes.h"
#include "MatrixOperations.h"
#include "BlackScholesSim.h"
#include "BlackScholesLSM.h"
#include "TrinomialTree.h"

using namespace std;



int main() {



        // Stock price, strike, maturity, volatility, risk free rate, dividend yield

        double S = 40.0;

        double K = 40.0;

        double T = 0.25;

        double v = 0.20;

        double r = 0.05;

        double q = 0.0;

        char PutCall = 'P';



        // Number of stock price paths and time steps

        int NT = 100;

        int NS = 20000;



        // Generate the stock price paths and transpose

        CBlackScholesSim BSSim;

        CMatrixOperations Mat;

        vector<vector<double> > St = BSSim.GenerateBSpaths(S,T,r,q,v,NS,NT);

        vector<vector<double> > StTranspose = Mat.MTrans(St);



        // Obtain the European and American LSM prices

        CBlackScholesLSM BSLSM;

        vector<double> Prices = BSLSM.LSM(StTranspose,K,r,q,T,NT,NS,PutCall);

        double EuroPrice = Prices[0];

        double AmerPrice = Prices[1];



        // Obtain the closed form Black Scholes European price

        CBlackScholes BS;

        double EuroClosed = BS.BSPrice(S,K,r,q,v,T,PutCall);



        // Obtain the American price by control variate

        double Premium = AmerPrice - EuroPrice;

        double CVPrice = EuroClosed + Premium;



        // Obtain the Trinomial tree prices

        int Nt = 250;

        CTrinomialTree Tree;

        double EuroTri = Tree.Trinomial(S,K,r,q,v,T,PutCall,'E',Nt);

        double AmerTri = Tree.Trinomial(S,K,r,q,v,T,PutCall,'A',Nt);



        // Output the results

        cout << setprecision(5) << fixed;

        cout << "Longstaff-Schwartz American option approximation" << endl;

        cout << "------------------------------------------------" << endl;

        cout << "Method              EuroPrice    AmerPrice " << endl;

        cout << "------------------------------------------------" << endl;

        cout << "Black-Scholes        " << EuroClosed << endl;

        cout << "LSM Simulation       " << EuroPrice  << "       " << AmerPrice << endl;

        cout << "LSM Control Variate                " << CVPrice   << endl;

        cout << "Trinomial            " << EuroTri  << "       " << AmerTri << endl;

        cout << "------------------------------------------------" << endl;

}



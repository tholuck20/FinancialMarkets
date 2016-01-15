
// Trinomial Tree

// by Fabrice Douglas Rouah, FRouah.com and Volopta.com



#include "StdAfx.h"

#include "TrinomialTree.h"



CTrinomialTree::CTrinomialTree(void) { }

CTrinomialTree::~CTrinomialTree(void) { }



double CTrinomialTree::Trinomial(double Spot,double K,double r,double q,double v,double T,char PutCall,char EuroAmer,int n)

{

        int i,j;

        double b = r - q;

        double dt = T/n;

        double u = exp(v*sqrt(2.0*dt));

        double d = 1.0/u;

        double pu = pow((exp(0.5*b*dt) - exp(-v*sqrt(0.5*dt))) / (exp(v*sqrt(0.5*dt)) - exp(-v*sqrt(0.5*dt))), 2);

        double pd = pow((exp(v*sqrt(0.5*dt)) - exp(0.5*b*dt))  / (exp(v*sqrt(0.5*dt)) - exp(-v*sqrt(0.5*dt))), 2);

        double pm = 1.0 - pu - pd;



        // Build the trinomial tree for the stock price evolution

        vector<vector<double> > S(2*n+1,vector<double> (n+1));

        for (j=0; j<=n; j++)

                for (i=0; i<=2*j; i++)

                         S[i][j] = Spot*pow(u,double(j-i));



        // Boost Matrices for the Euro and American calls and puts

        vector<vector<double> > V(2*n+1,vector<double> (n+1));



        // Compute terminal payoffs

        for (i=0; i<=2*n; i++) {

                if (PutCall == 'C')

                        V[i][n] = max(S[i][n] - K, 0.0);

                else if (PutCall == 'P')

                        V[i][n] = max(K - S[i][n], 0.0);

        }



        // Backward recursion through the tree

        for (j=n-1; j>=0; j--) {

                for (i=0; i<=2*j; i++) {

                        if (EuroAmer == 'E')

                                V[i][j] = exp(-r*dt)*(pu*V[i][j+1] + pm*V[i+1][j+1] + pd*V[i+2][j+1]);

                        else if (EuroAmer == 'A') {

                                if (PutCall == 'C')

                                        V[i][j] = max(S[i][j] - K, exp(-r*dt)*(pu*V[i][j+1] + pm*V[i+1][j+1] + pd*V[i+2][j+1]));

                                else if (PutCall  == 'P')

                                        V[i][j] = max(K - S[i][j], exp(-r*dt)*(pu*V[i][j+1] + pm*V[i+1][j+1] + pd*V[i+2][j+1]));

                        }

                }

        }

        // Output the results

        return V[0][0];

}





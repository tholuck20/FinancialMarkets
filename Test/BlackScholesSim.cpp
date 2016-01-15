
// Simulation of stock prices under Black Scholes

#include "StdAfx.h"

#include "BlackScholesSim.h"



CBlackScholesSim::CBlackScholesSim(void) { }

CBlackScholesSim::~CBlackScholesSim(void) { }

 

// Generate Black Scholes stock price paths

vector<vector<double> > CBlackScholesSim::GenerateBSpaths(double S0,double T,double rf,double q,double v,int NS,int NT)

{

        // Set the seed for random number generation

        srand (time(0));

        double u1,u2;

        double pi = 3.141592653589793;

        double dt = T/double(NT);

        // Random standard normal Z(0,1)

        double Z;

        vector<vector<double> > St(NT,vector<double> (NS));

        for (int s=0; s<=NS-1; s++) {

                for (int t=0; t<=NT-1; t++) {

                        //Z = RandNormal();

                        if (t==0)

                                St[t][s] = S0;

                        else {

                                // Independent uniform random variables

                                u1 = ((double)rand()/(double)RAND_MAX);

                                u2 = ((double)rand()/(double)RAND_MAX);

                                // Floor u1 to avoid errors with log function

                                u1 = max(u1,1.0e-10);

                                // Z ~ N(0,1) by Box-Muller transformation

                                Z = sqrt(-2.0*log(u1)) * sin(2*pi*u2);

                                // Simulated stock price S(t)

                                St[t][s] = St[t-1][s]*exp((rf-q-0.5*v*v)*dt + v*sqrt(dt)*Z);

                        }

                }

        }

        return St;

}



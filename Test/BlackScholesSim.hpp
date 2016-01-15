
#pragma once



#include <math.h>

#include <vector>

#include <time.h>

using namespace std;





class CBlackScholesSim {

public:

        CBlackScholesSim(void);

        ~CBlackScholesSim(void);

        vector<vector<double> > CBlackScholesSim::GenerateBSpaths(double S0,double T,double rf,double q,double v,int NS,int NT);

};



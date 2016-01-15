
#pragma once

#include <math.h>



class CBlackScholes {

public:

        CBlackScholes(void);

        ~CBlackScholes(void);

        double normcdf(double z);

        double BSPrice(double S,double K,double r,double q,double v,double T,char PutCall);

};



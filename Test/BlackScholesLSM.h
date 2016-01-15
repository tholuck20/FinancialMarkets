
#pragma once

#include <algorithm>
#include <set>
#include <iterator>
#include "MatrixOperations.h"



class CBlackScholesLSM {

public:

        CBlackScholesLSM(void);

        ~CBlackScholesLSM(void);

        vector<double> LSM(vector<vector<double> > S,double K,double r,double q,double T,int NT,int NS,char PutCall);

};



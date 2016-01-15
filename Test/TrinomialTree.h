
#pragma once

#include <vector>

using namespace std;



class CTrinomialTree {

public:

        CTrinomialTree(void);

        ~CTrinomialTree(void);

        double Trinomial(double Spot,double K,double r,double q,double v,double T,char PutCall,char EuroAmer,int n);

};



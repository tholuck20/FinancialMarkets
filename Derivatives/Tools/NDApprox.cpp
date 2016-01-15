// Normal Distribution Appoximations
// Created by mneri092115 on 11/12/2015.


/* As NDF is a density (integral), we can use several methods to approximate the function:
 *
 - Méthode des rectangles / Rectangle method
 - Méthode des trapèzes / Trapezoidal rule
 - Méthode de Simpson / Simpson's rule
 - Méthode de Gauss-Legendre /
 - Midpoint method
 - Heun's method
 - Euler method
 - Verlet integration
 *
 Which one is the most appropriate to approximate the Standard normal Cumulative distribution function (CDF)?
 *
*/

#define PI (3.141592653589793238462643383279)

#include "NDApprox.hpp"
#include <cmath>
#include <vector>

using namespace std;

double NDApprox::PDF(double x) // Probability Density Function
{
    return exp(-0.5 * x * x) / sqrt(2 * PI);
}

double NDApprox::ND(double x) // Normal Distribution Function (for easier use in future code)
{
    return exp(-0.5 * x * x) / sqrt(2 * PI);
}

double NDApprox::CND(double x) // Marsaglia (2004)
{
    double sum = x;
    double temp = x;

    for(int i=1; i<=1000; ++i){
        temp = (temp * x * x / (2 * i + 1));
        sum += temp;
    }
    return 0.5 + (sum / sqrt(2*PI)) * exp(-(x * x)/2);
}

// N(0,1) cdf with Boole's Rule
double NDApprox::CND2(double x)
{
    return Boole(-10.0, x, 240);
}

double NDApprox::CND3(double x) // Abramowitz and Stegun
{
    const double b = 0.2316419;
    const double a1 = 0.319381530; const double a2 = 0.356563782;
    const double a3 = 1.781477937; const double a4 = 1.821255978;
    const double a5 = 1.330274429;

    double Q = 1.00 / (1.00 + b * fabs(x));
    double R = NDApprox::PDF(x);

    double CND = R * (Q * (a1 + Q * (-a2 + Q * (a3 + Q * (-a4 + Q * a5)))));

    if (x < 0)
        return CND;
    else
        return 1-CND;
}

double NDApprox::CND4(double x) // Shore (2005)
{
    const double L = -0.61228883; const double A = -6.37309208;
    const double S1 = -0.11105481; const double S2 = 0.44334159;

    double tempA = A/(L/S1);
    double tempB = (pow(1 + S1 * x, L / S1) - 1);
    double tempB_ = (pow(1 + S1 * (-x), L / S1) - 1);

    double g = exp(-log(2) * exp(tempA * tempB + S2 * x)); // g(z)
    double g_ = exp(-log(2) * exp(tempA * tempB_ + S2 * (-x))); // g(-z)

    return 0.5 * (1 + g_ - g);
}

double NDApprox::CND5(double x) // Double precision univariate normal function
{
    double y, Exponential, SumA, SumB;
    double result;
    y = fabs(x);
    if(y > 37.0)
    {
        return 0.0;
    }
    else
    {
        Exponential = exp(-0.5 * y * y);
        if(y < 7.07106781186547)
        {
            SumA = 3.52624965998911E-02 * y + 0.700383064443688;
            SumA = SumA * y + 6.37396220353165;
            SumA = SumA * y + 33.912866078383;
            SumA = SumA * y + 112.079291497871;
            SumA = SumA * y + 221.213596169931;
            SumA = SumA * y + 220.206867912376;
            SumB = 8.83883476483184E-02 * y + 1.75566716318264;
            SumB = SumB * y + 16.064177579207;
            SumB = SumB * y + 86.7807322029461;
            SumB = SumB * y + 296.564248779674;
            SumB = SumB * y + 637.333633378831;
            SumB = SumB * y + 793.826512519948;
            SumB = SumB * y + 440.413735824752;
            result = Exponential * SumA / SumB;
        }
        else
        {
            SumA = y + 0.65;
            SumA = y + 4.0 / SumA;
            SumA = y + 3.0 / SumA;
            SumA = y + 2.0 / SumA;
            SumA = y + 1.0 / SumA;
            result = Exponential / (SumA * 2.506628274631);
        }

        if(x > 0) result = 1.0 - result;
        return result;
    }
}

double NDApprox::BND(double X, double y, double rho)
{
    return 1.0 / (2.0 * PI * sqrt(1.0 - rho * rho)) * \
    exp(-1.0 / (2.0 * (1.0 - rho * rho)) * (X * X + y * y - 2.0 * X * y * rho));
}

double NDApprox::CBND(double X, double y, double rho)
{
    // A function for computing bivariate normal probabilities
    //
    // This function is based on the method described by
    //      Drezner, Z and G.O. Wesolowsky, (1990),
    //      On the computation of the bivariate normal integral,
    //      Journal of Statist. Comput. Simul. 35, pp. 101-107,
    //  with major modifications for double precision, and for |R| close to 1.
    // Code originally translated into VBA by Graeme West
    int i, ISs, LG, NG;
    //Dim XX(10, 3) As Double, W(10, 3) As Double
    double XX[11][4];
    double W[11][4];

    double h, k, hk, hs, BVN, Ass, asr, sn;
    double A, b, bs, c, d;
    double xs, rs;

    W[1][1] = 0.17132449237917;
    XX[1][1] = -0.932469514203152;
    W[2][1] = 0.360761573048138;
    XX[2][1] = -0.661209386466265;
    W[3][1] = 0.46791393457269;
    XX[3][1] = -0.238619186083197;

    W[1][2] = 4.71753363865118E-02;
    XX[1][2] = -0.981560634246719;
    W[2][2] = 0.106939325995318;
    XX[2][2] = -0.904117256370475;
    W[3][2] = 0.160078328543346;
    XX[3][2] = -0.769902674194305;
    W[4][2] = 0.203167426723066;
    XX[4][2] = -0.587317954286617;
    W[5][2] = 0.233492536538355;
    XX[5][2] = -0.36783149899818;
    W[6][2] = 0.249147045813403;
    XX[6][2] = -0.125233408511469;

    W[1][3] = 1.76140071391521E-02;
    XX[1][3] = -0.993128599185095;
    W[2][3] = 4.06014298003869E-02;
    XX[2][3] = -0.963971927277914;
    W[3][3] = 6.26720483341091E-02;
    XX[3][3] = -0.912234428251326;
    W[4][3] = 8.32767415767048E-02;
    XX[4][3] = -0.839116971822219;
    W[5][3] = 0.10193011981724;
    XX[5][3] = -0.746331906460151;
    W[6][3] = 0.118194531961518;
    XX[6][3] = -0.636053680726515;
    W[7][3] = 0.131688638449177;
    XX[7][3] = -0.510867001950827;
    W[8][3] = 0.142096109318382;
    XX[8][3] = -0.37370608871542;
    W[9][3] = 0.149172986472604;
    XX[9][3] = -0.227785851141645;
    W[10][3] = 0.152753387130726;
    XX[10][3] = -7.65265211334973E-02;

    if(fabs(rho) < 0.3) {
        NG = 1;
        LG = 3;
    }
    else if (fabs(rho) < 0.75) {
        NG = 2;
        LG = 6;
    }
    else {
        NG = 3;
        LG = 10;
    }


    h = -X;
    k = -y;
    hk = h * k;
    BVN = 0.0;

    if(fabs(rho) < 0.925)
    {
        if(fabs(rho) > 0.0)
        {
            hs = (h * h + k * k) / 2.0;
            asr = ArcSin(rho);
            for(i = 1; i <= LG; ++i)
            {
                for(ISs = -1; ISs <= 1; i += 2)
                {
                    sn = sin(asr * (ISs * XX[i][NG] + 1) / 2.0);
                    BVN = BVN + W[i][NG] * exp((sn * hk - hs) / (1.0 - sn * sn));
                }
            }
            BVN = BVN * asr / (4.0 * PI);
        }
        BVN = BVN + CND(-h) * CND(-k);
    }
    else
    {
        if(rho < 0.0)
        {
            k = -k;
            hk = -hk;
        }
        if(fabs(rho) < 1.0)
        {
            Ass = (1.0 - rho) * (1.0 + rho);
            A = sqrt(Ass);
            bs = (h - k) *(h - k);
            c = (4 - hk) / 8.0;
            d = (12 - hk) / 16.0;
            asr = -1.0*(bs / Ass + hk) / 2.0;
            if(asr > -100.0) BVN = A * exp(asr) * (1.0 - c * (bs - Ass) * (1.0 - d * bs / 5) / 3.0 + c * d * Ass * Ass / 5.0);
            if(-hk < 100)
            {
                b = sqrt(bs);
                BVN = BVN - exp(-hk / 2) * sqrt(2.0 * PI) * CND(-b / A) * b * (1.0 - c * bs * (1.0 - d * bs / 5.0) / 3.0);
            }
            A = A / 2.0;
            for(i = 1; i<= LG; ++i)
            {
                for(ISs = -1 ; ISs<=1; ISs += 2)
                {
                    xs = (A * (ISs * XX[i][NG] + 1.0)) * (A * (ISs * XX[i][NG] + 1.0));
                    rs = sqrt(1.0 - xs);
                    asr = -(bs / xs + hk) / 2.0;
                    if(asr > -100)
                    {
                        BVN = BVN + A * W[i][NG] * exp(asr) * (exp(-hk * (1.0 - rs) / (2.0 * (1.0 + rs))) / rs - (1.0 + c * xs * (1.0 + d * xs)));
                    }
                }
            }
            BVN = -BVN / (2 * PI);
        }

        if(rho > 0.0)
        {
            BVN = BVN + CND(-Max(h, k));
        }
        else
        {
            BVN = -BVN;
            if(k > h) BVN = BVN + CND(k) - CND(h);
        }
    }


    return BVN; //CBND = BVN
}

double NDApprox::CBND2(double X, double y, double rho)
{
    double g, P, sum;

    double A[5] = {0.018854042, 0.038088059, 0.0452707394, 0.038088059, 0.018854042};
    double b[5] = {0.04691008, 0.23076534, 0.5, 0.76923466, 0.95308992};
    sum = 0.0;
    for(int i = 0; i < 5; ++i)
    {
        P = b[i] * rho;
        g = 1 - P * P;
        sum += A[i] * exp(0.5 * (2.0 * X * y * P - X * X - y * y) / g) / sqrt(g);
    }
    return rho * sum + CND(X) * CND(y);
}

double NDApprox::CBND3(double A, double b, double rho)
{
    double X[5] =  {0.24840615, 0.39233107, 0.21141819, 0.03324666, 0.00082485334};
    double y[5] =  {0.10024215, 0.48281397, 1.0609498, 1.7797294, 2.6697604 };
    double rho1, rho2, delta;
    double a1, b1, sum;
    int i, j;
    a1 = A / sqrt(2.0 * (1.0 - rho * rho));
    b1 = b / sqrt(2.0 * (1.0 - rho * rho));
    double result=0.0;

    if(A <= 0.0 && b <= 0.0 && rho <= 0.0)
    {
        sum = 0.0;
        for(i = 0; i<5; ++i)
        {
            for(j=0; j<5; ++j)
            {
                sum = sum + X[i] * X[j] * exp(a1 * (2.0 * y[i] - a1)
                                              + b1 * (2.0 * y[j] - b1) + 2.0 * rho * (y[i] - a1) * (y[j] - b1));
                result = sqrt(1.0 - rho * rho) / PI * sum;
            }
        }

    }
    else if(A <= 0.0 && b >= 0.0 && rho >= 0.0)
    {
        result = CND(A) - CBND3(A, -b, -rho);
    }
    else if (A >= 0.0 && b <= 0.0 && rho >= 0.0)
    {
        result = CND(b) - CBND3(-A, b, -rho);
    }
    else if (A >= 0.0 && b >= 0.0 && rho <= 0.0)
    {
        result = CND(A) + CND(b) - 1 + CBND3(-A, -b, rho);
    }
    else if (A * b * rho > 0.0)
    {
        rho1 = (rho * A - b) * Sign(A) / sqrt(A * A - 2 * rho * A * b + b * b);
        rho2 = (rho * b - A) * Sign(b) / sqrt(A * A - 2 * rho * A * b + b * b);
        delta = (1 - Sign(A) * Sign(b)) / 4.0;
        result = CBND3(A, 0.0, rho1) + CBND3(b, 0.0, rho2) - delta;
    }


    return result;
}

double NDApprox::CBND4(double A, double b, double rho)
{
    //'modified/corrected from the second function in Drez & Wes paper pg. 105
    //'0/0 case resolved by l'H rule

    int i;

    double X[5] =  {0.04691008, 0.23076534, 0.5, 0.76923466, 0.95308992 };
    double W[5] =  {0.018854042, 0.038088059, 0.0452707394, 0.038088059, 0.018854042 };

    double h1, h2;
    double LH=0.0, h12, h3, h5, h6, h7, h8;
    double r1, r2, r3, rr;
    double AA, ab;

    h1 = A;
    h2 = b;
    h12 = (h1 * h1 + h2 * h2) / 2.0;

    double result = 0.0;

    if(fabs(rho) >= 0.7)
    {
        r2 = 1.0 - rho * rho;
        r3 = sqrt(r2);
        if(rho < 0.0) h2 = -h2;
        h3 = h1 * h2;
        h7 = exp(-h3 / 2.0);
        if(fabs(rho) < 1.0)
        {
            h6 = fabs(h1 - h2);
            h5 = h6 * h6 / 2.0;
            h6 = h6 / r3;
            AA = 0.5 - h3 / 8.0;
            ab = 3.0 - 2.0 * AA * h5;
            LH = 0.13298076 * h6 * ab * (1 - CND(h6)) - exp(-h5 / r2) * (ab + AA * r2) * 0.053051647;
            for(i = 0 ; i<= 4; ++i)
            {
                r1 = r3 * X[i];
                rr = r1 * r1;
                r2 = sqrt(1 - rr);
                if(h7 == 0)
                {
                    h8 = 0;
                }
                else
                {
                    h8 = exp(-h3 / (1.0 + r2)) / r2 / h7;
                }
                LH = LH - W[i] * exp(-h5 / rr) * (h8 - 1 - AA * rr);
            }
        }

        result = LH * r3 * h7 + CND(Min(h1, h2));
        if (rho < 0.0)
        {
            result = CND(h1) - result;
        }
    }
    else
    {
        h3 = h1 * h2;
        if(rho != 0)
        {
            for(i = 0 ; i<= 4; ++i)
            {
                r1 = rho * X[i];
                r2 = 1 - r1 * r1;
                LH = LH + W[i] * exp((r1 * h3 - h12) / r2) / sqrt(r2);
            }
        }
        result = CND(h1) * CND(h2) + rho * LH;
    }

    return result;

}

double NDApprox::CBNDGeneral(int TypeFlag, double X, double y, double rho)
{
    switch (TypeFlag)
    {
        case 1:
            return CBND2(X, y, rho);
        case 2:
            return CBND3(X, y, rho);
        case 3:
            return CBND4(X, y, rho);
        case 4:
            return CBND(X, y, rho);
        default:
            return CBND(X, y, rho);
    }
}

double NDApprox::CNDEV(double p)
{
    double a[6] = {-39.69683028665376, 220.9460984245205, -275.9285104469687, 138.3577518672690,
                    -30.66479806614716, 2.506628277459239};
    double b[5] = {-54.47609879822406, 161.5858368580409, -155.6989798598866, 66.80131188771972,
                    -13.28068155288572e};
    double c[6] = {-7.784894002430293e-03, -0.3223964580411365, -2.400758277161838, -2.549732539343734,
                    4.374664141464968, 2.938163982698783};
    double d[4] = {7.784695709041462e-03, 0.3224671290700398, 2.445134137142996, 3.754408661907416};

    double LOW = 0.02425;
    double HIGH = 0.97575;

    double q = 0.0, r = 0.0;
    if (p < 0 || p > 1)
    {
        return 0.0;
    }
    else if (p == 0)
    {
        return -999999.99;
    }
    else if (p == 1)
    {
        return 999999.99;
    }
    else if (p < LOW)
    {
        /* Rational approximation for lower region */
        q = sqrt(-2.0 * log(p));
        return (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
               ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1.0);

    }
    else if (p > HIGH)
    {
        /* Rational approximation for upper region */
        q = sqrt(-2.0 * log(1.0 - p));
        return -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
               ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1.0);

    }
    else
    {
        /* Rational approximation for central region */
        q = p - 0.5;
        r = q * q;
        return (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q /
               (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1.0);


    }


}

Public Const PI = 3.14159265358979

Private Function ArcSin(x As Double) As Double
  
  If Abs(x) = 1 Then
    ArcSin = Sgn(x) * PI / 2
  Else
    ArcSin = Atn(x / Sqr(1 - x ^ 2))
  End If

End Function

Private Function Max(x As Double, y As Double) As Double

Max = Application.Max(x, y)

End Function

Private Function Min(x As Double, y As Double) As Double

Min = Application.Min(x, y)

End Function

Function PDF(x As Double) As Double

PDF = Exp(-0.5 * x * x) / Sqr(2 * PI)

End Function

Function ND(x As Double) As Double

ND = Exp(-0.5 * x * x) / Sqr(2 * PI)

End Function

Function CND(x As Double) As Double 'Marsaglia for Cumulative Normal Distribution (CND)

Dim sum, temp As Double

sum = x
temp = x

For i = 1 To 1000
    temp = (temp * x * x / (2 * i + 1))
    sum = sum + temp
Next i

CND = 0.5 + (sum / Sqr(2 * PI)) * Exp(-0.5 * (x * x))

End Function

Function CND2(x As Double) As Double 'Boole's Rule for CND
'Same as CND for 10 decimals

Dim startPoint, endPoint, delta_x, yTemp As Double
Dim n, ind As Integer
Dim y, Z

startPoint = -10#
endPoint = x
n = 240

ReDim y(1 To n + 1)
ReDim Z(1 To n + 1)

delta_x = (endPoint - startPoint) / n

For i = 1 To n + 1
    y(i) = startPoint + i * delta_x
    yTemp = y(i)
    Z(i) = NormDist.PDF(yTemp)
Next i

For i = 1 To (n - 1) / 4
    ind = 4 * i
    CND2 = CND2 + (1# / 45#) * (14 * Z(ind) + 64 * Z(ind + 1) + 24 * Z(ind + 2) + 64 * Z(ind + 3) + 14 * Z(ind + 4)) * delta_x
Next i

End Function

Function CND3(x As Double) As Double 'Abramowitz and Stegun
'Same as CND for 7 decimals

Dim Q, R As Double

Const b = 0.2316419
Const a1 = 0.31938153
Const a2 = 0.356563782
Const a3 = 1.781477937
Const a4 = 1.821255978
Const a5 = 1.330274429

Q = 1# / (1# + b * Abs(x))
R = NormDist.PDF(x)

CND3 = R * (Q * (a1 + Q * (-a2 + Q * (a3 + Q * (-a4 + Q * a5)))))

If x > 0 Then CND3 = 1 - CND3

End Function

Function CND4(x As Double) As Double  'Shore (2005)
'Same as CND for 8 decimals

Dim tempA, tempB, tempB_, g, g_ As Double

Const L = -0.61228883
Const A = -6.37309208
Const S1 = -0.11105481
Const S2 = 0.44334159

tempA = A / (L / S1)
tempB = (1 + S1 * x) ^ (L / S1) - 1
tempB_ = (1 + S1 * (-x)) ^ (L / S1) - 1

g = Exp(-Log(2#) * Exp(tempA * tempB + S2 * x))
g_ = Exp(-Log(2#) * Exp(tempA * tempB_ + S2 * (-x)))

CND4 = 0.5 * (1 + g_ - g)

End Function

Function CND5(x As Double) As Double 'Double precision univariate normal function
'Same as CND for 15 decimals (max for Excel)

Dim y, Exponential, SumA, SumB, result As Double

y = Abs(x)

If y > 37# Then
    CND5 = 0#
Else
    Exponential = Exp(-0.5 * y * y)
        If y < 7.07106781186547 Then
            SumA = 3.52624965998911E-02 * y + 0.700383064443688
            SumA = SumA * y + 6.37396220353165
            SumA = SumA * y + 33.912866078383
            SumA = SumA * y + 112.079291497871
            SumA = SumA * y + 221.213596169931
            SumA = SumA * y + 220.206867912376
            SumB = 8.83883476483184E-02 * y + 1.75566716318264
            SumB = SumB * y + 16.064177579207
            SumB = SumB * y + 86.7807322029461
            SumB = SumB * y + 296.564248779674
            SumB = SumB * y + 637.333633378831
            SumB = SumB * y + 793.826512519948
            SumB = SumB * y + 440.413735824752
            CND5 = Exponential * SumA / SumB
        Else
            SumA = y + 0.65
            SumA = y + 4# / SumA
            SumA = y + 3# / SumA
            SumA = y + 2# / SumA
            SumA = y + 1# / SumA
            CND5 = Exponential / (SumA * 2.506628274631)
        End If
        If x > 0 Then result = 1# - CND5
End If
End Function

Function BND(x, y, rho As Double) As Double 'Bivariate Normal Distribution

BND = (-1# / (2# * (1# - rho * rho)) * (x * x + y * y - 2# * x * y * rho))

End Function

Public Function CBND(x As Double, y As Double, rho As Double) As Double
'
'     A function for computing bivariate normal probabilities.
'
'       Alan Genz
'       Department of Mathematics
'       Washington State University
'       Pullman, WA 99164-3113
'       Email : alangenz@wsu.edu
'
'    This function is based on the method described by
'        Drezner, Z and G.O. Wesolowsky, (1990),
'        On the computation of the bivariate normal integral,
'        Journal of Statist. Comput. Simul. 35, pp. 101-107,
'    with major modifications for double precision, and for |R| close to 1.
'   This code was originally transelated into VBA by Graeme West

Dim i As Integer, ISs As Integer, LG As Integer, NG As Integer
Dim XX(10, 3) As Double, W(10, 3) As Double
Dim h As Double, k As Double, hk As Double, hs As Double, BVN As Double, Ass As Double, asr As Double, sn As Double
Dim A As Double, b As Double, bs As Double, c As Double, d As Double
Dim xs As Double, rs As Double

W(1, 1) = 0.17132449237917
XX(1, 1) = -0.932469514203152
W(2, 1) = 0.360761573048138
XX(2, 1) = -0.661209386466265
W(3, 1) = 0.46791393457269
XX(3, 1) = -0.238619186083197

W(1, 2) = 4.71753363865118E-02
XX(1, 2) = -0.981560634246719
W(2, 2) = 0.106939325995318
XX(2, 2) = -0.904117256370475
W(3, 2) = 0.160078328543346
XX(3, 2) = -0.769902674194305
W(4, 2) = 0.203167426723066
XX(4, 2) = -0.587317954286617
W(5, 2) = 0.233492536538355
XX(5, 2) = -0.36783149899818
W(6, 2) = 0.249147045813403
XX(6, 2) = -0.125233408511469

W(1, 3) = 1.76140071391521E-02
XX(1, 3) = -0.993128599185095
W(2, 3) = 4.06014298003869E-02
XX(2, 3) = -0.963971927277914
W(3, 3) = 6.26720483341091E-02
XX(3, 3) = -0.912234428251326
W(4, 3) = 8.32767415767048E-02
XX(4, 3) = -0.839116971822219
W(5, 3) = 0.10193011981724
XX(5, 3) = -0.746331906460151
W(6, 3) = 0.118194531961518
XX(6, 3) = -0.636053680726515
W(7, 3) = 0.131688638449177
XX(7, 3) = -0.510867001950827
W(8, 3) = 0.142096109318382
XX(8, 3) = -0.37370608871542
W(9, 3) = 0.149172986472604
XX(9, 3) = -0.227785851141645
W(10, 3) = 0.152753387130726
XX(10, 3) = -7.65265211334973E-02
      
If Abs(rho) < 0.3 Then
  NG = 1
  LG = 3
ElseIf Abs(rho) < 0.75 Then
  NG = 2
  LG = 6
Else
  NG = 3
  LG = 10
End If
      
h = -x
k = -y
hk = h * k
BVN = 0
      
If Abs(rho) < 0.925 Then
  If Abs(rho) > 0 Then
    hs = (h * h + k * k) / 2
    asr = ArcSin(rho)
    For i = 1 To LG
      For ISs = -1 To 1 Step 2
        sn = Sin(asr * (ISs * XX(i, NG) + 1) / 2)
        BVN = BVN + W(i, NG) * Exp((sn * hk - hs) / (1 - sn * sn))
      Next ISs
    Next i
    BVN = BVN * asr / (4 * PI)
  End If
  BVN = BVN + NormDist.CND(-h) * NormDist.CND(-k)
Else
  If rho < 0 Then
    k = -k
    hk = -hk
  End If
  If Abs(rho) < 1 Then
    Ass = (1 - rho) * (1 + rho)
    A = Sqr(Ass)
    bs = (h - k) ^ 2
    c = (4 - hk) / 8
    d = (12 - hk) / 16
    asr = -(bs / Ass + hk) / 2
    If asr > -100 Then BVN = A * Exp(asr) * (1 - c * (bs - Ass) * (1 - d * bs / 5) / 3 + c * d * Ass * Ass / 5)
    If -hk < 100 Then
      b = Sqr(bs)
      BVN = BVN - Exp(-hk / 2) * Sqr(2 * PI) * NormDist.CND(-b / A) * b * (1 - c * bs * (1 - d * bs / 5) / 3)
    End If
    A = A / 2
    For i = 1 To LG
      For ISs = -1 To 1 Step 2
        xs = (A * (ISs * XX(i, NG) + 1)) ^ 2
        rs = Sqr(1 - xs)
        asr = -(bs / xs + hk) / 2
        If asr > -100 Then
           BVN = BVN + A * W(i, NG) * Exp(asr) * (Exp(-hk * (1 - rs) / (2 * (1 + rs))) / rs - (1 + c * xs * (1 + d * xs)))
        End If
      Next ISs
    Next i
    BVN = -BVN / (2 * PI)
  End If
  If rho > 0 Then
    BVN = BVN + NormDist.CND(-Max(h, k))
  Else
    BVN = -BVN
    If k > h Then BVN = BVN + NormDist.CND(k) - NormDist.CND(h)
  End If
End If
CBND = BVN

End Function

Function CBND2(x As Double, y As Double, rho As Double) As Double 'Cumulative Bivariate Normal Distribution

Dim g As Double, P As Double, sum As Double
Dim A As Variant, b As Variant

A = Array(0.018854042, 0.038088059, 0.0452707394, 0.038088059, 0.018854042)
b = Array(0.04691008, 0.23076534, 0.5, 0.76923466, 0.95308992)

sum = 0#

For i = 0 To 4
    P = b(i) * rho
    g = 1 - P * P
    sum = sum + A(i) * Exp(0.5 * (2# * x * y * P - x * x - y * y) / g) / Sqr(g)
Next i

CBND2 = rho * sum + NormDist.CND(x) * NormDist.CND(y)



End Function

Public Function CBND3(A As Double, b As Double, rho As Double) As Double ' Based on Drezner-1978

    Dim x As Variant, y As Variant
    Dim rho1 As Double, rho2 As Double, delta As Double
    Dim a1 As Double, b1 As Double, sum As Double
    Dim i As Integer, j As Integer
    
    x = Array(0.24840615, 0.39233107, 0.21141819, 0.03324666, 0.00082485334)
    y = Array(0.10024215, 0.48281397, 1.0609498, 1.7797294, 2.6697604)
    a1 = A / Sqr(2 * (1 - rho ^ 2))
    b1 = b / Sqr(2 * (1 - rho ^ 2))
    
    If A <= 0 And b <= 0 And rho <= 0 Then
        sum = 0
        For i = 0 To 4
            For j = 0 To 4
                sum = sum + x(i) * x(j) * Exp(a1 * (2 * y(i) - a1) _
                + b1 * (2 * y(j) - b1) + 2 * rho * (y(i) - a1) * (y(j) - b1))
            Next
        Next
        CBND3 = Sqr(1 - rho ^ 2) / PI * sum
    ElseIf A <= 0 And b >= 0 And rho >= 0 Then
        CBND3 = CND(A) - CBND3(A, -b, -rho)
    ElseIf A >= 0 And b <= 0 And rho >= 0 Then
        CBND3 = CND(b) - CBND3(-A, b, -rho)
    ElseIf A >= 0 And b >= 0 And rho <= 0 Then
        CBND3 = CND(A) + CND(b) - 1 + CBND3(-A, -b, rho)
    ElseIf A * b * rho > 0 Then
        rho1 = (rho * A - b) * Sgn(A) / Sqr(A ^ 2 - 2 * rho * A * b + b ^ 2)
        rho2 = (rho * b - A) * Sgn(b) / Sqr(A ^ 2 - 2 * rho * A * b + b ^ 2)
        delta = (1 - Sgn(A) * Sgn(b)) / 4
        CBND3 = CBND3(A, 0, rho1) + CBND3(b, 0, rho2) - delta
    End If
End Function

Public Function CBND4(A As Double, b As Double, rho As Double) As Double
'modified/corrected from the second function in Drez & Wes paper pg. 105
'0/0 case resolved by l'H rule

  Dim i As Integer
  Dim x As Variant, W As Variant
  Dim h1 As Double, h2 As Double
  Dim LH As Double, h12 As Double, h3 As Double, h5 As Double, h6 As Double, h7 As Double, h8 As Double
  Dim r1 As Double, r2 As Double, r3 As Double, rr As Double
  Dim AA As Double, ab As Double
  
  x = Array(0.04691008, 0.23076534, 0.5, 0.76923466, 0.95308992)
  W = Array(0.018854042, 0.038088059, 0.0452707394, 0.038088059, 0.018854042)
  
  h1 = A
  h2 = b
  h12 = (h1 * h1 + h2 * h2) / 2
  
  If Abs(rho) >= 0.7 Then
    r2 = 1 - rho * rho
    r3 = Sqr(r2)
    If rho < 0 Then h2 = -h2
    h3 = h1 * h2
    h7 = Exp(-h3 / 2)
    If Abs(rho) < 1 Then
      h6 = Abs(h1 - h2)
      h5 = h6 * h6 / 2
      h6 = h6 / r3
      AA = 0.5 - h3 / 8
      ab = 3 - 2 * AA * h5
      LH = 0.13298076 * h6 * ab * (1 - CND(h6)) - Exp(-h5 / r2) * (ab + AA * r2) * 0.053051647
      For i = 0 To 4
        r1 = r3 * x(i)
        rr = r1 * r1
        r2 = Sqr(1 - rr)
        If h7 = 0 Then
          h8 = 0
        Else
          h8 = Exp(-h3 / (1 + r2)) / r2 / h7
        End If
        LH = LH - W(i) * Exp(-h5 / rr) * (h8 - 1 - AA * rr)
      Next i
    End If
    CBND4 = LH * r3 * h7 + CND(Min(h1, h2))
    If rho < 0 Then
      CBND4 = CND(h1) - CBND4
    End If
  Else
    h3 = h1 * h2
    If rho <> 0 Then
      For i = 0 To 4
        r1 = rho * x(i)
        r2 = 1 - r1 * r1
        LH = LH + W(i) * Exp((r1 * h3 - h12) / r2) / Sqr(r2)
      Next i
    End If
    CBND4 = CND(h1) * CND(h2) + rho * LH
  End If
     
End Function

Public Function CBNDGeneral(x, y, rho As Double) As Double 'Cumulative Bivariate Normal Distribution

End Function



Sub test()
Dim x As Double
Dim y As Double
Dim rho As Double

x = (Log(30 / 35) + (0.05 - 0# + 0.5 * 0.3 * 0.3) * 1) / (0.3 * Sqr(1))
y = 1
rho = 0.3

'Debug.Print x

Debug.Print CBND(x, y, rho)
Debug.Print CBND2(x, y, rho)
Debug.Print CBND3(x, y, rho)
Debug.Print CBND4(x, y, rho)

End Sub

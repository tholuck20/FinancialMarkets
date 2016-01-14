Attribute VB_Name = "NormDist"
Public Const PI = 3.14159265358979

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



Sub test()
Dim x As Double

x = (Log(30 / 35) + (0.05 - 0# + 0.5 * 0.3 * 0.3) * 1) / (0.3 * Sqr(1))

Debug.Print CND(x)
Debug.Print CND2(x)
Debug.Print CND3(x)
Debug.Print CND4(x)
Debug.Print CND5(x)

End Sub

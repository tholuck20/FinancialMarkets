Sub ClearIm()
Debug.Print Now
Application.SendKeys "^g ^a {BS}"
End Sub

Function BlackScholes(S, K, T, r, v, q As Double, optType As String, wToCalc As String) As Double
'Utilisable dans Excel
'Gère quoi calculer (Prix, Greeks, ImplyVol -> wToCalc)

'Types of underlyings: equity, FX, Futures (si future alors q = r)

'q = rf for FX options

End Function

'Black Scholes Price with dividend/rf rate
Function BSPrice(ByVal S, K, T, r, v, q, ByVal optType As String) As Double
Dim d1 As Double, d2 As Double

d1 = (Log(S / K) + (r - q + 0.5 * v * v) * T) / (v * Sqrt(T))
d2 = d1 - v * Sqrt(T)

If (q = "" Or q = Null) Then q = 0#

If (optType = "C") Then
    BSPrice = S * Exp(-q * T) * NormDist.CND(d1) - K * Exp(-r * T) * NormDist.CND(d2)
ElseIf (optType = "P") Then
    BSPrice = -S * Exp(-q * T) * NormDist.CND(-d1) + K * Exp(-r * T) * NormDist.CND(-d2)
Else
    BSPrice = 0
End If

End Function

Function BSDelta(ByVal S, K, T, r, v, q, ByVal optType As String) As Double
Dim d1 As Double

d1 = (Log(S / K) + (r - q + 0.5 * v * v) * T) / (v * Sqrt(T))

If (optType = "C") Then
    BSDelta = Exp(-q * T) * NormDist.CND(d1)
ElseIf (optType = "P") Then
    BSDelta = -Exp(-q * T) * NormDist.CND(-d1)
Else
    BSDelta = 0
End If

End Function

Function BSGamma(ByVal S, K, T, r, v, q) As Double
Dim d1 As Double

d1 = (Log(S / K) + (r - q + 0.5 * v * v) * T) / (v * Sqrt(T))

BSGamma = Exp(-q * T) * NormDist.PDF(d1) / (S * v * Sqrt(T))

End Function

Function BSVega(ByVal S, K, T, r, v, q) As Double
Dim d1 As Double

d1 = (Log(S / K) + (r - q + 0.5 * v * v) * T) / (v * Sqrt(T))

BSVega = 0.01 * NormDist.PDF(d1) * S * Exp(-q * T)  '0.01 because /100 to be in %

End Function

Function BSRho(ByVal S, K, T, r, v, q, ByVal optType As String) As Double
Dim d1 As Double, d2 As Double

d1 = (Log(S / K) + (r - q + 0.5 * v * v) * T) / (v * Sqrt(T))
d2 = d1 - v * Sqrt(T)

If (optType = "C") Then
    BSRho = K * T * Exp(-r * T) * NormDist.CND(d2)
ElseIf (optType = "P") Then
    BSRho = -K * T * Exp(-r * T) * NormDist.CND(-d2)
Else
    BSRho = 0
End If

End Function

Function BSTheta(ByVal S, K, T, r, v, q, ByVal optType As String) As Double
Dim d1 As Double, d2 As Double

d1 = (Log(S / K) + (r - q + 0.5 * v * v) * T) / (v * Sqrt(T))
d2 = d1 - v * Sqrt(T)

If (optType = "C") Then
    If (q = 0) Then 'Theta Call no dividend
        BSTheta = -K * Exp(-r * T) * (r * NormDist.CND(d2) + v * NormDist.PDF(d2) / 2 * Sqrt(T))
    Else 'q <> 0 -> Theta Call dividend
        BSTheta = q * S * Exp(-q * T) * NormDist.CND(d1) - K * Exp(-r * T) * (r * NormDist.CND(d2) + v * NormDist.PDF(d2) / 2 * Sqrt(T))
ElseIf (optType = "P") Then
    If (q = 0) Then 'Theta Put no dividend
        BSTheta = K * Exp(-r * T) * (r * NormDist.CND(-d2) - v * NormDist.PDF(d2) / 2 * Sqrt(T))
    Else 'Theta Put dividend
        BSTheta = -q * S * Exp(-q * T) * NormDist.CND(-d1) + K * Exp(-r * T) * (r * NormDist.CND(-d2) - v * NormDist.PDF(-d2) / 2 * Sqrt(T))
Else
    BSTheta = 0
End If

End Function

Function BSImplVol(ByVal S, K, T, r, v, q, cm, optType) As Double
Dim vi As Double, ci As Double, vegai As Double, minDiff As Double
Const epsilon = 0.00000001

vi = Sqrt(Abs(Log(S / K) + r * T) * 2 / T)
ci = BSPrice(S, K, T, r, vi, q, optType)
vegai = 100 * BSVega(S, K, T, r, vi, q)
minDiff = Abs(cm - ci)

'...

End Function



Sub Test()

S = 50#
K = 55#
T = 1
r = 0.05
v = 0.3
q = 0#
optType = "C"

Debug.Print BSPrice(S, K, T, r, v, q, optType)
Debug.Print BSDelta(S, K, T, r, v, q, optType)
Debug.Print BSGamma(S, K, T, r, v, q, optType)

End Sub



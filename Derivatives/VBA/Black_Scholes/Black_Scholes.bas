
Sub BlackScholes(S, K, T, R, v, Q As Double, optType As String)





End Sub

Function BSPRice(ByVal S, K, T, R, v, Q As Double, ByVal optType As String) As Double
Dim d1, d2 As Double

d1 = (Log(S / K) + (R - Q + 0.5 * v * v) * T) / (v * Sqrt(T))
d2 = d1 - v * Sqrt(T)

If (Q = "" Or Q = Null) Then Q = 0#

If optType = "C" Then
    BSPRice = S * Exp(-Q * T) * NormDist.CND(d1) - K * Exp(-R * T) * NormDist.CND(d2)
ElseIf optType = "P" Then
    BSPRice = -S * Exp(-Q * T) * NormDist.CND(-d1) + K * Exp(-R * T) * NormDist.CND(-d2)
End If

End Function

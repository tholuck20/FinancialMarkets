Option Explicit
Option Private Module

DefLng I-N
DefDbl A-H, O-Z

Private Function bico(n, K)
    'n things combined k at a time
    bico = Int(Exp(factln(n) - factln(K) - factln(n - K)) + 0.5)
End Function

Private Function bico_log(n, K)
    'log of n things combined k at a time
    bico_log = factln(n) - factln(K) - factln(n - K)
End Function

Private Function factln(n)
    'log of n factorial
    Static a(150)
    
    If n <= 1 Then
        factln = 0#
        Exit Function
    End If
    
    If n <= 150 Then
        If a(n) > 0# Then
            factln = a(n)
        Else
            a(n) = gammln(n + 1#)
            factln = a(n)
        End If
    Else
        factln = gammln(n + 1#)
    End If
    
End Function

Function binom_pdf(n, K, P)
    'binomial density: k successes in n trials
    Dim X, y, a
    
    'Check if inputs are reasonable
    If K > n Or K < 0 Then
        binom_pdf = 0#
        Exit Function
    End If
    
    'Usual case
    If P < 1# And P > 0# Then
        X = Log(P)
        y = Log(1# - P)
        binom_pdf = Exp(bico_log(n, K) + K * X + (n - K) * y)
        Exit Function
    End If
    
    'Bad probabilities
    If P > 1# Or P < 0# Then
        binom_pdf = 0#
        Exit Function
    End If
    
    If P = 1# Then
        If K = n Then
            binom_pdf = 1#
        Else
            binom_pdf = 0#
        End If
        Exit Function
    End If
    
    If P = 0# Then
        If K = 0 Then
            binom_pdf = 1#
        Else
            binom_pdf = 0#
        End If
        Exit Function
    End If
    
End Function

Function binom_cdf(n, K, P)
    'cumulative binomial distribution: up to k successes in n trials
    Dim sum, i
    
    sum = 0#
    For i = 0 To K
        sum = sum + binom_pdf(n, i, P)
    Next i
    
    binom_cdf = sum

End Function

Private Function gammln(xx)
    Static cof(6), iflag As Integer
    Dim X, tmp, ser, j
    
    If iflag = 0 Then
        cof(1) = 76.18009173
        cof(2) = -86.50532033
        cof(3) = 24.01409822
        cof(4) = -1.231739516
        cof(5) = 0.00120858003
        cof(6) = -0.00000536382
        iflag = 1
    End If
    
    If xx <= 0# Then
        gammln = 0#
        Exit Function
    End If
   
    X = xx - 1#
    tmp = X + 5.5
    tmp = (X + 0.5) * Log(tmp) - tmp
    ser = 1#
    For j = 1 To 6
        X = X + 1#
        ser = ser + cof(j) / X
    Next j
    gammln = tmp + Log(2.50662827465 * ser)
End Function



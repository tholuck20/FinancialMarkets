Function binary_compute(S_in, X, r, q, Divs As clsList, sig, T, cash, o_type As Long, IsCall As Boolean)
    Dim pv_Div, S
    Dim d1, d2, Sig_Root_T, pv_S, pv_X, Price
    Dim Item As clsListItem
    
    S = S_in
    pv_Div = 0#
    For Each Item In Divs.theList
        If (Item.T > 0 And Item.T <= T) Then
            pv_Div = pv_Div + Item.v * Exp(-r * Item.T)
        End If
    Next Item
    S = S - pv_Div
    
    If (T <= 0#) Then T = 0.00001
    Sig_Root_T = sig * Sqr(T)
    pv_S = S * Exp(-q * T)
    pv_X = X * Exp(-r * T)
    
    d1 = Log(pv_S / pv_X) / Sig_Root_T + 0.5 * Sig_Root_T
    d2 = d1 - Sig_Root_T
    
    If (o_type = CashOrNothing) Then
        If (IsCall) Then
            Price = cash * Exp(-r * T) * n_cdf(d2)
        Else
            Price = cash * Exp(-r * T) * n_cdf(-d2)
        End If
    Else
        If (IsCall) Then
            Price = pv_S * n_cdf(d1)
        Else
            Price = pv_S * n_cdf(-d1)
        End If
    End If
    
    binary_compute = Price

End Function

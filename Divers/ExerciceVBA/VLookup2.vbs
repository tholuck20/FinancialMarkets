Option Base 1

Public Function VLOOKUPDATE(Lookup_value As String, Table_array As Range, Col_index_num As Double, Range_lookup As Boolean, _
                            condition As String, Col_date_num As Double)

Dim valTabl() As Variant
Dim valKeep() As Variant
Dim minDate(1, 2) As Variant
Dim WS As Worksheet
    
    Set WS = Table_array.Worksheet
    
    cpt = Application.WorksheetFunction.CountIf(Table_array, Lookup_value)
    
    decalRow = Table_array.Row - 1
    decalCol = Table_array.Column - 1
    
    ReDim valTabl(cpt, 2)
    
    With Table_array
        
        Set c = .Find(Lookup_value)
        valTabl(1, 1) = c.Row
        valTabl(1, 2) = WS.Cells(c.Row, Col_date_num + decalCol)
        
        For i = 2 To cpt
            Set c = .Find(Lookup_value, after:=c)
            valTabl(i, 1) = c.Row
            valTabl(i, 2) = WS.Cells(c.Row, Col_date_num + decalCol)
            
        Next i
        
    End With
    
    
    If condition = "OLDEST" Then
    
        Call DateMin(valTabl(), minDate())
        
    ElseIf condition = "NEARESTB" Then
    
        cpt = 0
        cpt2 = 1
        
        For i = LBound(valTabl()) To UBound(valTabl())
            If valTabl(i, 2) >= Date Then cpt = cpt + 1
        Next i
        
        ReDim valKeep(cpt, 2)
        
        For i = LBound(valTabl()) To UBound(valTabl())
            
            If valTabl(i, 2) >= Date Then
            
                valKeep(cpt2, 1) = valTabl(i, 1)
                valKeep(cpt2, 2) = valTabl(i, 2)
                cpt2 = cpt2 + 1
                
            End If
            
        Next i
        
        Call DateMin(valKeep(), minDate())
        
    End If
    
    VLOOKUPDATE = Table_array(minDate(1, 1) - decalRow, Col_index_num)

End Function

Private Function DateMin(valTabl() As Variant, ByRef minDate()) As Variant()

    minDate(1, 2) = CDate("31/12/2100")
    
    For i = LBound(valTabl()) To UBound(valTabl())
        
        If valTabl(i, 2) < minDate(1, 2) Then
        
            minDate(1, 1) = valTabl(i, 1)
            minDate(1, 2) = valTabl(i, 2)
            
        End If
        
    Next i

End Function

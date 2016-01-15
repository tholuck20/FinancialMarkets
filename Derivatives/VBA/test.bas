Option Explicit

'Macro générant des caractères aleatoires, soit une lettre, soit un chiffre et enregistre le tout dans une chaine (en tout 40 caractères).
'Chaque caractère (lettre ou nombre) est séparé par un underscore "_".
'Les caractères sont ensuite extraits et sont dispatchés en deux colonne (A3, B3) suivant leur nature: Si c'est une lettre alors elle est rangée dans la colonne nom.
'Si c'est un nombre, il est rangé dans la colonne Nombre.

'/!\ Note: Les lettres et les nombres sont générées un à un et sont séparées par l'underscore "_")
'Ex de chaine : "A_10_2_B_120"

' Les chiffres et les lettres sont classées indépendament, ne sont pas triés et sont au nombre de 40 (chiffres et lettres confondus)


'Exercice:

'Debuggez la macro
'trier les prix par ordre décroissant







Sub Test46_DesChiffresEtDesLettres()

Dim i As Integer, j As Integer, k As Integer
Dim CetL As Boolean
Dim valeur As String
Dim donnee As String
Dim plage As Worksheet

Set plage = Range("A4:B100")
plage.Clear


Randomize

'/////////////////////////////////////             Generation de la chaine de caractères donnee                \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
For i = 1 To 40
    CetL = Int(Rnd() * 2)
    
    Select Case CetL

        Case Chr(Int(Rnd * 21) + 65)
    
    Case Else
    
        valeur = Int(Rnd() * 1000000)

    End Select

    donnee = donnee & valeur & "_"

Next i

i = 1

'////////////////////////////////////////            Decoupage de la chaine et rangement dans les colonnes respectives        \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
While i <= Len(donnee)

    If Mid(donnee, i, 1) = "_" Then
    
        
    
    End If
    Else
    
        If IsNumeric(Mid(donnee, i, 1)) Then
        
            valeur = Mid(donnee, i, 1)
            i = i + 1
            
                Do Until Not IsNumeric(Mid(donnee, i, 1))
                    valeur = valeur & Mid(donnee, i, 1)
                    i = i + 1
                Loop
                
            Range("A4").Offset(j, 1) = valeur
            j = j + 1
        
        Else
            Range("A4").Offset(k, 0) = Mid(donnee, i, 1)
            k = k + 1
            i = i + 1
            
        End If
        
    End If

Loop


End Sub

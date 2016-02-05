
Tester le type d'une variable:
is.character(variable)
is.numeric(variable)
is.logical(variable)

Déclarer / créer des variables typées:
as.numeric(42)
as.numeric("42")
as.character("Hello")
as.character(42)
as.logical() # 0 = False, 'Tout nombre != 0' = True ; "F" ou "FALSE" = FALSE, "T" ou "TRUE" = TRUE, 'Autre' = NA

# NA : utilisé lorsque la valeur n\'existe pas ou n\'est pas définie
# NULL : désigne une valeur nulle, indique le résultat n\'a pas de valeur (équivalent du 0 appliqué à tout type de données)

Anciennes version de R (similaires à la fonction as.numeric()):
  as.double()
  as.single()
  as.real()
  
floor(2.4) donne 2 #Renvoie à l'entier inférieur
ceiling(2.4) donne 3 #Renvoie à l'entier supérieur
round(2.4) donne 2 #Renvoie à l'entier le plus proche

scan() #Lis les données entrées par l'utilisateur (pour sortir taper sur Entrée sans mettre de valeur)
paste("abc", scan(nmax=1), "def")


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


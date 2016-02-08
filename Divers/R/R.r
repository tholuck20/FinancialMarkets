
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
  
floor(2.4) donne 2 #Renvoie à l\'entier inférieur
ceiling(2.4) donne 3 #Renvoie à l\'entier supérieur
round(2.4) donne 2 #Renvoie à l\'entier le plus proche

scan() #Lis les données entrées par l\'utilisateur (pour sortir taper sur Entrée sans mettre de valeur)
#Manipuler chaîne de caractères:
paste("abc", scan(nmax=1), "def") #Concaténation
nchar() #Compte le nombre de caractère (espace compris)
toupper() #Converti tout en MAJUSCULE
tolower() #Converti tout en minuscule
substr(x, start, strop) #Extraire une sous chaine allant du caractère n° start au caractère n° stop à partir de la chaine x

ls() #Permet de lister toutes les variables créées
ls(pattern="var") #Liste toutes les variables dont le nom contient "var"
ls(pat="var") #Abréviation de la commande ci-dessus
ls(pat="^var") #Le "^" permet de ne choisir que les variables dont le nom COMMENCE par "var"
ls.str() #Liste les variables avec des infos en plus (ex: le type)

rm(var1) #Permet de supprimer la variable "var1"
rm(lsit=ls()) #Permet de supprimer toutes les variables

help("bs", try.all.packages = TRUE) #Permet de chercher dans tous les packages
help("bs", package = "splines") #Demande de chercher spécifiquement dans un package

#Pour enregistrer ce qui s\'affiche à l\'écran dans un fichier:
sink("chemin/nomdufichier.extension") #Début: n\'affiche plus sur l\'écran mais enregistre dans le fichier
sink() #Fin : n\'enregistre plus dans le fichier mais affiche sur l\'écran
  #Argument facultatif: append
    #append=false (valeur par défault): les nouvelles données écrasent les données existantes dans le fichier
    #append=true : les nouvelles données sont ajoutées aux données existantes dans le fichier





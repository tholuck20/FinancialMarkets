
#Tester le type d\'une variable:
is.character(variable)
is.numeric(variable)
is.logical(variable)

#Déclarer / créer des variables typées:
as.numeric(42)
as.numeric("42")
as.character("Hello")
as.character(42)
as.logical() # 0 = False, 'Tout nombre != 0' = True ; "F" ou "FALSE" = FALSE, "T" ou "TRUE" = TRUE, 'Autre' = NA

# NA : utilisé lorsque la valeur n\'existe pas ou n\'est pas définie
# NULL : désigne une valeur nulle, indique le résultat n\'a pas de valeur (équivalent du 0 appliqué à tout type de données)

#Anciennes version de R (similaires à la fonction as.numeric()):
  as.double()
  as.single()
  as.real()

mod(var) #Donne le type de la variable var
length(var) #Donne le nombre d\'élément la variable var

#R sait gérer les valeurs numériques non-finies (exemple: x<-5/0, x retourne INF, exp(-x) retourne 0, x-x retourn NaN)
# NaN = Not a Number (exemple: Inf-Inf)

floor(2.4) donne 2 #Renvoie à l'entier inférieur
ceiling(2.4) donne 3 #Renvoie à l'entier supérieur
round(2.4) donne 2 #Renvoie à l'entier le plus proche

ls() #Permet de lister toutes les variables créées
ls(pattern="var") #Liste toutes les variables dont le nom contient "var"
ls(pat="var") #Abréviation de la commande ci-dessus
ls(pat="^var") #Le "^" permet de ne choisir que les variables dont le nom COMMENCE par "var"
ls.str() #Liste les variables avec des infos en plus (ex: le type)

rm(var1) #Permet de supprimer la variable "var1"
rm(list=ls()) #Permet de supprimer toutes les variables

help("bs", try.all.packages = TRUE) #Permet de chercher dans tous les packages
help("bs", package = "splines") #Demande de chercher spécifiquement dans un package

#For reading and writing files, R uses the working directory.
getwd() #Find the working directory
setwd("C:/date") #Change the working directory

scan(nmax=1) #Lis les données entrées par l'utilisateur (pour sortir taper sur Entrée sans mettre de valeur)
      #"nmax=1" permet de spécifier le nombre de ligne/variable que R doit lire

#Manipuler chaîne de caractères:
paste("abc", scan(nmax=1), "def", sep="") #Concaténation, "sep=" permet de définir par quel caractère les données sont séparées (" ", "", "-",...)
nchar() #Compte le nombre de caractère (espace compris)
toupper() #Converti tout en MAJUSCULE
tolower() #Converti tout en minuscule
substr(x, start, strop) #Extraire une sous chaine allant du caractère n° start au caractère n° stop à partir de la chaine x

#Pour enregistrer ce qui s'affiche à l'écran dans un fichier:
sink(file="chemin/nomdufichier.extension", append=true) #Début: n\'affiche plus sur l\'écran mais enregistre dans le fichier
sink() #Fin : n\'enregistre plus dans le fichier mais affiche sur l\'écran
  #Argument facultatif: append
    #append=false (valeur par défault): les nouvelles données écrasent les données existantes dans le fichier
    #append=true : les nouvelles données sont ajoutées aux données existantes dans le fichier





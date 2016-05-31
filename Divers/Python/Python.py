
#Clear Python Console
import os
clear = lambda: os.system('cls')
clear()

# Méthodes de test de l'état d'une chaîne
myVar.isupper() # que des maj
myVar.islower() # que des min
myVar.istitle() # 1ère lettre = maj

myVar.isalnum() # caractères alphanumériques
myVar.isalpha() # alphabétiques
myVar.isdigit() # numériques
myVar.isspace() # espaces

myVar.startswith(prefix[, start[, stop]]) # commence par...
myVar.endswith(suffix[, start[, stop]]) # se termine par...

# Suppriment toutes les combinaisons de chars (ou l'espace par défaut) 
# respectivement au début et en fin, au début, ou en fin d'une chaîne :
myVar.strip([chars]) #avant et après (=TRIM vba)
myVar.lstrip([chars]) #(=LTRIM)
myVar.rstrip([chars]) #(=RTRIM)



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

#Trouver

myVar.find(sub[, start[, stop]]) # renvoie l'index de la chaîne sub dans la sous-chaîne start à stop, sinon renvoie -1
myVar.rfind() # effectue le même travail en commençant par la fin 
myVar.index()
myVar.rindex() # de même, mais produisent une erreur (exception) si la chaîne n'est pas trouvée

myVar.replace(old, new[, count]) # remplace count instances (toutes par défaut) de old par new

myVar.split(seps[, maxsplit]) # découpe la chaîne en maxsplit morceaux (tous par défaut)
myVar.rsplit() # effectue la même chose en commençant par la fin
myVar.striplines() # effectue ce travail avec les caractères de fin de ligne


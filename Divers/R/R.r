
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

#Pour déclarer un vector:
vector("numeric", length=30) #"numeric" or "character" or "logical" (default value)
numeric(x) ou scan(nmax=x) #Permet de créer un vecteur de x éléments numeric
logical(10)
character(10)

c() #Permet de concaténer pour former un vecteur
names(var) #Permet de donner des noms aux éléments du vecteur

vec2 = vec[vec>x] #Permet de créer un vec2 à partir des valeurs de vec supérieures à x uniquement
#On peut aussi sélectionner les valeurs d'un vecteur à partir du nom de ses éléments

head(vec, n) #Permet de sélectionner les n premières valeurs du vecteur (par défaut n=6)
tail(vec, n) #Permet de sélectionner les n dernières valeurs du vecteur (par défaut n=6)
sort(vec, decreasing=T) #Permet de trier un vecteur (par défaut dans l'ordre croissant)
order(vec) #Fonctionne de la même manière que sort mais renvoie les indexes plutôt que les valeurs

mean(vec) #Moyenne de la distribution 
mean(vec, na.rm=T) #'na.rm=T' permet d'enlever les valeur NA
median(vec) #Médiane de la distribution

quantile(vec, probs=seq(0,1,0.1)) #10-quantiles (déciles)
summary(vec) #Affiche: Min, 1st Quantile, Median, Mean, 3rd Quantile, Max

#Pour générer des séquences régulières
1:10 #Permet de générer une série de nombre (ici de 1 à 10)
20:10 #Possible dans l'ordre décroissant aussi

rep(1,10) #Permet de répéter 10 fois le nombre 1, utilisable avec d'autres types d'éléments
seq(1,10) #<=>1:10 #Permet de générer une séquence, utilisable avec d'autres types d'éléments
seq(1,10,2) #Il est possible d'indiquer le pas (ici: 2)
gl(3, 5) #Retourne: 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3
expand.grid(Crit1=c(1,2), Crit2=c("A","B") #Permet de générer un tableau de la forme: 
  # Crit1 Crit2
  #     1     A
  #     2     B

#Pour générer des nombres aléatoires:
sample(1:10,3) #Permet de générer 3 entiers parmi (1:10)
sample(1:10,3, replace=T) #Permet comme ci-dessus avec remise
runif(3, 1.0, 10.5) #Permet de générer 3 nombres à partir d'une distribution uniforme
round(runif(50, min=40, max=60), digits=2) #Génère nombres à 2 décimales

###### fonction - loi ######
rnorm(n, mean=0, sd=1) #Gauss (normale) 
rexp(n, rate=1) #exponentielle
rgamma(n, shape, scale=1) #gamma
rpois(n, lambda) #Poisson
rweibull(n, shape, scale=1) #Weibull
rcauchy(n, location=0, scale=1) #Cauchy
rbeta(n, shape1, shape2) #beta
rt(n, df) #‘Student’ (t)
rf(n, df1, df2) #Fisher–Snedecor (F)
rchisq(n, df) #Pearson (χ2) 
rbinom(n, size, prob) #binomiale
rmultinom(n, size, prob) #multinomiale
rgeom(n, prob) #géométrique
rhyper(nn, m, n, k) #hypergéométrique
rlogis(n, location=0, scale=1) #logistique
rlnorm(n, meanlog=0, sdlog=1) #lognormale
rnbinom(n, size, prob) #binomiale négative
runif(n, min=0, max=1) #uniforme
rwilcox(nn, m, n), rsignrank(nn, n) #statistiques de Wilcoxon

#Il est possible de calculer la densité de prob, la densité de prob cumulée, et la valeur quantile pour la plupart de ces fonctions
#Par exemple pour distribution normale:
dnorm(x, mean = 0, sd = 1, log = FALSE)
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) #Pour Black&Scholes par exemple
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
rnorm(n, mean = 0, sd = 1)

#Création d'object
#Vecteur: voir plus haut
#Facteur:
factor(x, levels = sort(unique(x), na.last = TRUE), labels = levels, exclude = NA, ordered = is.ordered(x))
  #Exemple:
  factor(1:3) #return: 1 2 3    Levels: 1 2 3
  factor(1:3, levels=1:5) #return: 1 2 3    Levels: 1 2 3 4 5
  factor(1:5, exclude=4) #return: 1 2 3 NA 5    Levels: 1 2 3 5
#Matrice:
matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
#Tableau de données:
data.frame(..., row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())
  #Exemple
  > x <- 1:4; n <- 10;
  > data.frame(x, n)
    #return:  x n
    #       1 1 10
    #       2 2 10
    #       3 3 10
    #       4 4 10

#Permet d'ouvrir un fichier type tableau
read.table(file, header = FALSE, sep = "", quote = "\"’", dec = ".",
row.names, col.names, as.is = FALSE, na.strings = "NA",
colClasses = NA, nrows = -1,
skip = 0, check.names = TRUE, fill = !blank.lines.skip,
strip.white = FALSE, blank.lines.skip = TRUE,
comment.char = "#")

read.csv(file, header = TRUE, sep = ",", quote="\"", dec=".",
fill = TRUE, ...)
read.csv2(file, header = TRUE, sep = ";", quote="\"", dec=",",
fill = TRUE, ...)
read.delim(file, header = TRUE, sep = "\t", quote="\"", dec=".",
fill = TRUE, ...)
read.delim2(file, header = TRUE, sep = "\t", quote="\"", dec=",",
fill = TRUE, ...)

https://cran.r-project.org/doc/contrib/Paradis-rdebuts_fr.pdf

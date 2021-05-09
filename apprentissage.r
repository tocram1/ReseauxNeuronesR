## DÉCLARATIONS

sigmoide <- function(x) 1 / (1 + exp(-x))

sigmoide_derivee <- function(x) {
  s <- sigmoide(x)
  s * (1-s)
}

# Fonction de propagation dans le réseau neuronal.
# IN: x sont les valeurs en entrée du réseau
# IN: w1 sont les poids entre les couches 0 et 1
# IN: w2 sont les poids entre les couches 1 et 2
# OUT: objet list contenant les réponses des neurones des couches 2 (output), puis 1 (res_nv1)
propag <- function(x, w1, w2) {
  # on calcule les sommes pondérées du niveau 1
  z1 <- cbind(1, x) %*% w1
  # on les fait passer dans la fonction d'activation
  res_nv1 <- sigmoide(z1)
  # on calcule les sommes pondérées du niveau 2
  z2 <- cbind(1, res_nv1) %*% w2
  list(output = sigmoide(z2), res_nv1 = res_nv1, z1 = z1, z2 = z2)
}

# Fonction de rétropropagation par itération.
# Elle ne fonctionne pas correctement et je n'arrive pas à comprendre pourquoi...
# Si besoin d'exécuter le code, changer "retropropag_optim"  pour cette fonction à la ligne 100
#
# IN: x sont les valeurs en entrée du réseau
# IN: y sont les valeurs TRUE ou FALSE de la réponse de chaque cas (TRUE si ==1, FALSE sinon)
# IN: y_res_brut sont la valeur de réponse du réseau pour chaque cas de l'échantillon
# IN: w1 sont les poids entre les couches 0 et 1
# IN: w2 sont les poids entre les couches 1 et 2
# IN: res_nv1 est le résultat des calculs des neurones du niveau 1
# IN: le pas de modification des poids
# IN: z1 est la somme pondérée du niveau 1
# IN: z2 est la somme pondérée du niveau 2
retropropag <- function(x, y, y_res_brut, w1, w2, res_nv1, pas, z1, z2) {
  # calcul des deltas de Z
  dw2 <- (y - y_res_brut) %*% t(sigmoide_derivee(z2))
  # calcul des deltas du niveau 1
  dw1 <- sum(w1 * dw2) %*% t(sigmoide_derivee(z1))

  # mise à jour des poids
  w1 <- w1 + pas %*% dw1 %*% y_res_brut
  w2 <- w2 + pas %*% dw2 %*% res_nv1
  
  # retour des poids
  list(w1 = w1, w2 = w2)
}

# Autre fonction de rétropropagation, optimisée mais complexe.
# Je ne l'utilise pas dans le rendu car je me suis inspiré d'Internet pour celle-ci et ne saurai pas expliquer son fonctionnement en détail. Je creuserai le sujet plus tard dans mon temps libre.
# Elle utilise l'optimisation de "l'algorithme du gradient", plutôt que la recherche itérative des meilleurs poids.
# Elle utilise également la règle de dérivation en chaîne.
# Je l'ai incluse uniquement par curiosité aux premier abord, mais j'ai été contraint de l'utiliser au dernier moment car ma propre fonction s'avère être déficiente...
#
# IN: x sont les valeurs en entrée du réseau
# IN: y sont les valeurs TRUE ou FALSE de la réponse de chaque cas (TRUE si ==1, FALSE sinon)
# IN: y_res_brut sont la valeur de réponse du réseau pour chaque cas de l'échantillon
# IN: w1 sont les poids entre les couches 0 et 1
# IN: w2 sont les poids entre les couches 1 et 2
# IN: res_nv1 est le résultat des calculs des neurones du niveau 1
# IN: le pas de modification des poids
# IN: z1 n'est pas utilisé, juste présent pour simplifier le changement de fonction utilisée en cas de test
# IN: z2 n'est pas utilisé, juste présent pour simplifier le changement de fonction utilisée en cas de test
retropropag_optim <- function(x, y, y_res_brut, w1, w2, res_nv1, pas, z1, z2) {
  # calcul des deltas de Z
  dw2 <- t(cbind(1, res_nv1)) %*% (y_res_brut - y)
  # calcul des deltas du niveau 1
  dres_nv1  <- (y_res_brut - y) %*% t(w2[-1, , drop = FALSE])
  dw1 <- t(cbind(1, x)) %*% (res_nv1 * (1 - res_nv1) * dres_nv1)
  
  # mise à jour des poids
  w1 <- w1 - pas * dw1
  w2 <- w2 - pas * dw2
  
  # retour des poids
  list(w1 = w1, w2 = w2)
}

# Fonction d'apprentissage du réseau.
# IN: x sont les valeurs en entrée du réseau
# IN: y sont les valeurs TRUE ou FALSE de la réponse de chaque cas (TRUE si ==1, FALSE sinon)
# IN: neurones est le nombre de neurones de la couche cachée (niveau 1)
# IN: le pas de modification des poids
# IN: nombre d'iterations d'apprentissage du réseau 
apprend <- function(x, y, neurones = 5, pas = 1e-2, iterations = 1e4) {
  # on stocke le nombre d'entrées (biais compris)
  nb_entrees <- ncol(x) + 1
  # on définit des poids aléatoires
  w1 <- matrix(rnorm(nb_entrees * neurones), nb_entrees, neurones)
  w2 <- as.matrix(rnorm(neurones + 1))
  print('Poids initiaux :')
  print('Niveau 1')
  print(w1)
  print('Niveau 2')
  print(w2)

  for (i in 1:iterations) {
    # on lance la propagation
    prpg <- propag(x, w1, w2)
    # ...puis la rétropropagation grace aux résultats de prpg ainsi que d'autres données et variables
    # /!\ changer ici si besoin d'exécuter pour l'autre fonction écrite par moi-même
    rtprpg <- retropropag_optim(x, y,
                        y_res_brut = prpg$output,
                        w1, w2,
                        res_nv1 = prpg$res_nv1,
                        pas = pas,
                        z1 = prpg$z1,
                        z2 = prpg$z2)
    # on stocke les nouveaux poids
    w1 <- rtprpg$w1
    w2 <- rtprpg$w2
  }
  list(output = prpg$output, w1 = w1, w2 = w2)
}


## EXÉCUTION

# on lit le fichier de données et on formate correctement les réponses en éléments factor
dataPoints <- read.csv(file='C:\\Users\\tocra\\Documents\\Cours\\DM_réseaux_neuronaux\\donnees.csv', sep=';')
dataPoints$reponse <- as.factor(dataPoints$reponse)

# on stocke toutes les entrées du réseau
x <- data.matrix(dataPoints[, c('e1', 'e2')])
# on stocke les réponses, si elles sont positives (==1) ou négatives (autres résultats, 0 dans notre cas)
y <- dataPoints$reponse == 1
# on créée le réseau et on l'entraîne sur l'échantillon
nnet <- apprend(x, y, neurones = 2, iterations = 1e3)


## AFFICHAGE

# On créée une grille de points toutes les 0.25 unités sur X et Y qui seront de classes différentes
# en fonction de si leur position fait que le réseau les classerait avec les réponses 0 ou 1.
# Cela nous permettra de visualiser les zones délimitées par le réseau de neurones, de voir les "frontières".
grid <- expand.grid(e1 = seq(min(dataPoints$e1) - 1,
                             max(dataPoints$e1) + 1,
                             by = .25),
                    e2 = seq(min(dataPoints$e2) - 1,
                             max(dataPoints$e2) + 1,
                             by = .25))
prpg_grid <- propag(x = data.matrix(grid[, c('e1', 'e2')]),
                       w1 = nnet$w1,
                       w2 = nnet$w2)
grid$reponse <- factor((prpg_grid$output > .5) * 1,
                     labels = levels(as.factor(dataPoints$reponse)))

# on affiche tout sur un ggplot
library(ggplot2)
theme_set(theme_minimal())
ggplot(dataPoints) + aes(e1, e2, colour = reponse) +
  geom_point(data = grid, size = .5) +
  geom_point(size = 2) +
  labs(x = colnames(x)[1], y = colnames(x)[2])

cat(sprintf('Précision : %s %%\n', mean((nnet$output > .5) == y)*100))
print('Poids finaux :')
print('Niveau 1')
print(nnet$w1)
print('Niveau 2')
print(nnet$w2)
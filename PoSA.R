## PoSA

# On détermine d'abord pour chaque PSU si elle est malade ou pas. Si la prévalence dans une PSU est supérieure à 0.5%, alors on dira que cette PSU est malade. Sinon, cette PSU n'est pas malade. 
psu_counts$PSU_malade <- psu_counts$Y_h > 0.005

# Ensuite, on initialise les probabilités d'inclusion initiales. On décide de prendre des probabilités d'inclusion initiales proportionnelles à la taille des PSU. Et on veut en moyenne 27 PSU. (A voir pour expliquer pourquoi et peut-être modifier ce nombre.)
psu_counts <- psu_counts %>% 
  mutate(proba_initiale = total_individus / sum(total_individus) * 27)

# On initialise aussi les probabilités mises à jour (au début, elles sont égales aux probabilités initiales)
psu_counts$proba_mise_a_jour <- psu_counts$proba_initiale

# On crée une liste vide pour stocker les numéros des PSU sélectionnées. Cette liste est de taille 225 (comme le nombre de PSU).
psu_selectionnees <- vector("list", length = psu_size * psu_size)

set.seed(456)  # Pour la reproductibilité

# On va sélectionner la PSU 1 avec sa probabilité initiale d'inclusion. Pour cela, on tire une valeur aléatoirement entre 0 et 1 et si celle-ci est plus petite que la probabilité initiale d'inclusion de la PSU 1, alors on ajoutera la PSU 1 au tableau des PSU sélectionnées. Sinon, la PSU 1 ne sera pas sélectionnée.
if (runif(n = 1, min = 0, max = 1) < psu_counts$proba_initiale[1]) {
  psu_selectionnees[[1]] <- psu_counts[1, ]
}

# Sélection adaptative des autres PSU
for (i in 2:225) { 
  
  PSU_actuelle <- chemin[i]
  PSU_précédente <- chemin[i-1]
  
  # On met à jour les probabilités des PSU voisines si la PSU précédente est malade et incluse dans l'échantillon
  if (!(is.null(psu_selectionnees[[PSU_précédente]])) & (runif(n = 1, min = 0, max = 1) < psu_counts$proba_initiale[1])) { # si la PSU précédente est sélectionnée et qu'elle est malade (prévalence > 0.5%)
    # Alors, on inclut la PSU voisine suivante avec probabilité 1
    psu_counts$proba_mise_a_jour[PSU_actuelle] <- 1
    psu_selectionnees[[PSU_actuelle]] <- psu_counts[PSU_actuelle, ] # et on la met dans la liste des PSU sélectionnées
  } else { # sinon, on sélectionne la PSU avec sa probabilité initiale d'inclusion
    if (runif(n = 1, min = 0, max = 1) < psu_counts$proba_initiale[PSU_actuelle]) {
      psu_selectionnees[[PSU_actuelle]] <- psu_counts[PSU_actuelle, ]
    }
  }
}

```
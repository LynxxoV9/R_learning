# Jeu du Serpent sur Grille 10x10 en R

Projet de Bureau d'Etude - Algorithmique et Programmation R  
Sujet : Conception et implementation d'un jeu du serpent tour par tour sur grille discrete.

---

## Presentation du Projet

Ce projet implemente le Jeu du Serpent en langage R pur, en utilisant uniquement les fonctionnalites standards (base R et package graphics). Deux serpents evoluent sur une grille carree de 10x10 cases pour collecter 12 pommes placees aleatoirement.

Le repertoire contient deux programmes distincts :
1. `programme_1_deux_joueurs.R` : Mode permettant a deux joueurs humains de s'affronter sur le meme clavier.
2. `programme_2_joueur_contre_ordinateur.R` : Mode permettant a un joueur humain d'affronter l'ordinateur, pilote par un algorithme heuristique d'evitement et de ciblage.

---

## Regles du Jeu

- Grille : Quadrillage de taille 10x10 (coordonnees x de 1 a 10, y de 1 a 10).
- Pommes : 12 pommes vertes reparties de facon aleatoire au lancement, sans chevauchement avec les positions de depart des serpents.
- Deplacement : A chaque tour, les deux serpents avancent d'une case dans l'une des quatre directions (Haut, Bas, Gauche, Droite).
- Score : Quand un serpent passe sur une case contenant une pomme, il gagne 1 point et la pomme disparait.
- Conditions d'arret :
  - Sortie de la grille par un ou deux serpents (choc contre le mur).
  - Collision entre les deux serpents (meme case ou croisement frontal).
  - Disparition de toutes les pommes de la grille.
- Vainqueur : Le joueur qui a accumule le score le plus eleve a la fin de la partie (match nul en cas d'egalite).

---

## Controles et Commandes

Pour assurer une jouabilite optimale en console sous Linux, Windows et macOS, plusieurs types de saisies sont reconnus :

| Direction | Touches AZERTY | Pave Numerique | Lettres de base | Mots anglais |
| :--- | :---: | :---: | :---: | :---: |
| Haut | Z | 8 | H | UP, U |
| Bas | S | 2 | B | DOWN |
| Gauche | Q | 4 | G | LEFT, L |
| Droite | D | 6 | D | RIGHT, R |
| Quitter | ESC | - | - | QUITTER, STOP |

Remarque : Les entrees ne sont pas sensibles a la casse (minuscules et majuscules sont traitees a l'identique).

---

## Execution des Programmes

Les programmes necessitent une session interactive R afin d'ouvrir le dispositif graphique et de recevoir les saisies clavier.

### Methode 1 : En ligne de commande (Terminal)

1. Se placer dans le repertoire du projet :
   ```bash
   cd /chemin/vers/R_learning
   ```

2. Demarrer la console R :
   ```bash
   R
   ```

3. Executer le programme voulu avec la commande source :
   - Pour le mode deux joueurs :
     ```r
     source("programme_1_deux_joueurs.R")
     ```
   - Pour le mode contre l'ordinateur :
     ```r
     source("programme_2_joueur_contre_ordinateur.R")
     ```

4. Quitter la console R a la fin :
   ```r
   q()
   ```

### Methode 2 : Avec RStudio

1. Ouvrir le fichier `.R` dans RStudio.
2. Cliquer sur le bouton "Source" ou executer avec le raccourci `Ctrl + Shift + Enter`.
3. La grille apparait dans la fenetre graphique et les commandes sont entrees dans la console.

---

## Mode Touches Directes sur Fenetre Graphique

Si vous souhaitez piloter directement le serpent avec les fleches physiques sans appuyer sur la touche Entree a chaque tour, vous pouvez activer le mode touches directes :

```r
# Contre l'ordinateur :
lancer_partie_vs_ordinateur(mode_touches = TRUE)

# A deux joueurs :
lancer_partie_deux_joueurs(mode_touches = TRUE)
```

Dans ce mode, cliquez une fois sur la fenetre graphique pour lui donner le focus, puis pressez directement les touches flechees (Haut, Bas, Gauche, Droite) ou les touches ZQSD.

---

## Fonctionnement de l'Ordinateur (Programme 2)

L'ordinateur n'utilise pas de deplacement aleatoire. Il execute a chaque tour un algorithme heuristique glouton :
1. Calcul des distances : L'ordinateur evalue la distance de Manhattan vers chaque pomme restante sur la grille afin d'identifier la plus proche.
2. Filtrage de securite : Il simule les deplacements possibles et elimine les directions qui entrainent une collision immediate avec les murs ou avec l'autre serpent.
3. Decision : Il retient parmi les choix securises la direction qui le rapproche le plus de la pomme choisie. En cas d'egalite, un tirage aleatoire departage pour eviter les trajectoires figees.

---

## Structure du Repertoire

- `programme_1_deux_joueurs.R` : Code source autonome du jeu a deux joueurs.
- `programme_2_joueur_contre_ordinateur.R` : Code source autonome du joueur contre l'ordinateur.
- `Jeu du Serpent_sujet_v2.pdf` : Enonce officiel du bureau d'etude.
- `.gitignore` : Regles d'exclusion des fichiers generes et temporaires.
- `README.md` : Documentation technique du projet.

---

## Configuration Requise

- R version 4.0.0 ou superieure.
- Aucun paquet R supplementaire a installer via `install.packages()`.

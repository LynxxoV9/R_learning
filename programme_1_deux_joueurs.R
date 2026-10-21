# ==============================================================================
# Sujet : Jeu du Serpent sur Grille 10x10
#
# FICHIER : programme_1_deux_joueurs.R
# QUESTION 1 : Programme permettant à DEUX JOUEURS HUMAINS de s'affronter.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PARAMÈTRES GLOBAUX DU JEU
# ------------------------------------------------------------------------------
# On fixe la taille de la grille carrée (10x10) et le nombre initial de pommes (12)
TAILLE_GRILLE <- 10
NB_POMMES_INITIAL <- 12

# Dictionnaire des déplacements sous forme de vecteurs de coordonnées (dx, dy)
# Axe x : colonnes de 1 (gauche) à 10 (droite)
# Axe y : lignes de 1 (bas) à 10 (haut)
VECTEURS_DIRECTION <- list(
  "H" = c(dx =  0, dy =  1),
  "B" = c(dx =  0, dy = -1),
  "G" = c(dx = -1, dy =  0),
  "D" = c(dx =  1, dy =  0)
)

# ------------------------------------------------------------------------------
# 2. FONCTION D'INITIALISATION DE LA PARTIE
# ------------------------------------------------------------------------------
# Cette fonction configure l'état initial 
initialiser_jeu <- function() {
  # Tirage aléatoire sans remise de 14 identifiants de cases parmi 1..100
  cases_uniques <- sample(1:(TAILLE_GRILLE * TAILLE_GRILLE), 
                          size = NB_POMMES_INITIAL + 2, 
                          replace = FALSE)
  
  # Conversion mathématique d'un numéro de case k (1 à 100) en coordonnées (x, y)
  convertir_case <- function(k) {
    x <- ((k - 1) %% TAILLE_GRILLE) + 1
    y <- ((k - 1) %/% TAILLE_GRILLE) + 1
    return(c(x = x, y = y))
  }
  
  # Position de départ du Joueur 1 (Rouge)
  pos_j1 <- convertir_case(cases_uniques[1])
  
  # Position de départ du Joueur 2 (Bleu)
  pos_j2 <- convertir_case(cases_uniques[2])
  
  # Coordonnées des 12 pommes stockées dans un data frame pour faciliter les filtres
  pommes_coords <- t(sapply(cases_uniques[3:(NB_POMMES_INITIAL + 2)], convertir_case))
  pommes <- data.frame(
    x = as.numeric(pommes_coords[, 1]),
    y = as.numeric(pommes_coords[, 2])
  )
  
  # Retour de la structure complète représentant l'état du jeu
  return(list(
    pos_j1 = pos_j1,
    pos_j2 = pos_j2,
    pommes = pommes,
    score_j1 = 0,
    score_j2 = 0,
    tour = 1,
    partie_terminee = FALSE,
    motif_fin = "",
    gagnant = ""
  ))
}

# ------------------------------------------------------------------------------
# 3. FONCTION DE VISUALISATION GRAPHIQUE
# ------------------------------------------------------------------------------
afficher_grille <- function(etat_jeu, nom_fichier_sauvegarde = NULL) {
  
  # Si un nom de fichier est fourni, on sauvegarde l'image (pour le rapport)
  if (!is.null(nom_fichier_sauvegarde)) {
    png(nom_fichier_sauvegarde, width = 720, height = 750, res = 110)
  }
  
  # Réglage des marges du graphique (bas, gauche, haut, droite)
  par(mar = c(4.2, 3.5, 4.5, 3.5), bg = "#FBFBFB")
  
  # Initialisation du repère graphique vide [0.5, 10.5] x [0.5, 10.5]
  # 'asp = 1' impose un ratio carré strict pour préserver la forme des cellules
  plot(NULL, xlim = c(0.5, TAILLE_GRILLE + 0.5), ylim = c(0.5, TAILLE_GRILLE + 0.5),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       bty = "n", asp = 1,
       main = paste0("Jeu du Serpent (10x10) — Tour ", etat_jeu$tour))
  
  # 1. Tracé du quadrillage de la grille (fond blanc et bordures sombres)
  for (col in 1:TAILLE_GRILLE) {
    for (lig in 1:TAILLE_GRILLE) {
      rect(xleft   = col - 0.5, ybottom = lig - 0.5,
           xright  = col + 0.5, ytop    = lig + 0.5,
           col = "#FFFFFF", border = "#2B2B2B", lwd = 1.2)
    }
  }
  
  # 2. Dessin des pommes vertes restantes
  nb_pommes <- nrow(etat_jeu$pommes)
  if (nb_pommes > 0) {
    for (i in 1:nb_pommes) {
      px <- etat_jeu$pommes$x[i]
      py <- etat_jeu$pommes$y[i]
      # Case colorée en vert pomme (conforme Figure 1)
      rect(px - 0.5, py - 0.5, px + 0.5, py + 0.5,
           col = "#A9D08E", border = "#2B2B2B", lwd = 1.2)
      # Marqueur central esthétique pour symboliser le fruit
      points(px, py, pch = 21, bg = "#2D6A4F", col = "#1B4332", cex = 1.3)
    }
  }
  
  # 3. Dessin du Joueur 1 (Serpent Rouge)
  # Vérification que le joueur est encore sur la grille
  if (etat_jeu$pos_j1["x"] >= 1 && etat_jeu$pos_j1["x"] <= TAILLE_GRILLE &&
      etat_jeu$pos_j1["y"] >= 1 && etat_jeu$pos_j1["y"] <= TAILLE_GRILLE) {
    rect(etat_jeu$pos_j1["x"] - 0.5, etat_jeu$pos_j1["y"] - 0.5,
         etat_jeu$pos_j1["x"] + 0.5, etat_jeu$pos_j1["y"] + 0.5,
         col = "#C00000", border = "#2B2B2B", lwd = 1.8)
    text(etat_jeu$pos_j1["x"], etat_jeu$pos_j1["y"], "J1", 
         col = "white", font = 2, cex = 1.1)
  }
  
  # 4. Dessin du Joueur 2 (Serpent Bleu)
  if (etat_jeu$pos_j2["x"] >= 1 && etat_jeu$pos_j2["x"] <= TAILLE_GRILLE &&
      etat_jeu$pos_j2["y"] >= 1 && etat_jeu$pos_j2["y"] <= TAILLE_GRILLE) {
    rect(etat_jeu$pos_j2["x"] - 0.5, etat_jeu$pos_j2["y"] - 0.5,
         etat_jeu$pos_j2["x"] + 0.5, etat_jeu$pos_j2["y"] + 0.5,
         col = "#0070C0", border = "#2B2B2B", lwd = 1.8)
    text(etat_jeu$pos_j2["x"], etat_jeu$pos_j2["y"], "J2", 
         col = "white", font = 2, cex = 1.1)
  }
  
  # Numérotation discrète des axes pour faciliter le repérage (1 à 10)
  axis(1, at = 1:TAILLE_GRILLE, labels = 1:TAILLE_GRILLE, tick = FALSE, line = -0.6, cex.axis = 0.8, col.axis = "#666666")
  axis(2, at = 1:TAILLE_GRILLE, labels = 1:TAILLE_GRILLE, tick = FALSE, line = -0.6, las = 1, cex.axis = 0.8, col.axis = "#666666")
  
  # 5. Bandeaux d'information textuelle (Scores et État)
  ligne_score <- paste0("J1 (Rouge) : ", etat_jeu$score_j1, " pts   |   J2 (Bleu) : ", etat_jeu$score_j2, " pts   |   Pommes : ", nb_pommes, "/12")
  mtext(ligne_score, side = 3, line = 0.6, cex = 0.95, font = 2, col = "#1F2937")
  
  # Message contextuel en bas de graphique (sur 2 lignes pour une lisibilité parfaite)
  if (etat_jeu$partie_terminee) {
    mtext(paste0("FIN : ", etat_jeu$motif_fin), 
          side = 1, line = 1.4, cex = 0.82, font = 2, col = "#B91C1C")
    mtext(paste0("RÉSULTAT : ", etat_jeu$gagnant), 
          side = 1, line = 2.4, cex = 0.85, font = 2, col = "#15803D")
  } else {
    mtext("Contrôles : ZQSD | H/B/G/D | Pavé 8/2/4/6 | UP/DOWN", 
          side = 1, line = 1.8, cex = 0.85, font = 3, col = "#4B5563")
  }
  
  # Fermeture du device si sauvegarde fichier
  if (!is.null(nom_fichier_sauvegarde)) {
    dev.off()
  }
}

# ------------------------------------------------------------------------------
# 4. FONCTIONS DE SAISIE ET DE VALIDATION D'UNE DIRECTION
# ------------------------------------------------------------------------------
# Convertit une saisie utilisateur ou un code touche en direction standard ("H", "B", "G", "D")
convertir_touche_direction <- function(saisie) {
  if (is.null(saisie) || length(saisie) == 0) return(NULL)
  
  saisie_brute <- trimws(as.character(saisie))
  saisie_propre <- toupper(saisie_brute)
  
  # Haut : Z (AZERTY), H (Haut), 8 (Pavé numérique), UP, U
  if (saisie_propre %in% c("H", "HAUT", "Z", "8", "UP", "U")) {
    return("H")
  }
  # Bas : S (AZERTY), B (Bas), 2 (Pavé numérique), DOWN
  if (saisie_propre %in% c("B", "BAS", "S", "2", "DOWN")) {
    return("B")
  }
  # Gauche : Q (AZERTY), G (Gauche), 4 (Pavé numérique), LEFT, L
  if (saisie_propre %in% c("G", "GAUCHE", "Q", "4", "LEFT", "L")) {
    return("G")
  }
  # Droite : D (AZERTY / Droite), 6 (Pavé numérique), RIGHT, R
  if (saisie_propre %in% c("D", "DROITE", "6", "RIGHT", "R")) {
    return("D")
  }
  # Quitter
  if (saisie_propre %in% c("QUITTER", "QUIT", "STOP", "ESC", "ESCAPE", "EXIT")) {
    return("QUITTER")
  }
  
  return(NULL)
}

# Capture directe d'une touche sur la fenêtre graphique de R (sans appuyer sur Entrée)
demander_direction_fenetre <- function(message_titre = "Appuyez sur une touche...") {
  res <- NULL
  tryCatch({
    setGraphicsEventHandlers(
      prompt = message_titre,
      onKeybd = function(key) {
        # Détection des flèches natives sur la fenêtre graphique
        if (key == "Up")    return("H")
        if (key == "Down")  return("B")
        if (key == "Left")  return("G")
        if (key == "Right") return("D")
        
        dir <- convertir_touche_direction(key)
        if (!is.null(dir)) {
          res <<- dir
          return(dir)
        }
        return(NULL)
      }
    )
    getGraphicsEvent(prompt = message_titre)
  }, error = function(e) {
    res <<- NULL
  })
  return(res)
}

# Interroge le joueur au clavier (console standard R avec readline)
demander_direction_joueur <- function(num_joueur, nom_couleur) {
  repeat {
    message_prompt <- paste0(" -> [Joueur ", num_joueur, " - ", nom_couleur, "] Direction (ZQSD, H/B/G/D, Pavé 8/2/4/6 ou UP/DOWN) : ")
    
    # Lecture de la saisie utilisateur
    saisie <- readline(prompt = message_prompt)
    dir <- convertir_touche_direction(saisie)
    
    if (!is.null(dir)) {
      return(dir)
    } else {
      cat("   [!] Entrée invalide. Utilisez ZQSD, H/B/G/D, le pavé 8/2/4/6, ou tapez UP/DOWN.\n")
    }
  }
}

# ------------------------------------------------------------------------------
# 5. MOTEUR DU JEU : RÉSOLUTION D'UN TOUR COMPLET
# ------------------------------------------------------------------------------
# Cette fonction applique les règles strictes du jeu
executer_un_tour <- function(etat_jeu, dir_j1, dir_j2) {
  
  # Récupération des vecteurs de déplacement (dx, dy)
  v1 <- VECTEURS_DIRECTION[[dir_j1]]
  v2 <- VECTEURS_DIRECTION[[dir_j2]]
  
  # Calcul des positions prévisionnelles
  futur_j1 <- etat_jeu$pos_j1 + v1
  futur_j2 <- etat_jeu$pos_j2 + v2
  
  # Test de sortie de grille pour J1 et J2
  j1_hors_grille <- (futur_j1["x"] < 1 || futur_j1["x"] > TAILLE_GRILLE ||
                     futur_j1["y"] < 1 || futur_j1["y"] > TAILLE_GRILLE)
                     
  j2_hors_grille <- (futur_j2["x"] < 1 || futur_j2["x"] > TAILLE_GRILLE ||
                     futur_j2["y"] < 1 || futur_j2["y"] > TAILLE_GRILLE)
  
  # Test de collision entre les deux serpents
  # Cas 1 : les deux serpents atterrissent sur la même case
  # Cas 2 : les serpents se croisent frontalement en échangeant leurs positions
  collision_frontale <- all(futur_j1 == futur_j2)
  croisement_serpents <- (all(futur_j1 == etat_jeu$pos_j2) && all(futur_j2 == etat_jeu$pos_j1))
  collision_serpents <- (collision_frontale || croisement_serpents)
  
  # Vérification des conditions d'arrêt immédiates
  if (j1_hors_grille || j2_hors_grille || collision_serpents) {
    etat_jeu$partie_terminee <- TRUE
    
    # Identification détaillée du motif pour le rapport et la console
    if (j1_hors_grille && j2_hors_grille) {
      etat_jeu$motif_fin <- "Les deux serpents sont sortis de la grille simultanément !"
    } else if (j1_hors_grille) {
      etat_jeu$motif_fin <- "Le Joueur 1 (Rouge) est sorti de la grille !"
    } else if (j2_hors_grille) {
      etat_jeu$motif_fin <- "Le Joueur 2 (Bleu) est sorti de la grille !"
    } else if (collision_serpents) {
      etat_jeu$motif_fin <- "Collision entre les deux serpents !"
    }
    
    # Règle d'or du sujet : Le gagnant est le joueur ayant mangé le plus de pommes
    if (etat_jeu$score_j1 > etat_jeu$score_j2) {
      etat_jeu$gagnant <- "Victoire du Joueur 1 (Rouge)"
    } else if (etat_jeu$score_j2 > etat_jeu$score_j1) {
      etat_jeu$gagnant <- "Victoire du Joueur 2 (Bleu)"
    } else {
      etat_jeu$gagnant <- "Match Nul (Scores égaux)"
    }
    
    return(etat_jeu)
  }
  
  # Si aucune élimination, mise à jour effective des coordonnées des serpents
  etat_jeu$pos_j1 <- futur_j1
  etat_jeu$pos_j2 <- futur_j2
  
  # Gestion des pommes mangées par Joueur 1
  pomme_mangee_j1 <- which(etat_jeu$pommes$x == futur_j1["x"] & etat_jeu$pommes$y == futur_j1["y"])
  if (length(pomme_mangee_j1) > 0) {
    etat_jeu$score_j1 <- etat_jeu$score_j1 + 1
    etat_jeu$pommes <- etat_jeu$pommes[-pomme_mangee_j1, , drop = FALSE]
    cat("   [+] Le Joueur 1 (Rouge) mange une pomme ! (Score :", etat_jeu$score_j1, "pts)\n")
  }
  
  # Gestion des pommes mangées par Joueur 2
  pomme_mangee_j2 <- which(etat_jeu$pommes$x == futur_j2["x"] & etat_jeu$pommes$y == futur_j2["y"])
  if (length(pomme_mangee_j2) > 0) {
    etat_jeu$score_j2 <- etat_jeu$score_j2 + 1
    etat_jeu$pommes <- etat_jeu$pommes[-pomme_mangee_j2, , drop = FALSE]
    cat("   [+] Le Joueur 2 (Bleu) mange une pomme ! (Score :", etat_jeu$score_j2, "pts)\n")
  }
  
  # Condition d'arrêt : plus aucune pomme disponible sur la grille
  if (nrow(etat_jeu$pommes) == 0) {
    etat_jeu$partie_terminee <- TRUE
    etat_jeu$motif_fin <- "Toutes les pommes ont été mangées !"
    
    if (etat_jeu$score_j1 > etat_jeu$score_j2) {
      etat_jeu$gagnant <- "Victoire du Joueur 1 (Rouge)"
    } else if (etat_jeu$score_j2 > etat_jeu$score_j1) {
      etat_jeu$gagnant <- "Victoire du Joueur 2 (Bleu)"
    } else {
      etat_jeu$gagnant <- "Match Nul (Scores égaux)"
    }
    return(etat_jeu)
  }
  
  # Incrément du numéro de tour
  etat_jeu$tour <- etat_jeu$tour + 1
  return(etat_jeu)
}

# ------------------------------------------------------------------------------
# 6. BOUCLE PRINCIPALE DE JEU (MODE INTERACTIF)
# ------------------------------------------------------------------------------
lancer_partie_deux_joueurs <- function(mode_touches = FALSE) {
  cat("\n=======================================================\n")
  cat("   BIENVENUE DANS LE JEU DU SERPENT (QUESTION 1 - 2 JOUEURS) \n")
  cat("=======================================================\n")
  cat("Règles du jeu :\n")
  cat(" - 12 pommes vertes sont réparties sur la grille 10x10.\n")
  cat(" - Joueur 1 (Rouge) et Joueur 2 (Bleu) se déplacent d'une case par tour.\n")
  cat(" - Manger une pomme rapporte 1 point et la pomme disparaît.\n")
  cat(" - La partie se termine si un joueur sort de la grille, s'il y a collision,\n")
  cat("   ou s'il ne reste aucune pomme.\n")
  cat(" - Contrôles acceptés : H/B/G/D, ZQSD, UP/DOWN/LEFT/RIGHT ou touches fléchées.\n")
  if (mode_touches) {
    cat(" - Mode touches directes ACTIF : appuyez directement sur les flèches sur la fenêtre.\n")
  }
  cat(" - Le gagnant est celui qui a le score le plus élevé à la fin !\n\n")
  
  # 1. Initialisation de la grille
  jeu <- initialiser_jeu()
  
  # Ouverture d'une fenêtre graphique si supporté dans l'environnement
  if (interactive()) {
    if (mode_touches && .Platform$OS.type == "windows") {
      windows(title = "Jeu du Serpent - Question 1 (2 Joueurs)", width = 7, height = 7)
    } else if (mode_touches && capabilities("X11")) {
      X11(type = "Xlib", title = "Jeu du Serpent - Question 1 (2 Joueurs)", width = 7, height = 7)
    } else {
      dev.new(title = "Jeu du Serpent - Question 1 (2 Joueurs)", width = 7, height = 7)
    }
  }
  
  # Affichage de l'état initial
  afficher_grille(jeu)
  
  # 2. Boucle de déroulement tour par tour
  while (!jeu$partie_terminee) {
    cat(paste0("\n--- TOUR ", jeu$tour, " ---\n"))
    cat("Scores actuels -> J1 (Rouge) :", jeu$score_j1, "| J2 (Bleu) :", jeu$score_j2, "\n")
    cat("Pommes restantes :", nrow(jeu$pommes), "\n")
    
    # Saisie de la direction pour Joueur 1
    if (mode_touches) {
      cat(" -> [Joueur 1 - Rouge] Cliquez sur la fenêtre et appuyez sur une flèche (ou ZQSD)...\n")
      dir1 <- demander_direction_fenetre("Joueur 1 (Rouge) : flèches ou touches (Échap pour quitter)")
      if (is.null(dir1)) dir1 <- demander_direction_joueur(1, "Rouge")
    } else {
      dir1 <- demander_direction_joueur(1, "Rouge")
    }
    if (dir1 == "QUITTER") {
      cat("\nPartie interrompue par les joueurs.\n")
      break
    }
    
    # Saisie de la direction pour Joueur 2
    if (mode_touches) {
      cat(" -> [Joueur 2 - Bleu] Cliquez sur la fenêtre et appuyez sur une flèche (ou ZQSD)...\n")
      dir2 <- demander_direction_fenetre("Joueur 2 (Bleu) : flèches ou touches (Échap pour quitter)")
      if (is.null(dir2)) dir2 <- demander_direction_joueur(2, "Bleu")
    } else {
      dir2 <- demander_direction_joueur(2, "Bleu")
    }
    if (dir2 == "QUITTER") {
      cat("\nPartie interrompue par les joueurs.\n")
      break
    }
    
    # Exécution du tour
    jeu <- executer_un_tour(jeu, dir1, dir2)
    
    # Mise à jour graphique
    afficher_grille(jeu)
  }
  
  # 3. Écran final et annonce des résultats
  cat("\n=======================================================\n")
  cat("                    FIN DE LA PARTIE                   \n")
  cat("=======================================================\n")
  cat("Raison :", jeu$motif_fin, "\n")
  cat("Score Final -> Joueur 1 (Rouge) :", jeu$score_j1, "points\n")
  cat("Score Final -> Joueur 2 (Bleu)  :", jeu$score_j2, "points\n")
  cat("-------------------------------------------------------\n")
  cat("RÉSULTAT OFFICIEL :", jeu$gagnant, "\n")
  cat("=======================================================\n\n")
  
  return(invisible(jeu))
}

# Lancement automatique uniquement si le script est exécuté directement dans un terminal interactif
if (interactive()) {
  lancer_partie_deux_joueurs()
}

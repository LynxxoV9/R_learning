# ==============================================================================
# Sujet : Jeu du Serpent sur Grille 10x10
#
# FICHIER : programme_2_joueur_contre_ordinateur.R
# QUESTION 2 : Programme permettant à UN JOUEUR d'affronter L'ORDINATEUR (IA).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PARAMÈTRES GLOBAUX DU JEU
# ------------------------------------------------------------------------------
TAILLE_GRILLE <- 10
NB_POMMES_INITIAL <- 12

# Vecteurs unitaires de déplacement sur le plan cartésien (dx, dy)
VECTEURS_DIRECTION <- list(
  "H" = c(dx =  0, dy =  1),
  "B" = c(dx =  0, dy = -1),
  "G" = c(dx = -1, dy =  0),
  "D" = c(dx =  1, dy =  0) 
)

# ------------------------------------------------------------------------------
# 2. INITIALISATION DE LA PARTIE
# ------------------------------------------------------------------------------
# Génère 14 positions sans remise pour le Joueur 1 (Rouge), l'Ordinateur (Bleu)
# et les 12 pommes vertes réparties aléatoirement sur la grille 10x10.
initialiser_jeu <- function() {
  cases_uniques <- sample(1:(TAILLE_GRILLE * TAILLE_GRILLE), 
                          size = NB_POMMES_INITIAL + 2, 
                          replace = FALSE)
  
  convertir_case <- function(k) {
    x <- ((k - 1) %% TAILLE_GRILLE) + 1
    y <- ((k - 1) %/% TAILLE_GRILLE) + 1
    return(c(x = x, y = y))
  }
  
  pos_j1 <- convertir_case(cases_uniques[1])
  pos_ia <- convertir_case(cases_uniques[2])
  
  pommes_coords <- t(sapply(cases_uniques[3:(NB_POMMES_INITIAL + 2)], convertir_case))
  pommes <- data.frame(
    x = as.numeric(pommes_coords[, 1]),
    y = as.numeric(pommes_coords[, 2])
  )
  
  return(list(
    pos_j1 = pos_j1,
    pos_ia = pos_ia,
    pommes = pommes,
    score_j1 = 0,
    score_ia = 0,
    tour = 1,
    partie_terminee = FALSE,
    motif_fin = "",
    gagnant = ""
  ))
}

# ------------------------------------------------------------------------------
# 3. ALGORITHME DE L'ORDINATEUR
# ------------------------------------------------------------------------------
# Heuristique d'optimisation gloutonne avec évitement d'obstacles :
# 1. Évalue la distance de Manhattan |x - xp| + |y - yp| vers chaque pomme restante.
# 2. Cible la pomme la plus proche géographiquement.
# 3. Filtre les 4 directions potentielles pour exclure :
#    - Les sorties de grille (murs aux limites 1 et 10)
#    - Les collisions frontales immédiates avec le serpent adverse.
# 4. Sélectionne parmi les directions valides celle qui minimise la distance
#    à la pomme ciblée. En cas d'ex æquo, un tirage aléatoire départage pour
#    rendre le comportement naturel.
choisir_direction_ia <- function(etat_jeu) {
  pos_ia <- etat_jeu$pos_ia
  pos_j1 <- etat_jeu$pos_j1
  pommes <- etat_jeu$pommes
  
  # Liste des 4 directions de base
  directions <- c("H", "B", "G", "D")
  
  # Étape 1 : Si des pommes existent, déterminer la pomme la plus proche
  if (nrow(pommes) > 0) {
    distances_manhattan <- abs(pommes$x - pos_ia["x"]) + abs(pommes$y - pos_ia["y"])
    indice_cible <- which.min(distances_manhattan)
    pomme_cible <- pommes[indice_cible, ]
  } else {
    pomme_cible <- NULL
  }
  
  # Étape 2 : Filtrer les directions sûres (éviter les murs et collisions immédiates)
  options_valides <- data.frame(
    dir = character(),
    dist_cible = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (d in directions) {
    v <- VECTEURS_DIRECTION[[d]]
    futur_x <- pos_ia["x"] + v["dx"]
    futur_y <- pos_ia["y"] + v["dy"]
    
    # Condition de sécurité 1 : rester à l'intérieur de la grille [1, 10]
    dans_grille <- (futur_x >= 1 && futur_x <= TAILLE_GRILLE &&
                    futur_y >= 1 && futur_y <= TAILLE_GRILLE)
                    
    # Condition de sécurité 2 : ne pas se heurter immédiatement au joueur
    pas_sur_joueur <- !(futur_x == pos_j1["x"] && futur_y == pos_j1["y"])
    
    if (dans_grille && pas_sur_joueur) {
      if (!is.null(pomme_cible)) {
        dist <- abs(futur_x - pomme_cible$x) + abs(futur_y - pomme_cible$y)
      } else {
        dist <- 0
      }
      options_valides <- rbind(options_valides, data.frame(dir = d, dist_cible = dist))
    }
  }
  
  # Étape 3 : Décision finale
  if (nrow(options_valides) > 0) {
    # Choisir la direction qui minimise la distance à la pomme cible
    dist_min <- min(options_valides$dist_cible)
    meilleures_options <- options_valides$dir[options_valides$dist_cible == dist_min]
    direction_choisie <- sample(meilleures_options, 1)
  } else {
    # Si toutes les options sont dangereuses (coincé contre un mur/joueur),
    # tenter de rester dans la grille coûte que coûte
    options_dans_grille <- directions[sapply(directions, function(d) {
      fx <- pos_ia["x"] + VECTEURS_DIRECTION[[d]]["dx"]
      fy <- pos_ia["y"] + VECTEURS_DIRECTION[[d]]["dy"]
      return(fx >= 1 && fx <= TAILLE_GRILLE && fy >= 1 && fy <= TAILLE_GRILLE)
    })]
    if (length(options_dans_grille) > 0) {
      direction_choisie <- sample(options_dans_grille, 1)
    } else {
      direction_choisie <- sample(directions, 1)
    }
  }
  
  return(direction_choisie)
}

# ------------------------------------------------------------------------------
# 4. FONCTION DE VISUALISATION GRAPHIQUE
# ------------------------------------------------------------------------------
# Visualisation graphique personnalisée avec distinction explicite entre :
# - Le Serpent du Joueur humain (Rouge, label "J1")
# - Le Serpent de l'Ordinateur (Bleu, label "Ordi")
afficher_grille <- function(etat_jeu, nom_fichier_sauvegarde = NULL) {
  
  if (!is.null(nom_fichier_sauvegarde)) {
    png(nom_fichier_sauvegarde, width = 720, height = 750, res = 110)
  }
  
  par(mar = c(4.2, 3.5, 4.5, 3.5), bg = "#FBFBFB")
  
  plot(NULL, xlim = c(0.5, TAILLE_GRILLE + 0.5), ylim = c(0.5, TAILLE_GRILLE + 0.5),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       bty = "n", asp = 1,
       main = paste0("Jeu du Serpent : Joueur vs Ordinateur — Tour ", etat_jeu$tour))
  
  # Quadrillage de la grille 10x10
  for (col in 1:TAILLE_GRILLE) {
    for (lig in 1:TAILLE_GRILLE) {
      rect(xleft   = col - 0.5, ybottom = lig - 0.5,
           xright  = col + 0.5, ytop    = lig + 0.5,
           col = "#FFFFFF", border = "#2B2B2B", lwd = 1.2)
    }
  }
  
  # Dessin des pommes vertes
  nb_pommes <- nrow(etat_jeu$pommes)
  if (nb_pommes > 0) {
    for (i in 1:nb_pommes) {
      px <- etat_jeu$pommes$x[i]
      py <- etat_jeu$pommes$y[i]
      rect(px - 0.5, py - 0.5, px + 0.5, py + 0.5,
           col = "#A9D08E", border = "#2B2B2B", lwd = 1.2)
      points(px, py, pch = 21, bg = "#2D6A4F", col = "#1B4332", cex = 1.3)
    }
  }
  
  # Dessin du Joueur 1 (Serpent Rouge - Humain)
  if (etat_jeu$pos_j1["x"] >= 1 && etat_jeu$pos_j1["x"] <= TAILLE_GRILLE &&
      etat_jeu$pos_j1["y"] >= 1 && etat_jeu$pos_j1["y"] <= TAILLE_GRILLE) {
    rect(etat_jeu$pos_j1["x"] - 0.5, etat_jeu$pos_j1["y"] - 0.5,
         etat_jeu$pos_j1["x"] + 0.5, etat_jeu$pos_j1["y"] + 0.5,
         col = "#C00000", border = "#2B2B2B", lwd = 1.8)
    text(etat_jeu$pos_j1["x"], etat_jeu$pos_j1["y"], "J1", 
         col = "white", font = 2, cex = 1.1)
  }
  
  # Dessin de l'Ordinateur (Serpent Bleu)
  if (etat_jeu$pos_ia["x"] >= 1 && etat_jeu$pos_ia["x"] <= TAILLE_GRILLE &&
      etat_jeu$pos_ia["y"] >= 1 && etat_jeu$pos_ia["y"] <= TAILLE_GRILLE) {
    rect(etat_jeu$pos_ia["x"] - 0.5, etat_jeu$pos_ia["y"] - 0.5,
         etat_jeu$pos_ia["x"] + 0.5, etat_jeu$pos_ia["y"] + 0.5,
         col = "#0070C0", border = "#2B2B2B", lwd = 1.8)
    text(etat_jeu$pos_ia["x"], etat_jeu$pos_ia["y"], "Ordi", 
         col = "white", font = 2, cex = 0.85)
  }
  
  # Graduations discrètes
  axis(1, at = 1:TAILLE_GRILLE, labels = 1:TAILLE_GRILLE, tick = FALSE, line = -0.6, cex.axis = 0.8, col.axis = "#666666")
  axis(2, at = 1:TAILLE_GRILLE, labels = 1:TAILLE_GRILLE, tick = FALSE, line = -0.6, las = 1, cex.axis = 0.8, col.axis = "#666666")
  
  # Bandeau supérieur des scores
  ligne_score <- paste0("Joueur 1 (Rouge) : ", etat_jeu$score_j1, " pts   |   Ordinateur (Bleu) : ", etat_jeu$score_ia, " pts   |   Pommes : ", nb_pommes, "/12")
  mtext(ligne_score, side = 3, line = 0.6, cex = 0.95, font = 2, col = "#1F2937")
  
  # Bandeau inférieur contextuel (sur 2 lignes pour netteté)
  if (etat_jeu$partie_terminee) {
    mtext(paste0("FIN : ", etat_jeu$motif_fin), 
          side = 1, line = 1.4, cex = 0.82, font = 2, col = "#B91C1C")
    mtext(paste0("RÉSULTAT : ", etat_jeu$gagnant), 
          side = 1, line = 2.4, cex = 0.85, font = 2, col = "#15803D")
  } else {
    mtext("Contrôles : ZQSD | H/B/G/D | Pavé 8/2/4/6 | UP/DOWN", 
          side = 1, line = 1.8, cex = 0.85, font = 3, col = "#4B5563")
  }
  
  if (!is.null(nom_fichier_sauvegarde)) {
    dev.off()
  }
}

# ------------------------------------------------------------------------------
# 5. SAISIE DE LA DIRECTION DU JOUEUR HUMAIN
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
demander_direction_joueur <- function() {
  repeat {
    message_prompt <- " -> [Joueur 1 - Rouge] Direction (ZQSD, H/B/G/D, Pavé 8/2/4/6 ou UP/DOWN) : "
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
# 6. MOTEUR DU JEU : EXÉCUTION D'UN TOUR COMPLET
# ------------------------------------------------------------------------------
executer_un_tour <- function(etat_jeu, dir_j1, dir_ia) {
  
  v1  <- VECTEURS_DIRECTION[[dir_j1]]
  via <- VECTEURS_DIRECTION[[dir_ia]]
  
  futur_j1 <- etat_jeu$pos_j1 + v1
  futur_ia <- etat_jeu$pos_ia + via
  
  # Détection des sorties de grille
  j1_hors_grille <- (futur_j1["x"] < 1 || futur_j1["x"] > TAILLE_GRILLE ||
                     futur_j1["y"] < 1 || futur_j1["y"] > TAILLE_GRILLE)
                     
  ia_hors_grille <- (futur_ia["x"] < 1 || futur_ia["x"] > TAILLE_GRILLE ||
                     futur_ia["y"] < 1 || futur_ia["y"] > TAILLE_GRILLE)
  
  # Détection des collisions
  collision_frontale <- all(futur_j1 == futur_ia)
  croisement_serpents <- (all(futur_j1 == etat_jeu$pos_ia) && all(futur_ia == etat_jeu$pos_j1))
  collision_serpents <- (collision_frontale || croisement_serpents)
  
  # Arrêt immédiat si sortie ou collision
  if (j1_hors_grille || ia_hors_grille || collision_serpents) {
    etat_jeu$partie_terminee <- TRUE
    
    if (j1_hors_grille && ia_hors_grille) {
      etat_jeu$motif_fin <- "Le Joueur et l'Ordinateur sont sortis de la grille ensemble !"
    } else if (j1_hors_grille) {
      etat_jeu$motif_fin <- "Le Joueur 1 (Rouge) est sorti de la grille !"
    } else if (ia_hors_grille) {
      etat_jeu$motif_fin <- "L'Ordinateur (Bleu) est sorti de la grille !"
    } else if (collision_serpents) {
      etat_jeu$motif_fin <- "Collision entre le Joueur et l'Ordinateur !"
    }
    
    # Règle de décision du gagnant selon le cumul de pommes
    if (etat_jeu$score_j1 > etat_jeu$score_ia) {
      etat_jeu$gagnant <- "Victoire du Joueur 1 (Rouge) !"
    } else if (etat_jeu$score_ia > etat_jeu$score_j1) {
      etat_jeu$gagnant <- "Victoire de l'Ordinateur (Bleu) !"
    } else {
      etat_jeu$gagnant <- "Match Nul (Scores identiques)"
    }
    
    return(etat_jeu)
  }
  
  # Mise à jour des positions
  etat_jeu$pos_j1 <- futur_j1
  etat_jeu$pos_ia <- futur_ia
  
  # Gestion des pommes pour le Joueur 1
  mange_j1 <- which(etat_jeu$pommes$x == futur_j1["x"] & etat_jeu$pommes$y == futur_j1["y"])
  if (length(mange_j1) > 0) {
    etat_jeu$score_j1 <- etat_jeu$score_j1 + 1
    etat_jeu$pommes <- etat_jeu$pommes[-mange_j1, , drop = FALSE]
    cat("   [+] Le Joueur 1 (Rouge) mange une pomme ! (Score :", etat_jeu$score_j1, "pts)\n")
  }
  
  # Gestion des pommes pour l'Ordinateur
  mange_ia <- which(etat_jeu$pommes$x == futur_ia["x"] & etat_jeu$pommes$y == futur_ia["y"])
  if (length(mange_ia) > 0) {
    etat_jeu$score_ia <- etat_jeu$score_ia + 1
    etat_jeu$pommes <- etat_jeu$pommes[-mange_ia, , drop = FALSE]
    cat("   [+] L'Ordinateur (Bleu) mange une pomme ! (Score :", etat_jeu$score_ia, "pts)\n")
  }
  
  # Condition d'épuisement des pommes
  if (nrow(etat_jeu$pommes) == 0) {
    etat_jeu$partie_terminee <- TRUE
    etat_jeu$motif_fin <- "Toutes les pommes ont été mangées !"
    
    if (etat_jeu$score_j1 > etat_jeu$score_ia) {
      etat_jeu$gagnant <- "Victoire du Joueur 1 (Rouge) !"
    } else if (etat_jeu$score_ia > etat_jeu$score_j1) {
      etat_jeu$gagnant <- "Victoire de l'Ordinateur (Bleu) !"
    } else {
      etat_jeu$gagnant <- "Match Nul (Scores identiques)"
    }
    return(etat_jeu)
  }
  
  etat_jeu$tour <- etat_jeu$tour + 1
  return(etat_jeu)
}

# ------------------------------------------------------------------------------
# 7. BOUCLE PRINCIPALE DE JEU (JOUEUR VS ORDINATEUR)
# ------------------------------------------------------------------------------
lancer_partie_vs_ordinateur <- function(mode_touches = FALSE) {
  cat("\n=======================================================\n")
  cat("   JEU DU SERPENT : JOUEUR CONTRE L'ORDINATEUR (IA)   \n")
  cat("=======================================================\n")
  cat("Règles :\n")
  cat(" - Vous contrôlez le Serpent Rouge (J1).\n")
  cat(" - L'Ordinateur contrôle automatiquement le Serpent Bleu (IA).\n")
  cat(" - 12 pommes vertes sont réparties aléatoirement.\n")
  cat(" - Contrôles acceptés : H/B/G/D, ZQSD, UP/DOWN/LEFT/RIGHT ou touches fléchées.\n")
  if (mode_touches) {
    cat(" - Mode touches directes ACTIF : appuyez directement sur les flèches sur la fenêtre.\n")
  }
  cat(" - Le premier au score à la fin de la partie l'emporte !\n\n")
  
  # Initialisation
  jeu <- initialiser_jeu()
  
  if (interactive()) {
    if (mode_touches && .Platform$OS.type == "windows") {
      windows(title = "Jeu du Serpent - Question 2 (vs Ordi)", width = 7, height = 7)
    } else if (mode_touches && capabilities("X11")) {
      X11(type = "Xlib", title = "Jeu du Serpent - Question 2 (vs Ordi)", width = 7, height = 7)
    } else {
      dev.new(title = "Jeu du Serpent - Question 2 (vs Ordi)", width = 7, height = 7)
    }
  }
  
  afficher_grille(jeu)
  
  # Déroulement des tours
  while (!jeu$partie_terminee) {
    cat(paste0("\n--- TOUR ", jeu$tour, " ---\n"))
    cat("Scores -> Joueur 1 (Rouge) :", jeu$score_j1, "| Ordinateur (Bleu) :", jeu$score_ia, "\n")
    cat("Pommes restantes :", nrow(jeu$pommes), "\n")
    
    # 1. Choix du joueur humain
    if (mode_touches) {
      cat(" -> [Joueur 1 - Rouge] Cliquez sur la fenêtre et appuyez sur une flèche (ou ZQSD)...\n")
      dir_j1 <- demander_direction_fenetre("Joueur 1 (Rouge) : flèches ou touches (Échap pour quitter)")
      if (is.null(dir_j1)) dir_j1 <- demander_direction_joueur()
    } else {
      dir_j1 <- demander_direction_joueur()
    }
    if (dir_j1 == "QUITTER") {
      cat("\nPartie interrompue par le joueur.\n")
      break
    }
    
    # 2. Calcul automatique du coup de l'ordinateur
    dir_ia <- choisir_direction_ia(jeu)
    noms_dirs <- c("H" = "HAUT", "B" = "BAS", "G" = "GAUCHE", "D" = "DROITE")
    cat(" -> [Ordinateur] Déplacement choisi :", noms_dirs[dir_ia], "\n")
    
    # 3. Exécution du tour
    jeu <- executer_un_tour(jeu, dir_j1, dir_ia)
    
    # 4. Actualisation graphique
    afficher_grille(jeu)
  }
  
  # Bilan final
  cat("\n=======================================================\n")
  cat("                    FIN DE LA PARTIE                   \n")
  cat("=======================================================\n")
  cat("Raison :", jeu$motif_fin, "\n")
  cat("Score Final -> Joueur 1 (Rouge)   :", jeu$score_j1, "points\n")
  cat("Score Final -> Ordinateur (Bleu) :", jeu$score_ia, "points\n")
  cat("-------------------------------------------------------\n")
  cat("RÉSULTAT OFFICIEL :", jeu$gagnant, "\n")
  cat("=======================================================\n\n")
  
  return(invisible(jeu))
}

# Lancement interactif automatique si exécuté directement
if (interactive()) {
  lancer_partie_vs_ordinateur()
}

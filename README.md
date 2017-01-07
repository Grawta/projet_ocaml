# projet_ocaml


Lors de ce projet nous allons créer en ocaml un jeu simple. Ici nous avons fait un jeu de "guerre" chaque joueur doit éliminer toutes les pièces adverses ou capturer les deux villes. C'est un jeu au tour par tour. Les pièces peuvent se déplacer d'une case par une case dans toutes les directions (si aucun allié n'est présent sur la case et que la case fait partie du tableau.). Manger un adversaire revient à marcher sur sa case. Capturer une ville se fait en marchant sur la case et en la quittant au prochain tour. Les villes capturées par le joueur sont symbolisées par un (-9) sur la case et (-2) pour l'IA. De base les villes sont à (-1).

Notre IA va devoir trouver les meilleurs coups possibles pour battre son adversaire.

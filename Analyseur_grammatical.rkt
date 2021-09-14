#lang racket

; Va executer tout nos programmes
(define (syntaxe ph)
  (display "Bienvenue dans l'analyseur grammatical conçu par Dianht  \n")
  (if (string? ph)
  (values (sjet ph) (verba ph) (phrase ph) (advb ph) )
  '(entrer votre phrase sous cette forme "je suis une phrase")))


;Va séparer notre phrase en plusieurs mot


(define (verba ph)
  (verbe (string-split ph)))

(define (phrase ph)
   (nomcommun (string-split ph)))

(define (sjet ph)
   (sjt (miroir(string-split ph))))

(define (advb ph)
   (adv (string-split ph)))



;Va executer les différents pour trouver les verbes, sujet, adverbe, nom commun

(define (sjt b)
 (sujet b (varbe))
  (sujetbis b (varbe)))

(define (verbe b)
 (vb b (varbe))
  (vbbis b (varbe)))

(define (nomcommun a)
  (nm a (nmcommun))
  (nmbis a (nmcommun)))

(define (adv a)
  (adverb a (adverbe))
  (adverbbis a (adverbe)))

; Programme pour trouver le verbe 

(define (vb m l)
     (cond ((equal? l '()) #f)
           ((equal? m '()) string-append "verbe : pas trouvé")
      (else (if (pair? (car l))
              (if (vb (car m) (car l))
                     (res (car m) (caar l) (cdr m))
             (vb m (cdr l)))
          (if (equal? m (car l))
             (car l)
             (vb m (cdr l) ))
         ))))

(define (vbbis a r)
  (if (equal? (vb a r) #f)
      (verbe (cdr a))
      (vb a r)))


; Programme pour trouver le nom commun

(define (nm m l)
  (cond ((equal? l '()) #f)
        ((equal? m '()) string-append "nom commun : pas trouvé")
      (else (if (pair? (car l))
              (if (nm (car m) (car l))
                     (ras (caar l) (car m))
             (nm m (cdr l)))
          (if (equal? m (car l))
             (car l)
             (nm m (cdr l) ))
         ))))

(define (nmbis a r)
  (if (equal? (nm a r) #f)
      (nomcommun (cdr a))
      (nm a r)))




; Programme pour trouver le sujet
(define (sujet m l)
  (cond ((equal? l '()) #f)
        ((equal? m '()) string-append "sujet : pas trouvé")
      (else (if (pair? (car l))
              (if (sujet (car m) (car l))
                     (riz (cdr m))
             (sujet m (cdr l)))
          (if (equal? m (car l))
             (car l)
             (sujet m (cdr l) ))
         ))))

(define (sujetbis a r)
  (if (equal? (sujet a r) #f)
      (sjt (cdr a))
      (sujet a r)))

; Programme pour trouver l'adverbe

(define (adverb m l)
  (cond ((equal? l '()) #f)
        ((equal? m '()) string-append "adverbe : pas trouvé")
      (else (if (pair? (car l))
              (if (adverb (car m) (car l))
                     (ros (car m))
             (adverb m (cdr l)))
          (if (equal? m (car l))
             (car l)
             (adverb m (cdr l) ))
         ))))

(define (adverbbis a r)
  (if (equal? (adverb a r) #f)
      (adv (cdr a))
      (adverb a r)))




 

; Affiche les résultats
(define (res a b c)
  (string-append "verbe : " a "(" b ")"  " " "/" " " "complément :" " " (string-join c))
 )

(define (ras a b)
  (string-append "nom commun :" " " b "(" a ")"))

(define (riz a)
  (if (equal? (prono (car (miroir a)) (pronom)) #t)
  (string-append "sujet :" (car a) "(Pronom)")
  (string-append "sujet :" (string-join (miroir a)))))

(define (ros a)
 (string-append "adverbe :" " " a))



;Programmes utiles
(define (prono m l)
    (if (pair? l)
         (if (equal? m (car l))
             #t
             (prono m (cdr l)))
         #f))

(define (estvide l)
  (if (pair? l)
      #f
      #t))
(define (concat l1 l2)
  (if (estvide l1) l2
    (cons (car l1) (concat (cdr l1) l2))))

(define (miroir l)
  (if (estvide l)'()
  (concat (miroir (cdr l)) (cons (car l) '()))))

; Dictionnaire d'adverbe

(define (adverbe) '(("admirablement" "ainsi" "aussi" "bien" "comme" "comment" "debout" "doucement" "également" "ensemble" "exprès" "franco"
 "gratis" "impromptu" "incognito" "lentement" "mal" "mieux" "pis" "plutôt" "presque" "recta" "vite" "volontiers" "ainsi"
 "absolument" "assez" "aussi" "autant" "autrement" "approximativement" "beaucoup" "carrément" 
"combien" "comme" "complètement" "davantage" "diablement" "divinement" "drôlement" "encore" "entièrement" "environ" "extrêmement" 
"fort" "grandement" "guère" "infiniment" "insuffisamment" "joliment" "même" "moins" "passablement" "peu" "plus" "plutôt" "presque" 
"prou" "quasi" "quasiment" "quelque" "rudement" "si" "suffisamment" "tant" "tellement" "terriblement" "totalement" "tout"
"très" "trop" "alors" "après" "aujourd'hui" "auparavant" "aussitôt" "autrefois" "avant" )(
"avant-hier" "bientôt" "cependant" "d'abord" "déjà" "demain" "depuis" "derechef" "désormais" "dorénavant" "encore" "enfin" "ensuite" "entre-temps" 
"hier" "jadis" "jamais" "longtemps" "lors" "maintenant" "naguère" "parfois" "plus" "premièrement" "puis" "quand ?" "quelquefois" "sitôt" "soudain" "souvent"
 "subito" "tantôt" "tard" "tôt" "toujours" "ailleurs" "alentour" "arrière" "autour" "avant" "ça" "céans" "ci" 
"contre" "deçà" "dedans" "dehors" "derrière" "dessous" "dessus" "devant" "ici" "là" "là-haut" "loin" "où" "outre""partout" "près" "proche" "sus" "y" "apparemment" 
"assurément" "aussi" "bien" "bon" "certainement" "certes" "en vérité" "oui" "peut-être" "précisément" "probablement" "sans doute" "si" "soit" "toutefois" 
"volontiers" "vraiment" "vraisemblablement" "aucunement" "guère" "jamais" "ne" "non" "nullement" " pas" "plus")))


; Dictionnaire de pronom

(define (pronom) '("je" "Je" "tu" "Tu" "il" "Il" "elle" "Elle" "on" "On" "nous" "Nous" "Vous" "vous" "Ils" "ils" "elles" "Elles"))

; Dictionnaire de nom commun
(define (nmcommun) '
(("commun" "aide" "chef" "enfant" "garde" "gauche" "geste" "gosse" "livre" "merci" "mort" "ombre" "part" "poche" "professeur" "tour")	
("Nom commun féminin" "fois" "madame" "paix" "voix" "affaire" "année" "arme" "armée" "attention" "balle" "boîte" "bouche" "carte" "cause" "chambre"
"chance" "chose" "classe" "confiance" "couleur" "cour" "cuisine" "dame" "dent" "droite" "école" "église" "envie" "épaule" "époque"
"équipe" "erreur" "espèce" "face" "façon" "faim" "famille" "faute" "femme" "fenêtre"	"fête" "fille" "fleur" "force" "forme" "guerre"
"gueule" "habitude" "heure" "histoire" "idée" "image" "impression" "jambe" "joie" "journée" "langue" "lettre" "lèvre" "ligne" "lumière"
"main" "maison" "maman" "manière" "marche" "merde" "mère" "minute" "musique" "nuit" "odeur" "oreille" "parole" "partie" "peau" "peine"
"pensée" "personne" "peur" "photo" "pièce" "pierre"	"place"	"police" "porte" "présence" "prison" "putain" "question" "raison" "réponse"
"robe" "route" "salle" "scène" "seconde" "sécurité" "semaine" "situation" "soeur" "soirée" "sorte" "suite" "table" "terre" "tête" "vérité" "ville"
"voiture")	 
("masculin" "avis" "bois" "bras" "choix" "corps" "cours" "gars" "mois" "pays" "prix" "propos" "sens"	"temps"	"travers"
"vieux" "accord" "agent" "amour" "appel" "arbre" "argent" "avenir" "avion" "bateau" "bébé" "besoin" "bonheur" "bonjour" "bord" "boulot" "bout" "bruit"
"bureau" "café" "camp" "capitaine" "chat" "chemin" "chéri" "cheval" "cheveu" "chien" "ciel" "client" "cœur" "coin" "colonel" "compte" "copain" "côté"
"coup" "courant" "début" "départ" "dieu" "docteur" "doigt" "dollar" "doute" "droit" "effet" "endroit" "ennemi" "escalier" "esprit" "état" "être" "exemple" "fait" "film"
"flic" "fond" "français" "frère" "front" "garçon" "général" "genre" "goût" "gouvernement" "grand" "groupe" "haut" "homme" "honneur" "hôtel" "instant" "intérêt" "intérieur" "jardin" "jour" "journal" "lieu" "long"
"maître" "mari"	"mariage" "matin" "médecin" "mètre" "milieu" "million" "moment"	"monde"	"monsieur" "mouvement" "moyen" "noir" "nouveau" "numéro" "oeil"	"oiseau" "oncle" "ordre" "papa" "papier" "parent" "passage"
"passé" "patron" "père" "petit" "peuple" "pied" "plaisir" "plan" "point" "pouvoir" "premier" "présent" "président" "prince" "problème" "quartier" "rapport" "regard" "reste"
"retard" "retour" "rêve""revoir" "salut" "sang" "secret" "seigneur" "sentiment" "service" "seul" "siècle" "signe" "silence" "soir" "soldat" "soleil" "sourire" "souvenir" "sujet"
"téléphone" "tout" "train" "travail" "trou" "truc" "type" "vent" "ventre" "verre" "village" "visage" "voyage")	 	
("masculin pluriel" "fils")
("pluriel" "gens")))


; Dictionnaire de verbe
(define (varbe) '(
("être" "suis" "es" "est" "sommes" "êtes" "sont"
"étais" "étais" "était" "étions" "étiez" "étaient"
"fus" "fus" "fut" "fûmes" "fûtes" "furent"
"serai" "seras" "sera" "serons" "serez" "seront")
("avoir" "ai" "as" "a" "avons" "avez" "ont"
"avais" "avais" "avait" "avions" "aviez" "avaient"
"eus" "eus""eut" "eûmes" "eûtes" "eurent"
"aurai" "auras" "aura" "aurons" "aurez" "auront")
("pouvoir" "peux" "peux" "peut" "pouvons" "pouvez" "peuvent"
"pouvais" "pouvais" "pouvait" "pouvions" "pouviez" "pouvaient"
"pus" "pus" "put" "pûmes" "pûtes" "purent"
"pourrai" "pourras" "pourra" "pourrons" "pourrez" "pourront")
("faire" "fais" "fais" "fait" "faisons" "faites" "font"
"faisais" "faisais" "faisait" "faisions" "faisiez" "faisaient"
"fis" "fis" "fit" "fîmes" " fîtes" " firent"
"ferai" " feras" " fera" " ferons" " ferez" "feront")
("envoyer" "envoie" "envoies" "envoie" "envoyons" "envoyez" "envoient"
"envoyais" "envoyais" "envoyait" "envoyions" "envoyiez" "envoyaient"
"envoyai" "envoyas" "envoya" "envoyâmes" "envoyâtes" "envoyèrent"
"enverrai" "enverras" "enverra" "enverrons" "enverrez" "enverront")
("venir" "vais" "vas" "va" "allons" "allez" "vont"
"allais" "allais" "allait" "allions" "alliez" "allaient"
"allai" "allas" "alla" "allâmes" "allâtes" "allèrent"
"irai" "iras" "ira" "irons" "irez" "iront")
("prendre" "prends" "prends" "prend" "prenons" "prenez" "prennent"
"prenais" "prenais" "prenait" "prenions" "preniez" "prenaient"
"pris" "pris" "prit" "prîmes" "prîtes" "prirent"
"prendrai" "prendras" "prendra" "prendrons" "prendrez" "prendront")
("devoir" "dois" "dois" "doit" "devons" "devez" "doivent"
"devais" "devais" "devait" "devions" "deviez" "devaient"
"dus" "dus" "dut" "dûmes" "dûtes" "durent"
"devrai" "devras" "devra" "devrons" "devrez" "devront")
("voir" "vois" "vois" "voit" "voyons" "voyez" "voient"
"voyais" "voyais" "voyait" "voyions" "voyiez" "voyaient"
"vis" "vis" "vit" "vîmes" "vîtes" "virent"
"verrai" "verras" "verra" "verrons" "verrez" "verront")
("permettre" "permets" "permets" "permet" "permettons" "permettez" "permettent"
"permettais" "permettais" "permettait" "permettions" "permettiez" "permettaient"
"permis" "permis" "permit" "permîmes" "permîtes" "permirent"
"permettrai" "permettras" "permettra" "permettrons" "permettrez" "permettront")
("vouloir" "veux" "veux" "veut" "voulons" "voulez" "veulent"
"voulais" "voulais" "voulait" "voulions" "vouliez" "voulaient"
"voulus" "voulus" "voulut" "voulûmes" "voulûtes" "voulurent"
"voudrai" "voudras" "voudra" "voudrons" "voudrez" "voudront")
("mettre" "mets" "mets" "met" "mettons" "mettez" "mettent"
"mettais" "mettais" "mettait" "mettions" "mettiez" "mettaient"
"mis" "mis" "mit" "mîmes" "mîtes" "mirent"
"mettrai" "mettras" "mettra" "mettrons" "mettrez" "mettront")
("dire" "dis" "dis" "dit" "disons" "dites" "disent"
"disais" "disais" "disait" "disions" "disiez" "disaient"
"dis" "dis" "dit" "dîmes" "dîtes" "dirent"
"dirai" "diras" "dira" "dirons" "direz" "diront")
("savoir" "sais" "sais" "sait" "savons" "savez" "savent"
"savais" "savais" "savait" "savions" "saviez" "savaient"
"sus" "sus" "sut" "sûmes" "sûtes" "surent"
"saurai" "sauras" "saura" "saurons" "saurez" "sauront")
("partir" "pars" "pars" "part" "partons" "partez" "partent"
"partais" "partais" "partait" "partions" "partiez" "partaient"
"partis" "partis" "partit" "partîmes" "partîtes" "partirent"
"partirai" "partiras" "partira" "partirons" "partirez" "partiront")
("appeler" "appelle" "appelles" "appelle" "appelons" "appelez" "appellent"
"appelais" "appelais" "appelait" "appelions" "appeliez" "appelaient"
"appelai" "appelas" "appela" "appelâmes" "appelâtes" "appelèrent"
"appellerai" "appelleras" "appellera" "appellerons" "appellerez" "appelleront")
("venir" "viens" "viens" "vient" "venons" "venez" "viennent"
"venais" "venais" "venait" "venions" "veniez" "venaient"
"vins" "vins" "vint" "vînmes" "vîntes" "vinrent"
"viendrai" "viendras" "viendra" "viendrons" "viendrez" "viendront")
("attendre" "attends" "attends" "attend" "attendons" "attendez" "attendent"
"attendais" "attendais" "attendait" "attendions" "attendiez" "attendaient"
"attendis" "attendis" "attendit" "attendîmes" "attendîtes" "attendirent"
"attendrai" "attendras" "attendra" "attendrons" "attendrez" "attendront")
("aimer" "aime" "aimes" "aime" "aimons" "aimez" "aiment"
"aimais" "aimais" "aimait" "aimions" "aimiez" "aimaient"
"aimai" "aimas" "aima" "aimâmes" "aimâtes" "aimèrent"
"aimerai" "aimeras" "aimera" "aimerons" "aimerez" "aimeront")
("joindre" "joins" "joins" "joint" "joignons" "joignez" "joignent"
"joignais" "joignais" "joignait" "joignions" "joigniez" "joignaient"
"joignis" "joignis" "joignit" "joignîmes" "joignîtes" "joignirent"
"joindrai" "joindras" "joindra" "joindrons" "joindrez" "joindront")
("valoir" "vaux" "vaut" "valons" "valez" "valent" "valus" "valu" "valais" "valait" "valions" "valiez" "valaient" "valûmes" "valûtes" "valurent" "vaudrez"
 "vaudrons"))
)

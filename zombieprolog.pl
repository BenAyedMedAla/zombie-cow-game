% === Travail réalisé par : Syrine Smati, Mohamed Ala Benayed ===

% === Déclarations dynamiques ===

:- dynamic rocher/2.
:- dynamic arbre/2.
:- dynamic vache/4.
:- dynamic dimitri/2.

% === Dimensions du terrain ===

largeur(10).
hauteur(10).

dans_grille(X, Y) :-
    largeur(W), hauteur(H),
    X >= 0, X < W,
    Y >= 0, Y < H.

% === Faits de départ ===

vache(2, 3, brune, vivante).
vache(4, 5, simmental, vivante).
vache(6, 1, alpine_herens, zombie).

nombre_rochers(7).
nombre_arbres(11).
nombre_vaches(brune, 2).
nombre_vaches(simmental, 4).
nombre_vaches(alpine_herens, 1).

% === Occupation des cases ===

occupe(X, Y) :- rocher(X, Y).
occupe(X, Y) :- arbre(X, Y).
occupe(X, Y) :- vache(X, Y, _, _).
occupe(X, Y) :- dimitri(X, Y).

libre(X, Y) :-
    largeur(W), hauteur(H),
    repeat,
    X is random(W), Y is random(H),
    \+ occupe(X, Y),
    !.

% === Placement aléatoire des éléments ===

placer_rochers(0).
placer_rochers(N) :- N > 0, libre(X, Y), assert(rocher(X, Y)), N1 is N - 1, placer_rochers(N1).

placer_arbres(0).
placer_arbres(N) :- N > 0, libre(X, Y), assert(arbre(X, Y)), N1 is N - 1, placer_arbres(N1).

placer_vaches(_, 0).
placer_vaches(Race, N) :- N > 0, libre(X, Y), assert(vache(X, Y, Race, vivante)), N1 is N - 1, placer_vaches(Race, N1).

placer_dimitri :- libre(X, Y), assert(dimitri(X, Y)).

% === Gestion des vaches ===

vaches(L) :- bagof((X,Y), vache(X, Y, _, _), L).

creer_zombie :-
    vaches(L), length(L, N), N > 0,
    random(0, N, K), nth0(K, L, (X, Y)),
    retract(vache(X, Y, Race, _)),
    assert(vache(X, Y, Race, zombie)).

% === Zombification ===

voisin(X, Y, NX, Y) :- NX is X - 1.
voisin(X, Y, NX, Y) :- NX is X + 1.
voisin(X, Y, X, NY) :- NY is Y - 1.
voisin(X, Y, X, NY) :- NY is Y + 1.

zombification(X, Y) :-
    vache(X, Y, _, zombie),
    forall(
        (voisin(X, Y, NX, NY), vache(NX, NY, R, vivante)),
        (retract(vache(NX, NY, R, vivante)), assert(vache(NX, NY, R, zombie)))
    ).

zombification_liste([]).
zombification_liste([(X,Y)|L]) :- zombification(X, Y), zombification_liste(L).

zombification :-
    vaches(L),
    zombification_liste(L).

% === Déplacement des vaches ===

direction_aleatoire(Dir) :-
    member(Dir, [nord, sud, est, ouest, reste]),
    random_member(Dir, [nord, sud, est, ouest, reste]).

deplacement_vache(X, Y, Direction) :-
    (
        Direction = nord, NX is X - 1, NY is Y;
        Direction = sud,  NX is X + 1, NY is Y;
        Direction = est,  NX is X,     NY is Y + 1;
        Direction = ouest,NX is X,     NY is Y - 1;
        Direction = reste, NX is X,    NY is Y
    ),
    dans_grille(NX, NY),
    \+ occupe(NX, NY),
    retract(vache(X, Y, Race, Etat)),
    assert(vache(NX, NY, Race, Etat)),
    format('Vache déplacée de (~d, ~d) à (~d, ~d)~n', [X, Y, NX, NY]).

deplacement_vaches :-
    findall((X,Y,R,E), vache(X,Y,R,E), Vaches),
    deplacer_vaches_liste(Vaches).

deplacer_vaches_liste([], _).
deplacer_vaches_liste([(X,Y,R,E)|T]) :-
    direction_aleatoire(Dir),
    (deplacement_vache(X,Y,Dir) -> true ; true),
    deplacer_vaches_liste(T).

% === Déplacement du joueur ===

deplacement_joueur(Direction) :-
    dimitri(X, Y),
    (
        Direction = nord, NX is X - 1, NY is Y;
        Direction = sud,  NX is X + 1, NY is Y;
        Direction = est,  NX is X,     NY is Y + 1;
        Direction = ouest,NX is X,     NY is Y - 1;
        Direction = reste, NX is X,    NY is Y
    ),
    dans_grille(NX, NY),
    \+ occupe(NX, NY),
    retract(dimitri(X, Y)),
    assert(dimitri(NX, NY)),
    format('Dimitri déplacé de (~d, ~d) à (~d, ~d)~n', [X, Y, NX, NY]).

% === Vérification de la sécurite du joueur ===

dimitri_en_securite :-
    dimitri(X, Y),
    forall(voisin(X, Y, NX, NY), \+ vache(NX, NY, _, zombie)).

% === Initialisation ===

initialisation :-
    nombre_rochers(NR), placer_rochers(NR),
    nombre_arbres(NA), placer_arbres(NA),
    nombre_vaches(brune, NVB), placer_vaches(brune, NVB),
    nombre_vaches(simmental, NVS), placer_vaches(simmental, NVS),
    nombre_vaches(alpine_herens, NVH), placer_vaches(alpine_herens, NVH),
    placer_dimitri,
    creer_zombie.

% === Affichage ===

affichage(_, H) :- hauteur(HMax), H >= HMax, nl.
affichage(L, H) :-
    largeur(W), L >= W, nl, H1 is H + 1, affichage(0, H1).
affichage(L, H) :-
    ( rocher(L, H)           -> write('O')
    ; arbre(L, H)           -> write('T')
    ; dimitri(L, H)         -> write('D')
    ; vache(L, H, brune, vivante)      -> write('B')
    ; vache(L, H, brune, zombie)       -> write('b')
    ; vache(L, H, simmental, vivante)  -> write('S')
    ; vache(L, H, simmental, zombie)   -> write('s')
    ; vache(L, H, alpine_herens, vivante) -> write('H')
    ; vache(L, H, alpine_herens, zombie)  -> write('h')
    ; write('.') ),
    L1 is L + 1,
    affichage(L1, H).

affichage :- affichage(0, 0).

% === Saisie direction ===

question(Direction) :-
    write('Direction (nord, sud, est, ouest, reste) ? '), nl,
    read(X),
    (member(X, [nord, sud, est, ouest, reste]) -> Direction = X ; question(Direction)).

% === Boucle du jeu ===

jouer :- initialisation, tour(0).

tour(N) :-
    affichage,
    question(Direction),
    deplacement_joueur(Direction),
    deplacement_vaches,
    zombification,
    (   dimitri_en_securite ->
        N1 is N + 1,
        tour(N1)
    ;
        write('Dimitri s\'est fait mordre. Fin du jeu.'), nl
    ).

:- set_prolog_flag(encoding, utf8).

% --------------------------------
% Systeme Expert pour la Gestion Logistique d un Terminal a Conteneurs
% Projet : Systemes Formels et Intelligence Artificielle
% Contexte : Port Autonome de Kribi, Cameroun
% Description : Ce programme Prolog simule la gestion logistique d un terminal a conteneurs,
% incluant la planification, le dechargement, l empilage, le traitement douanier,
% le chargement pour exportation et le transport terrestre.
% Chaque etape est modulaire, avec une base de faits, une base de regles et un moteur d inference.
% --------------------------------

% Autoriser la modification dynamique des predicats pour une gestion en temps reel
:- dynamic quai/3.          % quai(Numero, Statut, Longueur_en_metres)
:- dynamic conteneur/4.     % conteneur(ID, Navire, Destination, Type)
:- dynamic zone_stockage/2. % zone_stockage(Nom, Capacite_max)
:- dynamic emplacement/4.   % emplacement(Conteneur, Zone, Rangee, Hauteur)
:- dynamic statut_douane/2. % statut_douane(Conteneur, Statut)
:- dynamic document/3.      % document(Conteneur, Type, Valide)
:- dynamic chargement/3.    % chargement(Navire, Conteneur, Position)
:- dynamic transporteur/4.  % transporteur(ID, Type, Destination, Capacite)
:- dynamic vehicule_interne/3. % vehicule_interne(ID, Type, Disponibilite)
:- dynamic inspection/2.    % inspection(Conteneur, Statut)
:- dynamic scelle/2.        % scelle(Conteneur, Valide)
:- dynamic file_porte/2.    % file_porte(Porte, NbConteneursEnAttente)

% --------------------------------
% BASE DE FAITS (Donnees initiales du terminal)
% --------------------------------

% Quais disponibles : quai(Numero, Statut, Longueur_en_metres)
% Statut : libre ou occupe
quai(1, libre, 350).  % Quai 1, longueur 350m, libre
quai(2, occupe, 300). % Quai 2, longueur 300m, occupe
quai(3, libre, 400).  % Quai 3, longueur 400m, libre
quai(4, libre, 450).  % Quai 4, longueur 450m, libre
quai(5, libre, 550).  % Quai 5, longueur 550m, libre
quai(6, libre, 400).  % Quai 6, longueur 400m, libre
quai(7, libre, 450).  % Quai 7, longueur 450m, libre
quai(8, libre, 650).  % Quai 8, longueur 650m, libre
quai(9, libre, 400).  % Quai 9, longueur 400m, libre
quai(10, libre, 450). % Quai 10, longueur 450m, libre

% Navires en attente : navire(Nom, Taille_en_metres, Nombre_conteneurs)
navire('EverGreen', 300, 3000).
navire('Maersk', 250, 2000).
navire('CMA_CGM', 320, 3500).
navire('MSC', 280, 2500).
navire('Port_de_Kribi', 300, 2500).
navire('Port_de_Douala', 300, 2000).

% Conteneurs : conteneur(ID, Navire, Destination, Type)
% Types : standard ou reefer (refrigere)
conteneur(101, 'EverGreen', 'Douala', standard).
conteneur(102, 'EverGreen', 'Yaounde', reefer).
conteneur(103, 'Maersk', 'Garoua', standard).
conteneur(104, 'CMA_CGM', 'Limbe', standard).
conteneur(105, 'CMA_CGM', 'Bamenda', reefer).
conteneur(106, 'MSC', 'Kribi', standard).
conteneur(107, 'MSC', 'Buea', reefer).
conteneur(108, 'EverGreen', 'Douala', standard).
conteneur(109, 'EverGreen', 'Yaounde', reefer).
conteneur(110, 'Maersk', 'Garoua', standard).
conteneur(111, 'CMA_CGM', 'Limbe', standard).
conteneur(112, 'CMA_CGM', 'Bamenda', reefer).
conteneur(113, 'MSC', 'Kribi', standard).
conteneur(114, 'MSC', 'Buea', reefer).  

% Zones de stockage : zone_stockage(Nom, Capacite_max)
zone_stockage('ZoneA', 1000). % Zone A, capacite 1000 conteneurs
zone_stockage('ZoneB', 500).  % Zone B, capacite 500 conteneurs
zone_stockage('ZoneC', 1000). % Zone C, capacite 1000 conteneurs
zone_stockage('ZoneD', 500).  % Zone D, capacite 500 conteneurs
zone_stockage('ZoneE', 1000). % Zone E, capacite 1000 conteneurs
zone_stockage('ZoneF', 500).  % Zone F, capacite 500 conteneurs
zone_stockage('ZoneG', 1000). % Zone G, capacite 1000 conteneurs
zone_stockage('ZoneH', 500).  % Zone H, capacite 500 conteneurs

% Prises electriques pour conteneurs refrigeres : prise_electrique(Rangee, Zone)
prise_electrique(1, 'ZoneA').
prise_electrique(3, 'ZoneB').
prise_electrique(5, 'ZoneC').
prise_electrique(7, 'ZoneD').
prise_electrique(9, 'ZoneE').
prise_electrique(11, 'ZoneF').
prise_electrique(13, 'ZoneG').
prise_electrique(15, 'ZoneH').

% Documents des conteneurs : document(Conteneur, Type, Valide)
% Types : connaissement, declaration_douane
document(101, connaissement, oui).
document(101, declaration_douane, oui).
document(102, connaissement, non).
document(102, declaration_douane, oui).
document(103, connaissement, oui).
document(103, declaration_douane, oui).
document(104, connaissement, oui).
document(104, declaration_douane, oui).
document(105, connaissement, oui).
document(105, declaration_douane, non).
document(106, connaissement, oui).
document(106, declaration_douane, oui).
document(107, connaissement, non).
document(107, declaration_douane, oui).

% Vehicules internes : vehicule_interne(ID, Type, Disponibilite)
% Types : tracteur, agv (Automated Guided Vehicle)
vehicule_interne(v1, tracteur, disponible).
vehicule_interne(v2, agv, disponible).
vehicule_interne(v3, tracteur, occupe).
vehicule_interne(v4, agv, disponible).
vehicule_interne(v5, tracteur, disponible).
vehicule_interne(v6, agv, disponible).
vehicule_interne(v7, tracteur, occupe).
vehicule_interne(v8, agv, disponible).
vehicule_interne(v9, tracteur, disponible).
vehicule_interne(v10, agv, disponible).

% Files d attente aux portes du port : file_porte(Porte, NbConteneursEnAttente)
file_porte('Porte1', 2). % Porte 1, aucun conteneur en attente
file_porte('Porte2', 5). % Porte 2, aucun conteneur en attente

% Transporteurs : transporteur(ID, Type, Destination, Capacite)
transporteur(t1, camion, 'Douala', 1).
transporteur(t2, train, 'Yaounde', 10).
transporteur(t3, camion, 'Limbe', 1).
transporteur(t4, train, 'Kribi', 5).
transporteur(t5, camion, 'Bamenda', 1).
transporteur(t6, train, 'Buea', 5).
transporteur(t7, camion, 'Garoua', 1).
transporteur(t8, train, 'Douala', 10).
transporteur(t9, camion, 'Yaounde', 1).
transporteur(t10, train, 'Limbe', 5).

% --------------------------------
% BASE DE REGLES (Logique metier)
% --------------------------------

% --- ETAPE 1 : Planification et arrivee du navire ---
% Regle : Trouver un quai compatible pour un navire
quai_compatible(Navire, Quai) :-
    navire(Navire, Taille, _),
    quai(Quai, libre, Longueur),
    Longueur >= Taille,
    write('Quai compatible trouve : Quai '), write(Quai), nl.

% Regle : Attribuer un quai a un navire
attribuer_quai(Navire, Quai) :-
    quai_compatible(Navire, Quai),
    retract(quai(Quai, libre, Longueur)),
    assertz(quai(Quai, occupe, Longueur)),
    write('Navire '), write(Navire), write(' attribue au quai '), write(Quai), nl.

% Regle : Afficher la liste des quais
afficher_quais :-
    write('Liste des quais :'), nl,
    (   quai(Num, Statut, Longueur),
        write('- Quai '), write(Num), write(' : '), write(Statut),
        write(' (Longueur : '), write(Longueur), write('m)'), nl,
        fail
    ;   true
    ).

% --- ETAPE 2 : Dechargement des conteneurs ---
% Regle : Verifier si un navire est a quai avant dechargement
navire_a_quai(Navire, Quai) :-
    quai(Quai, occupe, _),
    navire(Navire, _, _),
    write('Navire '), write(Navire), write(' est au quai '), write(Quai), nl.

% Regle : Inspecter un conteneur avant dechargement
inspecter_conteneur(Conteneur, Statut) :-
    conteneur(Conteneur, _, _, _),
    random_between(0, 9, X),
    (   X < 1
        -> Statut = endommage,
           write('Conteneur '), write(Conteneur), write(' : endommage ou fuite detectee !'), nl
        ;  Statut = conforme,
           write('Conteneur '), write(Conteneur), write(' : inspection conforme.'), nl
    ),
    assertz(inspection(Conteneur, Statut)).

% Regle : Assigner un vehicule interne pour le transport
assigner_vehicule_interne(Conteneur, Vehicule) :-
    conteneur(Conteneur, _, _, _),
    (   vehicule_interne(Vehicule, _, disponible)
        ->  retract(vehicule_interne(Vehicule, Type, disponible)),
            assertz(vehicule_interne(Vehicule, Type, occupe)),
            write('Vehicule interne '), write(Vehicule), write(' assigne au conteneur '), write(Conteneur), nl
        ;   write('Erreur : Aucun vehicule interne disponible.'), nl, fail
    ).

% Regle : Decharger un conteneur vers une zone
decharger_conteneur(Conteneur, Zone) :-
    conteneur(Conteneur, Navire, _, _),
    (   navire_a_quai(Navire, _)
        ->  true
        ;   write('Erreur : Le navire '), write(Navire), write(' n est pas a quai.'), nl, fail
    ),
    (   inspection(Conteneur, conforme)
        ->  true
        ;   inspecter_conteneur(Conteneur, Statut),
            (   Statut = conforme
                ->  true
                ;   write('Erreur : Inspection du conteneur '), write(Conteneur), write(' non conforme.'), nl, fail
            )
    ),
    (   zone_stockage(Zone, Capacite), Capacite > 0
        ->  true
        ;   write('Erreur : Aucune zone de stockage disponible.'), nl, fail
    ),
    assigner_vehicule_interne(Conteneur, Vehicule),
    retract(zone_stockage(Zone, Capacite)),
    NouvelleCapacite is Capacite - 1,
    assertz(zone_stockage(Zone, NouvelleCapacite)),
    write('Conteneur '), write(Conteneur), write(' decharge vers '), write(Zone),
    write(' avec vehicule '), write(Vehicule), nl.

% Regle : Afficher l etat des zones
afficher_zones :-
    write('Zones de stockage :'), nl,
    (   zone_stockage(Nom, Capacite),
        write('- '), write(Nom), write(' : '), write(Capacite), write(' places restantes'), nl,
        fail
    ;   true
    ).

% --- ETAPE 3 : Empilage dans la cour ---
% Regle : Trouver un emplacement optimal
assigner_emplacement(Conteneur, Zone, Rangee, Hauteur) :-
    conteneur(Conteneur, _, Destination, Type),
    (   zone_stockage(Zone, _)
        ->  true
        ;   write('Erreur : Aucune zone de stockage disponible.'), nl, fail
    ),
    (   Type == reefer
        ->  (   prise_electrique(Rangee, Zone)
                ->  write('Conteneur refrigere : rangee avec prise electrique selectionnee.'), nl
                ;   write('Erreur : Aucune rangee avec prise electrique pour conteneur refrigere.'), nl, fail
            )
        ;   (   Destination == 'Douala' -> Rangee = 1 ;
                Destination == 'Yaounde' -> Rangee = 2 ;
                Destination == 'Limbe' -> Rangee = 3 ;
                Destination == 'Bamenda' -> Rangee = 4 ;
                Destination == 'Kribi' -> Rangee = 5 ;
                Destination == 'Buea' -> Rangee = 6 ;
                Rangee = 7 )
    ),
    (   Hauteur =< 4, \+ emplacement(_, Zone, Rangee, Hauteur)
        ->  true
        ;   write('Erreur : Aucun emplacement libre dans la zone '), write(Zone), write('.'), nl, fail
    ),
    assigner_vehicule_interne(Conteneur, Vehicule),
    assertz(emplacement(Conteneur, Zone, Rangee, Hauteur)),
    write('Conteneur '), write(Conteneur), write(' empile en Zone '), write(Zone),
    write(', Rangee '), write(Rangee), write(', Hauteur '), write(Hauteur),
    write(' avec vehicule '), write(Vehicule), nl.

% Regle : Afficher la carte des emplacements
afficher_emplacements :-
    write('Carte des emplacements :'), nl,
    (   emplacement(Conteneur, Zone, Rangee, Hauteur),
        write('- Conteneur '), write(Conteneur), write(' : Zone '), write(Zone),
        write(', Rangee '), write(Rangee), write(', Hauteur '), write(Hauteur), nl,
        fail
    ;   write('Aucun emplacement occupe.'), nl, true
    ).

% --- ETAPE 4 : Traitement douanier ---
% Regle : Verifier la conformite douaniere
verifier_douane(Conteneur) :-
    conteneur(Conteneur, _, Destination, _),
    document(Conteneur, connaissement, ValideConnaissement),
    document(Conteneur, declaration_douane, ValideDeclaration),
    (   ValideConnaissement == oui,
        ValideDeclaration == oui
        ->  assertz(statut_douane(Conteneur, libre)),
            write('Conteneur '), write(Conteneur), write(' approuve pour '), write(Destination), nl
        ;   assertz(statut_douane(Conteneur, bloque)),
            write('ALERTE : Conteneur '), write(Conteneur), write(' bloque par la douane !'), nl
    ).

% Regle : Simuler un scan a rayons X
scanner_conteneur(Conteneur) :-
    statut_douane(Conteneur, bloque),
    write('Scan du conteneur '), write(Conteneur), nl,
    random_between(0, 1, X),
    (   X = 1
        -> write('RESULTAT : Marchandises suspectes ! Inspection physique requise.'), nl
        ;  write('RESULTAT : Rien a signaler. Erreur de documentation ?'), nl
    ).

% Regle : Simuler le paiement des taxes
payer_taxes(Conteneur) :-
    conteneur(Conteneur, _, _, _),
    random_between(100, 500, Montant),
    write('Paiement des taxes pour conteneur '), write(Conteneur),
    write(' : '), write(Montant), write(' unites monetaires effectue.'), nl.

% Regle : Afficher les statuts douaniers
afficher_statuts_douane :-
    write('Statuts douaniers :'), nl,
    (   statut_douane(Conteneur, Statut),
        write('- Conteneur '), write(Conteneur), write(' : '), write(Statut), nl,
        fail
    ;   write('Aucun statut douanier enregistre.'), nl, true
    ).

% --- ETAPE 5 : Chargement pour exportation ---
% Regle : Verifier qu un conteneur est pret
conteneur_pret(Conteneur) :-
    conteneur(Conteneur, _, _, _),
    statut_douane(Conteneur, libre),
    emplacement(Conteneur, _, _, _),
    \+ chargement(_, Conteneur, _),
    scelle_conteneur(Conteneur).

% Regle : Verifier ou poser un scelle
scelle_conteneur(Conteneur) :-
    conteneur(Conteneur, _, _, _),
    (   scelle(Conteneur, oui)
        -> write('Scelle du conteneur '), write(Conteneur), write(' verifie : valide.'), nl
        ;  assertz(scelle(Conteneur, oui)),
           write('Nouveau scelle pose sur conteneur '), write(Conteneur), nl
    ).

% Regle : Charger un conteneur sur un navire
charger_conteneur(Navire, Conteneur, Position) :-
    conteneur_pret(Conteneur),
    navire(Navire, _, _),
    conteneur(Conteneur, _, _, Type),
    (   Type == reefer
        -> Position = [avant, 1]
        ;  Position = [centre, 2]
    ),
    assigner_vehicule_interne(Conteneur, Vehicule),
    assertz(chargement(Navire, Conteneur, Position)),
    retract(emplacement(Conteneur, _, _, _)),
    write('Conteneur '), write(Conteneur), write(' charge sur '), write(Navire),
    write(' en position '), write(Position), write(' avec vehicule '), write(Vehicule), nl.

% Regle : Afficher le plan de chargement
afficher_chargement(Navire) :-
    write('Plan de chargement pour '), write(Navire), write(':'), nl,
    (   chargement(Navire, Conteneur, Position),
        write('- Conteneur '), write(Conteneur), write(' : '), write(Position), nl,
        fail
    ;   write('Aucun conteneur charge sur '), write(Navire), write('.'), nl, true
    ).
afficher_chargement(_) :-
    write('Aucun chargement enregistre.'), nl.


% --- ETAPE 6 : Transport terrestre et sortie du port ---

scelle_conteneur(Conteneur) :-
    conteneur(Conteneur, _, _, _),
    \+ scelle(Conteneur, _),
    assertz(scelle(Conteneur, oui)),
    write('Scellé posé sur le conteneur '), write(Conteneur), nl.

controler_sortie(Conteneur, Porte) :-
    statut_douane(Conteneur, libre),
    (   scelle(Conteneur, oui) -> true ; scelle_conteneur(Conteneur) ),
    file_porte(Porte, _),
    emplacement(Conteneur, _, _, _),
    write('Conteneur '), write(Conteneur), write(' autorisé à sortir par '), write(Porte), nl.

assigner_transport(Conteneur, Transporteur, Porte) :-
    conteneur(Conteneur, _, Destination, _),
    controler_sortie(Conteneur, Porte),
    transporteur(Transporteur, _, Destination, Capacite),
    findall(C, chargement(Transporteur, C, _), Conteneurs),
    length(Conteneurs, Nb),
    Nb < Capacite,
    retract(file_porte(Porte, NbEnAttente)),
    NbEnAttente >= 0,
    NouvelleFile is NbEnAttente - 1,
    (   NouvelleFile >= 0 -> assertz(file_porte(Porte, NouvelleFile))
    ;   assertz(file_porte(Porte, 0))
    ),
    assertz(chargement(Transporteur, Conteneur, [])),
    write('Conteneur '), write(Conteneur), write(' assigné au transporteur '),
    write(Transporteur), write(' via '), write(Porte), nl.

afficher_transports :-
    (   transporteur(ID, Type, Dest, _),
        findall(C, chargement(ID, C, _), Conteneurs),
        write('Transporteur '), write(ID), write(' ('), write(Type),
        write(' vers '), write(Dest), write(') : '), write(Conteneurs), nl,
        fail
    ;   write('Aucun transporteur chargé.'), nl, true
    ).

afficher_files_portes :-
    write('Files aux portes du port :'), nl,
    (   file_porte(Porte, Nb),
        write('- '), write(Porte), write(' : '), write(Nb), write(' conteneurs en attente'), nl,
        fail
    ;   write('Aucune file d attente.'), nl, true
    ).
reinitialiser_files :-
    retractall(file_porte(_, _)),
    assertz(file_porte('Porte1', 2)),
    assertz(file_porte('Porte2', 5)).
% --- ETAPE 7 : Statistiques et predictions ---
% Regle : Calculer le taux d occupation
taux_occupation_quais(Taux) :-
    findall(Q, quai(Q, occupe, _), Occupe),
    findall(Q, quai(Q, _, _), Tous),
    length(Occupe, NbOccupe),
    length(Tous, NbTotal),
    Taux is (NbOccupe / NbTotal) * 100,
    write('Taux d occupation des quais : '), write(Taux), write('%'), nl.

% Regle : Lister les conteneurs bloques
conteneurs_bloques(Liste) :-
    findall(C, statut_douane(C, bloque), Liste),
    write('Conteneurs bloques : '), write(Liste), nl.

% Regle : Compter les conteneurs par destination
conteneurs_par_destination :-
    write('Conteneurs par destination :'), nl,
    findall(Dest, conteneur(_, _, Dest, _), Destinations),
    list_to_set(Destinations, DestUniques),
    (   member(D, DestUniques),
        findall(C, conteneur(C, _, D, _), Conteneurs),
        length(Conteneurs, Nb),
        write('- '), write(D), write(' : '), write(Nb), write(' conteneurs'), nl,
        fail
    ;   true
    ).

% Regle : Predire le temps de dechargement
predire_temps_dechargement(Navire, Temps) :-
    navire(Navire, _, NbConteneurs),
    Temps is NbConteneurs / 50,
    write('Temps estime pour decharger '), write(Navire),
    write(' : '), write(Temps), write(' heures'), nl.

% --- Interface Utilisateur ---
% Regle : Afficher le menu interactif
menu :-
    repeat,
    nl,
    write('=== TERMINAL A CONTENEURS - MENU PRINCIPAL ==='), nl,
    write('1. Planification : Attribuer un quai a un navire'), nl,
    write('2. Dechargement : Assigner un conteneur a une zone'), nl,
    write('3. Empilage : Placer un conteneur dans la cour'), nl,
    write('4. Douane : Verifier un conteneur'), nl,
    write('5. Chargement : Preparer l exportation'), nl,
    write('6. Transport : Assigner a un camion/train'), nl,
    write('7. Statistiques : Consulter les rapports'), nl,
    write('0. Quitter'), nl,
    nl,
    write('Votre choix : '),
    read_string(user_input, "\n", "\r", _, ChoixStr),
    string_codes(ChoixStr, Codes),
    (   Codes \= [],
        catch(atom_number(ChoixStr, Choix), _, fail),
        between(0, 7, Choix)
        ->  (   Choix = 0
                ->  write('Au revoir !'), nl, !, halt
                ;   (   catch(executer_action(Choix), _, fail)
                        ->  write('Action executee avec succes.'), nl
                        ;   write('Erreur : L action a echoue. Verifiez les conditions.'), nl
                    ),
                    write('Appuyez sur Entree pour revenir au menu...'),
                    read_string(user_input, "\n", "\r", _, _)
                )
        ;   write('Erreur : Veuillez entrer un nombre entre 0 et 7.'), nl
    ),
    fail.

% Regle : Executer l action choisie
executer_action(1) :-
    write('État actuel des quais :'), nl,
    afficher_quais,
    write('Navires disponibles : '), nl,
    findall(N, navire(N, _, _), Navires), write(Navires), nl,
    write('Nom du navire : '), read_string(user_input, "\n", "\r", _, NavireStr),
    (   NavireStr \= "",
        atom_string(Navire, NavireStr),
        navire(Navire, _, _)
        -> attribuer_quai(Navire, _),
           write('État des quais après attribution :'), nl,
           afficher_quais
        ;  write('Erreur : Navire inconnu ou entree vide.'), nl, fail
    ).

executer_action(2) :-
    write('État actuel des zones de stockage :'), nl,
    afficher_zones,
    write('Conteneurs à décharger : '), nl,
    findall(C, conteneur(C, _, _, _), Conteneurs), write(Conteneurs), nl,
    write('ID du conteneur : '), read_string(user_input, "\n", "\r", _, ConteneurStr),
    (   ConteneurStr \= "",
        catch(atom_number(ConteneurStr, Conteneur), _, fail),
        conteneur(Conteneur, _, _, _)
        -> decharger_conteneur(Conteneur, _),
           write('État des zones après déchargement :'), nl,
           afficher_zones
        ;  write('Erreur : Conteneur inconnu ou ID invalide.'), nl, fail
    ).

executer_action(3) :-
    write('État actuel des emplacements :'), nl,
    afficher_emplacements,
    write('Conteneurs en attente d empilage : '), nl,
    findall(C, (conteneur(C, _, _, _), \+ emplacement(C, _, _, _)), Conteneurs), write(Conteneurs), nl,
    write('ID du conteneur : '), read_string(user_input, "\n", "\r", _, ConteneurStr),
    (   ConteneurStr \= "",
        catch(atom_number(ConteneurStr, Conteneur), _, fail),
        conteneur(Conteneur, _, _, _)
        -> assigner_emplacement(Conteneur, _, _, _),
           write('État des emplacements après empilage :'), nl,
           afficher_emplacements
        ;  write('Erreur : Conteneur inconnu ou ID invalide.'), nl, fail
    ).

executer_action(4) :-
    write('État actuel des statuts douaniers :'), nl,
    afficher_statuts_douane,
    write('Conteneurs à vérifier : '), nl,
    findall(C, conteneur(C, _, _, _), Conteneurs), write(Conteneurs), nl,
    write('ID du conteneur : '), read_string(user_input, "\n", "\r", _, ConteneurStr),
    (   ConteneurStr \= "",
        catch(atom_number(ConteneurStr, Conteneur), _, fail),
        conteneur(Conteneur, _, _, _)
        ->  (   retract(statut_douane(Conteneur, _)) -> true ; true ),
            verifier_douane(Conteneur),
            (   random_between(1, 10, X), X = 1
                ->  write('Scan aléatoire sélectionné pour conteneur '), write(Conteneur), nl,
                    (   statut_douane(Conteneur, bloque) -> true
                        ;   assertz(statut_douane(Conteneur, bloque)),
                            write('ALERTE : Conteneur '), write(Conteneur), write(' sélectionné pour scan aléatoire !'), nl
                    ),
                    scanner_conteneur(Conteneur)
                ;   true
            ),
            payer_taxes(Conteneur),
            (   statut_douane(Conteneur, bloque) -> scanner_conteneur(Conteneur) ; true ),
            write('État des statuts douaniers après vérification :'), nl,
            afficher_statuts_douane
        ;   write('Erreur : Conteneur inconnu ou ID invalide.'), nl, fail
    ).

executer_action(5) :-
    write('Navires a charger : '), nl,
    findall(N, navire(N, _, _), Navires), write(Navires), nl,
    write('Nom du navire : '), read_string(user_input, "\n", "\r", _, NavireStr),
    (   NavireStr \= "",
        atom_string(Navire, NavireStr),
        navire(Navire, _, _)
        ->  write('Conteneurs disponibles : '), nl,
            findall(C, conteneur_pret(C), Conteneurs), write(Conteneurs), nl,
            write('ID du conteneur : '), read_string(user_input, "\n", "\r", _, ConteneurStr),
            (   ConteneurStr \= "",
                catch(atom_number(ConteneurStr, Conteneur), _, fail),
                conteneur_pret(Conteneur)
                -> charger_conteneur(Navire, Conteneur, _)
                ;  write('Erreur : Conteneur inconnu ou non pret.'), nl, fail
            )
        ;   write('Erreur : Navire inconnu ou entree vide.'), nl, fail
    ).

% --- Choix 6 ---

executer_action(6) :-
    write('État actuel des transporteurs :'), nl,
    afficher_transports,
    write('État actuel des files aux portes :'), nl,
    afficher_files_portes,
    write('Transporteurs disponibles : '), nl,
    findall(T, transporteur(T, _, _, _), Transporteurs), write(Transporteurs), nl,
    write('ID du transporteur : '), read_string(user_input, "\n", "\r", _, TransporteurStr),
    (   TransporteurStr \= "",
        atom_string(Transporteur, TransporteurStr),
        transporteur(Transporteur, _, _, _)
        ->  write('Conteneurs à transporter : '), nl,
            findall(C, (statut_douane(C, libre), emplacement(C, _, _, _)), Conteneurs), write(Conteneurs), nl,
            write('ID du conteneur : '), read_string(user_input, "\n", "\r", _, ConteneurStr),
            (   ConteneurStr \= "",
                catch(atom_number(ConteneurStr, Conteneur), _, fail),
                conteneur(Conteneur, _, _, _)
                ->  write('Portes de sortie disponibles : '), nl,
                    findall(P, file_porte(P, _), Portes), write(Portes), nl,
                    write('Porte de sortie : '), read_string(user_input, "\n", "\r", _, PorteStr),
                    (   PorteStr \= "",
                        atom_string(Porte, PorteStr),
                        file_porte(Porte, _)
                        ->  (   assigner_transport(Conteneur, Transporteur, Porte)
                            ->  write('État des transporteurs après assignation :'), nl,
                                afficher_transports,
                                write('État des files aux portes après assignation :'), nl,
                                afficher_files_portes
                            ;   write('Erreur : Assignation échouée. Vérifiez le statut douanier, l\'empilage, ou la file d\'attente.'), nl, fail
                            )
                        ;   write('Erreur : Porte invalide ou entrée vide.'), nl, fail
                    )
                ;   write('Erreur : Conteneur inconnu ou ID invalide.'), nl, fail
            )
        ;   write('Erreur : Transporteur inconnu ou ID invalide.'), nl, fail
    ).

executer_action(7) :-
    write('=== STATISTIQUES ==='), nl,
    taux_occupation_quais(_),
    conteneurs_bloques(_),
    conteneurs_par_destination,
    afficher_emplacements,
    (   navire(Navire, _, _),
        afficher_chargement(Navire),
        fail
    ;   true
    ),
    afficher_files_portes,
    findall(N, navire(N, _, _), Navires),
    forall(member(N, Navires), predire_temps_dechargement(N, _)).

% --- Point d entree du programme ---
:- initialization(menu).
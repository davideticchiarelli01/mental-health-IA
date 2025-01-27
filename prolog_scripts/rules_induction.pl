
:- ensure_loaded(attributes).
:- ensure_loaded(training_set).
:- ensure_loaded(test_set).

:- op(300,xfx,<==).


apprendi(Classe) :-
	findall( e(C,O), e(C,O), Esempi),      % raccoglie  gli Esempi
	apprendi(Esempi, Classe, Descrizione), % induce la Descrizione della Classe
	nl,write(Classe),write('<=='),nl,      % la stampa
	writelist(Descrizione),
	assert( Classe <== Descrizione ).      % e la asserisce, ovvero APPRENDE

apprendi( Esempi, Classe, []) :-                % Descrizione vuota perché non ci sono
	\+ member( e(Classe,_), Esempi ).       % esempi da coprire di quella Classe
apprendi( Esempi, Classe, [Cong|Congi] ) :-
	apprendi_cong( Esempi, Classe, Cong),   % induce una Cong di coppie Attr=Val che copre
						% almeno un esempio di Classe e nessun esempio
						% di una qualunque altra classe
	rimuovi( Esempi, Cong, RestoEsempi ),   % rimuove gli esempi coperti da Cong
	apprendi( RestoEsempi, Classe, Congi ). % copre gli esempi rimasti

apprendi_cong( Esempi, Classe, []) :-
	\+ (member( e(Cl,_), Esempi), Cl \== Classe),
	!.	                               % non ci sono esempi di altre classi
apprendi_cong( Esempi, Cl, [Cond|Conds] ) :-
	scegli_cond( Esempi, Cl, Cond ),       % sceglie una coppia Attr=Val
	filtra( Esempi, [Cond], Esempi1 ),     % seleziona in Esempi1 quelli che hanno Attr=Val
	apprendi_cong( Esempi1, Cl, Conds ).

scegli_cond( Esempi, Classe, AttVal) :-
	findall( AV/Punti, punteggioAV(Esempi,Classe,AV,Punti), AVs),
	best( AVs, AttVal).

best([AttVal/_],AttVal).
best([AV0/S0,AV1/S1|AVSlist],AttVal) :-
	S1 > S0, !,    % AV1 è meglio di AV0
	best([AV1/S1|AVSlist],AttVal)
	;
	best([AV0/S0|AVSlist],AttVal).

filtra(Esempi,Cond,Esempi1) :-
	findall(e(Classe,Ogg), (member(e(Classe,Ogg),Esempi),soddisfa(Ogg,Cond)), Esempi1).

rimuovi([],_,[]).
rimuovi([e(_,Ogg)|Es],Conge,Es1) :-
	soddisfa(Ogg,Conge), !, % il primo esempio matcha Conge
	rimuovi(Es,Conge,Es1).  % lo rimuove

rimuovi([E|Es],Conge,[E|Es1]) :- % mantiene il primo esempio
	rimuovi(Es,Conge,Es1).

soddisfa( Oggetto, Congiunzione) :-
%	hanno_attributo_in_comune(Oggetto,Congiunzione),
	\+ (member(Att=Valx,Congiunzione), member(Att=Valy,Oggetto), Valx \== Valy).

hanno_attributo_in_comune(Oggetto,Congiunzione) :-
	member(Att=_,Congiunzione),
	member(Att=_,Oggetto),
	!.

punteggioAV( Esempi, Classe, AttVal, Punti ) :-
	candidato( Esempi, Classe, AttVal),  % un attributo/valore adatto
	filtra(Esempi,[AttVal],Esempi1),  % gli Esempi1 soddisfano la condizione Att=Val
	length(Esempi1,N1),
	conta_pos(Esempi1,Classe,Npos1),  % numero di esempi positivi
	Npos1 > 0,                        % almeno un esempio positivo
	Punti is (Npos1 + 1) / (N1 + 2).

candidato(Esempi,Classe,Att=Val) :-
	a(Att, Valori),                  % un attributo
	member(Val,Valori),              % un valore
	adatto(Att=Val,Esempi,Classe).

adatto(AttVal,Esempi,Classe) :-
	member(e(ClasseX,OggX),Esempi),	% esempio
	ClasseX \== Classe,		% negativo
	\+ soddisfa(OggX,[AttVal]), !.	% non soddisfatto dalla coppia Att=Val

conta_pos([],_,0).
conta_pos([e(ClasseX,_)|Esempi],Classe,N) :-
	conta_pos(Esempi,Classe,N1),
	(ClasseX=Classe,!,N is N1+1 ; N=N1).

writelist([]).
writelist([X|L]) :-
	tab(2), writeq(X), nl,
	writelist(L).

classifica(Oggetto,Classe) :- % Oggetto descritto da una lista Att=Val appartiene a Classe
	Classe <== Descrizione, % se Classe è una lista di liste Attx=Valx, di cui una è
	member(CongiunzioneAttributi,Descrizione), % la lista CongiunzioneAttributi e
	soddisfa(Oggetto,CongiunzioneAttributi). % questa soddisfa la lista Att=Val

stampa_matrice_di_confusione :-
	findall(Classe/Oggetto,s(Classe,Oggetto),TestSet),
	length(TestSet,N),
	valuta(TestSet, VD, 0, VA, 0, VB, 0, VU, 0, DA, 0, DB, 0, DU, 0, AD, 0, AB, 0, AU, 0, BD, 0, BA,0, BU,0, UD, 0, UA, 0, UB, 0, NC, 0),
	A is (VD + VA + VB + VU) / (N - NC), % Accuratezza
	E is 1 - A,		   % Errore
	write('Test effettuati :'),  writeln(N),
	write('Test non classificati :'),  writeln(NC),
	write('Vero depression :'),  writeln(VD),
	write('Vero anxiety :'),  writeln(VA),
	write('Vero burnout :'),  writeln(VB),
	write('Vero unknown :'),  writeln(VU),
	write('Depression classificati come anxiety :'),  writeln(DA),
	write('Depression classificati come burnout :'),  writeln(DB),
	write('Depression classificati come unknown :'),  writeln(DU),
	write('Anxiety classificati come depression :'),  writeln(AD),
	write('Anxiety classificati come burnout :'),  writeln(AB),
	write('Anxiety classificati come unknown :'),  writeln(AU),
	write('Burnout classificati come depression :'),  writeln(BD),
	write('Burnout classificati come anxiety :'),  writeln(BA),
	write('Burnout classificati come unknown :'),  writeln(BU),
	write('Unknown classificati come depression :'),  writeln(UD),
	write('Unknown classificati come anxiety :'),  writeln(UA),
	write('Unknown classificati come burnout :'),  writeln(UB),
	write('Accuratezza: '), writeln(A),
	write('Errore: '), writeln(E).


valuta([], VD, VD, VA, VA, VB, VB, VU, VU, DA, DA, DB, DB, DU, DU, AD, AD, AB, AB, AU, AU, BD, BD, BA, BA, BU, BU, UD, UD, UA, UA, UB, UB, NC, NC).

% Classificazioni corrette
valuta([depression/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, depression), !,
    VDA1 is VDA + 1,
    valuta(Coda, VD, VDA1, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta([anxiety/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, anxiety), !,
    VAA1 is VAA + 1,
    valuta(Coda, VD, VDA, VA, VAA1, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta([burnout/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, burnout), !,
    VBA1 is VBA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA1, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta([unknown/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, unknown), !,
    VUA1 is VUA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA1, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).


% Classificazioni errate
% Oggetti depression classificati erroneamente
valuta([depression/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, anxiety), !,
    DAA1 is DAA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA1, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta([depression/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, burnout), !,
    DBA1 is DBA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA1, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta([depression/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, unknown), !,
    DUA1 is DUA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA1, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).


% Oggetti anxiety classificati erroneamente
valuta([anxiety/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, depression), !,
    ADA1 is ADA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA1, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta([anxiety/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, burnout), !,
    ABA1 is ABA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA1, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta([anxiety/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, unknown), !,
    AUA1 is AUA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA1, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).


% Oggetti burnout classificati erroneamente
valuta([burnout/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, depression), !,
    BDA1 is BDA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA1, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta([burnout/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, anxiety), !,
    BAA1 is BAA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA1, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta([burnout/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, unknown), !,
    BUA1 is BUA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA1, UD, UDA, UA, UAA, UB, UBA, NC, NCA).


% Oggetti unknown classificati erroneamente
valuta([unknown/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, depression), !,
    UDA1 is UDA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA1, UA, UAA, UB, UBA, NC, NCA).

valuta([unknown/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, anxiety), !,
    UAA1 is UAA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA1, UB, UBA, NC, NCA).

valuta([unknown/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, burnout), !,
    UBA1 is UBA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA1, NC, NCA).


% Caso generale: Non classificato
valuta([_/_ | Coda], VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    NCA1 is NCA + 1,
    valuta(Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA1).

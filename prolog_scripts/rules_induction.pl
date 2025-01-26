
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
	valuta(TestSet,VN,0,VP,0,FN,0,FP,0,NC,0),
	A is (VP + VN) / (N - NC), % Accuratezza
	E is 1 - A,		   % Errore
	write('Test effettuati :'),  writeln(N),
	write('Test non classificati :'),  writeln(NC),
	write('Veri Negativi  '), write(VN), write('   Falsi Positivi '), writeln(FP),
	write('Falsi Negativi '), write(FN), write('   Veri Positivi  '), writeln(VP),
	write('Accuratezza: '), writeln(A),
	write('Errore: '), writeln(E).


valuta([], VN, VN, VP, VP, FN, FN, FP, FP, NC, NC).

% Classificazioni corrette
valuta([depression/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, depression), !,
    VNA1 is VNA + 1,
    valuta(Coda, VN, VNA1, VP, VPA, FN, FNA, FP, FPA, NC, NCA).

valuta([anxiety/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, anxiety), !,
    VPA1 is VPA + 1,
    valuta(Coda, VN, VNA, VP, VPA1, FN, FNA, FP, FPA, NC, NCA).

valuta([burnout/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, burnout), !,
    VNA1 is VNA + 1,
    valuta(Coda, VN, VNA1, VP, VPA, FN, FNA, FP, FPA, NC, NCA).

valuta([unknown/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, unknown), !,
    NCA1 is NCA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA1).

% Classificazioni errate
% Oggetti depression classificati erroneamente
valuta([depression/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, anxiety), !,
    FNA1 is FNA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA1, FP, FPA, NC, NCA).

valuta([depression/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, burnout), !,
    FNA1 is FNA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA1, FP, FPA, NC, NCA).

valuta([depression/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, unknown), !,
    FPA1 is FPA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA, FP, FPA1, NC, NCA).

% Oggetti anxiety classificati erroneamente
valuta([anxiety/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, depression), !,
    FPA1 is FPA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA, FP, FPA1, NC, NCA).

valuta([anxiety/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, burnout), !,
    FPA1 is FPA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA, FP, FPA1, NC, NCA).

valuta([anxiety/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, unknown), !,
    FPA1 is FPA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA, FP, FPA1, NC, NCA).

% Oggetti burnout classificati erroneamente
valuta([burnout/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, depression), !,
    FNA1 is FNA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA1, FP, FPA, NC, NCA).

valuta([burnout/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, anxiety), !,
    FNA1 is FNA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA1, FP, FPA, NC, NCA).

valuta([burnout/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, unknown), !,
    FPA1 is FPA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA, FP, FPA1, NC, NCA).

% Oggetti unknown classificati erroneamente
valuta([unknown/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, depression), !,
    FNA1 is FNA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA1, FP, FPA, NC, NCA).

valuta([unknown/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, anxiety), !,
    FNA1 is FNA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA1, FP, FPA, NC, NCA).

valuta([unknown/Oggetto | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    classifica(Oggetto, burnout), !,
    FNA1 is FNA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA1, FP, FPA, NC, NCA).

% Caso generale: Non classificato
valuta([_/_ | Coda], VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA) :-
    NCA1 is NCA + 1,
    valuta(Coda, VN, VNA, VP, VPA, FN, FNA, FP, FPA, NC, NCA1).

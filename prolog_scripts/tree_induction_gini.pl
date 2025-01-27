% programma per apprendere inducendo Alberi di Decisione testandone
% l' efficacia

:- ensure_loaded(attributes).
:- ensure_loaded(training_set).
:- ensure_loaded(test_set).

:- dynamic alb/1.

induce_albero( Albero ) :-
	findall( e(Classe,Oggetto), e(Classe,Oggetto), Esempi),
        findall( Att,a(Att,_), Attributi),
        induce_albero( Attributi, Esempi, Albero),
	assert(alb(Albero)).

% induce_albero( +Attributi, +Esempi, -Albero):
% l'Albero indotto dipende da questi tre casi:
% (1) Albero = null: l'insieme degli esempi è vuoto
% (2) Albero = l(Classe): tutti gli esempi sono della stessa classe
% (3) Albero = t(Attributo, [Val1:SubAlb1, Val2:SubAlb2, ...]):
%     gli esempi appartengono a più di una classe
%     Attributo è la radice dell'albero
%     Val1, Val2, ... sono i possibili valori di Attributo
%     SubAlb1, SubAlb2,... sono i corrispondenti sottoalberi di
%     decisione.
% (4) Albero = l(Classi): non abbiamo Attributi utili per
%     discriminare ulteriormente
induce_albero( _, [], null ) :- !.			         % (1)
induce_albero( _, [e(Classe,_)|Esempi], l(Classe)) :-	         % (2)
	\+ ( member(e(ClassX,_),Esempi), ClassX \== Classe ),!.  % no esempi di altre classi (OK!!)
induce_albero( Attributi, Esempi, t(Attributo,SAlberi) ) :-	 % (3)
	sceglie_attributo( Attributi, Esempi, Attributo), !,     % implementa la politica di scelta
	del( Attributo, Attributi, Rimanenti ),			 % elimina Attributo scelto
	a( Attributo, Valori ),					 % ne preleva i valori
	induce_alberi( Attributo, Valori, Rimanenti, Esempi, SAlberi).
induce_albero( _, Esempi, l(Classi)) :-                          % finiti gli attributi utili (KO!!)
	findall( Classe, member(e(Classe,_),Esempi), Classi).

% sceglie_attributo( +Attributi, +Esempi, -MigliorAttributo):
% seleziona l'Attributo che meglio discrimina le classi; si basa sul
% concetto della "Gini-disuguaglianza"; utilizza il setof per ordinare
% gli attributi in base al valore crescente della loro disuguaglianza
% usare il setof per far questo è dispendioso e si può fare di meglio ..
sceglie_attributo( Attributi, Esempi, MigliorAttributo )  :-
	setof( Disuguaglianza/A,
	      (member(A,Attributi) , disuguaglianza(Esempi,A,Disuguaglianza)),
	      [MinorDisuguaglianza/MigliorAttributo|_] ).

% disuguaglianza(+Esempi, +Attributo, -Dis):
% Dis è la disuguaglianza combinata dei sottoinsiemi degli esempi
% partizionati dai valori dell'Attributo
disuguaglianza( Esempi, Attributo, Dis) :-
	a( Attributo, AttVals),
	somma_pesata( Esempi, Attributo, AttVals, 0, Dis).

% somma_pesata( +Esempi, +Attributo, +AttVals, +SommaParziale, -Somma)
% restituisce la Somma pesata delle disuguaglianze
% Gini = sum from{v} P(v) * sum from{i <> j} P(i|v)*P(j|v)
somma_pesata( _, _, [], Somma, Somma).
somma_pesata( Esempi, Att, [Val|Valori], SommaParziale, Somma) :-
	length(Esempi,N),                                            % quanti sono gli esempi
	findall( C,						     % EsempiSoddisfatti: lista delle classi ..
		 (member(e(C,Desc),Esempi) , soddisfa(Desc,[Att=Val])), % .. degli esempi (con ripetizioni)..
		 EsempiSoddisfatti ),				     % .. per cui Att=Val
	length(EsempiSoddisfatti, NVal),			     % quanti sono questi esempi
	NVal > 0, !,                                                 % almeno uno!
	findall(P,			           % trova tutte le P robabilità
                (bagof(1,		           %
                       member(_,EsempiSoddisfatti),
                       L),
                 length(L,NVC),
                 P is NVC/NVal),
                ClDst),
        gini(ClDst,Gini),
	NuovaSommaParziale is SommaParziale + Gini*NVal/N,
	somma_pesata(Esempi,Att,Valori,NuovaSommaParziale,Somma)
	;
	somma_pesata(Esempi,Att,Valori,SommaParziale,Somma). % nessun esempio soddisfa Att = Val

% gini(ListaProbabilità, IndiceGini)
%    IndiceGini = SOMMATORIA Pi*Pj per tutti i,j tali per cui i\=j
%    E' equivalente a 1 - SOMMATORIA Pi*Pi su tutti gli i
gini(ListaProbabilità,Gini) :-
	somma_quadrati(ListaProbabilità,0,SommaQuadrati),
	Gini is 1-SommaQuadrati.
somma_quadrati([],S,S).
somma_quadrati([P|Ps],PartS,S)  :-
	NewPartS is PartS + P*P,
	somma_quadrati(Ps,NewPartS,S).

% induce_alberi(Attributi, Valori, AttRimasti, Esempi, SAlberi):
% induce decisioni SAlberi per sottoinsiemi di Esempi secondo i Valori
% degli Attributi
induce_alberi(_,[],_,_,[]).     % nessun valore, nessun sottoalbero
induce_alberi(Att,[Val1|Valori],AttRimasti,Esempi,[Val1:Alb1|Alberi])  :-
	attval_subset(Att=Val1,Esempi,SottoinsiemeEsempi),
	induce_albero(AttRimasti,SottoinsiemeEsempi,Alb1),
	induce_alberi(Att,Valori,AttRimasti,Esempi,Alberi).

% attval_subset( Attributo = Valore, Esempi, Subset):
%   Subset è il sottoinsieme di Examples che soddisfa la condizione
%   Attributo = Valore
attval_subset(AttributoValore,Esempi,Sottoinsieme) :-
	findall(e(C,O),(member(e(C,O),Esempi),soddisfa(O,[AttributoValore])),Sottoinsieme).

% soddisfa(Oggetto, Descrizione):
soddisfa(Oggetto,Congiunzione)  :-
	\+ (member(Att=Val,Congiunzione),
	    member(Att=ValX,Oggetto),
	    ValX \== Val).

del(T,[T|C],C) :- !.
del(A,[T|C],[T|C1]) :-
	del(A,C,C1).

mostra(T) :-
	mostra(T,0).
mostra(null,_) :- writeln(' ==> ???').
mostra(l(X),_) :- write(' ==> '),writeln(X).
mostra(t(A,L),I) :-
	nl,tab(I),write(A),nl,I1 is I+2,
	mostratutto(L,I1).
mostratutto([],_).
mostratutto([V:T|C],I) :-
	tab(I),write(V), I1 is I+2,
	mostra(T,I1),
	mostratutto(C,I).


% ================================================================================
% classifica( +Oggetto, -Classe, t(+Att,+Valori))
%  Oggetto: [Attributo1=Valore1, .. , AttributoN=ValoreN]
%  Classe: classe a cui potrebbe appartenere un oggetto caratterizzato da quelle coppie
%  Attributo=Valore
%  t(-Att,-Valori): Albero di Decisione
% presuppone sia stata effettuata l'induzione dell'Albero di Decisione

classifica(Oggetto,nc,t(Att,Valori)) :- % dato t(+Att,+Valori), Oggetto è della Classe
	member(Att=Val,Oggetto),  % se Att=Val è elemento della lista Oggetto
        member(Val:null,Valori). % e Val:null è in Valori

classifica(Oggetto,Classe,t(Att,Valori)) :- % dato t(+Att,+Valori), Oggetto è della Classe
	member(Att=Val,Oggetto),  % se Att=Val è elemento della lista Oggetto
        member(Val:l(Classe),Valori). % e Val:l(Classe) è in Valori

classifica(Oggetto,Classe,t(Att,Valori)) :-
	member(Att=Val,Oggetto),  % se Att=Val è elemento della lista Oggetto
	delete(Oggetto,Att=Val,Resto),
	member(Val:t(AttFiglio,ValoriFiglio),Valori),
	classifica(Resto,Classe,t(AttFiglio,ValoriFiglio)).


stampa_matrice_di_confusione :-
	alb(Albero),
	findall(Classe/Oggetto,s(Classe,Oggetto),TestSet),
	length(TestSet,N),
    valuta(Albero, TestSet, VD, 0, VA, 0, VB, 0, VU, 0, DA, 0, DB, 0, DU, 0, AD, 0, AB, 0, AU, 0, BD, 0, BA,0, BU,0, UD, 0, UA, 0, UB, 0, NC, 0),
	A is (VD + VA + VB + VU) / (N - NC), % Accuratezza
	E is 1 - A,		   % Errore
	write('Tests performed: '), writeln(N),
	write('Tests not classified: '), writeln(NC),
	write('True depression: '), writeln(VD),
	write('True anxiety: '), writeln(VA),
	write('True burnout: '), writeln(VB),
	write('True unknown: '), writeln(VU),
	write('Depression classified as anxiety: '), writeln(DA),
	write('Depression classified as burnout: '), writeln(DB),
	write('Depression classified as unknown: '), writeln(DU),
	write('Anxiety classified as depression: '), writeln(AD),
	write('Anxiety classified as burnout: '), writeln(AB),
	write('Anxiety classified as unknown: '), writeln(AU),
	write('Burnout classified as depression: '), writeln(BD),
	write('Burnout classified as anxiety: '), writeln(BA),
	write('Burnout classified as unknown: '), writeln(BU),
	write('Unknown classified as depression: '), writeln(UD),
	write('Unknown classified as anxiety: '), writeln(UA),
	write('Unknown classified as burnout: '), writeln(UB),
	write('Accuracy: '), writeln(A),
	write('Error: '), writeln(E).



valuta(_, [], VD, VD, VA, VA, VB, VB, VU, VU, DA, DA, DB, DB, DU, DU, AD, AD, AB, AB, AU, AU, BD, BD, BA, BA, BU, BU, UD, UD, UA, UA, UB, UB, NC, NC).


% Classificazioni corrette
valuta(Albero, [depression/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, depression, Albero), !,
    VDA1 is VDA + 1,
    valuta(Albero, Coda, VD, VDA1, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta(Albero,[anxiety/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, anxiety, Albero), !,
    VAA1 is VAA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA1, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta(Albero,[burnout/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, burnout, Albero), !,
    VBA1 is VBA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA1, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta(Albero,[unknown/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, unknown, Albero), !,
    VUA1 is VUA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA1, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).


% Classificazioni errate
% Oggetti depression classificati erroneamente
valuta(Albero,[depression/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, anxiety, Albero), !,
    DAA1 is DAA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA1, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta(Albero,[depression/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, burnout, Albero), !,
    DBA1 is DBA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA1, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta(Albero,[depression/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, unknown, Albero), !,
    DUA1 is DUA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA1, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).


% Oggetti anxiety classificati erroneamente
valuta(Albero,[anxiety/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, depression, Albero), !,
    ADA1 is ADA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA1, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta(Albero,[anxiety/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, burnout, Albero), !,
    ABA1 is ABA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA1, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta(Albero,[anxiety/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, unknown, Albero), !,
    AUA1 is AUA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA1, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).


% Oggetti burnout classificati erroneamente
valuta(Albero,[burnout/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, depression, Albero), !,
    BDA1 is BDA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA1, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta(Albero,[burnout/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, anxiety, Albero), !,
    BAA1 is BAA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA1, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA).

valuta(Albero,[burnout/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, unknown, Albero), !,
    BUA1 is BUA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA1, UD, UDA, UA, UAA, UB, UBA, NC, NCA).


% Oggetti unknown classificati erroneamente
valuta(Albero,[unknown/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, depression, Albero), !,
    UDA1 is UDA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA1, UA, UAA, UB, UBA, NC, NCA).

valuta(Albero,[unknown/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, anxiety, Albero), !,
    UAA1 is UAA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA1, UB, UBA, NC, NCA).

valuta(Albero,[unknown/Oggetto | Coda],  VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :- 
    classifica(Oggetto, burnout, Albero), !,
    UBA1 is UBA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA1, NC, NCA).


% Caso generale: Non classificato
valuta(Albero,[_/Oggetto | Coda], VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA) :-
    classifica(Oggetto, nc, Albero), !,
    NCA1 is NCA + 1,
    valuta(Albero, Coda, VD, VDA, VA, VAA, VB, VBA, VU, VUA, DA, DAA, DB, DBA, DU, DUA, AD, ADA, AB, ABA, AU, AUA, BD, BDA, BA, BAA, BU, BUA, UD, UDA, UA, UAA, UB, UBA, NC, NCA1).



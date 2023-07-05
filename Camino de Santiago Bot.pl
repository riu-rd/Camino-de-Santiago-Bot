start :- retractall(has(_, _)), retractall(gender(_, _)), retractall(person(_)),
    retractall(age(_, _)),
    retractall(budget(_)), retractall(travel(_)), retractall(prefer(_)),
    general, preferences, infer.

%GETTING USER INPUT
general :- write('Are you traveling with someone?'), nl,
      write('[1] Alone'), nl,
      write('[2] With a partner'), nl,
      write('[3] With a family'), nl,
      read(A), nl, inputtravel(A),
      write('What is your estimated budget per pilgrim? [in Peso]'), nl,
      read(F), nl,
      inputbudget(F).

inputtravel(1) :- assert(travel(alone)), nl, askinfo.
inputtravel(2) :- assert(travel(partner)), nl, askinfo,
      write('Enter partner information:'), nl, askinfo.
inputtravel(3) :- assert(travel(family)), nl, askinfo,
      write('Enter family member 1 information:'), nl, askinfo,
      write('Enter family member 2 information:'), nl, askinfo,
      write('Enter family member 3 information:'), nl, askinfo.

askinfo :- write('Enter name : (Input inside "<name>"'), nl,
      read(Name), nl,
      write('Enter gender:'), nl,
      write('[m] Male'), nl,
      write('[f] Female'), nl,
      write('[o] Others'), nl,
      read(A), nl,
      write('Enter age: '), nl,
      read(B), nl,
      write('Has a Schengen Visa? [y/n] '), nl,
      read(C), nl,
      write('Has a Philippine Passport? [y/n] '), nl,
      read(D), nl,
      write('Has a digital COVID certificate or EU Equivalent certificate? [y/n] '), nl,
      read(E), nl,
      inputgender(Name, A),
      inputage(Name, B),
      inputvisa(Name, C),
      inputpassport(Name, D),
      inputvaccine(Name, E),
      isperson(Name).

inputgender(Name, A) :- assert(gender(Name, A)).
inputage(Name, B) :- assert(age(Name, B)).
inputvisa(Name, y) :- assert(has(Name, visa)).
inputvisa(Name, n) :- not(has(Name, visa)).
inputpassport(Name, y) :- assert(has(Name, passport)).
inputpassport(Name, n) :- not(has(Name, passport)).
inputvaccine(Name, y) :- assert(has(Name, vaccine)).
inputvaccine(Name, n) :- not(has(Name, vaccine)).
inputbudget(F) :- assert(budget(F)).
isperson(Name) :- assert(person(Name)).

preferences :- write('What would you like the most during your walk?'), nl,
      write('[1] Has a floral path'), nl,
      write('[2] Many people (peak season)'), nl,
      write('[3] During a grape harvest'), nl,
      write('[4] Has comfortable kayak trips and beaches'), nl,
      write('[5] windy and cold'), nl, read(W), nl,
      wpref(W),
      write('During this walk, what temperature would be ideal for you?'), nl,
      write('[1] slightly lower than the average'), nl,
      write('[2] around the average'), nl,
      write('[3] slightly higher than the average'), nl, read(X), nl,
      tpref(X),
      write('What is your preferred price point for daily expenses?'), nl,
      write('[1] Cheap (approx. EUR25 or PHP1385)'), nl,
      write('[2] Average (approx. EUR35 or PHP1940)'), nl,
      write('[3] Expensive(approx. EUR45 or PHP2490)'), nl, read(Y), nl,
      ppref(Y).
wpref(1) :- assert(prefer(floral)), wprint.
wpref(2) :- assert(prefer(peak)), wprint.
wpref(3) :- assert(prefer(grape)), wprint.
wpref(4) :- assert(prefer(beach)), wprint.
wpref(5) :- assert(prefer(cold)), wprint.
wprint :- season(S), temperature(S, T),
      write('The average temperature would be '), write(T), write(' degrees.'), nl.
tpref(1) :- assert(prefer(lowertemp)).
tpref(2) :- assert(prefer(averagetemp)).
tpref(3) :- assert(prefer(highertemp)).
ppref(1) :- assert(prefer(cheap)).
ppref(2) :- assert(prefer(average)).
ppref(3) :- assert(prefer(expensive)).

%KNOWLEDGE BASE
adult(X) :- person(X), age(X, Y), Y >= 18.
minor(X) :- person(X), age(X, Y), Y < 18.

town(sarria).
town(portomarin).
town(palasderei).
town(arzua).
town(opedrouzo).
town(santiago).

accommodation_sarria(magdalena_monastery, 10) :- town(sarria), prefer(cheap).
accommodation_sarria(siete_en_el_camino, 38) :- town(sarria), prefer(average).
accommodation_sarria(hotel_mar_de_plata, 45) :- town(sarria), prefer(expensive).

accommodation_portomarin(pasinoapasino,10):- town(portomarin), prefer(cheap).
accommodation_portomarin(portosantiago,20):- town(portomarin), prefer(average).
accommodation_portomarin(villajardin,60):- town(portomarin), prefer(expensive).

accommodation_palasderei(buencamino,12):- town(palasderei), prefer(cheap).
accommodation_palasderei(zendoira, 25):- town(palasderei), prefer(average).
accommodation_palasderei(casabenilde, 31):- town(palasderei), prefer(expensive).

accommodation_arzua(ultreia,10):- town(arzua), prefer(cheap).
accommodation_arzua(casateodora,54):- town(arzua), prefer(expensive).
accommodation_arzua(suiza,43):- town(arzua), prefer(average).

accommodation_opedrouzo(asolaina,40):- town(opedrouzo).
accommodation_santiago(prfornos,39):- town(santiago).

restaurant(sarria, roma) :- prefer(expensive).
restaurant(sarria, mesonroberto):- prefer(average).
restaurant(sarria, pizzeriasalento):-prefer(cheap).

restaurant(portomarin, restaurantepasadadecamino):- prefer(expensive) .
restaurant(portomarin, omirador):- prefer(average).
restaurant(portomarin, restauranteperez):-prefer(cheap).

restaurant(palasderei, paradodasbestas):- prefer(expensive).
restaurant(palasderei, restaurantecastro):- prefer(average).
restaurant(palasderei, pulperiaanosaterra):-prefer(cheap).

restaurant(arzua, casateodoro):- prefer(expensive).
restaurant(arzua, mesonruralribadiso):- prefer(average).
restaurant(arzua, parralidabuenosaires):-prefer(cheap).

restaurant(opedrouzo, fondedopicho):- prefer(expensive).
restaurant(opedrouzo, casanene):- prefer(average).
restaurant(opedrouzo, okm19):-prefer(cheap).

restaurant(santiago, casamarcelor):- prefer(expensive).
restaurant(santiago, currodaparra):-prefer(average).
restaurant(santiago, mamapeixetaberna):-prefer(cheap).

distance(sarria, portomarin, 22.2).
distance(portomarin, palasderei, 24.8).
distance(palasderei, arzua, 28.5).
distance(arzua, opedrouzo, 19.3).
distance(opedrouzo, santiago, 19.4).

price(cheap).
price(average).
price(expensive).

temperature(spring, 12).
temperature(summer, 18.3).
temperature(autumn, 13.9).
temperature(winter, 8.2).

path(floral).
path(peak).
path(grape).
path(beach).
path(cold).

season(spring) :- prefer(floral).
season(summer) :- prefer(peak); prefer(beach).
season(autumn) :- prefer(grape).
season(winter) :- prefer(cold).

month(january, 7.8) :- season(winter), prefer(lowertemp).
month(february, 8.3) :- season(winter), prefer(averagetemp).
month(march, 10.3) :- season(spring), prefer(lowertemp).
month(april, 11.5) :- season(spring), prefer(averagetemp).
month(may, 14.2) :- season(spring), prefer(highertemp).
month(june, 16.9) :- season(summer), prefer(lowertemp).
month(july, 18.8) :- season(summer), prefer(averagetemp).
month(august, 19.2) :- season(summer), prefer(highertemp).
month(september, 17.4) :- season(autumn), prefer(highertemp).
month(october, 14.1) :- season(autumn), prefer(averagetemp).
month(november, 10.3) :- season(autumn), prefer(lowertemp).
month(december, 8.5) :- season(winter), prefer(highertemp).

clothes(thin) :- season(summer).
clothes(medium) :- season(spring); season(autumn).
clothes(heavy) :- season(winter).

%Essentials
bring(Name, backpack) :- person(Name).
bring(Name, hiking_shoes) :- person(Name), (clothes(medium); clothes(heavy)).
bring(Name, hiking_sandals) :- person(Name), clothes(thin).
bring(Name, sleeping_bag) :- person(Name), (clothes(medium); clothes(heavy)).
bring(Name, silk_liner) :- person(Name), clothes(thin).
bring(Name, trekking_poles) :- person(Name).
bring(Name, quick - dry_towel) :- person(Name).
bring(Name, water_bottle) :- person(Name).
bring(Name, headlamp) :- person(Name).
bring(Name, waterproof_pouch) :- person(Name).
bring(Name, toiletries) :- person(Name).
bring(Name, guide_book) :- person(Name).

% Clothing Specifics
bring(Name, hiking_socks) :- person(Name), nl,
      write('Clothing specifics:'), nl.
bring(Name, rain_jacket) :- person(Name), (clothes(medium); clothes(heavy)).
bring(Name, rain_poncho) :- person(Name), clothes(thin).
bring(Name, trekking_pants) :- person(Name), clothes(medium).
bring(Name, trekking_shorts) :- person(Name), clothes(thin).
bring(Name, hiking_shirts) :- person(Name), (clothes(thin); clothes(medium)).
bring(Name, fleece_jacket) :- person(Name), (clothes(medium); clothes(heavy)).
bring(Name, fleece_vest) :- person(Name), clothes(thin).
bring(Name, cap) :- person(Name), clothes(thin).
bring(Name, hat) :- person(Name), clothes(medium).
bring(Name, sunglasses) :- person(Name), clothes(thin).
bring(Name, underwear) :- person(Name).
bring(Name, boardshorts) :- person(Name), gender(Name, m), prefer(beach).
bring(Name, swimsuits) :- person(Name), gender(Name, f), prefer(beach).
bring(Name, fleece_hiking_pants) :- person(Name), clothes(heavy).
bring(Name, smart_wool_shirt) :- person(Name), clothes(heavy).
bring(Name, thermals) :- person(Name), clothes(heavy).
bring(Name, gaiters) :- person(Name), clothes(heavy).
bring(Name, gloves) :- person(Name), clothes(heavy).
bring(Name, beanie) :- person(Name), clothes(heavy).

%Misc
bring(Name, smartphone) :- person(Name), nl,
      write('Miscellaneous:'), nl.
bring(Name, power_adapter) :- person(Name).
bring(Name, gps) :- person(Name).
bring(Name, camera) :- person(Name).
bring(Name, book) :- person(Name), travel(alone).

%INFERENCE ENGINE
infer :- suggesttravel, suggestdistance, accommodationlist, accommodationlist2, accommodationlist3, accomodationlist4,accommodationlist5, accommodationlist6,
      suggestrestaurant, suggestweather, suggestclothes.

suggesttravel :- nl, nl, write(' - - - - - - - - - - - - - - - - - - - - -'), nl,
      hasvisa,haspassport,hasvaccine,hasminor.
hasvisa :-
      forall(person(X), has(X, visa)) ; nl,
      write('It seems that the following people have not yet acquired a'), nl,
      write('visa for travelling to Spain:'), nl,
      forall((person(X), not(has(X, visa))), writeln(' '>X)),
      write('For more information, please visit the link below:'), nl,
      write('https://ph.blsspainvisa.com/'), nl,nl,
      write('    -    -    -    -').
haspassport :- forall(person(X), has(X, passport)) ; nl,
    write('It seems that the following have not yet acquired a'), nl,
    write('Philippine passport:'), nl,
    forall((person(X), not(has(X, passport))), writeln(' '>X)),
    write('For more information, please visit the link below:'), nl,
    write('https://consular.dfa.gov.ph/services/passport/passport-cl/opas'), nl
    ,nl, write('    -    -    -    -').
hasvaccine :- forall(person(X), has(X, vaccine)) ; nl,
    write('It seems that the following have not yet fulfilled the'),nl,
    write('required health requirements for travelling to Spain:'),nl,
    forall((person(X), not(has(X, vaccine))), writeln(' '>X)),
    write('For more information, please visit the link below:'),nl,
    write('https://www.sanidad.gob.es/en/profesionales/saludPublica/ccayes/alertasActual/nCov/spth.htm'),nl,nl, write('    -    -    -    -').
hasminor :- forall(person(X),adult(X)) ; minor(_), nl,
    write('Minors are not allowed to walk in the camino.'),nl,
    write('The following is a minor:'),nl,
    forall((person(X), minor(X)), writeln(' '>X)),nl,
    write('    -    -    -    -').

suggestdistance :-
    nl,writeln('TOWNS AND DISTANCES- - - - - - - - - - - -'),nl,nl,
    write('Towns: '),nl, forall(town(X),writeln(' '>X)),nl,
    writeln('Distances: '),
    forall(distance(A,B,C),(write(' >The distance from '), write(A),
    write(' to '),write(B), write(' is '),write(C),write(' km.'),nl)),
    nl,writeln('The total distance is 114.2 km').

accommodationlist :-
      nl,writeln('ACCOMMODATIONS SUGGESTION- - - - - - - - - - -'),nl,
      write('Here are my following accommodation suggestions for'),nl,
      accommodation_sarria(Name, Price),
      write('SARRIA:'),nl,
      write('Name of Accommodation: '), write(Name),nl,
      write('Price: '), write(Price),nl.

accommodationlist2 :-
    nl,write('Here are my following accommodation suggestions for'),nl,
    accommodation_portomarin(Name,Price),
    write('PORTO MARIN:'), nl,
    write('Name of Accommodation: '), write(Name),nl,
    write('Price: '), write(Price),nl.

accommodationlist3 :-
    nl,write('Here are my following accommodation suggestions for'),nl,
    accommodation_palasderei(Name,Price),
    write('Palas De Rei'), nl,
    write('Name of Accommodation: '), write(Name),nl,
    write('Price: '), write(Price),nl.

accomodationlist4 :-
    nl,write('Here are my following accommodation suggestions for'),nl,
    accommodation_arzua(Name,Price),
    write('ARZUA'), nl,
    write('Name of Accommodation: '), write(Name),nl,
    write('Price: '), write(Price),nl.

accommodationlist5 :-
    nl,write('Here are my following accommodation suggestions for'),nl,
    accommodation_opedrouzo(Name,Price),
    write('O PEDROUZO'), nl,
    write('Name of Accommodation: '), write(Name),nl,
    write('Price: '), write(Price),nl.

accommodationlist6 :-
    nl,write('Here are my following accommodation suggestions for'),nl,
    accommodation_santiago(Name,Price),
    write('SANTIAGO DE COMPOSTELA'), nl,
    write('Name of Accommodation: '), write(Name),nl,
    write('Price: '), write(Price),nl.

suggestrestaurant :-
      nl,writeln('RESTAURANT SUGGESTIONS- - - - - - - - - - - -'),nl,
      forall((town(X), restaurant(X,Y)),
      (write('  >In '),write(X),write(', I suggest you go to the restaurant '),
      write(Y),nl)).

suggestweather :-  month(Month, Temp1), season(Season),
    temperature(Season, Temp2), nl,nl,
    write('BEST TIME TO TRAVEL SUGGESTIONS - - - - - -'),nl,nl,
    write('I suggest that you travel during '), write(Season),nl,
    write('which has an average temperature of '),
    write(Temp2),write(' degrees.'),nl,
    write('During this season, I suggested '),
    write('that you go during the month of '),
    write(Month),write('.'),nl,
    write('This month has a temperature of around '),
    write(Temp1),write(' degrees.'), nl,nl.

suggestclothes :- write('CLOTHING SUGGESTIONS- - - - - - - - - - - -'),nl,nl,
    forall(person(Name), (write('For '), write(Name),nl,
    write('It is essential that you bring the clothes listed below: '),nl,
     forall(bring(Name,X),writeln('  '>X)),nl)).

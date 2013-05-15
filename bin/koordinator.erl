%% @author carola
%% @doc @todo Add description to koordinator.

-module(koordinator).
-import(werkzeug, [to_String/1,timeMilliSecond/0,logging/2,get_config_value/2,bestimme_mis/2]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

% Log-Methode
log(Text, Liste) ->
	{ok, Hostname} = inet:gethostname(),
	Filename = io_lib:format("Koordinator@~s.log", [Hostname]),
	logging(Filename, io_lib:format(Text, Liste)).

start() ->
	%Lade Koordinator Konfiguration
	{ok, ConfigListe} = file:consult("koordinator.cfg"),
  	{ok, Arbeitszeit} = get_config_value(arbeitszeit, ConfigListe),
	{ok, Termzeit} = get_config_value(termzeit, ConfigListe),
	{ok, GGTProzessnummer} = get_config_value(ggtprozessnummer, ConfigListe),
	{ok, Nameservicenode} = get_config_value(nameservicenode, ConfigListe),
	{ok, Nameservicename} = get_config_value(nameservicename, ConfigListe),
	{ok, Koordinatorname} = get_config_value(koordinatorname, ConfigListe),
	{ok, Korrigieren} = get_config_value(korrigieren, ConfigListe),
	
	ets:new(koordinator_config, [named_table, public, set, {keypos,1}]),
	
	ets:insert(koordinator_config, {arbeitszeit, Arbeitszeit}),
	ets:insert(koordinator_config, {termzeit, Termzeit}),
	ets:insert(koordinator_config, {ggtprozessnummer, GGTProzessnummer}),
	ets:insert(koordinator_config, {nameservicenode, Nameservicenode}),
	ets:insert(koordinator_config, {nameservicename, Nameservicename}),
	ets:insert(koordinator_config, {koordinatorname, Koordinatorname}),
	ets:insert(koordinator_config, {korrigieren, Korrigieren}),
	
	PID = spawn(fun() -> prepare() end),
	register(Koordinatorname,PID),
	log("~s Startzeit: ~s mit PID ~p ~n", [Koordinatorname, timeMilliSecond(), self()]),
	PID.

% Vorbereitung
prepare() ->	
	%Ping auf den Nameserver
	[{_,Nameservicenode}] = ets:lookup(koordinator_config, nameservicenode),
	net_adm:ping(Nameservicenode),
	timer:sleep(1000),
	% mit Namensdienst verbinden
	[{_,Koordinatorname}] = ets:lookup(koordinator_config, koordinatorname),
	[{_,Nameservicename}] = ets:lookup(koordinator_config, nameservicename),
	io:format("Nameservicename: ~p~n",[Nameservicename]),
	Nameservice = global:whereis_name(Nameservicename),
	io:format("Nameservice: ~p~n",[Nameservice]),
	Nameservice ! {self(),{rebind,Koordinatorname,node()}},
	receive 
		ok -> io:format("..beim Namensdienst gebunden.\n");
        in_use -> io:format("..schon gebunden.\n")
	end,
	initial([], 0, 0).

% Initialisierung (Clients ist die Liste aller registrierten ggT-Prozesse)
initial(Clients, Starter_Anzahl, GGT_Anzahl) ->
	receive
		% Steuernde Werte an Starter uebergeben
		{getsteeringval,Starter} ->
			[{_,Arbeitszeit}] = ets:lookup(koordinator_config, arbeitszeit),
			[{_,Termzeit}] = ets:lookup(koordinator_config, termzeit),
			[{_,GGTProzessnummer}] = ets:lookup(koordinator_config, ggtprozessnummer),
			Starter ! {steeringval,Arbeitszeit,Termzeit,GGTProzessnummer},
			Starter_Anzahl_neu = Starter_Anzahl + 1,
			log("getsteeringval: ~p ~n", [Starter]),
			initial(Clients, Starter_Anzahl_neu, GGT_Anzahl);
		% Client registrieren
		{hello,Clientname} ->
			% Client in der Liste speichern
			Clients_new = Clients ++ [Clientname],
			GGT_Anzahl_neu = GGT_Anzahl + 1,
			log("hello: ~p ~n", [Clientname]),
			initial(Clients_new, Starter_Anzahl, GGT_Anzahl_neu);
		% in Zustand "bereit" wechseln
		step ->
			[{_,GGTProzessnummer}] = ets:lookup(koordinator_config, ggtprozessnummer),
			Alle_GGT_Anzahl = Starter_Anzahl * GGTProzessnummer,
			Vermisst = Alle_GGT_Anzahl - GGT_Anzahl,
			log("Anmeldefrist für ggT-Prozesse abgelaufen. Vermisst werden aktuell ~p ggT-Prozesse.~n", [Vermisst]),
			log("Angemeldet haben sich ~p Starter und ~p ggT-Prozesse.~n",[Starter_Anzahl, GGT_Anzahl]),
			
			% Clients in einem Ring anordnen und Nachbarn bestimmen
			io:format("Clients: ~p~n", [Clients]),
			ring_bilden(Clients),
			log("Ring wurde erstellt. Koordinator geht in den Zustand 'Bereit für Berechnung'.~n", []),
			bereit(Clients, 0, 0)
	end.

% Arbeitsphase (Minimum ist die bisher bekannte kleinste Zahl)
bereit(Clients, Flag, Minimum) ->
	receive
		% ggT-Berechnung mit Wunsch-ggT ausfuehren
		{calc,WggT} ->
			starte_Berechnung(Clients, WggT, Flag);
		% neues Mi empfangen
		{briefmi,{Clientname,CMi,CZeit}} ->
			case (Minimum == 0) or (CMi < Minimum) of
				true ->
					Neues_Minimum = CMi;
				false ->
					Neues_Minimum = Minimum
			end,
			log("~p meldet neues Mi ~p um ~p (~s).~n", [Clientname, CMi, CZeit, timeMilliSecond()]),
			bereit(Clients, Flag, Neues_Minimum);
		% Ergebnis einer ggT-Berechnung empfangen
		{briefterm,{Clientname,CMi,CZeit},From} ->
			case CMi > Minimum of
				% Fehler: Berechnetes Ergebnis ist größer als bisher bekannte kleinste Zahl
				true ->
					case Flag == 0 of
					 	% wenn Flag = 0, nur den Fehler loggen
						true ->
							log("~p meldet falsche Terminierung mit ggT ~p um ~p (~s).~n", [Clientname, CMi, CZeit, timeMilliSecond()]),
						 	bereit(Clients, Flag, Minimum);
						% wenn Flag = 1, sende dem GGT-Prozess die kleinere Zahl per sendy
					 	false ->
							From ! {sendy, Minimum},
						 	bereit(Clients, Flag, Minimum)
					end;
				% Ergebnis ist kleiner/gleich der bisher bekannten Zahl
				false ->
					log("~p meldet Terminierung mit ggT ~p um ~p (~s).~n", [Clientname, CMi, CZeit, timeMilliSecond()]),
					bereit(Clients, Flag, Minimum)
			end;
		% aktuelles Mi bei allen Clients erfragen
		prompt ->
			lists:foreach(fun(X) -> X ! {tellmi, self()} end, Clients),
		 	bereit(Clients, Flag, Minimum);
		% aktuelles Mi empfangen
		{mi, Mi} ->
			log("Aktuelles Mi ~p (~s).~n", [Mi, timeMilliSecond()]),
			bereit(Clients, Flag, Minimum);
		% aktuellen Lebenszustand aller Clients erfragen
		nudge ->
			lists:foreach(fun(X) -> X ! {pingGGT, self()} end, Clients),
		 	bereit(Clients, Flag, Minimum);
		% aktuellen Lebenszustand empfangen
		{pongGGT, GGTname} ->
			log("ggT-Prozess ~p ist lebendig (~s).~n", [GGTname, timeMilliSecond()]),
		 	bereit(Clients, Flag, Minimum);
		% Flag veraendern
		toggle ->
			case Flag == 0 of
				true ->
					Flag_neu = 1,
					log("toggle des Koordinators um ~s: 0 zu 1.~n", [timeMilliSecond()]);
				false ->
					Flag_neu = 0,
					log("toggle des Koordinators um ~s: 1 zu 0.~n", [timeMilliSecond()])
			end,
			bereit(Clients, Flag_neu, Minimum);
		% Neustart des Programms
		reset ->
		 	log("Neustart des Systems (reset) ~n",[]),
			killClients(Clients),
			initial([], 0, 0);
		% Beenden des Programms
		kill ->
		 	log("Beenden des Systems (kill) ~n",[]),
			beenden(Clients)
  	end.

% Beendigung
beenden(Clients) ->
	% Kill-Kommando an alle Clients schicken
	killClients(Clients),
	% Abmelden vom Namensdienst
	Nameservice = global:whereis_name(nameservice),
	Nameservice ! {self(),{unbind, "koordinator"}},
	receive 
        ok -> io:format("..unbind..done.\n")
	end,
	unregister("koordinator").

% Terminierungsnachricht an Clients senden
killClients(Clients) ->
	lists:foreach(fun(X) -> X ! {kill} end, Clients),
	log("Allen ggT-Prozessen ein 'kill' gesendet.~n",[]).

% ggT-Berechnung
starte_Berechnung(Clients, WggT, Flag) ->
	log("Beginne eine neue ggT-Berechnung mit Ziel ~p ~n",[WggT]),
	
	% Startwerte (Mis) bestimmen
	Mis = bestimme_mis(WggT, length(Clients)),
	sende_Mis(Clients, Mis),
	
	% 15% (oder mind. zwei) ggT-Prozesse fuer den Start auswaehlen
	Start_Anzahl = length(Clients) * 15 / 100,
	case Start_Anzahl < 2 of
		true ->
			Start_Prozesse = waehle_Prozesse(Clients, 2),
			Y_Werte = bestimme_mis(WggT, 2);
		false ->
			Start_Prozesse = waehle_Prozesse(Clients, Start_Anzahl),
			Y_Werte = bestimme_mis(WggT, Start_Anzahl)
	end,
	% sende den Startprozessen die y-Werte
	sende_Y_Werte(Start_Prozesse, Y_Werte),
	bereit(Clients, Flag, 0).

% Clients in Ring anordnen und Nachbarn bestimmen
ring_bilden(Clients) -> ring_bilden(Clients, length(Clients)).

ring_bilden(Clients, Index) ->
	io:format("Ring Bilden (~p)",[Index]),
	case Index == 1 of
		true ->
			Client = lists:nth(Index, Clients),
			LeftN = lists:last(Clients),
			RightN = lists:nth(Index+1, Clients),
			Client ! {setneighbors, LeftN, RightN},
			log("ggT-Prozess ~p über linken (~p) und rechten (~p) Nachbarn informiert.",[Client, LeftN, RightN]),
			log("Alle ggT-Prozesse über Nachbarn informiert.",[]);
		false ->
			Client = lists:nth(Index, Clients),
			case Client == lists:last(Clients) of
				true ->
					LeftN = lists:nth(Index-1, Clients),
					RightN = lists:nth(1, Clients);
				false ->
					LeftN = lists:nth(Index-1, Clients),
					RightN = lists:nth(Index+1, Clients)
			end,
			Client ! {setneighbors, LeftN, RightN},
			log("ggT-Prozess ~p über linken (~p) und rechten (~p) Nachbarn informiert.",[Client, LeftN, RightN]),
			ring_bilden(Clients, Index-1)
	end.

% sende ein zufälliges Mi an jeden Client per setpm
sende_Mis(Clients, Mis) ->
	case length(Clients) == 0 of
		true ->
			log("Allen ggT-Prozessen ein initiales Mi gesendet.~n",[]);
		false ->
			Client = lists:last(Clients),
			Mi = lists:nth(random:uniform(length(Mis)), Mis),
			Client ! {setpm, Mi},
			log("ggT-Prozess ~p initiales Mi ~p gesendet.~n",[Client, Mi]),
			Clients_neu = lists:delete(Client, Clients),
			Mis_neu = lists:delete(Mi, Mis),
			sende_Mis(Clients_neu, Mis_neu)
	end.

% sende einen zufälligen Y-Wert an jeden Client per sendy
sende_Y_Werte(Clients,Y_Werte) ->
	case length(Clients) == 0 of
		true ->
			log("Allen ausgewählten ggT-Prozessen ein y gesendet.~n",[]);
		false ->
			Client = lists:last(Clients),
			Y_Wert = lists:nth(random:uniform(length(Y_Werte)), Y_Werte),
			Client ! {sendy, Y_Wert},
			log("ggT-Prozess ~p startendes y ~p gesendet.~n",[Client, Y_Wert]),
			Clients_neu = lists:delete(Client, Clients),
			Y_Werte_neu = lists:delete(Y_Wert, Y_Werte),
			sende_Mis(Clients_neu, Y_Werte_neu)
	end.

% waehle x Prozesse aus allen Clients
waehle_Prozesse(Clients, Anzahl) -> waehle_Prozesse(Clients, [], Anzahl).
waehle_Prozesse(_Clients, Ausgewaehlt, 0) -> Ausgewaehlt;
waehle_Prozesse(Clients, Ausgewaehlt, Anzahl) ->
	Ausgewaehlter_Prozess = lists:nth(random:uniform(length(Clients)), Clients),
	Ausgewaehlt_neu = Ausgewaehlt ++ [Ausgewaehlter_Prozess],
	Clients_neu = lists:delete(Ausgewaehlter_Prozess, Clients),
	waehle_Prozesse(Clients_neu, Ausgewaehlt_neu, Anzahl-1).

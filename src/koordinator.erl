%% @author thathalas
%% @doc @todo Add description to koordinator.


-module(koordinator).
-import(werkzeug, [to_String/1,timeMilliSecond/0,logging/2,get_config_value/2,reset_timer/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

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
	
	%Ping auf den Nameserver
	net_adm:ping(Nameservicenode),
	
	PID = spawn(fun() -> prepare() end),
	register("koordinator",PID),
	PID.

% Vorbereitung
prepare() ->
	% mit Namensdienst verbinden
	Nameservice = global:whereis_name(nameservice),
	Nameservice ! {self(),{bind,"koordinator",node()}},
	receive 
		ok -> io:format("..bind.done.\n");
        in_use -> io:format("..schon gebunden.\n")
	end,
	initial([]).

% Initialisierung (Clients ist die Liste aller registrierten ggT-Prozesse)
initial(Clients) ->
	receive
		% Steuernde Werte an Starter �bergeben
		{getsteeringval,Starter} ->
			[{_,Arbeitszeit}] = ets:lookup(koordinator_config, arbeitszeit),
			[{_,Termzeit}] = ets:lookup(koordinator_config, termzeit),
			[{_,GGTProzessnummer}] = ets:lookup(koordinator_config, ggtprozessnummer),
			Starter ! {steeringval,Arbeitszeit,Termzeit,GGTProzessnummer},
			initial(Clients);
		% Client registrieren
		{hello,Clientname} ->
			% Client in der Liste speichern
			Clients_new = Clients ++ [Clientname],
			initial(Clients_new);
		% in Zustand "bereit" wechseln
		step ->
			% TODO ggT-Prozesse in einem Ring anordnen und Nachbarn bestimmen
			% ({setneighbors,LeftN,RightN})
			Flag = 0,
			bereit(Clients, Flag)
	end.

% Arbeitsphase
bereit(Clients, Flag) ->
	receive
		% ggT-Berechnung mit Wunsch-ggT ausf�hren
		{calc,WggT} ->
			ggtBerechnung(Clients, WggT, Flag);
		% Flag ver�ndern
		toggle ->
			case Flag == 0 of
				true ->
					Flag = 1;
				false ->
					Flag = 0
			end;
		% Neustart des Programms
		reset ->
			killClients(Clients),
			initial([]);
		% Beenden des Programms
		kill ->
			beenden(Clients)
  	end.

% Beendigung
beenden(Clients) ->
	% Kill-Kommando an alle Clients schicken
	killClients(Clients),
	% Abmelden vom Namensdienst
	Nameservice ! {self(),{unbind, "koordinator"}},
	receive 
        ok -> io:format("..unbind..done.\n")
	end,
	unregister("koordinator").

% Terminierungsnachricht an Clients senden
killClients(Clients) ->
	lists:foreach(fun(X) -> X ! {kill}, Clients).

% ggT-Berechnung
ggtBerechnung(Clients, WggT, Flag) ->
	% Startwerte (Mis) bestimmen
	Mis = bestimme_mis(WggT, length(Clients)),
	% TODO Mis an die ggT-Prozesse verteilen (setpm)
	% TODO 15% (oder mind. zwei) ggT-Prozesse f�r den Start ausw�hlen (sendy)
	receive
		% neues Mi empfangen
		{briefmi,{Clientname,CMi,CZeit}} ->
			% TODO
			ok;
		% Ergebnis einer ggT-Berechnung empfangen
		{briefterm,{Clientname,CMi,CZeit},From} ->
			% TODO
			bereit(Clients, Flag);
		% aktuelles Mi bei allen Clients erfragen
		prompt ->
			lists:foreach(fun(X) -> X ! {tellmi, self()}, Clients);
		% aktuelles Mi empfangen
		{mi, Mi} ->
			% TODO logging
			ok;
		% aktuellen Lebenszustand aller Clients erfragen
		nudge ->
			lists:foreach(fun(X) -> X ! {pingGGT, self()}, Clients);
		% aktuellen Lebenszustand empfangen
		{pongGGT, GGTname} ->
			% TODO logging
	end.

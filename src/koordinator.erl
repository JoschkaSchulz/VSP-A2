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
		{getsteeringval,Starter} ->
			[{_,Arbeitszeit}] = ets:lookup(koordinator_config, arbeitszeit),
			[{_,Termzeit}] = ets:lookup(koordinator_config, termzeit),
			[{_,GGTProzessnummer}] = ets:lookup(koordinator_config, ggtprozessnummer),
			Starter ! {steeringval,Arbeitszeit,Termzeit,GGTProzessnummer},
			initial(Clients);
		{hello,Clientname} ->
			% Client in der Liste speichern
			Clients_new = Clients ++ [Clientname],
			% loggen (Rechner muss glaub ich ersetzt werden) und hier fehlt noch was
			logging("Koordinator@Rechner.log", io_lib:format("hello: " ++ Clientname,[])),
			initial(Clients_new);
		{bereit} ->
			% ggT-Prozesse in einem Ring anordnen
			bereit(Clients)
	end.

% Arbeitsphase
bereit(Clients) ->
	receive
		{ggtberechnung} ->
			ggtBerechnung()
  	end.

% eine ggT-Berechnung
ggtBerechnung() ->
	0.

% Beendigung
beenden() ->
	0.

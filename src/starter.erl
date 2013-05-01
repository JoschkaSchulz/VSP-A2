%% @author thathalas
%% @doc @todo Add description to starter.


-module(starter).
-import(werkzeug,[get_config_value/2]).
-import(ggt,[start/7]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
	
	%Lade ggT-Prozess Konfiguration
	{ok, ConfigListe} = file:consult("ggt.cfg"),
  	{ok, Praktikumsgruppe} = get_config_value(praktikumsgruppe, ConfigListe),
	{ok, Teamnummer} = get_config_value(teamnummer, ConfigListe),
	{ok, Nameservicenode} = get_config_value(nameservicenode, ConfigListe),
	{ok, Nameservicename} = get_config_value(nameservicename, ConfigListe),
	{ok, Koordinatorname} = get_config_value(koordinatorname, ConfigListe),
	
	ets:new(ggt_config, [named_table, public, set, {keypos,1}]),
	
	ets:insert(ggt_config, {praktikumsgruppe, Praktikumsgruppe}),
	ets:insert(ggt_config, {teamnummer, Teamnummer}),
	ets:insert(ggt_config, {nameservicenode, Nameservicenode}),
	ets:insert(ggt_config, {nameservicename, Nameservicename}),
	ets:insert(ggt_config, {koordinatorname, Koordinatorname}),
	
	%Ping auf den Nameserver
	net_adm:ping(Nameservicenode),
	
	PID = spawn(fun() -> starter() end),
	register("starter",PID),
	PID.

starter() ->
	Nameservice = global:whereis_name(nameservice),						%Namenservice holen
	[{_,Koordinatorname}] = ets:lookup(ggt_config, koordinatorname),	%Aus der Config den Koordinatornamen holen
	Nameservice ! {self(),{lookup,Koordinatorname}},					%Nach dem Node des Koordinators fragen
	receive 
	        not_found -> io:format("..meindienst..not_found.\n"); 		%Wenn nicht gefunden
	        {pin,{Name,Node}} -> 
				Koordinator = {Node,Name},								%Speichere den Koordinator
				io:format("...ok: {~p,~p}.\n",[Name,Node])				%Wenn gefunden
	end,
	
	Koordinator ! getsteeringval,										%Sende Nachfrage an Koordinator
	
	receive
		{steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer} ->			%Empfange Daten vom Koordinator
			io:format("...ok.\n",[Name,Node])
	end,
	
	[{_,Praktikumsgruppe}] = ets:lookup(ggt_config, praktikumsgruppe),
	[{_,Teamnummer}] = ets:lookup(ggt_config, teamnummer),
	ggt:start(GGTProzessnummer,ArbeitsZeit,TermZeit, Praktikumsgruppe, Teamnummer, Nameservice, Koordinator).

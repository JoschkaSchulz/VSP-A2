
%% @author thathalas
%% @doc @todo Add description to starter.


-module(starter).
-import(werkzeug,[get_config_value/2]).
-import(ggt,[start/8]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start(Starternummer) ->
	
	%TODO: ets darf nicht doppelt geladen werden beim 2. starter!
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
	
	PID = spawn(fun() -> starter(Starternummer) end),
	register("starter",PID),
	PID.

starter(Starternummer) ->
	Nameservice = global:whereis_name(nameservice),						%Namenservice holen
	[{_,Koordinatorname}] = ets:lookup(ggt_config, koordinatorname),	%Aus der Config den Koordinatornamen holen
	Nameservice ! {self(),{lookup,Koordinatorname}},					%Nach dem Node des Koordinators fragen
	receive 
	        not_found -> io:format("..meindienst..not_found.\n"); 		%Wenn nicht gefunden
	        {pin,{Name,Node}} -> 
				Koordinator = {Node,Name},								%Speichere den Koordinator
				io:format("...ok: {~p,~p}.\n",[Name,Node]),				%Wenn gefunden
				Koordinator ! getsteeringval,							%Sende Nachfrage an Koordinator
				receive
					{steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer} ->	%Empfange Daten vom Koordinator
						io:format("...ok{~p~p}.\n",[Name,Node]),
						[{_,Praktikumsgruppe}] = ets:lookup(ggt_config, praktikumsgruppe),
						[{_,Teamnummer}] = ets:lookup(ggt_config, teamnummer),
						starte_prozesse(Praktikumsgruppe, Teamnummer,Starternummer,GGTProzessnummer,ArbeitsZeit,TermZeit, Nameservice, Koordinator)
				end
	end.

starte_prozesse(Praktikumsgruppe, Teamnummer,Starternummer,GGTProzessnummer,ArbeitsZeit,TermZeit, Nameservice, Koordinator) ->
	case GGTProzessnummer > 0 of
		%Solange noch nicht alle Prozesse gestartet sind
		true ->
			%Stubse den Start des ggT Prozess an
			ggt:start(Praktikumsgruppe, Teamnummer,Starternummer,GGTProzessnummer,ArbeitsZeit,TermZeit, Nameservice, Koordinator),
			%Wieder hole solange noch weitere Prozesse gestartet werden müssen
			starte_prozesse(Praktikumsgruppe, Teamnummer,Starternummer,GGTProzessnummer-1,ArbeitsZeit,TermZeit, Nameservice, Koordinator);
		false ->
			ok
	end.
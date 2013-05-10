
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
	
	%Ping auf den Nameserver
	net_adm:ping(Nameservicenode),
	
	starter(Starternummer,Koordinatorname,Praktikumsgruppe,Teamnummer).
	
	%PID = spawn(fun() -> starter(Starternummer) end),
	%register("starter",PID),
	%PID.

starter(Starternummer,Koordinatorname,Praktikumsgruppe,Teamnummer) ->
	Nameservice = global:whereis_name(nameservice),						%Namenservice holen
	Nameservice ! {self(),{lookup,Koordinatorname}},					%Nach dem Node des Koordinators fragen
	receive 
	        not_found -> io:format("..meindienst..not_found.\n"); 		%Wenn nicht gefunden
	        {pin,{Name,Node}} -> 
				Koordinator = {Node,Name},								%Speichere den Koordinator
				io:format("...ok: {~p,~p}.\n",[Name,Node]),				%Wenn gefunden
				Koordinator ! {getsteeringval,self()},							%Sende Nachfrage an Koordinator
				receive
					{steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer} ->	%Empfange Daten vom Koordinator
						io:format("...ok{~p~p}.\n",[Name,Node]),
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
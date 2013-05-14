
%% @author thathalas
%% @doc @todo Add description to starter.


-module(starter).
-import(werkzeug,[get_config_value/2, to_String/1, logging/2, timeMilliSecond/0]).
-import(ggt,[start/8]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start(Starternummer) ->
	%Logge das Starten
	{ok, Hostname} = inet:gethostname(),
	Filename = io_lib:format("ggtStarter_~p@~s.log", [Starternummer,Hostname]),
	logging(Filename, io_lib:format("ggtStarter_~p@~s Startzeit: ~s mit PID ~p~n", [Starternummer,Hostname,timeMilliSecond(),self()])),
	
	
	%Lade ggT-Prozess Konfiguration
	{ok, ConfigListe} = file:consult("ggt.cfg"),
  	{ok, Praktikumsgruppe} = get_config_value(praktikumsgruppe, ConfigListe),
	{ok, Teamnummer} = get_config_value(teamnummer, ConfigListe),
	{ok, Nameservicenode} = get_config_value(nameservicenode, ConfigListe),
	{ok, Nameservicename} = get_config_value(nameservicename, ConfigListe),
	{ok, Koordinatorname} = get_config_value(koordinatorname, ConfigListe),
	
	%Logge das config lesen
	logging(Filename, io_lib:format("ggt.cfg gelesen", [])),
	
	%Ping auf den Nameserver
	net_adm:ping(Nameservicenode),
	
	starter(Starternummer,Koordinatorname,Praktikumsgruppe,Teamnummer).

starter(Starternummer,Koordinatorname,Praktikumsgruppe,Teamnummer) ->
	Nameservice = global:whereis_name(nameservice),						%Namenservice holen
	
	%Logge das Starten
	{ok, Hostname} = inet:gethostname(),
	Filename = io_lib:format("ggtStarter_~p@~s.log", [Starternummer,Hostname]),
	logging(Filename, io_lib:format("Nameservice gebunden", [])),
	
	
	Nameservice ! {self(),{lookup,Koordinatorname}},					%Nach dem Node des Koordinators fragen
	receive 
	        not_found -> io:format("..meindienst..not_found.\n"); 		%Wenn nicht gefunden
	        {pin,{Name,Node}} -> 
				Koordinator = {Node,Name},								%Speichere den Koordinator

				%Logge die verbindung mit dem Koordinator
				logging(Filename, io_lib:format("Koordinator (~s) gebunden", [Koordinatorname])),
				
				io:format("...ok: {~p,~p}.\n",[Name,Node]),				%Wenn gefunden
				Koordinator ! {getsteeringval,self()},							%Sende Nachfrage an Koordinator
				receive
					{steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer} ->	%Empfange Daten vom Koordinator
						io:format("...ok{~p~p}.\n",[Name,Node]),
						
						%Logge erhaltene Informationen
						logging(Filename, io_lib:format("getsteeringval: ~p Arbeitszeit, ~p Terminierungszeit, ~p Anzahl der Prozesse", [ArbeitsZeit,TermZeit,GGTProzessnummer])),
				
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
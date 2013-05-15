%% @author thathalas
%% @doc @todo Add description to ggt.


-module(ggt).
-import(werkzeug,[get_config_value/2, timeMilliSecond/0, reset_timer/3, logging/2, to_String/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/8]).



%% ====================================================================
%% Internal functions
%% ====================================================================

log(Text, Liste, GGTname) ->
	%Logge das Starten
	{ok, Hostname} = inet:gethostname(),
	Filename = io_lib:format("GGTP_~p@~s.log", [GGTname,Hostname]),
	logging(Filename, io_lib:format(Text ++ "~n", Liste)).
	

start(Praktikumsgruppe, Teamnummer,Starternummer,GGTProzessnummer,ArbeitsZeit,TermZeit, Nameservice, Koordinator) ->
	%Erstelle den Namen aus den übergebenen Parametern
	GGTname = list_to_atom(lists:flatten(io_lib:format("~p~p~p~p", [Praktikumsgruppe, Teamnummer,GGTProzessnummer,Starternummer]))),
	
	%Starte einen neuen Prozess
	PID = spawn(fun() -> prozess_start(GGTname, Nameservice, Koordinator,ArbeitsZeit,TermZeit) end),
	
	log("~p Startzeit: ~s mit PID ~p~n", [GGTname,timeMilliSecond(),PID],GGTname),
	
	%Regestriere auf der lokalen Node
	register(GGTname,PID),
	
	%Gebe die PID zurück
	PID.
	
%Meldet den Prozess überall an und macht ihn erst benutzbar
prozess_start(GGTname, Nameservice, Koordinator,ArbeitsZeit,TermZeit) ->
	%Sich beim Nameservice mit seinen Namen anmelden
	Nameservice ! {self(),{rebind,GGTname,node()}},
	receive 
		ok -> log("Beim Nameservice gebunden",[],GGTname)
	end,
	
	%Sich beim Koordinator mit seinen Namen anmelden
	Koordinator ! {hello, GGTname},
	
	%Bevor es los geht auf die Nachbarn warten
	receive
		%Setzt den linken und rechten Nachbarn
		{setneighbors,LeftN,RightN} ->		
			%Logge die anmeldung beim Koordinator, erst nach erfolgreicher Verbindung
			log("Beim Koordinator mit 'hello' gemeldet.",[],GGTname),
	
			%Logge die Nachbarn
			log("Linker Nachbar ~p gebunden",[LeftN],GGTname),
			log("Rechter Nachbar ~p gebunden",[RightN],GGTname),
			
			%Warte auf erstes Mi
			neues_mi(GGTname,LeftN,RightN, Koordinator,ArbeitsZeit,TermZeit)
	end.

%Funktion zum Warten auf erstes Mi
neues_mi(GGTname,LeftN,RightN, Koordinator,ArbeitsZeit,TermZeit) ->
	%Warte auf erstes Mi
	receive
			{setpm,MiNeu} ->
				%Logge neues Mi
				log("Erstes Mi (~p) erhalten durch setpm",[MiNeu],GGTname),
				
				%Start des Terminierungstimer
				{ok,TermTimerRef} = timer:send_after(TermZeit, abstimmungsstart),
				
				%Starte den loop nachdem die Nachbarn angekommen sind
				loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,get_seconds())
	end.

get_seconds() ->
	{Mega,Seconds,Micro} = now(),
	(Mega * 1000000) + Seconds + (Micro / 1000000). 

rechne_ggt(Mi,Y, RightN, LeftN, ArbeitsZeit) ->
	
	%Warte vor der rechnung eine gewisse Zeit
	timer:sleep(ArbeitsZeit*1000),
	
	case Y < Mi of 
    	true -> 
			% Modulo in Erlang = "rem"
			NeuMi = (Mi-1 rem Y)+1,
			
			%Sende das neue Mi an die Nachbarn
         	RightN ! {sendy,NeuMi},
			LeftN ! {sendy,NeuMi};
		false ->
			%Bei false lasse das Mi wie es ist
			NeuMi = Mi
	end,

	%Gebe das Mi zurück
	NeuMi.

loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,TimerLastNumber) ->
	receive
		%1. Mi in andere Funktion ausgelagert, gut oder schlecht?
		{setpm,MiNeu} ->					%Erhalte Mi
			%Logge neues Mi
			log("Neues Mi (~p) erhalten durch setpm",[MiNeu],GGTname),
			
			%Starte Terminierungstimer neu
			NeuTermTimerRef = reset_timer(TermTimerRef,TermZeit,abstimmungsstart),
						
			%Neue berechnung mit neuem Mi
			loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,NeuTermTimerRef,get_seconds());
		{sendy,Y} ->						%Erhalte Y			
			%Starte Terminierungstimer neu
			NeuTermTimerRef = reset_timer(TermTimerRef,TermZeit,abstimmungsstart),
			
			%Überprüfe und Rechne neues Mi
			MiNeu = rechne_ggt(Mi, Y, RightN, LeftN, ArbeitsZeit),
			
			%Hat sich das Mi verändert?
			case Mi == MiNeu of 
				true ->				  
				  	%Logge sendy
					log("sendy: ~p (~p); keine Berechnung ~s",[Y,Mi,timeMilliSecond()],GGTname),
				  
				  	%Führe Schleife weiter aus
					loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,NeuTermTimerRef,get_seconds());
			  	false ->
				  	%Logge sendy
					log("sendy: ~p (~p); berechnet als neues Mi ~p ~s",[Y,Mi,MiNeu,timeMilliSecond],GGTname),
				  
				  	%Informiere den Koordinator darüber
				  	Koordinator ! {briefmi,{GGTname,MiNeu,timeMilliSecond()}},	
				  
				  	%Führe Schleife weiter aus
					loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,NeuTermTimerRef,get_seconds())
			end;
		abstimmungsstart ->					%Start des Terminierungsvotings
		  	%initiiere neue Abstimmung
			log("~p: initiiere eine Terminierungsabstimmung (~p). ~s",[GGTname,Mi,timeMilliSecond()],GGTname),
		  
		  	%Starten einer Abstimmung mit senden an den Rechten Nachbarn
			RightN ! {abstimmung,GGTname},
		  
		  	%Führe Schleife weiter aus
			loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,TimerLastNumber);
		{abstimmung,Initiator} ->			%Terminierungs Voting
		  	case Initiator == GGTname of
			  	true ->
				  	%Informiere den Koordinator
					Koordinator ! {briefterm,{GGTname,Mi,timeMilliSecond()},self()},
				  
				  	%initiiere neue Abstimmung
					log("~p: melde Koordinator erfolgreiche Terminierungs Abstimmung (~p). ~s",[GGTname,Mi,timeMilliSecond()],GGTname),
				  
				  	%Gehe zurück in den loop
				  	loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,TimerLastNumber);
				false->
				  	%Wenn die letzte Nachricht größer ist als termZeit/2
				  	case TimerLastNumber > TermZeit/2 of
						true ->
						  	%Wenn es der Fall ist Sende die Abstimmung weiter
						  	RightN ! {abstimmung,Initiator},
					 
					   		%initiiere neue Abstimmung
							log("~p: stimme ab (~p) mit >JA< und leite weiter. ~s",[GGTname,Mi,timeMilliSecond()],GGTname),
					   
					   		%Gehe zurück in die Schleife
						  	loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,TimerLastNumber);
					 	false ->
					   		%initiiere neue Abstimmung
							log("~p: stimme ab (~p) mit >NEIN<. ~s",[GGTname,Mi,timeMilliSecond()],GGTname),
					   
					   		%Ansonsten ignoriere die Anfrage und gehe zurück zur Schleife
						  	loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,TimerLastNumber)
					end
			end;
		{tellmi,From} ->					%Sendet das aktuelle Mi zurück
			%Sende das Mi an From zurück
			From ! {mi,Mi};
		{pingGGT,From} ->					%Sendet ein pong zurück
			%Sende ein pong an "From" zurück
			From ! {pongGGT,GGTname};
		kill ->								%Beendet den ggT Prozess
			%Nameservice suchen
	   		Nameservice = global:whereis_name(nameservice),	
	   
	   		%Vom Nameservice abmelden
	  	 	Nameservice ! {self(),{unbind,GGTname}},
			receive 
			        ok -> log("Downtime ~s vom Client ~p",[timeMilliSecond(),GGTname],GGTname)
			end,
			unregister(GGTname),
			ende
	end.
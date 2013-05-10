%% @author thathalas
%% @doc @todo Add description to ggt.


-module(ggt).
-import(werkzeug,[get_config_value/2, timeMilliSecond/0, reset_timer/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/8]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start(Praktikumsgruppe, Teamnummer,Starternummer,GGTProzessnummer,ArbeitsZeit,TermZeit, Nameservice, Koordinator) ->
	%Erstelle den Namen aus den �bergebenen Parametern
	GGTname = io_lib:format("~p~p~p~p", [Praktikumsgruppe, Teamnummer,GGTProzessnummer,Starternummer]),
	
	%TODO: Die l�nge der Arbeitszeit mit �bergeben
	
	%Starte einen neuen Prozess
	PID = spawn(fun() -> prozess_start(GGTname, Nameservice, Koordinator,ArbeitsZeit,TermZeit) end),
	
	%Regestriere auf der lokalen Node
	register(PID,GGTname),
	
	%Gebe die PID zur�ck
	PID.
	
%Meldet den Prozess �berall an und macht ihn erst benutzbar
prozess_start(GGTname, Nameservice, Koordinator,ArbeitsZeit,TermZeit) ->
	%Sich beim Nameservice mit seinen Namen anmelden
	Nameservice ! {self(),{rebind,GGTname,node()}},
	receive 
		ok -> io:format("..rebind.done.\n")
	end,
	
	%Sich beim Koordinator mit seinen Namen anmelden
	Koordinator ! {hello, GGTname},
	
	%Bevor es los geht auf die Nachbarn warten
	receive
		%Setzt den linken und rechten Nachbarn
		{setneighbors,LeftN,RightN} ->		
			
			%Warte auf erstes Mi
			neues_mi(GGTname,LeftN,RightN, Koordinator,ArbeitsZeit,TermZeit)
	end.

%Funktion zum Warten auf erstes Mi
neues_mi(GGTname,LeftN,RightN, Koordinator,ArbeitsZeit,TermZeit) ->
	%Warte auf erstes Mi
	receive
			{setpm,MiNeu} ->
				%Start des Terminierungstimer
				{ok,TermTimerRef} = timer:send_after(TermZeit, abstimmungsstart),
				
				%Starte den loop nachdem die Nachbarn angekommen sind
				loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,get_seconds())
	end.

get_seconds() ->
	{Mega,Seconds,Micro} = now(),
	(Mega * 1000000) + Seconds + (Micro / 1000000). 

rechne_ggt(Mi,Y, RightN, LeftN, Koordinator, ArbeitsZeit) ->
	
	%Warte vor der rechnung eine gewisse Zeit
	timer:sleep(ArbeitsZeit),
	
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

	%Gebe das Mi zur�ck
	NeuMi.

loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,TimerLastNumber) ->
	receive
		%1. Mi in andere Funktion ausgelagert, gut oder schlecht?
		{setpm,MiNeu} ->					%Erhalte Mi
			%Starte Terminierungstimer neu
			NeuTermTimerRef = reset_timer(TermTimerRef,TermZeit,abstimmungsstart),
						
			%Neue berechnung mit neuem Mi
			loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,NeuTermTimerRef,get_seconds());
		{sendy,Y} ->						%Erhalte Y
			%TODO: Testen was passiert wenn grade kein Timer vorhanden ist
			%Starte Terminierungstimer neu
			NeuTermTimerRef = reset_timer(TermTimerRef,TermZeit,abstimmungsstart),
			
			%�berpr�fe und Rechne neues Mi
			MiNeu = rechne_ggt(Mi, Y, RightN, LeftN, Koordinator, ArbeitsZeit),
			
			%Hat sich das Mi ver�ndert?
			case Mi == MiNeu of 
				true ->				  
				  	%F�hre Schleife weiter aus
					loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,NeuTermTimerRef,get_seconds());
			  	false ->
				  	%Informiere den Koordinator dar�ber
				  	Koordinator ! {briefmi,{GGTname,MiNeu,timeMilliSecond()}},	
				  
				  	%F�hre Schleife weiter aus
					loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,NeuTermTimerRef,get_seconds())
			end;
		abstimmungsstart ->					%Start des Terminierungsvotings
		  	%Starten einer Abstimmung mit senden an den Rechten Nachbarn
			RightN ! {abstimmung,GGTname},
		  
		  	%F�hre Schleife weiter aus
			loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,TimerLastNumber);
		{abstimmung,Initiator} ->			%Terminierungs Voting
			%TODO: Erhalte eine Terminierungsabstimmung

		  	case Initiator == GGTname of
			  	true ->
				  	%Informiere den Koordinator
					Koordinator ! {briefterm,{GGTname,Mi,timeMilliSecond()},self()},
				  
				  	%Gehe zur�ck in den loop
				  	loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,TimerLastNumber);
				false->
				  	%Wenn die letzte Nachricht gr��er ist als termZeit/2
				  	case TimerLastNumber > TermZeit/2 of
						true ->
						  	%Wenn es der Fall ist Sende die Abstimmung weiter
						  	RightN ! {abstimmung,Initiator},
					   
					   		%...und teile es dem Koordinator mit (steht noch in Frage?!? TODO)
							Koordinator ! {briefterm,{GGTname,Mi,timeMilliSecond()},self()},
					   
					   		%Gehe zur�ck in die Schleife
						  	loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,TimerLastNumber);
					 	false ->
					   		%Ansonsten ignoriere die Anfrage und gehe zur�ck zur Schleife
						  	loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef,TimerLastNumber)
					end
			end;
		{tellmi,From} ->					%Sendet das aktuelle Mi zur�ck
			%Sende das Mi an From zur�ck
			From ! {mi,Mi};
		{pingGGT,From} ->					%Sendet ein pong zur�ck
			%Sende ein pong an "From" zur�ck
			From ! {pongGGT,GGTname};
		kill ->								%Beendet den ggT Prozess
			%Nameservice suchen
	   		Nameservice = global:whereis_name(nameservice),	
	   
	   		%Vom Nameservice abmelden
	  	 	Nameservice ! {self(),{unbind,GGTname}},
			receive 
			        ok -> io:format("..unbind..done.\n")
			end,
			unregister(GGTname),
			ende
	end.
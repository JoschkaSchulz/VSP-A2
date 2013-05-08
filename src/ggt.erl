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
	%Erstelle den Namen aus den übergebenen Parametern
	GGTname = io_lib:format("~p~p~p~p", [Praktikumsgruppe, Teamnummer,GGTProzessnummer,Starternummer]),
	
	%TODO: Die länge der Arbeitszeit mit übergeben
	
	%Starte einen neuen Prozess
	PID = spawn(fun() -> prozess_start(GGTname, Nameservice, Koordinator,ArbeitsZeit,TermZeit) end),
	
	%Regestriere auf der lokalen Node
	register(PID,GGTname),
	
	%Gebe die PID zurück
	PID.
	
%Meldet den Prozess überall an und macht ihn erst benutzbar
prozess_start(GGTname, Nameservice, Koordinator,ArbeitsZeit,TermZeit) ->
	%Sich beim Koordinator mit seinen Namen anmelden
	Koordinator ! {hello, GGTname},
	
	%Sich beim Nameservice mit seinen Namen anmelden
	Nameservice ! {self(),{rebind,GGTname,node()}},
	receive 
		ok -> io:format("..rebind.done.\n")
	end,
	
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
			{setpn,MiNeu} ->
				%Start des Terminierungstimer
				{ok,TermTimerRef} = timer:send_after(TermZeit, abstimmungsstart),
				
				%Starte den loop nachdem die Nachbarn angekommen sind
				loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef)
	end.

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

	%Gebe das Mi zurück
	NeuMi.

loop(GGTname,LeftN,RightN,Mi, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef) ->
	receive
		%1. Mi in andere Funktion ausgelagert, gut oder schlecht?
		{setpm,MiNeu} ->					%Erhalte Mi
			%Neue berechnung mit neuem Mi
			loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,TermTimerRef);
		{sendy,Y} ->						%Erhalte Y
			%Starte Terminierungstimer neu
			NeuTermTimerRef = reset_timer(TermTimerRef,TermZeit,abstimmungsstart),
			
			%Überprüfe und Rechne neues Mi
			MiNeu = rechne_ggt(Mi, Y, RightN, LeftN, Koordinator, ArbeitsZeit),
			
			%Hat sich das Mi verändert?
			case Mi == MiNeu of 
				true ->
				  	%Führe Schleife weiter aus
					loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,NeuTermTimerRef);
			  	false ->
				  	%Informiere den Koordinator darüber
				  	Koordinator ! {briefmi,{GGTname,MiNeu,timeMilliSecond()}},	
				  
				  	%Führe Schleife weiter aus
					loop(GGTname,LeftN,RightN,MiNeu, Koordinator,ArbeitsZeit,TermZeit,NeuTermTimerRef)
			end;
		abstimmungsstart ->
		  	ok;
		{abstimmung,Initiator} ->			%Terminierungs Voting
			%TODO: Erhalte eine Terminierungsabstimmung
			ok;
		{tellmi,From} ->					%Sendet das aktuelle Mi zurück
			%Sende das Mi an From zurück
			From ! {mi,Mi};
		{pingGGT,From} ->					%Sendet ein pong zurück
			%Sende ein pong an "From" zurück
			From ! {pongGGT,GGTname};
		kill ->								%Beendet den ggT Prozess
			ende
	end.
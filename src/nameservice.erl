-module(nameservice).
-export([start/0]).

%% Namensdienst wird global bei allen Erlang nodes unter nameservice registriert.
start() ->
	{ok, HostName} = inet:gethostname(),
	Datei = lists:concat(["nameservice@",HostName,".log"]),	
	ServerPid = spawn(fun() -> loop(dict:new(),Datei) end),
	global:register_name(nameservice,ServerPid),
    Zeit = lists:concat(["Nameservice Startzeit: ",werkzeug:timeMilliSecond()]),
	Inhalt = lists:concat([Zeit," mit PID ",pid_to_list(ServerPid)," registriert mit Namen 'nameservice'.\r\n"]),
	werkzeug:logging(Datei,Inhalt).

loop(Dict,Datei) ->
	receive
		{From,{lookup,Name}} when is_pid(From) ->
			Inhalt1 = lists:concat(["lookup um ",werkzeug:timeMilliSecond()," von ",pid_to_list(From),":"]),
			case dict:find(Name,Dict) of
				{ok,Node} ->
					Inhalt = lists:concat([Inhalt1,"{",Name,",",Node,"}.\n"]),
					From ! {pin,{Name,Node}};
				error ->
				    SName = werkzeug:to_String(Name),
					Inhalt = lists:concat([Inhalt1,"not_found:",SName,".\n"]),
					From ! not_found
			end,		
			werkzeug:logging(Datei,Inhalt),
			
			loop(Dict,Datei);
		{From,{bind,Name,Node}} when is_pid(From) ->
		    SName = werkzeug:to_String(Name),
			Inhalt1 = lists:concat(["bind um ",werkzeug:timeMilliSecond()," von ",pid_to_list(From)," (",SName,"):"]),
			case dict:find(Name,Dict) of
				{ok,Node} ->
					Inhalt = lists:concat([Inhalt1,"in_use ({",SName,",",Node,"}.\n"]),
					DictNew = Dict,
					From ! in_use;
				error ->
					Inhalt = lists:concat([Inhalt1,"{",SName,",",Node,"}.\n"]),
					DictNew = dict:store(Name, Node, Dict),
					From ! ok
			end,		
			werkzeug:logging(Datei,Inhalt),
			
			loop(DictNew,Datei);
		{From,{rebind,Name,Node}} when is_pid(From) ->
		    SName = werkzeug:to_String(Name),
			DictNew = dict:store(Name, Node, Dict),
			Inhalt = lists:concat(["rebind um ",werkzeug:timeMilliSecond()," von ",pid_to_list(From),": {",SName,",",Node,"}.\n"]),
			From ! ok,
			werkzeug:logging(Datei,Inhalt),
			
			loop(DictNew,Datei);
		{From,{unbind,Name}} when is_pid(From) ->
		    SName = werkzeug:to_String(Name),
			DictNew = dict:erase(Name, Dict),
			Inhalt = lists:concat(["unbind um ",werkzeug:timeMilliSecond()," von ",pid_to_list(From),":",SName,".\n"]),
			From ! ok,
			werkzeug:logging(Datei,Inhalt),

			loop(DictNew,Datei);
		{From,listall} when is_pid(From) ->
			Inhalt = lists:concat(["listall um ",werkzeug:timeMilliSecond()," von ",pid_to_list(From),".\n"]),
			List = dict:to_list(Dict),
			From ! {list,List},
			werkzeug:logging(Datei,Inhalt),

			loop(Dict,Datei);
		{From,reset} when is_pid(From) ->
			Inhalt = lists:concat(["reset um ",werkzeug:timeMilliSecond()," von ",pid_to_list(From),".\n"]),
			lib:flush_receive(),
			From ! ok,
			werkzeug:logging(Datei,Inhalt),

			loop(dict:new(),Datei);
		{From,kill} when is_pid(From) ->
			Inhalt = lists:concat(["kill um ",werkzeug:timeMilliSecond()," von ",pid_to_list(From),".\n"]),
			From ! ok,
			werkzeug:logging(Datei,Inhalt);
		Any -> 
		    Inhalt = "in loop unerwartete Nachricht erhalten."++werkzeug:to_String(Any)++"\r\n",
			werkzeug:logging(Datei,Inhalt),	

			loop(Dict,Datei)			
	end.
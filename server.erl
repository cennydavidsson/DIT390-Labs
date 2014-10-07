-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, Pid, Nick}) ->

    case lists:keymember(Nick, 1, St#server_st.clients) of
		false ->
	    	{ok, St#server_st { clients = St#server_st.clients ++ [{Nick, Pid}] }};
		_ ->
	    	{{error, user_already_connected, "User is already connected."}, St}
	end;

loop(St, {disconnect, Pid, Nick}) ->
	io:format("~p", [St#server_st.clients]),
    case lists:keymember(Pid, 2, St#server_st.clients) of
     	false ->
	  		{{error, user_not_connected, "User is not connected to the server."}, St};
      	_ ->
	  		{ok, St#server_st {clients = lists:delete({Nick,Pid}, St#server_st.clients)}}
    end;

loop(St, {join, Pid, Channel}) ->
    case (lists:member(Channel, St#server_st.channels)) of
	true ->
	    {genserver:request(list_to_atom(Channel), {join, Pid}), St };
	false ->
	    genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:loop/2),
	    {genserver:request(list_to_atom(Channel), {join, Pid}), St#server_st { channels = St#server_st.channels ++ [Channel] }}
    end;
    
    
    
loop(St, _Msg) -> 
      {ok, St}.


initial_state(_Server) ->
    #server_st{ name = _Server}.
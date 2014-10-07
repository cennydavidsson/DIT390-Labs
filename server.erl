-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, Pid, Nick}) ->
    case {lists:keyfind(Nick, 1, St#server_st.clients)} of
	{false} ->
	    NewSt = St#server_st { clients = St#server_st.clients ++ [{Nick, Pid}] },
	    Result = ok;
	{_} ->
	    Result = {error, user_already_connected, "User is already connected."},
	    NewSt = St
	end,
    
    {Result, NewSt};

loop(St, {disconnect, Pid, Nick}) ->
    case {lists:keyfind(Pid, 2, St#server_st.clients)} of
      {false} ->
	  Result = {error, user_not_connected, "User is not connected to the server."},
	  NewSt = St;
      {_} ->
	  NewSt = St#server_st {clients = lists:delete({Nick,Pid}, St#server_st.clients) },
	  Result = ok
      end,
      
    {Result, NewSt};

loop(St, {join, Pid, Channel}) ->
    case (lists:member(Channel, St#server_st.channels)) of
	true ->
	    {genserver:request(list_to_atom(Channel), {join, Pid}), St };
	false ->
	    genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:loop/2),
	    Result = genserver:request(list_to_atom(Channel), {join, Pid}),
	    NewSt = St#server_st { channels = St#server_st.channels ++ [Channel] },
	    {Result, NewSt}
    end;
    
    
    
loop(St, _Msg) -> 
      {ok, St}.


initial_state(_Server) ->
    #server_st{ name = _Server}.
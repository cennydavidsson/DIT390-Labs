-module(channel).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {join, Pid}) ->
    case (lists:member(Pid, St#channel_st.clients)) of
	true -> 
	    {{error, user_already_joined, "You are already a member of this channel."}, St};
	false ->
	    {ok, St#channel_st { clients = St#channel_st.clients ++ [Pid] } }
    end;
  

loop(St, {leave, Pid}) ->
    case (lists:member(Pid, St#channel_st.clients)) of
	true ->
	    {ok, St#channel_st { clients = lists:delete(Pid, St#channel_st.clients)} };
	false ->
	    {{error, user_not_joined, "You are not a member of this channel."}, St }
    end;
  
  
loop(St, {message, Pid, Nick, Channel, Msg}) ->
    case (lists:member(Pid, St#channel_st.clients)) of
	true ->
	    Others = lists:delete(Pid, St#channel_st.clients),
	    spawn(fun() -> sendMessage(Others, Nick, Channel, Msg) end), 
	    {ok, St};
	    %spawn(?MODULE,fun(ClientPID) -> genserver:request(ClientPID, {message, Channel, Nick, Msg}) end,lists:delete(Pid, St#channel_st.clients)),
	    %Result = lists:foreach(fun(ClientPID) -> spawn (genserver:request(ClientPID, {message, Channel, Nick, Msg})) end, lists:delete(Pid, St#channel_st.clients)),
	    %{Result, St};
	false ->
	    {{error, user_not_joined, "You are not a member of this channel"}, St}
    end;

loop(St, _Msg) -> 
      {ok, St}.
    
    
sendMessage([Receiver|Restclients], Nick, Channel, Msg) ->
    spawn( fun() -> genserver:request(Receiver, {message, Channel, Nick, Msg}) end ),
    sendMessage(Restclients, Nick, Channel, Msg);
    
sendMessage([], Nick, Channel, Msg) ->
    do_nothing.



initial_state(_Name) ->
    #channel_st{ name = _Name}.
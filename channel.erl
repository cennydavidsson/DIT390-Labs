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
	    	spawn(fun() -> sendMessage(lists:delete(Pid, St#channel_st.clients), Nick, Channel, Msg) end), 
	    	{ok, St};
		false ->
	    	{{error, user_not_joined, "You are not a member of this channel"}, St}
    end.
    
    
sendMessage([Receiver|Restclients], Nick, Channel, Msg) ->
    spawn( fun() -> genserver:request(Receiver, {message, Channel, Nick, Msg}) end ),
    sendMessage(Restclients, Nick, Channel, Msg);
    
sendMessage([], Nick, Channel, Msg) ->
    do_nothing.


initial_state(_Name) ->
    #channel_st{ name = _Name}.
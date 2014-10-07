-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
	
	case {St#cl_st.server, whereis(list_to_atom(_Server))} of
    	{undefined, NewServer} when NewServer =/= undefined -> 
  			{genserver:request(list_to_atom(_Server), {connect, self(), St#cl_st.nick}), St#cl_st {server = _Server }};
  		{_, undefined} ->
  			{{error, server_not_reached, "Server not reached"}, St};
  		_ ->
  			{genserver:request(list_to_atom(_Server), {connect, self(), St#cl_st.nick}), St#cl_st { server = undefined }}
      end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
    case {St#cl_st.server, (length(St#cl_st.channels))} of
    	{undefined, _}  -> 
    		{{error, user_not_connected, "You are not connected to a server."}, St};
    	{_, L} when L > 0 -> 
    		{{error, leave_channels_first, "You need to leave all channels first."}, St};
    	{_, _} -> 
    		{genserver:request(list_to_atom(St#cl_st.server), {disconnect,self(),St#cl_st.nick}), St#cl_st {server = undefined }}
    end;


%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
    case (lists:member(_Channel, St#cl_st.channels)) of
		true ->
	    	{{error, user_already_joined, "You are already a member of this channel."},St};
		false ->
	    	{genserver:request(list_to_atom(St#cl_st.server), {join,self(),_Channel}), St#cl_st {channels = St#cl_st.channels ++ [_Channel] } }
    end;
    
    

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
    case (lists:member(_Channel, St#cl_st.channels)) of
		true ->
	   		{genserver:request(list_to_atom(_Channel), {leave, self()}), St#cl_st { channels = lists:delete(_Channel, St#cl_st.channels) } };
		false ->
	  		{{error, user_not_joined, "You are not a member of this channel."}, St }
    end;


%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
     {genserver:request(list_to_atom(_Channel), {message, self(), St#cl_st.nick, _Channel, _Msg}), St};


%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
    {St#cl_st.nick, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
    {ok, St#cl_st { nick = _Nick }} ;

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {St, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #cl_st { gui = GUIName }, {message, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.


initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick }.

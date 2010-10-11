-module(btcdns_worker).
-export([handle_query/2]).
-include_lib("include/btcdns.hrl").

%% INTERNAL METHODS

%% ONLY MATCH QUERIES WE CARE ABOUT
handle_query(Request, {Socket,RemoteAddress, Port}) ->
	case btcdns_util:is_valid_query(Request) of
		{ok, Question} ->
			%% send message to a worker process
			%% to answer this query!
      gen_udp:send(Socket, RemoteAddress, Port, inet_dns:encode(Request));
		{error, Response} ->
			%% we don't want to handle this query
      gen_udp:send(Socket, RemoteAddress, Port, inet_dns:encode(Response))
	end,
  %%buh-bye!
  exit(normal).

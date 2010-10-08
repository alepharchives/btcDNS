-module(btcdns_server).

-compile(export_all).

-behaviour(gen_server).

-include_lib("include/btcdns.hrl").

-define(SERVER, ?MODULE).

-record(state, {
  s % socket
}).

stop() ->
  gen_server:call(?MODULE, stop).

ping() ->
	gen_server:call(?MODULE, ping).

%% GEN_SERVER API

%% STARTUP AND SHUTDOWN
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Socket} = gen_udp:open(?DNS_PORT, [binary]),
  {ok, #state{s = Socket}}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% CALLBACKS
handle_cast(stop, State = #state{s = Socket}) ->
  gen_upd:close(Socket),
  {stop, normal, ok, State};

handle_cast(_Msg, State = #state{s = Socket}) ->
	{noreply, State}.

handle_call(ping, _From) ->
	io:format("Pong", []),
	{reply, ok}.

handle_call(stop, _From, State = #state{s = Socket}) ->
  gen_udp:close(Socket),
  {stop, normal, State}.

handle_info({udp, Socket, RemoteAddress, Port, Data}, State = #state{s = Socket}) ->
  case inet_dns:decode(Data) of
    {ok, Request} ->
      %% valid query
      Response = handle_query(Request),
      gen_udp:send(Socket, RemoteAddress, Port, Response),
      {noreply, State};
    _Else ->
      error_logger:error_report({error, Data}),
      {stop, badpacket, {_Else, Data}}
  end;
    
handle_info(Info, State) ->
  error_logger:error_report([{wtf, Info}, {state, State}]),
  {noreply, State}.

%% INTERNAL METHODS

%% ONLY MATCH QUERIES WE CARE ABOUT
handle_query(Request) ->
	case btcdns_util:is_valid_query(Request) of
		{ok} ->
			%% send message to a worker process
			%% to answer this query!
		ok;
		{error, Response} ->
			%% we don't want to handle this query
			inet_dns:encode(Response)
	end.

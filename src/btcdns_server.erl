-module(btcdns_server).

-export([stop/0, start_link/0, init/1, terminate/2, code_change/3]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-behaviour(gen_server).

-include_lib("include/btcdns.hrl").

-define(SERVER, ?MODULE).

-record(state, {
  s % socket
}).

%% PUBLIC APi

stop() ->
  gen_server:call(?MODULE, stop).

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

handle_call(stop, _From, State = #state{s = Socket}) ->
  gen_udp:close(Socket),
  {stop, normal, State}.

handle_info({udp, Socket, RemoteAddress, Port, Data}, State = #state{s = Socket}) ->
  case inet_dns:decode(Data) of
    {ok, Request} ->
      %% valid query
      spawn(btcdns_worker,handle_query,[Request, {Socket, RemoteAddress, Port}]), 
      {noreply, State};
    _Else ->
      error_logger:error_report({error, Data}),
      {stop, badpacket, {_Else, Data}}
  end;
    
handle_info(Info, State) ->
  error_logger:error_report([{wtf, Info}, {state, State}]),
  {noreply, State}.

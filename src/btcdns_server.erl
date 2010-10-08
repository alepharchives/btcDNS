-module(btcdns_server).
-compile(export_all).
-behaviour(gen_server).
-include_lib("kernel/src/inet_dns.hrl").
-define(SERVER, ?MODULE).

-record(state, {
  s % socket
}).

-define(DNS_PORT,  53).

%% GEN_SERVER API
stop() ->
  gen_server:call(?MODULE, stop).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Socket} = gen_udp:open(?DNS_PORT, [binary]),
  {ok, #state{s = Socket}}.

handle_cast(stop, State = #state{s = Socket}) ->
  gen_upd:close(Socket),
  {stop, normal, ok, State}.

handle_call(stop, _From, State = #state{s = Socket}) ->
  gen_udp:close(Socket),
  {stop, normal, ok, State}.

handle_info({udp, Socket, RemoteAddress, Port, Data}, State = #state{s = Socket}) ->
  case inet_dns:decode(Data) of
    {ok, Request} ->
      %%valid query
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

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_query(Request) ->
  Header = Request#dns_rec.header,
  Record = #dns_rec {
    header = #dns_header {
      id = Header#dns_header.id,
      opcode = 'query',
      aa = true,
      qr = true,
      tc = false,
      rd = false,
      ra = false,
      pr = false,
      rcode = ?NOERROR
    }
  },
  inet_dns:encode(Record).

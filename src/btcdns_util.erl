-module(btcdns_util).
-include_lib("include/btcdns.hrl").

-export([is_valid_query/1, is_interesting/1]).
-export([make_rec/2, make_rec/3, make_rec/4]).

%% assumes Query has been decoded...
is_valid_query(Query) when Query#dns_rec.header#dns_header.opcode=='query'  ->
	btcdns_util:is_interesting(Query);


is_valid_query(Query) ->
	Response = btcdns_util:make_rec(Query, ?HEADER(Query, ?NOTIMP)),
	{error, Response}.

is_interesting(Query) ->
	{ok, Query}.

%% shortcuts for making response records!

make_rec(Query, Header) ->
	Query#dns_rec{header = Header, anlist = ?AUTHORITY_LIST}.

make_rec(Query, Header, Answers) ->
	Query#dns_rec{header = Header, nslist = ?AUTHORITY_LIST, anlist=Answers}.

make_rec(Query, Header, Answers, Resources) ->
	Query#dns_rec{
		header = Header, nslist = ?AUTHORITY_LIST,
		anlist=Answers, arlist = Resources
	}.
	
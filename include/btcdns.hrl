-include_lib("kernel/src/inet_dns.hrl").

-define(DNS_PORT, 53).
%% THESE ARENT DEFINED IN inet_dns
-define(NOTAUTH, 9).
-define(NOTZONE, 10).
-define(SLD_SERVED, ["btcdns.com"]).
-define(AUTHORITY_LIST, []).

-define(HEADER(Header, Rcode),Header#dns_header{rcode = Rcode}).

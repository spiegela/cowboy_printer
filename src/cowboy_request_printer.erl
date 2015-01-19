-module(cowboy_request_printer).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) -> print_request(Req), {ok, Req, Env}.

print_request(Req) -> print_uri(Req), print_headers(Req), print_body(Req).

print_uri(Req) ->
  Method = cowboy_req:method(Req),
  URL = cowboy_req:url(Req),
  Version = cowboy_req:version(Req),
  lager:debug("REQUEST: ~s ~s ~s~n", [Method, URL, Version]).

print_headers(Req) ->
  lists:foreach(fun({H,V}) ->
  	              lager:debug("~s: ~s~n", [H,V])
  	            end, cowboy_req:headers(Req)).

print_body(Req) ->
  {Body, _Req1} = read_chunked_body(cowboy_req:body(Req), []),
  lager:debug("~n~s~n", [Body]).

read_chunked_body({more, Data, Req}, Acc) ->
  read_chunked_body(cowboy_req:body(Req), [Data|Acc]);
read_chunked_body({ok, Data, Req}, Acc) ->
  Body = iolist_to_binary(lists:reverse([Data|Acc])),
  {Body, Req}.

-module(cowboy_printer).

-behaviour(cowboy_middleware).

-export([execute/2, execute/4]).

execute(Req, Env) ->
  print_request(Req), {ok, Req, Env}.

execute(Code, Headers, Resp, Req) ->
  print_response(Code, Headers, Resp, Req),
  Req.

print_request(Req) ->
  print_uri(Req),
  print_headers(cowboy_req:headers(Req)),
  print_body(read_chunked_body(cowboy_req:body(Req), [])).

print_uri(Req) ->
  Method = cowboy_req:method(Req),
  URL = cowboy_req:url(Req),
  Version = cowboy_req:version(Req),
  lager:debug("REQUEST: ~s ~s ~s~n", [Method, URL, Version]).

print_headers(Headers) ->
  lists:foreach(fun({H,V}) ->
                  lager:debug("~s: ~s~n", [H,V])
                end, Headers).

read_chunked_body({more, Data, Req}, Acc) ->
  read_chunked_body(cowboy_req:body(Req), [Data|Acc]);
read_chunked_body({ok, Data, _Req}, Acc) ->
  iolist_to_binary(lists:reverse([Data|Acc])).

print_response(Code, Headers, Resp, Req) ->
  print_code(Code, Req),
  print_headers(Headers),
  print_body(Resp).

print_code(Code, Req) ->
  Version = cowboy_req:version(Req),
  lager:debug("RESPONSE: ~s ~i ~s", [Version, Code, description(Code)]).

print_body(Resp) ->
  lager:debug("~n~s~n", [Resp]).

description(100) -> "Continue";
description(101) -> "Switching Protocols";
description(200) -> "OK";
description(201) -> "Created";
description(202) -> "Accepted";
description(204) -> "No Content";
description(300) -> "Multiple Choices";
description(301) -> "Moved Permanently";
description(303) -> "See Other";
description(304) -> "Not Modified";
description(307) -> "Temporary Redirect";
description(400) -> "Bad Request";
description(401) -> "Unauthorized";
description(403) -> "Forbidden";
description(404) -> "Not Found";
description(405) -> "Method Not Allowed";
description(406) -> "Not Acceptable";
description(408) -> "Request Timeout";
description(409) -> "Conflict";
description(410) -> "Gone";
description(412) -> "Precondition Failed";
description(413) -> "Request Entity Too Large";
description(414) -> "Request-URI Too Long";
description(415) -> "Unsupported Media Type";
description(500) -> "Internal Server Error";
description(501) -> "Not Implemented";
description(503) -> "Service Unavailable";
description(505) -> "HTTP Version Not Supported".
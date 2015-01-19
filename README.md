Cowboy Printer
==============

Prints out request/response traffic for your Cowboy server.  With luck, more to
come...

```erlang
    cowboy:start_http(http, 100, [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]},
                       {middlewares, [ cowboy_router,
                                       cowboy_request_printer,
                                       cowboy_handler
                                     ]},
                       {onresponse, fun cowboy_response_printer:execute/4}])
    ...
```
-module(erlskat_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([websocket_handle/2]).

init(Req0, State) ->
    {cowboy_websocket, Req0, State}.

websocket_handle(Frame = {text, Msg}, State) ->
    error_logger:info_report(
      [{module, ?MODULE},
       {line, ?LINE},
       {function, ?FUNCTION_NAME},
       {frame, Frame}]),
    try jsx:decode(Msg) of
        Json -> error_logger:info_report(
                  [{module, ?MODULE},
                   {line, ?LINE},
                   {function, ?FUNCTION_NAME},
                   {json, Json}]),
                {reply, {binary, jsx:encode(#{recieved => Json})}, State}
    catch
        _:_ ->
            error_logger:info_report(
              [{module, ?MODULE},
               {line, ?LINE},
               {function, ?FUNCTION_NAME},
               decode_failed]),
            {reply, {binary, jsx:encode(#{error => <<"invalid json">>})}, State}
    end.


-module(erlskat_handler).
-behavior(cowboy_handler).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-define(SESSION_HEADER, <<"skat_session_id">>).
-define(SESSION_SECRET, <<"skat_session_secret">>).

init(Req0, _) ->
    ?LOG_INFO(
       #{module => ?MODULE,
         line => ?LINE,
         function => ?FUNCTION_NAME,
         req => Req0,
         event => connection_receieved}),
    {PlayerId, Req1} = session(Req0),
    {cowboy_websocket, Req1, #{playerId => PlayerId}}.

%% messages from client
websocket_handle({text, Msg} = _Req0, #{playerId := PlayerId} = State) ->
    try jsx:decode(Msg, [return_maps]) of
        Json ->
            ?LOG_INFO(
               #{module => ?MODULE,
                 line => ?LINE,
                 function => ?FUNCTION_NAME,
                 msg => Msg,
                 player => PlayerId,
                 state => State}),
            erlskat_manager:socket_message(#{id => PlayerId,
                                             socket => self()},
                                           Json),
            {ok, State}
    catch
        _:_ ->
            {reply,
             {binary,
              to_json(
                #{error => <<"invalid json">>,
                  msg => Msg,
                  playerId => PlayerId,
                  event => decode_error})},
             State}
    end.

websocket_info(Msg, #{playerId := PlayerId} = State) ->
    try to_json(Msg) of
        Json ->
            ?LOG_INFO(
               #{module => ?MODULE,
                 line => ?LINE,
                 function => ?FUNCTION_NAME,
                 msg => Msg,
                 player => PlayerId,
                 state => State}),
            {reply, {binary, Json}, State}
    catch
        _:_ ->
            {ok, State}
    end.

to_json(Reply) ->
    jsx:encode(Reply).

session(Req@) ->
    case cowboy_req:header(?SESSION_HEADER, Req@) of
        undefined ->
            set_session(Req@);
        SessionHdr ->
            decrypt_session(Req@, SessionHdr)
    end.

set_session(Req@) ->
    quickrand:seed(),
    PlayerId = generate_session_id(),
    {PlayerId, cowboy_req:set_resp_header(
                  ?SESSION_HEADER,
                  encrypt_session(PlayerId),
                  Req@)}.

generate_session_id() -> uuid:get_v4().

encrypt_session(PlayerId) ->
    ?LOG_INFO(#{ module => ?MODULE,
                 line => ?LINE,
                 function => ?FUNCTION_NAME,
                 session => PlayerId }),
    base64:encode(<<?SESSION_SECRET/binary, ":"/utf8, PlayerId/binary>>).

decrypt_session(Req, SessionHdr) ->
    DecodedCredentials = base64:decode(SessionHdr),
    case binary:split(DecodedCredentials, <<$:>>) of
        [?SESSION_SECRET, PlayerId] ->
            {Req, PlayerId};
        _ ->
            {undefined, undefined}
    end.

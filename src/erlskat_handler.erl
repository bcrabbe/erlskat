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
    {SessionId, Req1} = session(Req0),
     {cowboy_websocket, Req1, #{sessionId => SessionId}}.

%% messages from client
websocket_handle({text, Msg} = _Req0, #{sessionId := SessionId} = State) ->
    try jsx:decode(Msg, [return_maps]) of
        Json ->
            ?LOG_INFO(
               #{module => ?MODULE,
                 line => ?LINE,
                 function => ?FUNCTION_NAME,
                 msg => Msg,
                 event => msg_decoded}),
            {reply, {binary, erlskat_manager:handle(SessionId, Json)}, State}
    catch
        _:_ ->
            {reply,
             {binary,
              to_json(
                #{error => <<"invalid json">>,
                  msg => Msg,
                  sessionId => SessionId,
                  event => decode_error})},
             State}
    end.

%% messages to client
websocket_info(Reply, State) ->
    {reply, {binary, to_json(Reply)}, State}.

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
    SessionId = generate_session_id(),
    {SessionId, cowboy_req:set_resp_header(
                  ?SESSION_HEADER,
                  encrypt_session(SessionId),
                  Req@)}.

generate_session_id() -> uuid:get_v4().

encrypt_session(SessionId) ->
    ?LOG_INFO(#{ session => SessionId }),
    base64:encode(<<?SESSION_SECRET/binary, ":"/utf8, SessionId/binary>>).

decrypt_session(Req, SessionHdr) ->
    DecodedCredentials = base64:decode(SessionHdr),
    case binary:split(DecodedCredentials, <<$:>>) of
        [?SESSION_SECRET, SessionId] ->
            {Req, SessionId};
        _ ->
            {undefined, undefined}
    end.

    %% decoded_credentials(EncodedCredentials) ->

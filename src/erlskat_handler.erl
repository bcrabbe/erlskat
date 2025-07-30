-module(erlskat_handler).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([websocket_init/1]).
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
         event => connection_received}),
    {PlayerId, Req1} = session(Req0),
    {cowboy_websocket,
     Req1,
     #{player_id => PlayerId},
     #{idle_timeout => infinity, max_frame_size => infinity}}.

websocket_init(#{player_id := PlayerId} = State) ->
    erlskat_manager:socket_message(
      #{id => PlayerId, socket => self()},
      #{}),
    {ok, State}.

%% messages from client
websocket_handle({text, Msg} = _Req0, #{player_id := PlayerId} = State) ->
    try jsx:decode(Msg, [return_maps]) of
        Json ->
            ?LOG_INFO(
               #{module => ?MODULE,
                 line => ?LINE,
                 function => ?FUNCTION_NAME,
                 msg => Msg,
                 player => PlayerId,
                 state => State,
                 pid => self()}),
            erlskat_manager:socket_message(
              #{id => PlayerId,
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
                  player_id => PlayerId,
                  event => decode_error})},
             State}
    end.

%% messages from server
websocket_info(Msg, #{player_id := PlayerId} = State) ->
    ?LOG_INFO(
       #{module => ?MODULE,
         line => ?LINE,
         function => ?FUNCTION_NAME,
         msg => Msg,
         pid => self(),
         player => PlayerId,
         state => State}),
    try to_json(Msg) of
        Json ->
            {reply, {binary, Json}, State}
    catch
        _:_ ->
            {ok, State}
    end.

to_json(Reply) ->
    jsx:encode(Reply).

session(Req) ->
    case cowboy_req:header(?SESSION_HEADER, Req) of
        undefined ->
            set_session(Req);
        SessionHdr ->
            decrypt_session(Req, SessionHdr)
    end.

set_session(Req) ->
    quickrand:seed(),
    PlayerId = generate_session_id(),
    ?LOG_INFO(#{player_id => PlayerId}),
    {binary_uuid_to_hex(PlayerId), cowboy_req:set_resp_header(
                  ?SESSION_HEADER,
                  encrypt_session(PlayerId),
                  Req)}.

generate_session_id() -> uuid:get_v4().

binary_uuid_to_hex(BinaryUuid) ->
    <<A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16>> = BinaryUuid,
    FormatString = "~2.16.0b~2.16.0b~2.16.0b~2.16.0b-~2.16.0b~2.16.0b-~2.16.0b~2.16.0b-"
                   "~2.16.0b~2.16.0b-~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b",
    FmtIolist = io_lib:format(FormatString,
                              [A1, A2, A3, A4, A5, A6, A7, A8,
                               A9, A10, A11, A12, A13, A14, A15, A16]),
    list_to_binary(FmtIolist).

encrypt_session(PlayerId) ->
    ?LOG_INFO(#{ module => ?MODULE,
                 line => ?LINE,
                 function => ?FUNCTION_NAME,
                 session => PlayerId }),
    base64:encode(<<?SESSION_SECRET/binary, ":"/utf8, PlayerId/binary>>).

decrypt_session(Req, SessionHdr) ->
    DecodedCredentials = base64:decode(SessionHdr),
    [?SESSION_SECRET, PlayerId] = binary:split(DecodedCredentials, <<$:>>),
    {binary_uuid_to_hex(PlayerId), Req}.

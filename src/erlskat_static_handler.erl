-module(erlskat_static_handler).

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),

    case Path of
        <<"/">> ->
            serve_file(Req0, State, "ui/build/index.html", "text/html");
        <<"/static/", Rest/binary>> ->
            FilePath = "ui/build/static/" ++ binary_to_list(Rest),
            serve_file(Req0, State, FilePath, get_mime_type(Rest));
        _ ->
            serve_file(Req0, State, "ui/build/index.html", "text/html")
    end.

serve_file(Req0, State, FilePath, MimeType) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            Req1 = cowboy_req:set_resp_header(<<"content-type">>, MimeType, Req0),
            {ok, cowboy_req:reply(200, #{}, Content, Req1), State};
        {error, _} ->
            {ok, cowboy_req:reply(404, Req0), State}
    end.

get_mime_type(Path) ->
    case filename:extension(Path) of
        <<".js">> -> <<"application/javascript">>;
        <<".css">> -> <<"text/css">>;
        <<".json">> -> <<"application/json">>;
        <<".png">> -> <<"image/png">>;
        <<".jpg">> -> <<"image/jpeg">>;
        <<".jpeg">> -> <<"image/jpeg">>;
        <<".gif">> -> <<"image/gif">>;
        <<".svg">> -> <<"image/svg+xml">>;
        <<".ico">> -> <<"image/x-icon">>;
        _ -> <<"text/plain">>
    end.

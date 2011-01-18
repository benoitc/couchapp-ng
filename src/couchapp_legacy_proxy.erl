%%% -*- erlang -*-
%%%
%%% This file is part of couchapp_legacy released under the MIT license. 
%%% See the NOTICE for more information.


-module(couchapp_legacy_proxy).

-export([do_proxy/3]).

-include("couch_db.hrl").
-include_lib("deps/ibrowse/include/ibrowse.hrl").

-define(TIMEOUT, infinity).
-define(PKT_SIZE, 4096).


do_proxy(Req, ProxyDest, Url) ->
    Method = get_method(Req),
    Version = get_version(Req),
    Headers = get_headers(Req),
    Body = get_body(Req),
    Options = [
        {http_vsn, Version},
        {headers_as_is, true},
        {response_format, binary},
        {stream_to, {self(), once}}
    ],
    case ibrowse:send_req(Url, Headers, Method, Body, Options, ?TIMEOUT) of
        {ibrowse_req_id, ReqId} ->
            stream_response(Req, ProxyDest, ReqId);
        {error, Reason} ->
            throw({error, Reason})
    end.
    
get_method(#httpd{mochi_req=MochiReq}) ->
    case MochiReq:get(method) of
        Method when is_atom(Method) ->
            list_to_atom(string:to_lower(atom_to_list(Method)));
        Method when is_list(Method) ->
            list_to_atom(string:to_lower(Method));
        Method when is_binary(Method) ->
            list_to_atom(string:to_lower(?b2l(Method)))
    end.

get_version(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(version).


get_headers(#httpd{mochi_req=MochiReq}) ->
    to_ibrowse_headers(mochiweb_headers:to_list(MochiReq:get(headers)), []).

to_ibrowse_headers([], Acc) ->
    lists:reverse(Acc);
to_ibrowse_headers([{K, V} | Rest], Acc) when is_atom(K) ->
    to_ibrowse_headers([{atom_to_list(K), V} | Rest], Acc);
to_ibrowse_headers([{K, V} | Rest], Acc) when is_list(K) ->
    case string:to_lower(K) of
        "content-length" ->
            to_ibrowse_headers(Rest, [{content_length, V} | Acc]);
        % This appears to make ibrowse too smart.
        %"transfer-encoding" ->
        %    to_ibrowse_headers(Rest, [{transfer_encoding, V} | Acc]);
        _ ->
            to_ibrowse_headers(Rest, [{K, V} | Acc])
    end.

get_body(#httpd{method='GET'}) ->
    fun() -> eof end;
get_body(#httpd{method='HEAD'}) ->
    fun() -> eof end;
get_body(#httpd{method='DELETE'}) ->
    fun() -> eof end;
get_body(#httpd{mochi_req=MochiReq}) ->
    case MochiReq:get(body_length) of
        undefined ->
            <<>>;
        {unknown_transfer_encoding, Unknown} ->
            exit({unknown_transfer_encoding, Unknown});
        chunked ->
            {fun stream_chunked_body/1, {init, MochiReq, 0}};
        0 ->
            <<>>;
        Length when is_integer(Length) andalso Length > 0 ->
            {fun stream_length_body/1, {init, MochiReq, Length}};
        Length ->
            exit({invalid_body_length, Length})
    end.

stream_chunked_body({init, MReq, 0}) ->
    % First chunk, do expect-continue dance.
    init_body_stream(MReq),
    stream_chunked_body({stream, MReq, 0, [], ?PKT_SIZE});
stream_chunked_body({stream, MReq, 0, Buf, BRem}) ->
    % Finished a chunk, get next length. If next length
    % is 0, its time to try and read trailers.
    {CRem, Data} = read_chunk_length(MReq),
    case CRem of
        0 ->
            BodyData = iolist_to_binary(lists:reverse(Buf, Data)),
            {ok, BodyData, {trailers, MReq, [], ?PKT_SIZE}}; 
        _ ->
            stream_chunked_body(
                {stream, MReq, CRem, [Data | Buf], BRem-size(Data)}
            )
    end;
stream_chunked_body({stream, MReq, CRem, Buf, BRem}) when BRem =< 0 ->
    % Time to empty our buffers to the upstream socket.
    BodyData = iolist_to_binary(lists:reverse(Buf)),
    {ok, BodyData, {stream, MReq, CRem, [], ?PKT_SIZE}};
stream_chunked_body({stream, MReq, CRem, Buf, BRem}) ->
    % Buffer some more data from the client.
    Length = lists:min([CRem, BRem]),
    Socket = MReq:get(socket),
    NewState = case mochiweb_socket:recv(Socket, Length, ?TIMEOUT) of
        {ok, Data} when size(Data) == CRem ->
            case mochiweb_socket:recv(Socket, 2, ?TIMEOUT) of
                {ok, <<"\r\n">>} ->
                    {stream, MReq, 0, [<<"\r\n">>, Data | Buf], BRem-Length-2};
                _ ->
                    exit(normal)
            end;
        {ok, Data} ->
            {stream, MReq, CRem-Length, [Data | Buf], BRem-Length};
        _ ->
            exit(normal)
    end,
    stream_chunked_body(NewState);
stream_chunked_body({trailers, MReq, Buf, BRem}) when BRem =< 0 ->
    % Empty our buffers and send data upstream.
    BodyData = iolist_to_binary(lists:reverse(Buf)),
    {ok, BodyData, {trailers, MReq, [], ?PKT_SIZE}};
stream_chunked_body({trailers, MReq, Buf, BRem}) ->
    % Read another trailer into the buffer or stop on an
    % empty line.
    Socket = MReq:get(socket),
    mochiweb_socket:setopts(Socket, [{packet, line}]),
    case mochiweb_socket:recv(Socket, 0, ?TIMEOUT) of
        {ok, <<"\r\n">>} ->
            mochiweb_socket:setopts(Socket, [{packet, raw}]),
            BodyData = iolist_to_binary(lists:reverse(Buf, <<"\r\n">>)),
            {ok, BodyData, eof};
        {ok, Footer} ->
            mochiweb_socket:setopts(Socket, [{packet, raw}]),
            NewState = {trailers, MReq, [Footer | Buf], BRem-size(Footer)},
            stream_chunked_body(NewState);
        _ ->
            exit(normal)
    end;
stream_chunked_body(eof) ->
    % Tell ibrowse we're done sending data.
    eof.


stream_length_body({init, MochiReq, Length}) ->
    % Do the expect-continue dance
    init_body_stream(MochiReq),
    stream_length_body({stream, MochiReq, Length});
stream_length_body({stream, _MochiReq, 0}) ->
    % Finished streaming.
    eof;
stream_length_body({stream, MochiReq, Length}) ->
    BufLen = lists:min([Length, ?PKT_SIZE]),
    case MochiReq:recv(BufLen) of
        <<>> -> eof;
        Bin -> {ok, Bin, {stream, MochiReq, Length-BufLen}}
    end.


init_body_stream(MochiReq) ->
    Expect = case MochiReq:get_header_value("expect") of
        undefined ->
            undefined;
        Value when is_list(Value) ->
            string:to_lower(Value)
    end,
    case Expect of
        "100-continue" ->
            MochiReq:start_raw_response({100, gb_trees:empty()});
        _Else ->
            ok
    end.


read_chunk_length(MochiReq) ->
    Socket = MochiReq:get(socket),
    mochiweb_socket:setopts(Socket, [{packet, line}]),
    case mochiweb_socket:recv(Socket, 0, ?TIMEOUT) of
        {ok, Header} ->
            mochiweb_socket:setopts(Socket, [{packet, raw}]),
            Splitter = fun(C) ->
                C =/= $\r andalso C =/= $\n andalso C =/= $\s
            end,
            {Hex, _Rest} = lists:splitwith(Splitter, ?b2l(Header)),
            {mochihex:to_int(Hex), Header};
        _ ->
            exit(normal)
    end.


stream_response(Req, ProxyDest, ReqId) ->
    receive
        {ibrowse_async_headers, ReqId, "100", _} ->
            % ibrowse doesn't handle 100 Continue responses which
            % means we have to discard them so the proxy client
            % doesn't get confused.
            ibrowse:stream_next(ReqId),
            stream_response(Req, ProxyDest, ReqId);
        {ibrowse_async_headers, ReqId, Status, Headers} ->
            {Source, Dest} = get_urls(Req, ProxyDest),
            FixedHeaders = fix_headers(Source, Dest, Headers, []),
            case body_length(FixedHeaders) of
                chunked ->
                    {ok, Resp} = couch_httpd:start_chunked_response(
                        Req, list_to_integer(Status), FixedHeaders
                    ),
                    ibrowse:stream_next(ReqId),
                    stream_chunked_response(Req, ReqId, Resp),
                    {ok, Resp};
                Length when is_integer(Length) ->
                    {ok, Resp} = couch_httpd:start_response_length(
                        Req, list_to_integer(Status), FixedHeaders, Length
                    ),
                    ibrowse:stream_next(ReqId),
                    stream_length_response(Req, ReqId, Resp),
                    {ok, Resp};
                _ ->
                    {ok, Resp} = couch_httpd:start_response(
                        Req, list_to_integer(Status), FixedHeaders
                    ),
                    ibrowse:stream_next(ReqId),
                    stream_length_response(Req, ReqId, Resp),
                    % XXX: MochiWeb apparently doesn't look at the
                    % response to see if it must force close the
                    % connection. So we help it out here.
                    erlang:put(mochiweb_request_force_close, true),
                    {ok, Resp}
            end
    end.


stream_chunked_response(Req, ReqId, Resp) ->
    receive
        {ibrowse_async_response, ReqId, {error, Reason}} ->
            throw({error, Reason});
        {ibrowse_async_response, ReqId, Chunk} ->
            couch_httpd:send_chunk(Resp, Chunk),
            ibrowse:stream_next(ReqId),
            stream_chunked_response(Req, ReqId, Resp);
        {ibrowse_async_response_end, ReqId} ->
            couch_httpd:last_chunk(Resp)
    end.


stream_length_response(Req, ReqId, Resp) ->
    receive
        {ibrowse_async_response, ReqId, {error, Reason}} ->
            throw({error, Reason});
        {ibrowse_async_response, ReqId, Chunk} ->
            couch_httpd:send(Resp, Chunk),
            ibrowse:stream_next(ReqId),
            stream_length_response(Req, ReqId, Resp);
        {ibrowse_async_response_end, ReqId} ->
            ok
    end.


get_urls(Req, ProxyDest) ->
    SourceUrl = couch_httpd:absolute_uri(Req, "/" ++ hd(Req#httpd.path_parts)),
    Source = parse_url(?b2l(iolist_to_binary(SourceUrl))),
    case (catch parse_url(ProxyDest)) of
        Dest when is_record(Dest, url) ->
            {Source, Dest};
        _ ->
            DestUrl = couch_httpd:absolute_uri(Req, ProxyDest),
            {Source, parse_url(DestUrl)}
    end.


fix_headers(_, _, [], Acc) ->
    lists:reverse(Acc);
fix_headers(Source, Dest, [{K, V} | Rest], Acc) ->
    Fixed = case string:to_lower(K) of
        "location" -> rewrite_location(Source, Dest, V);
        "content-location" -> rewrite_location(Source, Dest, V);
        "uri" -> rewrite_location(Source, Dest, V);
        "destination" -> rewrite_location(Source, Dest, V);
        "set-cookie" -> rewrite_cookie(Source, Dest, V);
        _ -> V
    end,
    fix_headers(Source, Dest, Rest, [{K, Fixed} | Acc]).


rewrite_location(Source, #url{host=Host, port=Port, protocol=Proto}, Url) ->
    case (catch parse_url(Url)) of
        #url{host=Host, port=Port, protocol=Proto} = Location ->
            DestLoc = #url{
                protocol=Source#url.protocol,
                host=Source#url.host,
                port=Source#url.port,
                path=couchapp_legacy_util:join_url_path(Source#url.path, Location#url.path)
            },
            url_to_url(DestLoc);
        #url{} ->
            Url;
        _ ->
            url_to_url(Source#url{path=couchapp_legacy_util:join_url_path(Source#url.path, Url)})
    end.


rewrite_cookie(_Source, _Dest, Cookie) ->
    Cookie.


parse_url(Url) when is_binary(Url) ->
    ibrowse_lib:parse_url(?b2l(Url));
parse_url(Url) when is_list(Url) ->
    ibrowse_lib:parse_url(?b2l(iolist_to_binary(Url))).

url_to_url(#url{host=Host, port=Port, path=Path, protocol=Proto}) ->
    LPort = case {Proto, Port} of
        {http, 80} -> "";
        {https, 443} -> "";
        _ -> ":" ++ integer_to_list(Port)
    end,
    LPath = case Path of
        "/" ++ _RestPath -> Path;
        _ -> "/" ++ Path
    end,
    atom_to_list(Proto) ++ "://" ++ Host ++ LPort ++ LPath.


body_length(Headers) ->
    case is_chunked(Headers) of
        true -> chunked;
        _ -> content_length(Headers)
    end.


is_chunked([]) ->
    false;
is_chunked([{K, V} | Rest]) ->
    case string:to_lower(K) of
        "transfer-encoding" ->
            string:to_lower(V) == "chunked";
        _ ->
            is_chunked(Rest)
    end.

content_length([]) ->
    undefined;
content_length([{K, V} | Rest]) ->
    case string:to_lower(K) of
        "content-length" ->
            list_to_integer(V);
        _ ->
            content_length(Rest)
    end.



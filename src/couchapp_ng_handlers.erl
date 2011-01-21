%%% -*- erlang -*-
%%%
%%% This file is part of couchapp_ng released under the MIT license. 
%%% See the NOTICE for more information.

-module(couchapp_ng_handlers).

-include("couch_db.hrl").

-export([rewrite_handler/3, proxy_handler/3]).

rewrite_handler(#httpd{mochi_req=MochiReq}=Req, Path, Options) ->    
    % assemble path
    Prefix = proplists:get_value(prefix, Options, ""),
    Path1 = "/" ++ couchapp_ng_util:normalize_path(
        couchapp_ng_util:join_url_path(Prefix, Path)
    ),

    % build raw path
    RawPath = build_raw_path(Req, Path1, Options),

    ?LOG_INFO("Rewritten to ~p~n", [RawPath]),
    MochiReq1 = mochiweb_request:new(
        MochiReq:get(socket), 
        MochiReq:get(method),
        RawPath,
        MochiReq:get(version),
        MochiReq:get(headers)
    ),

    % cleanup, It force mochiweb to reparse raw uri.
    MochiReq1:cleanup(),

    #httpd{
     db_url_handlers = DbUrlHandlers,
     design_url_handlers = DesignUrlHandlers,
     default_fun = DefaultFun,
     url_handlers = UrlHandlers
    } = Req,
    couch_httpd:handle_request_int(MochiReq1, DefaultFun, 
        UrlHandlers, DbUrlHandlers, DesignUrlHandlers).

proxy_handler(Req, Path, Options) ->
    case proplists:get_value(proxy_dest, Options) of
        undefined ->
            throw({error, no_proxy_dest});
        ProxyDest ->
            ProxyDest1 = couchapp_ng_util:remove_trailing_slash(
                binary_to_list(ProxyDest)),
            Url = couchapp_ng_util:join_url_path(ProxyDest1, Path),
            
            % build raw path
            Url1 = build_raw_path(Req, Url, Options),
            ?LOG_DEBUG("Proxy To: ~p~nP", [Url1]),

            couchapp_ng_proxy:do_proxy(Req, ProxyDest1, Url1)
    end.


build_raw_path(Req, Path, Options) ->
    QueryList = couch_httpd:qs(Req),
    ExtraQuery = proplists:get_value(extra_query, Options, []),

    FinalQueryList = fix_query(lists:append(QueryList, ExtraQuery)),
    
    % build raw path
    lists:flatten(lists:append([Path], case FinalQueryList of
            [] -> [];
            _ -> [$?, mochiweb_util:urlencode(FinalQueryList)]
        end)).



fix_query(QS) ->
    fix_query(QS, []).

fix_query([], Acc) ->
    lists:reverse(Acc);
fix_query([{K, V}|R], Acc) ->
    Acc1 = case lists:member(K, ["key", "start_key", "end_key", "starkey",
                "endkey"]) of
        true ->
            V1 = try iolist_to_binary(?JSON_ENCODE(?JSON_DECODE(V))) of
                Value -> Value
                catch
                     _:_ ->
                         iolist_to_binary(?JSON_ENCODE(iolist_to_binary(V)))                end,
            [{K,V1}|Acc];
        false ->
            [{K, V}|Acc]
    end,
    fix_query(R, Acc1).


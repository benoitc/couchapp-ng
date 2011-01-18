%%% -*- erlang -*-
%%%
%%% This file is part of couchapp_legacy released under the MIT license. 
%%% See the NOTICE for more information.

-module(couchapp_legacy_handlers).

-include("couch_db.hrl").

-export([rewrite_handler/3, proxy_handler/3]).

rewrite_handler(#httpd{mochi_req=MochiReq}=Req, Path, Options) ->
    % get final query list
    QueryList = couch_httpd:qs(Req),
    ExtraQuery = proplists:get_value(extra_query, Options, []),
    FinalQueryList = lists:append(QueryList, ExtraQuery),

    % build raw path
    Prefix = proplists:get_value(prefix, Options, ""),
    Path1 = lists:append([couchapp_legacy_util:join_url_path(Prefix, Path)], 
        case FinalQueryList of
            [] -> [];
            _ -> [$?, couchapp_legacy_util:encode_query(FinalQueryList)]
        end),

    RawPath = binary_to_list(iolist_to_binary(
                couchapp_legacy_util:normalize_path(Path1)
            )),

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
            ProxyDest1 = couchapp_legacy_util:remove_trailing_slash(
                binary_to_list(ProxyDest)),
            Url = couchapp_legacy_util:join_url_path(ProxyDest1, Path),
            couchapp_legacy_proxy:do_proxy(Req, ProxyDest1, Url)
    end.



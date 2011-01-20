%%% -*- erlang -*-
%%%
%%% This file is part of couchapp_legacy released under the MIT license. 
%%% See the NOTICE for more information.

-module(couchapp_legacy_httpd).

-include("couch_db.hrl").

-export([handle_app_req/3]).

-define(SEPARATOR, $\/).

% @doc couchapp_legacy design handler
handle_app_req(#httpd{
        path_parts=[DbName, <<"_design">>, DesignName, 
            _App|PathParts]}=Req, _db, DDoc) ->

    DesignId = <<"_design/", DesignName/binary>>,
    Prefix = binary_to_list(<<"/", DbName/binary, "/",
        DesignId/binary>>),
  
    ?LOG_DEBUG("prefix ~p~n", [Prefix]), 
    PathParts1 = lists:map(fun binary_to_list/1, PathParts),

    Path = "/" ++ string:join(PathParts1, [?SEPARATOR]),

    ?LOG_DEBUG("rewrite against ~p~n", [Path]),

    case couchapp_legacy_routes:load_routes(DbName, DesignId, DDoc) of
        {error, undefined} ->
            couch_httpd:send_error(Req, 404, <<"couchapp_error">>,
                <<"Routes not defined">>);
        {error, bad_format} ->
            couch_httpd:send_error(Req, 400, <<"couchapp_error">>,
                <<"Route property must be a JSON Array.">>);
        _Else ->
            Routes = couchapp_legacy_routes:get_routes(Req, DbName,
                DesignId),
            ?LOG_DEBUG("Available routes ~p~n", [Routes]),
            dispatch(Req, Routes, Path, Prefix)
    end.

dispatch(Req, Routes, Path, Prefix) ->
    Selector = fun(X) -> selector(Path, X) end,
    Action = case filter(Selector, Routes) of
	    {ok, A} -> A;
		nomatch -> nomatch 
	end,
    process(Action, Req, Path, Prefix).

process({attachment, _, _, enoent, _, _}, Req, Path, Prefix) ->
    couchapp_legacy_handlers:rewrite_handler(Req, Path, [{prefix,
                Prefix}]);
process({attachment, _, _, Path, _, _}, Req, _, Prefix) ->
    couchapp_legacy_handlers:rewrite_handler(Req, Path, [{prefix,
                Prefix}]);
process({route, _, Regexp, Route, Handler, Opts}, Req, Path, Prefix) ->
    % add extra query
    QueryList = couch_httpd:qs(Req),
    Substitutions = proplists:get_value(substitutions, Opts, []),
    ExtraQuery = maybe_extra_query(Opts, fun(Patterns) ->
                Substitutions1 = case re:run(Route, Regexp,[{capture,
                                Substitutions, list}]) of
                    match ->
                        QueryList;
                    {match, Matched} ->
                        lists:append(lists:zip(Substitutions, Matched),
                            QueryList) 
                end,
                substitute_query(Substitutions1, Patterns)
        end),

    HandlerFun = get_handler_fun(Handler),
    HandlerFun(Req, Path, [{prefix, Prefix}, {extra_query, ExtraQuery}|Opts]);
process({alias, _, RegExp, To, Handler, Opts}, Req, Path, Prefix) ->
    QueryList = couch_httpd:qs(Req),    
    {Path1, Opts1} = substitute_alias(Path, RegExp, To, 
        proplists:get_value(substitutions, Opts, []), QueryList, Opts),
    HandlerFun = get_handler_fun(Handler),
    HandlerFun(Req, Path1, [{prefix, Prefix}|Opts1]);
process(nomatch, Req, _, _) ->
    couch_httpd:send_error(Req, 404, <<"nomatch">>,
                <<"no route found">>).


%% Returns first element which satisfies Fun

filter(_Fun, []) -> 
    nomatch;
filter(Fun, [Rule|Rest]) ->
    case Fun(Rule) of 
        true ->
            {ok, Rule};
        false -> 
	        filter(Fun, Rest)
    end.

selector(Element, {_, _, Regexp, _, _, []}) -> 
    selector_exec(Element, Regexp);
selector(Element, {_, _, Regexp, _, _, Opts}) ->
    selector_exec(Element, Regexp, Opts).

selector_exec(Element, Regexp) ->
    case re:run(Element, Regexp, [{capture,first}]) of
	{match, _} ->
	    true;
	nomatch ->
	    false;
	{error, Reason} ->
	    exit({?MODULE, Reason})
    end.

selector_exec(Element, Regexp, Opts) ->
    case lists:keysearch(named_subpatterns, 1, Opts) of
	false ->
	    selector_exec(Element, Regexp);
	{_, {_, Names}} ->
	    case re:run(Element, Regexp, [{capture, Names, list}]) of
		nomatch ->
		    false;
		match ->
		    true;
		{match, _Matched} ->		    
		    true
	    end
    end.

substitute_alias(URL, Regexp, Target0, Substitutions, QueryList, Options) ->
    {Target1, FinalSubstitutions} = case re:run(URL, Regexp, 
            [{capture, Substitutions, list}]) of
	match ->
        {replace_substitions(QueryList, Target0), QueryList};
	{match, Matched} ->
        Substitutions1 = lists:append(lists:zip(Substitutions, Matched),
            QueryList),

        {replace_substitions(Substitutions1, Target0), Substitutions1}
    end,
    ExtraQuery = maybe_extra_query(Options, fun(Patterns) ->
                    substitute_query(FinalSubstitutions, Patterns)
            end), 
    {Target1, [{extra_query, ExtraQuery}|Options]}.


maybe_extra_query(Options, Fun) ->
    case proplists:get_value(query_substitute, Options) of
        undefined ->
            [];
        Patterns ->
            ?LOG_INFO("patterns ~p~n", [Patterns]),
            Fun(Patterns)
    end.


substitute_query(Substitutions, Patterns) ->
    lists:foldr(fun({K,V}, Acc) ->
                V1 = replace_substitions(Substitutions, V),
                if V1 =:= V
                    -> Acc;
                    true -> [{K, V1}|Acc]
                end
        end, [], Patterns).

replace_substitions(Substitutions, Target) ->
    lists:foldl(fun({Name, Replacement}, String) ->
				re:replace(String, "\\(\\?<" ++ Name ++ ">\\)", 
					   Replacement, [{return, list}])
			end, Target, Substitutions).

get_handler_fun(Name) when is_binary(Name) ->
    get_handler_fun(binary_to_list(Name));
get_handler_fun(Name) ->
    HandlersList = handlers_funs(),
    case proplists:get_value(Name, HandlersList) of
        undefined ->
            throw({error, unkown_handler});
        Fun ->
            Fun
    end.

handlers_funs() ->
    lists:map(
        fun({Name, SpecStr}) ->
                {Name, couch_httpd:make_arity_3_fun(SpecStr)}
        end, couch_config:get("couchapp_legacy_handlers")).

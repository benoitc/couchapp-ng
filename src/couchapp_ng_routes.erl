%%% -*- erlang -*-
%%%
%%% This file is part of couchapp_ng released under the MIT license. 
%%% See the NOTICE for more information.

-module(couchapp_ng_routes).
-behaviour(gen_server).

-include("couch_db.hrl").

-define(ROUTES, cl_routes).

-export([start_link/0, config_change/1]).

-export([load_routes/3, get_routes/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {
    db_notifier 
}).

config_change("couchapp_ng_handler") ->
    exit(?MODULE, shutdown).

load_routes(DbName, DocId, DDoc) ->
    gen_server:call(?MODULE, {load_routes, {DbName, DocId}, DDoc}).

get_routes(Req, DbName, DocId) ->
    gen_server:call(?MODULE, {get_routes, Req, {DbName, DocId}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    ?ROUTES = ets:new(?ROUTES, [set, named_table, public]),

    ok = couch_config:register(fun ?MODULE:config_change/1),
    {ok, Notifier} = couch_db_update_notifier:start_link(fun handle_db_event/1),
    {ok, #state{db_notifier=Notifier}}.

handle_call({load_routes, {DbName, DocId}, DDoc}, _From, State) ->
    Result = case ets:lookup(?ROUTES, {DbName, DocId}) of 
        [] ->
            #doc{body={Props}} = DDoc,
            case couch_util:get_value(<<"routes">>, Props) of
                undefined ->
                    {error, undefined};
                Bin when is_binary(Bin) ->
                    {error, bad_format};
                Prop ->
                    try load({DbName, DocId}, Prop) of
                        _ -> ok
                    catch
                        error:Error -> {error, Error}
                    end
            end;
        _Else ->
            ok 
    end,
    {reply, Result, State};
handle_call({get_routes, Req, {DbName, DocId}}, _From, State) ->
    #httpd{method=Method} = Req,
    
    Key =  {DbName, DocId},
    Routes = case ets:lookup(?ROUTES, Key) of
        [{Key, R}] -> R;
        [] -> []
    end,

    Routes1 = lists:foldl(fun ({_, M, _, _, _, _}=Rule, Acc) ->
                case lists:member(M, [Method, '*']) of
                   true ->
                       [Rule|Acc];
                   false ->
                        Acc
                end
        end, [], Routes),
    {reply, Routes1, State};

handle_call({delete_routes, DbName}, _From, State) ->
    ok = delete_routes(DbName),
    {reply, ok, State};

handle_call({updates_routes, DbName}, _From, State) ->
    Opts = [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}],
    case couch_db:open(DbName, Opts) of
        {ok, Db} ->
            LastSeq = couch_db:get_update_seq(Db) -1,
            couch_db:changes_since(Db, main_only, LastSeq, 
                fun (#doc_info{id=DocId}, _) ->
                    % we just delete infos here
                    delete_routes(DbName, DocId),
                    {ok, []}
                    
                end, []),
            couch_db:close(Db);
        Error ->
            ?LOG_ERROR("error ~p", [Error]),
            delete_routes(DbName)
    end,
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{db_notifier = Notifier}) ->
    couch_db_update_notifier:stop(Notifier),
    true = ets:delete(?ROUTES),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_db_event({Event, DbName}) ->
    case ets:match(?ROUTES, {{DbName, '$1'}, '_'}) of
        [] -> 
            ok;
        _ ->
            case Event of
                deleted -> 
                    gen_server:call(?MODULE, {delete_routes, DbName}, infinity);
                updated -> 
                    ok = gen_server:call(?MODULE, {updates_routes, DbName},
                        infinity);
                _Else ->
                    ok
            end
    end.

%% @private

delete_routes(DbName, DocId) ->
    ets:delete(?ROUTES, {DbName, DocId}).
    
delete_routes(DbName) ->
    case ets:match(?ROUTES, {{DbName, '$1'}, '_'}) of
        [] ->
            ok;
        Apps ->
            lists:foreach(fun(DocId) ->
                        ets:delete(?ROUTES, {DbName, DocId})
                end, Apps)
    end,
    ok.

load(Key, Prop) ->
    Routes = lists:map(fun preprocess/1, Prop),
    {ok, NamedRegexp} = re:compile("\\?<[A-Za-z_0-9]+>", [ungreedy]),
    Routes1 = lists:map(fun(X) -> parser(X, NamedRegexp) end, Routes),

    Rules =lists:foldl(fun({Type, Method, RegexSrc, RegexTgt, Handler,
                    Options}, Acc) ->
                {ok, Compiled} = re:compile(RegexSrc),
                Rule = {Type, Method, Compiled, RegexTgt, Handler,
                    Options},
                
                [Rule|Acc] 
        end, [], Routes1),
    true = ets:insert(?ROUTES, {Key, Rules}),
    ok.

parser({alias, Method, Regexp, RegexpTrgt, HandlerFun, Opts} = Org, Compiled) ->
    case get_all_names(Regexp, Compiled) of
	[] ->
	    Org;
	ANames ->
	    SNames = lists:map(fun atom_to_list/1, ANames),
	    {alias, Method, Regexp, RegexpTrgt, HandlerFun, [{substitutions, SNames} | Opts]}
    end;
parser(X, _) ->
    X.
        
preprocess({Route}) ->
    Method = couch_util:to_existing_atom(couch_util:get_value(<<"method">>, 
            Route, '*')),
    RouteString = binary_to_list(get_required_value(<<"from">>, Route,
            missing_route_source)),

    case couch_util:get_value(<<"type">>, Route, <<"rewrite">>) of
        <<"attachments">> ->
            AttName = case couch_util:get_value(<<"to">>, Route) of
                undefined -> enoent;
                <<"enoent">> -> enoent;
                To -> binary_to_list(To)
            end,

            {attachment, Method, RouteString, AttName, nil, []};
        Handler ->
            {Type, DestString} = case couch_util:get_value(<<"to">>, Route) of
                undefined ->
                    {route, nil};
                To ->
                    {alias, binary_to_list(To)}
            end,

            {ok, Options} = check_options(couch_util:get_value(<<"options">>,
                    Route, {[]})),

            Options1 = case proplists:get_value(query_patterns, Options) of
                undefined -> Options;
                {Patterns} ->
                    Patterns1 = lists:foldr(fun({K, V}, Acc) ->
                                V1 = reverse_alias(V, Options),
                                [{K, V1}|Acc]
                        end,[], Patterns),

                    [{query_substitute, Patterns1}|Options]
            end,
                    
            RouteString1 = reverse_route(RouteString, Options1),
            DestString1 = reverse_alias(DestString, Options1),

            
            {Type, Method, RouteString1, DestString1, binary_to_list(Handler),
                Options1}
    end.

reverse_route(RouteString, Options) ->
    case couch_util:get_value(patterns, Options) of
        undefined ->
            RouteString;
        {Patterns} ->
            Regexp = lists:foldl(
                fun({PatternName, Pattern}, String) ->
                        Name = binary_to_list(PatternName),
                        P = binary_to_list(Pattern),
                        P1 = re:replace(P, "\\\\", "\\\\\\\\", 
                            [{return, list}, global]), 
                        re:replace(String, [$:|Name], [$(,$?,$<|Name] ++ 
                            [$>|P1] ++ [$)], [{return, list}])
                end,
                RouteString, Patterns),
            [$^|Regexp] ++ "$"
    end.

reverse_alias(RouteString, Options) ->
    case couch_util:get_value(patterns, Options) of
        undefined ->
            RouteString;
        {Patterns} ->
            lists:foldl(
                fun({PatternName, _}, String) ->
                        Name = binary_to_list(PatternName),
                        re:replace(String, [$:|Name], [$(,$?,$<|Name] ++ 
                            [$>,$)], [{return, list}])
                end,
                RouteString, Patterns)
    end.

get_all_names(String, Regexp) ->
    get_all_names(String, Regexp, []).

get_all_names(String, Regexp, Names) ->
    case re:run(String, Regexp, [{capture, all}]) of
	nomatch ->
	    Names;
	{match, [{Start, Len}]} ->
	    Name = list_to_atom(string:substr(String, Start+3, Len-3)),
	    get_all_names(string:substr(String, Start+Len), Regexp, [Name | Names])
    end.

get_required_value(Name, Props, Error) ->
    case couch_util:get_value(Name, Props) of
        undefined ->
            throw({error, Error});
        Value ->
            Value
    end.

check_options({Options}) ->
    check_options(Options, []).

check_options([], Acc) ->
    {ok, Acc};
check_options([{K, V}|Rest], Acc) when is_binary(K)->
    check_options(Rest, [{couchapp_ng_util:to_atom(K), V}|Acc]);
check_options(_, _) ->
    throw({error, invalid_route_option}).

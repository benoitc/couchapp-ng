%%% -*- erlang -*-
%%%
%%% This file is part of couchapp_ng released under the MIT license. 
%%% See the NOTICE for more information.

-module(couchapp_ng_util).

-export([normalize_path/1, 
        remove_trailing_slash/1,
        quote_plus/1,
        join_url_path/2,
        to_atom/1
]).

%% @doc normalize path.
normalize_path(Path)  ->
    string:join(normalize_path1(string:tokens(Path,
                "/"), []), [$\/]).

remove_trailing_slash(Url) ->
    rem_slash(lists:reverse(Url)).

quote_plus(X) ->
    mochiweb_util:quote_plus(X).

% @private
normalize_path1([], Acc) ->
    lists:reverse(Acc);
normalize_path1([".."|Rest], Acc) ->
    Acc1 = case Acc of
        [] -> [".."|Acc];
        [T|_] when T =:= ".." -> [".."|Acc];
        [_|R] -> R
    end,
    normalize_path1(Rest, Acc1);
normalize_path1(["."|Rest], Acc) ->
    normalize_path1(Rest, Acc);
normalize_path1([Path|Rest], Acc) ->
    normalize_path1(Rest, [Path|Acc]).

rem_slash([]) ->
    [];
rem_slash([$\s | RevUrl]) ->
    rem_slash(RevUrl);
rem_slash([$\t | RevUrl]) ->
    rem_slash(RevUrl);
rem_slash([$\r | RevUrl]) ->
    rem_slash(RevUrl);
rem_slash([$\n | RevUrl]) ->
    rem_slash(RevUrl);
rem_slash([$/ | RevUrl]) ->
    rem_slash(RevUrl);
rem_slash(RevUrl) ->
    lists:reverse(RevUrl).

join_url_path(Src, Dst) ->
    Src2 = case lists:reverse(Src) of
        "/" ++ RestSrc -> lists:reverse(RestSrc);
        _ -> Src
    end,
    Dst2 = case Dst of
        "/" ++ RestDst -> RestDst;
        _ -> Dst
    end,
    Src2 ++ "/" ++ Dst2.

to_atom(S) when is_atom(S) ->
    S;
to_atom(S) when is_list(S) ->
    list_to_atom(S);
to_atom(S) when is_binary(S) ->
    list_to_atom(binary_to_list(S)).

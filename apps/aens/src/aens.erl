%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Resolve registered names
%%% @end
%%%-------------------------------------------------------------------

-module(aens).

%% API
-export([resolve/2,
         get_name_entry/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(LABEL_SEPARATOR, <<".">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec resolve(atom(), binary()) -> {ok, binary} | {error, atom()}.
resolve(Type, Binary) ->
    %% TODO: add validation
    case binary:match(Binary, ?LABEL_SEPARATOR) of
        nomatch ->
            aec_base58c:safe_decode(Type, Binary);
        {_,_} ->
            StateTrees = aec_conductor:top_state_trees(),
            NameTree = aec_trees:ns(StateTrees),
            case get_name(Binary, NameTree) of
                #{<<"pointers">> := Pointers} ->
                    {ok, proplists:get_value(Type, Pointers)};
                {error, _} = Err ->
                    Err
            end
    end.

-spec get_name_entry(binary()) -> {ok, map()} | {error, atom()}.
get_name_entry(Name) ->
    {ok, StateTrees} = aec_conductor:top_state_trees(),
    NSTree = aec_trees:ns(StateTrees),
    get_name(Name, NSTree).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_name(Name, NSTree) ->
    NameHash = aens_hash:name_hash(Name),
    case aens_state_tree:lookup_name(NameHash, NSTree) of
        {value, N} ->
            {ok, #{<<"name">>     => Name,
                   <<"name_ttl">> => aens_names:ttl(N),
                   <<"pointers">> => aens_names:pointers(N)}};
        none ->
            {error, name_not_found}
    end.

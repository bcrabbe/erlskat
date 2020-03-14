-module(erlskat_util).

%% API
-export([trie_to_map/1]).

trie_to_map(#{root := Root}) ->
    trie_to_map_recurse(Root).

trie_to_map_recurse(Node) ->
    error_logger:info_report(
      [{module, ?MODULE},
       {line, ?LINE},
       {function, ?FUNCTION_NAME},
       {node, ets:tab2list(Node)}]),
    case ets:lookup(Node, children) of
        [{children, #{} = NoChildren}] when map_size(NoChildren) == 0 ->
            [{data, Data}] = ets:lookup(Node, data),
            #{data => Data};
        [{children, #{} = Children}] ->
            maps:fold(
              fun(Char, Tid, Acc) ->
                      Acc#{<<Char>> => trie_to_map_recurse(Tid)}
              end,
              #{},
              Children)
    end.

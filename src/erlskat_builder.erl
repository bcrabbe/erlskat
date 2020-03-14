-module(erlskat_builder).

%% API
-export([init/0]).
-export([add_words/2]).

init() ->
    RootTid = ets:new(root_node, node_table_opts()),
    true = ets:insert(
             RootTid,
             [{char, <<>>},
              {children, #{}},
              {data, root},
              {suffix, RootTid},
              {tid, RootTid}]),
    #{root => RootTid}.

add_words(Trie, [] = _Nodes) ->
    {ok, Trie};
add_words(#{root := RootTid} = Trie,
          [NewNode | Rest] = NewNodes) when is_list(NewNodes) ->
    {Word, _Data} = NewNode,
    add_node(RootTid, NewNode, <<"">>, Word),
    add_words(Trie, Rest);
add_words(Trie, NewNode) ->
    add_words(Trie, [NewNode]).


add_node(Cursor, {<<LastChar:8>>, Data}, Path, Word) ->
    case ets:lookup(Cursor, children) of
        [{children, #{LastChar := NodeTid}}] = _NodeExists ->
            add_word_to_words_in_branch(NodeTid, Word),
            add_data_to_node(NodeTid, Data),
            NodeTid;
        [{children, CusorChildren}] = _NodeDoesNotExist ->
            NewNode = new_node(
                        Cursor,
                        LastChar,
                        Path,
                        Word,
                        Data),
            true = ets:insert(
                     Cursor,
                     [{children, CusorChildren#{LastChar => NewNode}}]),
            NewNode
    end;

add_node(Cursor, {<<Char:8, Rest/binary>>, Data}, Path, Word) ->
    case ets:lookup(Cursor, children) of
        [{children, #{Char := NodeTid}}] = _NodeExists ->
            add_word_to_words_in_branch(NodeTid, Word),
            add_node(NodeTid, {Rest, Data}, Path, Word);
        [{children, CusorChildren}] = _NodeDoesNotExist ->
            NewNode = new_node(
                        Cursor,
                        Char,
                        Path,
                        Word),
            true = ets:insert(
                     Cursor,
                     [{children, CusorChildren#{Char => NewNode}}]),
            add_node(NewNode, {Rest, Data}, Path, Word)
    end.

new_node(Parent, Char, Path, Word) ->
    NodeTid = ets:new(leaf_node, node_table_opts()),
    Suffix = suffix(Char, Parent),
    true = ets:insert(
             NodeTid,
             [{char, Char},
              {children, #{}},
              {tid, NodeTid},
              {parent, Parent},
              {suffix, Suffix},
              {dictionary_suffix, dictionary_suffix(
                                    Suffix,
                                    Parent)},
              {path, Path},
              {words_in_branch, [Word]}]),
    NodeTid.

new_node(Parent, Char, Path, Word, Data) ->
    NewNodeTid = new_node(Parent, Char, Path, Word),
    true = ets:insert(
             NewNodeTid,
             [{data, Data}]),
    NewNodeTid.

suffix(Char, Parent) ->
    [{suffix, ParentSuffix}] = ets:lookup(Parent, suffix),
    case ets:lookup(ParentSuffix, children) of
        [{children, #{Char := NodeTid}}] = _NodeExists ->
            NodeTid;
        [{children, #{} = _MapNotContainingChar}] = _NodeDoesNotExist ->
            Parent
    end.

dictionary_suffix(Suffix, Parent) ->
    case ets:lookup(Suffix, data) of
        [{data, root}] ->
            Suffix;
        [] ->
            [{dictionary_suffix, ParentsSuffix}] = ets:lookup(Parent, dictionary_suffix),
            ParentsSuffix;
        [{data, #{} = _SomeData}] ->
            Suffix
    end.

add_word_to_words_in_branch(NodeTid, Word) ->
    [{words_in_branch, ExistingWordsInBranch}] = ets:lookup(NodeTid, words_in_branch),
    ets:insert(NodeTid, {words_in_branch, [Word | ExistingWordsInBranch]}).

%% overwrites old data
add_data_to_node(NodeTid, Data) ->
    ets:insert(NodeTid, {data, Data}).

node_table_opts() ->
    [].

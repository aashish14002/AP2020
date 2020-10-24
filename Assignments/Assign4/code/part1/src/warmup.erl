-module(warmup).
-export([move/2, insert/3, lookup/2]).

% direction is one of the atoms north, south, east or west

move(north, {X, Y}) -> {X, Y+1};
move(south, {X, Y}) -> {X, Y-1};
move(east, {X,Y}) -> {X+1, Y};
move(west,  {X, Y}) -> {X-1, Y}.
% complete the definition


% A binary search tree is either
%      the atom leaf
%   or a tuple {node, Key, Value, Left, Right}
%      where Left and Right are binary search trees, and all the keys
%      in Left are smaller than Key and all the keys in Right are
%      larger than Key


% insert inserts a key and a value into a binary search tree. If the
% key is already there the value is updated.

insert(Key, Value, Tree) -> 
    case Tree of
        leaf -> {node, Key, Value, leaf, leaf};
        {node, Key, _, Left, Right} -> {node, Key, Value, Left, Right};
        {node, K, V, Left, Right} when Key<K -> {node, K, V, insert(Key, Value, Left), Right};
        {node, K, V, Left, Right} when Key>K -> {node, K, V, Left, insert(Key, Value, Right)}
    end.

% complete the definition.


% lookup find the value associated to a key in a binary search
% tree. Returns {ok, Value} if the key is in the tree; or none if the
% key is not in the tree.

lookup(Key, Tree) -> 
    case Tree of
        leaf -> none;
        {node, Key, V, _, _} -> {ok, V};
        {node, K, _, Left, _} when Key<K -> lookup(Key, Left);
        {node, K, _, _, Right} when Key>K -> lookup(Key, Right)
    end.
% complete the definition.

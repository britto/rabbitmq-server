-module(oqueue).

-export([new/0,
         in/2,
         out/1,
         peek/1,
         len/1,
         to_list/1,
         from_list/1
        ]).

-record(oqueue, {length = 0 :: non_neg_integer(),
                 rear = [] :: list(),
                 front = [] :: list(),
                 last_front_item :: undefined | term()}).

-opaque oqueue() :: #oqueue{}.

-export_type([oqueue/0]).

%% O(1)
-spec new() -> oqueue().
new() -> #oqueue{}.

-spec in(term(), oqueue()) -> oqueue().
in(Item, #oqueue{length = Len,
                 front = [_ | _] = Front,
                 last_front_item = LastFrontItem} = Q)
  when Item < LastFrontItem ->
    Q#oqueue{length = Len + 1,
             front = enqueue_front(Item, Front)};
in(Item, #oqueue{length = Len,
                 rear = Rear} = Q) ->
    Q#oqueue{length = Len + 1,
             rear = enqueue_rear(Item, Rear)}.

-spec out(oqueue()) ->
    {empty | {value, term()}, oqueue()}.
out(#oqueue{length = 0} = Q) ->
    {empty, Q};
out(#oqueue{front = [Item], length  = Len} = Q) ->
    {{value, Item}, Q#oqueue{front = [],
                             last_front_item = undefined,
                             length = Len - 1}};
out(#oqueue{front = [Item | Rem], length  = Len} = Q) ->
    {{value, Item}, Q#oqueue{front = Rem,
                             length = Len - 1}};
out(#oqueue{front = [], rear = Rear} = Q) ->
    out(Q#oqueue{front = lists:reverse(Rear),
                 rear = [],
                 last_front_item = hd(Rear)}).

-spec peek(oqueue()) ->
    empty | {value, term()}.
peek(Q) ->
    %% TODO: optimse this to not make any allocations for common cases
    {Result, _} = out(Q),
    Result.

-spec len(oqueue()) -> non_neg_integer().
len(#oqueue{length = Len}) ->
    Len.

-spec to_list(oqueue()) -> list().
to_list(#oqueue{rear = Rear, front = Front}) ->
    Front ++ lists:reverse(Rear).

-spec from_list(list()) -> oqueue().
from_list(List) ->
    lists:foldl(fun in/2, new(), List).

%% internal

enqueue_rear(Item, [H | T]) when Item < H->
    [H | enqueue_rear(Item, T)];
enqueue_rear(Item, List) ->
    [Item | List].

enqueue_front(Item, [H | T]) when Item > H->
    [H | enqueue_front(Item, T)];
enqueue_front(Item, List) ->
    [Item | List].

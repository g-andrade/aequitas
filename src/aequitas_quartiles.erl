-module(aequitas_quartiles).

-export(
   [empty/0,
    add/2,
    delete/2
   ]).

empty() ->
    {}.

add(Element, {}) ->
    Median = Element,
    Q1 = gb_sets:empty(),
    Q3 = gb_sets:from_list([Element]),
    {Median, Q1, Q3};
add(Element, {Median, Q1, Q3}) when Element < Median ->
    UpdatedQ1 = gb_sets:add(Element, Q1),
    maybe_rebalance(Median, UpdatedQ1, Q3);
add(Element, {Median, Q1, Q3}) when Element > Median ->
    UpdatedQ3 = gb_sets:add(Element, Q3),
    maybe_rebalance(Median, Q1, UpdatedQ3);
add(_Element, {Median, Q1, Q3}) ->
    {Median, Q1, Q3}.

delete(Element, {Median, Q1, Q3}) when Element < Median ->
    UpdatedQ1 = gb_sets:delete(Element, Q1),
    maybe_rebalance(Median, UpdatedQ1, Q3);
delete(Element, {Median, Q1, Q3}) when Element > Median ->
    UpdatedQ3 = gb_sets:delete(Element, Q3),
    maybe_rebalance(Median, Q1, UpdatedQ3);
delete(_Element, {Median, Q1, Q3}) ->
    {Median, PurgedQ3} = gb_sets:take_smallest(Q3),
    try gb_sets:take_largest(Q1) of
        {UpdatedMedian, UpdatedQ1} ->
            UpdatedQ3 = gb_sets:add(UpdatedMedian, PurgedQ3),
            maybe_rebalance(UpdatedMedian, UpdatedQ1, UpdatedQ3)
    catch
        error:function_clause ->
            % Q1 is empty; as Q3 is kept balanced and
            % should never contain more than (len[Q1] + 1) elements,
            % we can assume Q3 is empty too; and therefore no elements remain
            % at all.
            {}
    end.

maybe_rebalance(Median, Q1, Q3) ->
    SizeDiff = gb_sets:size(Q1) - gb_sets:size(Q3),
    if SizeDiff < -1 ->
           {Median, UpdatedQ3} = gb_sets:take_smallest(Q3),
           UpdatedQ1 = gb_sets:add(Median, Q1),
           UpdatedMedian = gb_sets:smallest(UpdatedQ3),
           maybe_rebalance(UpdatedMedian, UpdatedQ1, UpdatedQ3);
       SizeDiff > 0 ->
           {UpdatedMedian, UpdatedQ1} = gb_sets:take_largest(Q1),
           UpdatedQ3 = gb_sets:add(UpdatedMedian, Q3),
           maybe_rebalance(UpdatedMedian, UpdatedQ1, UpdatedQ3);
       true ->
           {Median, Q1, Q3}
    end.

%% Copyright (c) 2018 Guilherme Andrade
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO WORK SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(aequitas_time_interval).

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export([to_milliseconds/1]).

%%-------------------------------------------------------------------
%% Record and Type
%%-------------------------------------------------------------------

-type t() :: {unit(), number()}.
-export_type([t/0]).

-type unit() ::
    milliseconds |
    seconds |
    minutes |
    hours |
    days |
    weeks.
-export_type([unit/0]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec to_milliseconds(t()) -> {ok, non_neg_integer()} | error.
%% @private
to_milliseconds({milliseconds, HowMany}) ->
    to_milliseconds(HowMany, 1);
to_milliseconds({seconds, HowMany}) ->
    to_milliseconds(HowMany, 1000);
to_milliseconds({minutes, HowMany}) ->
    to_milliseconds(HowMany, 1000 * 60);
to_milliseconds({hours, HowMany}) ->
    to_milliseconds(HowMany, 1000 * 60 * 60);
to_milliseconds({days, HowMany}) ->
    to_milliseconds(HowMany, 1000 * 60 * 60 * 24);
to_milliseconds({weeks, HowMany}) ->
    to_milliseconds(HowMany, 1000 * 60 * 60 * 24 * 7);
to_milliseconds(_Value) ->
    error.

%%-------------------------------------------------------------------
%% Internal Function Definitions
%%-------------------------------------------------------------------

to_milliseconds(HowMany, Ratio) when is_number(HowMany), HowMany >= 0 ->
    {ok, trunc(HowMany * Ratio)};
to_milliseconds(_HowMany, _Ratio) ->
    error.

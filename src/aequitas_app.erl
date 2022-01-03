%% Copyright (c) 2018-2022 Guilherme Andrade
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
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @private
-module(aequitas_app).
-behaviour(application).

%%-------------------------------------------------------------------
%% application Function Exports
%%-------------------------------------------------------------------

-export(
   [start/2,
    stop/1,
    config_change/3
   ]).

-ignore_xref(
   [config_change/3
   ]).

%%-------------------------------------------------------------------
%% application Function Definitions
%%-------------------------------------------------------------------

-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    aequitas_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

-spec config_change([{term(), term()}], [{term(), term()}], [term()]) -> ok.
config_change(Changed, New, Removed) when Changed =/= []; New =/= []; Removed =/= [] ->
    aequitas_category_sup:async_reload_settings_in_children();
config_change(_Changed, _New, _Removed) ->
    ok.

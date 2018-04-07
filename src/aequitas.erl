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
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(aequitas).

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [ask/1,
    ask/2,
    ask/3,
    reconfigure/2
   ]).

-ignore_xref(
   [ask/1,
    ask/2,
    ask/3,
    reconfigure/2
   ]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec ask(atom())
        -> {accepted | refused, #{ deviation => number() }} |
           {error, {process_down, term()}}.
ask(Group) ->
    ask(Group, self()).

-spec ask(atom(), term())
        -> {accepted | refused, #{ deviation => number() }} |
           {error, {process_down, term()}}.
ask(Group, Id) ->
    ask(Group, Id, []).

-spec ask(atom(), term(), [aequitas_group:ask_opt()])
        -> {accepted | refused, #{ deviation => number() }} |
           {error, {process_down, term()}}.
ask(Group, Id, Opts) ->
    aequitas_group:ask(Group, Id, Opts).

-spec reconfigure(atom(), [aequitas_group:setting_opt()])
        -> ok | {error, bad_settings}.
reconfigure(Group, SettingOpts) ->
    aequitas_group:set_settings(Group, SettingOpts).

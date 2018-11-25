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

%% @private
-module(aequitas_category_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/0,
    start_child/1,
    async_reload_settings_in_children/0
   ]).

-ignore_xref(
   [start_link/0
   ]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?CB_MODULE, []).

-spec start_child([term()]) -> {ok, pid()} | {error, term()}.
start_child(Args) ->
    supervisor:start_child(?SERVER, Args).

-spec async_reload_settings_in_children() -> ok.
async_reload_settings_in_children() ->
    lists:foreach(
      fun ({_Id, undefined, _Type, _Modules}) ->
              ok;
          ({_Id, Pid, _Type, _Modules}) ->
              aequitas_category:async_reload_settings(Pid)
      end,
      supervisor:which_children(?SERVER)).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

-spec init([]) -> {ok, {supervisor:sup_flags(),
                        [supervisor:child_spec(), ...]}}.
init([]) ->
    SupFlags =
        #{ strategy => simple_one_for_one,
           intensity => 10,
           period => 1
         },
    ChildSpecs =
        [#{ id => category,
            start => {aequitas_category, start_link, []},
            restart => temporary
          }
        ],
    {ok, {SupFlags, ChildSpecs}}.

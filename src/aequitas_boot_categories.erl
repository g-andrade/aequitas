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
-module(aequitas_boot_categories).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/0
   ]).

-ignore_xref(
   [start_link/0
   ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type state() :: no_state.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?CB_MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([]) -> {ok, state()} | {stop, {atom(), term()}}.
init([]) ->
    try launch_foreknown_categories() of
        ok ->
            {ok, no_state}
    catch
        Class:Reason ->
            {stop, {Class, Reason}}
    end.

-spec handle_call(term(), {pid(), reference()}, state())
        -> {stop, unexpected_call, state()}.
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state()) -> {stop, unexpected_cast, state()}.
handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

-spec handle_info(term(), state()) -> {stop, unexpected_info, state()}.
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec launch_foreknown_categories() -> ok | no_return().
launch_foreknown_categories() ->
    AppConfig = application:get_all_env(aequitas),
    lists:foreach(
      fun ({categories, SettingOptsPerCategory}) ->
              launch_foreknown_categories(SettingOptsPerCategory);
          ({{category, Category}, _SettingOpts}) ->
              % legacy format which is not compatible with releases
              % (nor does it respect the documented sys.config
              %  constraints - all keys must be atoms)
              error({release_incompatible_static_configuration, Category});
          ({_Key, _Value}) ->
              ok
      end,
      AppConfig).

-spec launch_foreknown_categories(#{ term() => [aequitas_category:setting_opt()] }) -> ok | no_return().
launch_foreknown_categories(SettingOptsPerCategory) when is_map(SettingOptsPerCategory) ->
    lists:foreach(
      fun ({Category, SettingOpts}) ->
              launch_foreknown_category(Category, SettingOpts)
      end,
      maps:to_list(SettingOptsPerCategory)).

-spec launch_foreknown_category(term(), [aequitas_category:setting_opt()]) -> ok | no_return().
launch_foreknown_category(Category, SettingOpts) ->
    case aequitas_category:start(Category, false, SettingOpts) of
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            error(#{ category => Category,
                     reason => Reason })
    end.

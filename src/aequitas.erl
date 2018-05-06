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
   [start/1,
    start/2,
    ask/2,
    ask/3,
    async_ask/2,
    async_ask/3,
    reconfigure/2
   ]).

-ignore_xref(
   [start/1,
    start/2,
    ask/2,
    ask/3,
    async_ask/2,
    async_ask/3,
    reconfigure/2
   ]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

%% @doc Like `:async_ask/3' but with default options
%% @see start/2
%% @see reconfigure/2
-spec start(Category) -> ok | {error, Reason}
    when Category :: term(),
         Reason :: already_started.
start(Category) ->
    start(Category, []).

%% @doc Starts handler for `Category'
%%
%% <ul>
%% <li>`Category' can be any term.</li>
%% <li>`Opts' must be a list of `aequitas_category:setting_opt()' values.</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`ok' in case of success</li>
%% <li>`{error, Reason}' otherwise</li>
%% </ul>
%% @see start/1
%% @see reconfigure/2
-spec start(Category, Opts) -> ok | {error, Reason}
    when Category :: term(),
         Opts :: [aequitas_category:setting_opt()],
         Reason :: (already_started |
                    {invalid_setting_opt, term()} |
                    {invalid_setting_opts, term()}).
start(Category, Opts) ->
    case aequitas_category:start(Category, true, Opts) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            {error, already_started};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Like `:ask/3' but with default options
%% @see ask/3
%% @see async_ask/3
%% @see async_ask/2
-spec ask(Category, ActorId) -> Status
    when Category :: term(),
         ActorId :: term(),
         Status :: accepted | rejected.
ask(Category, ActorId) ->
    ask(Category, ActorId, []).

%% @doc Request permission to perform work, identified under `ActorId', within `Category'
%%
%% <ul>
%% <li>`Category' can by any term.</li>
%% <li>`ActorId' can be any term.</li>
%% <li>`Opts' must be a list of `aequitas_category:ask_opt()' values</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`accepted' if work execution was granted</li>
%% <li>`rejected' if work execution was denied</li>
%% </ul>
%% @see ask/2
%% @see async_ask/3
%% @see async_ask/2
-spec ask(Category, ActorId, Opts) -> Status | {Status, Stats}
    when Category :: term(),
         ActorId :: term(),
         Opts :: [aequitas_category:ask_opt()],
         Status :: accepted | rejected,
         Stats :: aequitas_work_stats:t().
ask(Category, ActorId, Opts) ->
    aequitas_category:ask(Category, ActorId, Opts).

%% @doc Like `:async_ask/3' but with default options
%% @see async_ask/3
%% @see ask/3
%% @see ask/2
-spec async_ask(Category, ActorId) -> {Tag, Monitor}
    when Category :: term(),
         ActorId :: term(),
         Tag :: reference(),
         Monitor :: reference().
async_ask(Category, ActorId) ->
    async_ask(Category, ActorId, []).

%% @doc Like `:ask/3' but the reply is sent asynchronously
%%
%% Returns a `{Tag, Monitor}' pair whose members can be used
%% to pattern match against the reply, which will be sent as a message
%% to the calling process in one of the following formats:
%% <ul>
%% <li>`{Tag, accepted}' if work execution as granted</li>
%% <li>`{Tag, rejected}' if work execution was denied</li>
%% <li>`{Tag, {accepted, Stats}}' if work execution as granted and stats requested</li>
%% <li>`{Tag, {rejected, Stats}}' if work execution was denied and stats requested</li>
%% <li>`{''`DOWN''`, Monitor, process, _Pid, _Reason}' in case of crash</li>
%% </ul>
%%
%% In case of a successful reply, <b>don't forget to clean `Monitor' up</b>,
%% which can be done like this:
%%      `demonitor(Monitor, [flush])'
%%
%% @see async_ask/2
%% @see ask/3
%% @see ask/2
-spec async_ask(Category, ActorId, Opts) -> {Tag, Monitor}
    when Category :: term(),
         ActorId :: term(),
         Opts :: [aequitas_category:ask_opt()],
         Tag :: reference(),
         Monitor :: reference().
async_ask(Category, ActorId, Opts) ->
    aequitas_category:async_ask(Category, ActorId, Opts).

%% @doc Tweak settings of work `Category'
%%
%% <ul>
%% <li>`Category' can be any term.</li>
%% <li>`SettingOpts' must be a list of `aequitas_category:setting_opt()' values</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`ok' in case of success</li>
%% <li>`{error, Reason}' otherwise</li>
%% </ul>
-spec reconfigure(Category, SettingOpts)
        -> ok | {error, Reason}
    when Category :: term(),
         SettingOpts :: [aequitas_category:setting_opt()],
         Reason :: {invalid_setting_opt | invalid_setting_opts, term()}.
reconfigure(Category, SettingOpts) ->
    aequitas_category:update_settings(Category, SettingOpts).

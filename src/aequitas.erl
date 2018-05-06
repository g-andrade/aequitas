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
    stop/1,
    ask/2,
    ask/3,
    async_ask/2,
    async_ask/3,
    reconfigure/2
   ]).

-ignore_xref(
   [start/1,
    start/2,
    stop/1,
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
%% @see stop/1
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
%% @see stop/1
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

%% @doc Stops a `Category' handler
%%
%% <ul>
%% <li>`Category' must correspond to a started handler</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`ok' in case of success</li>
%% <li>`{error, Reason}' otherwise</li>
%% </ul>
%% @see start/1
%% @see reconfigure/2
-spec stop(Category) -> ok | {error, Reason}
    when Category :: term(),
         Reason :: not_started.
stop(Category) ->
    aequitas_category:stop(Category).

%% @doc Like `:ask/3' but with default options
%% @see ask/3
%% @see async_ask/3
%% @see async_ask/2
-spec ask(Category, ActorId) -> Status | {error, Reason}
    when Category :: term(),
         ActorId :: term(),
         Status :: accepted | {rejected, RejectionReason},
         RejectionReason :: outlier | rate_limited,
         Reason :: not_started.
ask(Category, ActorId) ->
    ask(Category, ActorId, []).

%% @doc Request permission to perform work, identified under `ActorId', within `Category'
%%
%% <ul>
%% <li>`Category' must refer to a started category handler.</li>
%% <li>`ActorId' can be any term.</li>
%% <li>`Opts' must be a list of `aequitas_category:ask_opt()' values</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`accepted' if work execution was granted</li>
%% <li>`{rejected, Reason}' if work execution was denied</li>
%% <li>`{error, Reason}' if something went wrong</li>
%% </ul>
%% @see ask/2
%% @see async_ask/3
%% @see async_ask/2
-spec ask(Category, ActorId, Opts) -> Status | {Status, Stats} |
                                      {error, Reason}
    when Category :: term(),
         ActorId :: term(),
         Opts :: [aequitas_category:ask_opt()],
         Status :: accepted | {rejected, RejectionReason},
         RejectionReason :: outlier | rate_limited,
         Stats :: aequitas_work_stats:t(),
         Reason :: not_started.
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
%% <li>`{Tag, {rejected, Reason}}' if work execution was denied</li>
%% <li>`{Tag, {accepted, Stats}}' if work execution as granted and stats requested</li>
%% <li>`{Tag, {{rejected, Reason}, Stats}}' if work execution was denied and stats requested</li>
%% <li>`{''`DOWN''`, Monitor, process, _Pid, _Reason}' if the handler stopped</li>
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
%% <li>`Category' must refer to a started category handler.</li>
%% <li>`SettingOpts' must be a list of `aequitas_category:setting_opt()' values</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`ok' in case of success</li>
%% <li>`{error, Reason}' otherwise</li>
%% </ul>
%%
%% @see start/2
%% @see stop/1
-spec reconfigure(Category, SettingOpts)
        -> ok | {error, Reason}
    when Category :: term(),
         SettingOpts :: [aequitas_category:setting_opt()],
         Reason :: not_started | {invalid_setting_opt | invalid_setting_opts, term()}.
reconfigure(Category, SettingOpts) ->
    aequitas_category:update_settings(Category, SettingOpts).

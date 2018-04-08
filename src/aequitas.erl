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
   [ask/2,
    ask/3,
    async_ask/2,
    async_ask/3,
    configure/2
   ]).

-ignore_xref(
   [ask/2,
    ask/3,
    async_ask/2,
    async_ask/3,
    configure/2
   ]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

%% @doc Like `:ask/3' but with defaults options
%% @see ask/3
%% @see async_ask/3
%% @see async_ask/2
-spec ask(Group, Id)
        -> accepted | rejected
    when Group :: atom(),
         Id :: term().
ask(Group, Id) ->
    ask(Group, Id, []).

%% @doc Request permission to perform work identified under `Id' within `Group'
%%
%% <ul>
%% <li>`Group' must be an atom.</li>
%% <li>`Id' must be a term.</li>
%% <li>`Opts' must be a list of `aequitas_group:ask_opt()' values</li>
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
-spec ask(Group, Id, Opts)
        -> accepted | rejected
    when Group :: atom(),
         Id :: term(),
         Opts :: [aequitas_group:ask_opt()].
ask(Group, Id, Opts) ->
    aequitas_group:ask(Group, Id, Opts).

%% @doc Like `:async_ask/3' but with defaults options
%% @see async_ask/3
%% @see ask/3
%% @see ask/2
-spec async_ask(Group, Id) -> {Tag, Monitor}
    when Group :: atom(),
         Id :: term(),
         Tag :: reference(),
         Monitor :: reference().
async_ask(Group, Id) ->
    async_ask(Group, Id, []).

%% @doc Like `:ask/3' but the reply is sent asynchronously
%%
%% Returns a `{Tag, Monitor}' pair whose members can be used
%% to pattern match against the reply, which will be sent as a message
%% to the calling process in one of the following formats:
%% <ul>
%% <li>`{Tag, accepted}' if work execution as granted</li>
%% <li>`{Tag, rejected}' if work execution was denied</li>
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
-spec async_ask(Group, Id, Opts) -> {Tag, Monitor}
    when Group :: atom(),
         Id :: term(),
         Opts :: [aequitas_group:ask_opt()],
         Tag :: reference(),
         Monitor :: reference().
async_ask(Group, Id, Opts) ->
    aequitas_group:async_ask(Group, Id, Opts).

%% @doc Tweak settings of work `Group'
%%
%% <ul>
%% <li>`Group' must be an atom.</li>
%% <li>`SettingOpts' must be a list of `aequitas_group:setting_opt()' values</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`ok' in case of success</li>
%% <li>`{error, Reason}' otherwise</li>
%% </ul>
-spec configure(Group, SettingOpts)
        -> ok | {error, Reason}
    when Group :: atom(),
         SettingOpts :: [aequitas_group:setting_opt()],
         Reason :: {invalid_setting_opt | invalid_setting_opts, term()}.
configure(Group, SettingOpts) ->
    aequitas_group:set_settings(Group, SettingOpts).

%%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%---------------------------------------------------------------------------
%%% @author Eric Merrit <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Eric Merrit
%%% @doc
%%% This module provides a reasonable way to get decent semver compatible vsn
%%% from the system. This uses the rebar post_compile hook to rewrite the app
%%% file metadata with the correct version.
-module(rebar_vsn_plugin).

-export([post_compile/2]).

%%============================================================================
%% API
%%============================================================================
post_compile(Config, AppFile) ->
    {ok, [{application, AppName, SrcDetail}]} = file:consult(AppFile),
    case proplists:get_value(vsn, SrcDetail) of
        semver ->
            do_vsn_replacement(AppName, Config, AppFile);
        _ ->
            ok
    end.

%%============================================================================
%% Internal Functions
%%============================================================================

do_vsn_replacement(AppName, Config, AppFile) ->
    EbinAppFile= filename:join("ebin", erlang:atom_to_list(AppName) ++ ".app"),

    {AppName, Details0} =
        rebar_config:get_xconf(Config, {appfile, {app_file, AppFile}}, []),

    %% Get the tag timestamp and minimal ref from the system. The
    %% timestamp is really important from an ordering perspective.
    {ok, RawRef} = rebar_utils:sh("git log -n 1 --pretty=format:'%ct.%h\n' .", []),
    {ok, RawTag} = rebar_utils:sh("git describe --always --abbrev=0 --tags "
                                  "`git log -n 1 --pretty=format:%h`", []),

    %% Cleanup the tag and the Ref information. Basically leading 'v's and
    %% whitespace needs to go away.
    Tag = re:replace(RawTag, "(^v)|\\s", "", [global]),
    Ref = re:replace(RawRef, "\\s", "", [global]),

    %% Create the valid [semver](http://semver.org) version from the tag
    Vsn = erlang:binary_to_list(erlang:iolist_to_binary([Tag, "+build.", Ref])),

    %% Replace the old version with the new one
    Details1 = lists:keyreplace(vsn, 1, Details0, {vsn, Vsn}),

    write_app_file(EbinAppFile, {application, AppName, Details1}),
    {ok, rebar_config:set_xconf(Config, {appfile, {app_file, AppFile}},
                                 {AppName, Details1})}.

%%============================================================================
%% Internal Functions
%%============================================================================

write_app_file(AppFile, AppTerms) ->
    AppHeader = "%%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-\n\n",
    file:write_file(AppFile, [AppHeader, io_lib:fwrite("~p. ", [AppTerms])]).

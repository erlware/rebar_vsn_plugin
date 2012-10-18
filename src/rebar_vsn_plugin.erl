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
    {AppName, SrcDetail} =
        rebar_config:get_xconf(Config, {appfile, {app_file, AppFile}}, []),
    case proplists:get_value(vsn, SrcDetail) of
        "semver" ->
            do_vsn_replacement(AppName, Config, AppFile);
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
    RawRef = os:cmd("git log -n 1 --pretty=format:'%h\n' "),

    {Tag, TagVsn} = parse_tags(),
    RawCount =
        case Tag of
            undefined ->
                os:cmd("git rev-list HEAD | wc -l");
            _ ->
                os:cmd(io_lib:format("git rev-list ~s..HEAD | wc -l",
                                     [Tag]))
        end,

    %% Cleanup the tag and the Ref information. Basically leading 'v's and
    %% whitespace needs to go away.
    Ref = re:replace(RawRef, "\\s", "", [global]),
    Count = erlang:iolist_to_binary(re:replace(RawCount, "\\s", "", [global])),

    %% Create the valid [semver](http://semver.org) version from the tag
    Vsn = case Count of
              <<"0">> ->
                  erlang:binary_to_list(erlang:iolist_to_binary(TagVsn));
              _ ->
                  erlang:binary_to_list(erlang:iolist_to_binary([TagVsn, "+build.",
                                                                 Count, ".", Ref]))
          end,

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

parse_tags() ->
    first_valid_tag(os:cmd("git log --oneline --decorate  | fgrep \"tag: \" -1000")).

first_valid_tag(Line) ->
    case re:run(Line, "(\\(|\\s)tag:\\s(v([^,\\)]+))", [{capture, [2, 3], list}]) of
        {match,[Tag, Vsn]} ->
            {Tag, Vsn};
        nomatch ->
            {undefined, "0.0.0"}
    end.

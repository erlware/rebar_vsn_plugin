%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%%
%% @author Eric Merritt <ericbmerritt@gmail.com>
%% @copyright Erlware, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

{application, rebar_vsn_plugin,
 [{description, "Correct version manipulation for rebar"},
  {vsn, git},
  {modules, []},
  {registered, []},
  {applications, [kernel, stdlib]}]}.

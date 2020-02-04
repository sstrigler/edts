%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc get_module_info command
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_cmd_get_function_info).

-behaviour(edts_cmd).

%%%_* Exports ==================================================================

%% API
-export([spec/0,
         execute/1]).

%%%_* Includes =================================================================
%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================

spec() ->
  [nodename, module, function, arity].

execute(Ctx) ->
    Node     = orddict:fetch(nodename, Ctx),
    Module   = orddict:fetch(module, Ctx),
    Function = orddict:fetch(function, Ctx),
    Arity    = orddict:fetch(arity, Ctx),
    {ok, Info} = edts:call(Node,
                           edts_code,
                           get_function_info,
                           [Module, Function, Arity]),
    {ok, maps:from_list(lists:map(fun format_element/1, Info))}.

%%%_* Internal functions =======================================================

format_element({module, M}) ->
  {<<"module">>, atom_to_binary(M, utf8)};
format_element({function, F}) ->
  {<<"function">>, atom_to_binary(F, utf8)};
format_element({arity, A}) ->
  {<<"arity">>, A};
format_element({exported, ExportedP}) ->
  {<<"exported">>, ExportedP};
format_element({source, Source}) ->
  {<<"source">>, list_to_binary(Source)};
format_element({line, Line}) ->
  {<<"line">>, Line}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

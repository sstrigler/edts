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
-module(edts_cmd_get_module_info).

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
  [nodename, module, info_level].

execute(Ctx) ->
  Node   = orddict:fetch(nodename, Ctx),
  Module = orddict:fetch(module, Ctx),
  Level  = orddict:fetch(info_level, Ctx),
  case edts:call(Node, edts_code, get_module_info, [Module, Level]) of
    {ok, {ok, Info}}     ->
      {ok, maps:from_list(lists:map(fun format_element/1 , Info))};
    {ok, {error, _} = E} -> E
  end.

%%%_* Internal functions =======================================================

-spec format_element( {compile_cwd, string()} |
                      {exports,     [[{function, function()} |
                                      {arity, arity()}]]} |
                      {imports,     [[ {module, module()} |
                                       {function, function()} |
                                       {arity, non_neg_integer()}]]} |
                      {includes,    [string()]} |
                      {functions,   [[{module, module()} |
                                      {function, function()} |
                                      {arity, non_neg_integer()} |
                                      {exported, boolean()} |
                                      {source, string()} |
                                      {line, pos_integer()}]]} |
                      {records,     [[{record, atom()} |
                                      {fields, [atom()]} |
                                      {line, pos_integer()} |
                                      {source, string()}]]} |
                      {module, module()} |
                      {source, string()}
                    ) -> {binary(), term()}.

format_element({compile_cwd, C}) ->
  {<<"compile_cwd">>, list_to_binary(C)};
format_element({exports, Exports}) ->
  {<<"exports">>,
   [lists:map(fun format_fun_elements/1, Es) || Es <- Exports, is_list(Es)]};
format_element({source, Source}) ->
  {<<"source">>, list_to_binary(Source)};
%% format_element({time, {{Y, Mo, D}, {H, Mi, S}}}) ->
%%   Fmt = "~b-~2.10.0b-~2.10.0b ~2.10.0b:~2.10.0b:~2.10.0b",
%%   Str = lists:flatten(io_lib:format(Fmt, [Y, Mo, D, H, Mi, S])),
%%   {<<"time">>, list_to_binary(Str)};
format_element({records, Recs0}) ->
  Recs = [lists:map(fun format_rec_element/1, Rec) || Rec <- Recs0],
  {<<"records">>, Recs};
format_element({functions, Funs0}) ->
  Funs = [lists:map(fun format_fun_elements/1, Fun) || Fun <- Funs0],
  {<<"functions">>, Funs};
format_element({imports, Imports0}) ->
  Imports = [lists:map(fun format_fun_elements/1, Imp) || Imp <- Imports0],
  {<<"imports">>, Imports};
format_element({includes, Includes}) ->
  {<<"includes">>, [list_to_binary(I) || I <- Includes]};
format_element({module, M}) ->
  {<<"module">>, atom_to_binary(M, utf8)}.

format_fun_elements({module, M}) ->
  {<<"module">>, atom_to_binary(M, utf8)};
format_fun_elements({function, F}) ->
  {<<"function">>, atom_to_binary(F, utf8)};
format_fun_elements({arity, A}) ->
  {<<"arity">>, A};
format_fun_elements({exported, B}) ->
  {<<"exported">>, B};
format_fun_elements({source, Source}) ->
  {<<"source">>, list_to_binary(Source)};
format_fun_elements({line, L}) ->
  {<<"line">>, L}.

format_rec_element({record, R}) ->
  {<<"record">>, atom_to_binary(R, utf8)};
format_rec_element({fields, Fs}) ->
  {<<"fields">>, [atom_to_binary(F, utf8) || F <- Fs]};
format_rec_element({line, L}) ->
  {<<"line">>, L};
format_rec_element({source, S}) ->
  {<<"source">>, list_to_binary(S)}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

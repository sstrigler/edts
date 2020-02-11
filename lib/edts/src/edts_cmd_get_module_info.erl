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

-spec execute(edts_cmd:ctx()) -> {ok, #{compile_cwd => string(),
                                        exports => [#{function => atom(),
                                                      arity => arity()}],
                                        imports => [ #{module => module(),
                                                       function => atom(),
                                                       arity => non_neg_integer()}],
                                        includes => [string()],
                                        functions => [#{module => module(),
                                                        function => atom(),
                                                        arity => non_neg_integer(),
                                                        exported => boolean(),
                                                        source => string(),
                                                        line => pos_integer()}],
                                        records => [#{record => atom(),
                                                      fields => [atom()],
                                                      line => pos_integer(),
                                                      source => string()}],
                                        module => module(),
                                        source => string()}}.

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

format_element({K, Vs}) when K =:= exports;
                             K =:= records;
                             K =:= functions;
                             K =:= imports ->
  {K, [maps:from_list(V) || V <- Vs]};
format_element(H) ->
    H.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

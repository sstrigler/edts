%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc get_event command
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
-module(edts_cmd_get_event).

-behaviour(edts_cmd).

%%%_* Exports ==================================================================

%% API
-export([spec/0,
         execute/1]).

%%%_* Includes =================================================================
-include_lib("kernel/include/logger.hrl").

%%%_* Defines ==================================================================
%%%_* Types ====================================================================
%%%_* API ======================================================================

spec() ->
  [].

execute(Ctx) ->
    try
        {ok, Event} = edts_event:listen(),
        {ok, #{<<"event">> => lists:map(fun format_events/1, Event)}}
    catch
        C:E:S ->
            ?LOG_ERROR("Event Listener failed with ~p:~p~nStacktrace:~n~p",
                       [C,E,S]),
            execute(Ctx)
    end.

%%%_* Internal functions =======================================================

format_events({node, N}) when is_atom(N) ->
  {<<"node">>, atom_to_binary(N, utf8)};
format_events({class, C}) when is_atom(C) ->
  {<<"class">>, atom_to_binary(C, utf8)};
format_events({type, Ty}) when is_atom(Ty) ->
  {<<"type">>, atom_to_binary(Ty, utf8)};
format_events({info, I}) ->
  {<<"type">>, lists:map(fun format_info/1, I)}.

format_info({K, V}) when is_atom(V) ->
  {atom_to_binary(K, utf8), atom_to_binary(V, utf8)};
format_info({K, V}) when is_integer(V) ->
  {atom_to_binary(K, utf8), V}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

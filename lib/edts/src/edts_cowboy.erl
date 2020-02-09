%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc HTTP server
%%% @end
%%% @author Sebastian Weddmark Olsson <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2020 Sebastian Weddmark Olsson <visnae@gmail.com>
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
-module(edts_cowboy).

-behaviour(cowboy_rest).

-export([start_link/0]).

-export([init/2,
         allowed_methods/2,
         forbidden/2,
         content_types_accepted/2,
         from_json/2
        ]).

%%%_* Includes =================================================================
-include_lib("kernel/include/logger.hrl").

%%%_* Defines ==================================================================

-define(EDTS_PORT_DEFAULT, 4587).

-define(AVAILABLE_CMDS,
        [compile_and_load,
         get_event,
         get_free_vars,
         get_function_info,
         get_mfas,
         get_module_info,
         get_modules,
         get_nodes,
         init_node,
         pretty_print,
         run_eunit
        ]).

%%%_* Types ====================================================================
%%%_* API ======================================================================

start_link() ->
  Dispatch = cowboy_router:compile(
               [{'_', [{"/:command", ?MODULE, [command]},
                       {"/lib/:plugin/:command", ?MODULE, [plugin]}
                      ]}
               ]),
  Port = application:get_env(edts, port, ?EDTS_PORT_DEFAULT),
  {ok, _} = cowboy:start_clear(?MODULE,
                               [{port, Port}],
                               #{env => #{dispatch => Dispatch}}).

init(Req, [State]) ->
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

forbidden(Req, command = State) ->
  Cmd = cowboy_req:binding(command, Req),
  {not lists:member(binary_to_atom(Cmd, utf8), ?AVAILABLE_CMDS), Req, State};
forbidden(Req, plugin = State) ->
  Plg = cowboy_req:binding(plugin, Req),
  {not lists:member(binary_to_atom(Plg, utf8), edts_plugins:names()), Req, State}.

content_types_accepted(Req, State) ->
  {[{{ <<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

from_json(Req0, State) ->
  Cmd = binary_to_atom(cowboy_req:binding(command, Req0), utf8),
  {ok, Body, Req1} = read_body(Req0, <<>>),
  InputCtx = case Body of
               <<>> ->
                 orddict:new();
               <<"null">> ->
                 orddict:new();
               _ ->
                 case jsone:decode(Body, [{object_format, proplist}]) of
                   [{}] ->
                     orddict:new();
                   null ->
                     orddict:new();
                   Decoded ->
                     orddict:from_list([{binary_to_atom(K, utf8), convert_type(V)}
                                        || {K, V} <- Decoded])
                 end
             end,
  R = case State of
        command ->
          edts_cmd:execute(Cmd, InputCtx);
        plugin ->
          Plugin = binary_to_atom(cowboy_req:binding(plugin, Req1), utf8),
          edts_plugins:execute(Plugin, Cmd, InputCtx)
      end,
  Data = case R of
          ok -> <<"{}">>;
          {ok, D} ->
             ?LOG_DEBUG("Trying to encode: ~p", [D]),
             try jsone:encode(D) of
                 Enc -> Enc
             catch
               {'EXIT', E} ->
                 ?LOG_ERROR(#{error => E,
                              data => D,
                              cmd => Cmd
                             }),
                 <<"{}">>
             end;
           {error, Err} ->
             ?LOG_ERROR(#{error => Err, cmd => Cmd, input_ctx => InputCtx}),
             <<"{}">>
         end,
  {true, cowboy_req:set_resp_body(Data, Req1), State}.

%%%_* Internal functions =======================================================

read_body(Req0, Acc) ->
  case cowboy_req:read_body(Req0) of
    {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
    {more, Data, Req} -> read_body(Req, <<Acc/binary, Data/binary>>)
  end.

convert_type(V) when is_integer(V) ->
  integer_to_list(V);
convert_type(V) when is_binary(V) ->
  binary_to_list(V);
convert_type(V) when is_atom(V) ->
  atom_to_list(V);
convert_type(L) when is_list(L) ->
  [convert_type(V) || V <- L].



%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

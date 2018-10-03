%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @doc
%%%         returnatom root supervisor implementation.
%%% @end
%%% @hidden

%% -----------------------------------------------------------------------------
-module(returnatom_sup).
-author("pouriya.jahanbakhsh@gmail.com").
-behaviour(supervisor).
%% -----------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/0
        ,start/1
        ,stop/1
        ,modules/0]).

%% 'supervisor' callback:
-export([init/1]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(PROC, ?MODULE).

%% -----------------------------------------------------------------------------
%% API:

-spec
start_link() ->
    {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link({local, ?PROC}, ?MODULE, undefined).


start(Mod) ->
    case supervisor:start_child(?PROC, {Mod
                                       ,{returnatom_server, start_link, [Mod]}
                                       ,temporary
                                       ,infinity
                                       ,worker
                                       ,[returnatom_server]}) of
        {ok, _} ->
            ok;
        Err ->
            Err
    end.


stop(Mod) ->
    supervisor:terminate_child(?PROC, Mod).


modules() ->
    [erlang:element(1, X) || X <- supervisor:which_children(?PROC)].

%% -----------------------------------------------------------------------------
%% 'supervisor' callbacks:

%% @hidden
init(_) -> % (undefined)
    {ok, {{one_for_all, 0, 1}, []}}.

%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @doc
%%%         Library for creating dynamic module which its functions yield atoms.
%%% @end

%% -----------------------------------------------------------------------------
-module(returnatom).
-author("pouriya.jahanbakhsh@gmail.com").
%% -----------------------------------------------------------------------------
%% Exports:

%% API:
-export([start/0
        ,stop/0
        ,start/1
        ,stop/1
        ,add/3
        ,replace/3
        ,delete/2
        ,modules/0
        ,start_link/1
        ,stop_link/1]).

%% -----------------------------------------------------------------------------
%% API:

-spec
start() ->
    ok | {error, term()}.
%% @doc
%%     Starts returnatom application.
%% @end
start() ->
    application:start(returnatom).


-spec
stop() ->
    ok.
%% @doc
%%     Stops returnatom application.
%% @end
stop() ->
    application:stop(returnatom).


-spec
start(atom()) ->
    ok.
%% @doc
%%     Starts a compiler server under root supervisor.
%% @end
start(Mod) when erlang:is_atom(Mod) ->
    returnatom_sup:start(Mod).


-spec
stop(atom()) ->
    ok | {error, term()}.
%% @doc
%%     Stops a compiler server from root supervisor.
%% @end
stop(Mod) when erlang:is_atom(Mod) ->
    returnatom_sup:stop(Mod).


-spec
add(atom(), atom(), atom()) ->
    ok | {error, term()}.
%% @doc
%%     Adds a function with specified return value to available module.<br/>
%%     Note that if function exists, it does not replace it.
%% @end
add(Name, Func, ReturnAtom) ->
    returnatom_server:add(Name, Func, ReturnAtom).


-spec
replace(atom(), atom(), atom()) ->
    ok | {error, term()}.
%% @doc
%%     Adds a function with specified return value to available module or
%%     replace it if exists.
%% @end
replace(Name, Func, ReturnAtom) ->
    returnatom_server:replace(Name, Func, ReturnAtom).


-spec
delete(atom(), atom()) ->
    ok | {error, term()}.
%% @doc
%%     Deletes a function from module if exists.
%% @end
delete(Name, Func) ->
    returnatom_server:delete(Name, Func).


-spec
modules() ->
    [module()] | [].
%% @doc
%%     Gives list of available modules which their server processes are under
%%     root supervision.
%% @end
modules() ->
    returnatom_sup:modules().


-spec
start_link(module()) ->
    {ok, pid()} | {error, term()}.
%% @doc
%%     Starts and links server process to caller. Note that server process will
%%     be registered with module name locally.
%% @end
start_link(Mod) ->
    returnatom_server:start_link(Mod).


-spec
stop_link(atom()) ->
    ok.
%% @doc
%%     Stops a server process directly.
%% @end
stop_link(Name) ->
    returnatom_server:stop(Name).
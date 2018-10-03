%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(returnatom_server).
-author("pouriya").
-behaviour(gen_server).
%% -----------------------------------------------------------------------------
%% Exports:

%% API
-export([start_link/1
        ,add/3
        ,replace/3
        ,delete/2
        ,stop/1]).

%% 'gen_server' callbacks:
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(ADD_TAG, add).
-define(DELETE_TAG, delete).
-define(STOP_TAG, stop).
-define(REPLACE_TAG, replace).

%% -----------------------------------------------------------------------------
%% API:

start_link(Mod) when erlang:is_atom(Mod) ->
    gen_server:start_link({local, Mod}, ?MODULE, Mod, []).


add(Name, FuncName, ReturnAtom) when erlang:is_atom(Name)     andalso
                                     erlang:is_atom(FuncName) andalso
                                     erlang:is_atom(ReturnAtom)    ->
    gen_server:call(Name, {?ADD_TAG, FuncName, ReturnAtom}).


replace(Name, FuncName, ReturnAtom) when erlang:is_atom(Name)     andalso
                                         erlang:is_atom(FuncName) andalso
                                         erlang:is_atom(ReturnAtom)    ->
    gen_server:call(Name, {?REPLACE_TAG, FuncName, ReturnAtom}).


delete(Name, FuncName) when erlang:is_atom(Name) andalso
                            erlang:is_atom(FuncName)  ->
    gen_server:call(Name, {?DELETE_TAG, FuncName}).


stop(Name) when erlang:is_atom(Name) ->
    gen_server:call(Name, ?STOP_TAG).

%% -----------------------------------------------------------------------------
%% 'gen_server' callbacks:

%% @hidden
init(Mod) ->
    erlang:process_flag(trap_exit, true),
    _ = code:load_file(Mod),
    case erlang:function_exported(Mod, module_info, 0) of
        false ->
            build_module(Mod, []),
            {ok, Mod};
        _ -> % true
            {stop, {already_loaded, [{module, Mod}]}}
    end.


%% @hidden
handle_call({?REPLACE_TAG, Func, ReturnAtom}, _, Mod) ->
    Result = do_replace(Mod, Func, ReturnAtom),
    _ = maybe_reload(Result, Mod),
    {reply, Result, Mod};
handle_call({?ADD_TAG, Func, ReturnAtom}, _, Mod) ->
    Result = do_add(Mod, Func, ReturnAtom),
    _ = maybe_reload(Result, Mod),
    {reply, Result, Mod};
handle_call({?DELETE_TAG, Func}, _, Mod) ->
    Result = do_delete(Mod, Func),
    _ = maybe_reload(Result, Mod),
    {reply, Result, Mod};
handle_call(?STOP_TAG, _, Mod) ->
    {stop, normal, ok, Mod};
handle_call(Request, _, S) ->
    {reply, {error, {unknown_request, [{request, Request}]}}, S}.


%% @hidden
handle_cast(_, S) ->
    {noreply, S}.


%% @hidden
handle_info(_, S) ->
    {noreply, S}.


%% @hidden
terminate(_, Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    ok.


%% @hidden
code_change(_, S, _) ->
    {ok, S}.

%% -----------------------------------------------------------------------------
%% Internals:

maybe_reload(ok, Mod) ->
    code:soft_purge(Mod),
    code:load_file(Mod);
maybe_reload(_, _) ->
    ok.

    

build_module(Mod, ExtraForms) ->
    ModAttr = attribute(module, [atom(Mod)]),
    ExportAllAttr = attribute(compile, [atom(export_all)]),
    Forms = [ModAttr, ExportAllAttr | ExtraForms],
    {ok, _, Binary} = compile:forms(Forms, [return_errors
                                           ,nowarn_export_all
                                           ,debug_info]),
    {module, _} = code:load_binary(Mod, erlang:atom_to_list(Mod), Binary),
    ok.


do_add(Mod, Func, ReturnAtom) ->
    case lists:keyfind(Func, 1, Mod:module_info(exports)) of
        {_, _} ->
            case Mod:Func() of
                ReturnAtom ->
                    {error, {already_exists, [{function, Func}]}};
                Other ->
                    {error, {already_exists, [{function, Func}
                                             ,{return_value, Other}]}}
            end;
        _ -> % false
            build_module(Mod, [function(Func, ReturnAtom) | get_functions(Mod)])
    end.


do_replace(Mod, Func, ReturnAtom) ->
    case lists:keyfind(Func, 1, Mod:module_info(exports)) of
        {_, _} ->
            build_module(Mod
                        ,[function(Func, ReturnAtom)
                         |get_functions(Mod, Func)]);
        _ -> % false
            build_module(Mod, [function(Func, ReturnAtom) | get_functions(Mod)])
    end.


do_delete(Mod, Func) ->
    case lists:keyfind(Func, 1, Mod:module_info(exports)) of
        {_, _} ->
            build_module(Mod, get_functions(Mod, Func));
        _ -> % false
            {error, {not_found, [{function, Func}]}}
    end.


get_functions(Mod) ->
    [function(Name, Mod:Name()) || {Name, Arity} <- Mod:module_info(exports)
        ,Arity == 0 andalso Name /= module_info].


get_functions(Mod, Func) ->
    [function(Name, Mod:Name()) || {Name, Arity} <- Mod:module_info(exports)
        ,Arity == 0 andalso Name /= module_info andalso Name /= Func].


atom(Name) ->
    abstract(atom, [Name]).


attribute(Name, Args) ->
    abstract(attribute, [atom(Name), Args]).

    
function(Name, ReturnAtom) ->
    abstract(function, [atom(Name), [clause([], none, [atom(ReturnAtom)])]]).


clause(Args, Guards, Bodies) ->
    abstract(clause, [Args, Guards, Bodies]).


abstract(Type, Args) ->
    erl_syntax:revert(erlang:apply(erl_syntax, Type, Args)).
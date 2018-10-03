%% -----------------------------------------------------------------------------
-module(returnatom_SUITE).
-author("pouriya.jahanbakhsh@gmail.com").
%% -----------------------------------------------------------------------------
%% Exports:

%% ct callbacks:
-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

%% Testcases:
-export(['1'/1]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%% ct callbacks:


all() ->
    IsInteger =
        fun(Func) ->
            try
                _ = erlang:list_to_integer(erlang:atom_to_list(Func)),
                true
            catch
                _:_ ->
                    false
            end
        end,
    % '1', '2', ...
    lists:sort([Func || {Func, Arity} <- ?MODULE:module_info(exports)
               ,Arity == 1 andalso IsInteger(Func)]).


init_per_suite(Cfg) ->
    application:start(sasl),
    returnatom:start(),
    Cfg.


end_per_suite(Cfg) ->
    application:stop(sasl),
    returnatom:stop(),
    Cfg.


init_per_testcase(_TestCase, Cfg) ->
    Cfg.


end_per_testcase(_TestCase, _Cfg) ->
    ok.

%% -----------------------------------------------------------------------------
%% Test cases:


'1'(Cfg) ->
    _ = Cfg,
    ?assertEqual([], returnatom:modules()),
    ?assertEqual(false, erlang:function_exported(foo, module_info, 0)),
    ?assertEqual(ok, returnatom:start(foo)),
    ?assertEqual([foo], returnatom:modules()),
    ?assertMatch({error, _}, returnatom:start(foo)),
    ?assertEqual(true, erlang:function_exported(foo, module_info, 0)),
    
    ?assertEqual(false, erlang:function_exported(foo, bar, 0)),
    ?assertEqual(ok, returnatom:add(foo, bar, baz)),
    ?assertEqual(true, erlang:function_exported(foo, bar, 0)),
    ?assertEqual(baz, foo:bar()),
    ?assertMatch({error, _}, returnatom:add(foo, bar, baz)),
    ?assertMatch({error, _}, returnatom:add(foo, bar, qux)),
    ?assertEqual(ok, returnatom:replace(foo, bar, qux)),
    ?assertEqual(qux, foo:bar()),
    
    ?assertEqual(ok, returnatom:delete(foo, bar)),
    ?assertEqual(false, erlang:function_exported(foo, bar, 0)),
    ?assertMatch({error, _}, returnatom:delete(foo, bar)),
    
    ?assertEqual(ok, returnatom:add(foo, bar, baz)),
    ?assertEqual(ok, returnatom:replace(foo, baz, qux)),
    ?assertEqual(true, erlang:function_exported(foo, bar, 0)),
    ?assertEqual(true, erlang:function_exported(foo, baz, 0)),
    ?assertEqual(ok, returnatom:stop(foo)),
    ?assertMatch({error, _}, returnatom:stop(foo)),
    ?assertEqual(false, erlang:function_exported(foo, module_info, 0)),
    
    ?assertMatch({ok, _}, returnatom:start_link(foo)),
    ?assertEqual(true, erlang:function_exported(foo, module_info, 0)),
    ?assertMatch({error, {unknown_request, _}}, gen_server:call(foo, undef)),
    _ = gen_server:cast(foo, undef),
    _ = foo ! undef,
    _ = returnatom_server:code_change(1, 2, 3),
    ?assertEqual(ok, returnatom:stop_link(foo)),
    ?assertEqual(false, erlang:function_exported(foo, module_info, 0)),
    
    erlang:process_flag(trap_exit, true),
    ?assertMatch({error, {already_loaded, _}}, returnatom:start_link(gen_server)).
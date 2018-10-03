%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @doc
%%%         returnatom application behaviour implementation.
%%% @end
%%% @hidden

%% -----------------------------------------------------------------------------
-module(returnatom_app).
-author("pouriya.jahanbakhsh@gmail.com").
-behaviour(application).
%% -----------------------------------------------------------------------------
%% Exports:

%% 'application' callbacks:
-export([start/2
        ,stop/1]).

%% -----------------------------------------------------------------------------
%% 'application' callbacks:

%% @hidden
start(_, _) -> % (Type, InitArg)
    returnatom_sup:start_link().


%% @hidden
stop(_) -> % (State)
    ok.

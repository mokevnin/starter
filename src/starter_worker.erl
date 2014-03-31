-module(starter_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -record(state, {conn}).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  process_flag(trap_exit, true),
  {ok, undefined_state}.

handle_call({evaluate, Lang, Code}, _From, State) ->
  {ok, DockerImage} = application:get_env(starter, docker_image),
  Runner = "DOCKER_HOST=tcp://localhost:4243 docker run",
  LangCommand = langs:command(Lang, Code),
  DockerCommand = lists:concat([Runner, " ", DockerImage, " ", LangCommand]),

  lager:debug([DockerCommand]),
  Reply = case exec:run(DockerCommand, [stdout, stderr, sync]) of
    {ok, Stdout} -> {ok, [{stderr, []} | Stdout]};
    {error, Stderr} -> {ok, [{stdout, []} | Stderr]}
  end,
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

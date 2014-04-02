-module(starter_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {waiting}).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  process_flag(trap_exit, true),

  Waiting = queue:new(),
  {ok, #state{waiting=Waiting}}.

handle_call({evaluate, Lang, Code}, From, State) ->
  #state{waiting=Waiting} = State,

  {ok, DockerImage} = application:get_env(starter, docker_image),
  UUID = uuid:to_string(uuid:uuid1()),
  Runner = "DOCKER_HOST=tcp://localhost:4243 docker run",
  LangCommand = langs:command(Lang, Code),
  DockerCommand = lists:concat([Runner, " ", DockerImage, " ", LangCommand, "; echo '", UUID, "'"]),

  lager:debug([DockerCommand]),
  exec:run(DockerCommand, [stdout, stderr]),
  NewWaiting = queue:in({From, UUID, [], []}, Waiting),
  {noreply, State#state{waiting=NewWaiting}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Msg, #state{waiting=Waiting} = State) ->
  {{value, {From, UUID, Stdout, Stderr}} = Member, WaitingWithoutCurr} = queue:out(Waiting),

  %% lager:debug("~p", [Member]),
  %% lager:debug("~p", [Msg]),

  EofMarker =list_to_binary([UUID, "\n"]),
  NewWaiting = case Msg of
                 {stdout, _, EofMarker} ->
                   Reply = [{stderr, list_to_binary(Stderr)}, {stdout, list_to_binary(Stdout)}],
                   gen_server:reply(From, Reply),
                   WaitingWithoutCurr;
                 {stdout, _, Data} ->
                   Item = {From, UUID, [Data | Stdout], Stderr},
                   queue:in_r(Item, WaitingWithoutCurr);
                 {stderr, _, Data} ->
                   Item = {From, UUID, Stdout, [Data | Stderr]},
                   queue:in_r(Item, WaitingWithoutCurr)
               end,
  {noreply, State#state{waiting=NewWaiting}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

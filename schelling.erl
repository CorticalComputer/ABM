-module(schelling).
-compile(export_all).

%start/0 is a wrapper for start/6, in which we define the number of swimmers and surfers to be created, and the grid size. We also define here what the ratio of the neighbors must be of the same type for each agent for it to stay put. The function spawns the environment process.
start()->start(20,0.33,20,0.5,8,8).
start(TotSurfers,SurferRatio,TotSwimmers,SwimmerRatio,XRange,YRange)->
	Scape_PId=spawn(?MODULE,init,[TotSurfers,SurferRatio,TotSwimmers,SwimmerRatio,XRange,YRange]),
	register(schelling,Scape_PId).

%init/6 spawns surfer and swimmer agents, each with its own process, PId, and a random position on the grid. Afterwards, the init function executes grid/5 to enter the environment loop.
init(TotSurfers,SurferRatio,TotSwimmers,SwimmerRatio,X,Y)->
	random:seed(now()),
	Surfer_PIds=spawn_agents(surfer,TotSurfers,X,Y,SurferRatio,[]),
	Swimmer_PIds=spawn_agents(swimmer,TotSwimmers,X,Y,SwimmerRatio,[]),
	Agent_PIds = Surfer_PIds++Swimmer_PIds,
	grid(Agent_PIds,Agent_PIds,X,Y,1).

%spawn_agents/6 function first finds a new empty location on the grid using the find_empty_loc/2, and then spawns an agent process of the particular type (either swimmer or surfer), initializing it with its location and ratio, and then adds that agent's PId to the accumulator. Once the spawn_agents/6 has spawned the required number of agents, it returns the list of accumulated PIds.
	spawn_agents(_Type,0,_XRange,_YRange,_Ratio,Acc)->
		Acc;
	spawn_agents(Type,Index,XRange,YRange,Ratio,Acc)->
		{X,Y}=find_empty_loc(XRange,YRange),
		Agent_PId = spawn(?MODULE,Type,[self(),{X,Y},Ratio]),
		put({X,Y},{Agent_PId,Type}),
		spawn_agents(Type,Index-1,XRange,YRange,Ratio,[Agent_PId|Acc]).

%find_empty_loc/2 checks the process' dictionary if the randomly generated coordinate already has an agent in it, if it does not, the function returns the coordinate, and if it does, the function checks a new random coordinate.
	find_empty_loc(XRange,YRange)->
		Random_X = random:uniform(XRange),
		Random_Y = random:uniform(YRange),
		case get({Random_X,Random_Y}) of
			undefined -> {Random_X,Random_Y};
			_ -> find_empty_loc(XRange,YRange)
		end.

%grid/5 is the environment/scape process, it understands 5 types of messages, 2 from agents, and 4 from the researcher which can put the environment on pause, continue, and terminate it. When the environment is terminated, it sends termination messages to the agents, terminating all agents and then terminating itself. When the scape process receives the message of the form: {Agent_PId,Loc,get_LocalState}, the scape extracts 8 positions directly neighboring the Location (Loc) that the agent sent it, and returns the list back to it. Each location on the gird either contains nothing in it and thus the atom undefined, or an agent, specified by the tuple {Agent_PId,Type}, where the Type is either the atom swimmer or surfer. When the scape process receives the message: {Agent_PId,Type,Loc,Choice} from the agent, it simply uses the atom stored in Choice as a function name, executing it. This can either be stay or move. In this manner the scape process accepts a message from each agent, then calculates for every empty coordinate whether it belongs to the surfers or the swimmers (depending whether there are more surfers or swimmers directly neighboring it). Finally, the scape process loops back, accepting a new set of messages from the agents.
grid(_,MPIds,_,_,50)->
	[APId ! terminate || APId <-MPIds],
	io:format("Terminating scape:~p~n",[self()]);	
grid([Agent_PId|PIds],MPIds,XRange,YRange,RoundIndex)->
	receive
		{Agent_PId,{Target_X,Target_Y},get_LocalState} ->
			Local_State = [get({X,Y})|| X <- [Target_X-1,Target_X,Target_X+1],Y<-[Target_Y-1,Target_Y,Target_Y+1]],
			Agent_PId ! {self(),Local_State},
			grid([Agent_PId|PIds],MPIds,XRange,YRange,RoundIndex);
		{Agent_PId,Type,Loc,Choice}->
			U_Loc = ?MODULE:Choice(Agent_PId,Type,Loc,XRange,YRange),
			Agent_PId ! {self(),updated_loc,U_Loc},
			grid(PIds,MPIds,XRange,YRange,RoundIndex);
		pause ->
			receive
				continue ->
					grid([Agent_PId|PIds],MPIds,XRange,YRange,RoundIndex);
				terminate ->
					[APId ! terminate || APId <-MPIds],
					io:format("Terminating scape:~p~n",[self()])
			end;
		terminate ->
			[APId ! terminate || APId <-MPIds],
			io:format("Terminating Scape: ~p~n",[self()])
	end;
grid([],MPIds,XRange,YRange,RoundIndex)->
	{A,B}=gather_stats(1,1,XRange+1,YRange,0,0),
	io:format("Round Index: ~p A:~p B:~p~n",[RoundIndex,A,B]),
	grid(MPIds,MPIds,XRange,YRange,RoundIndex+1).

%stay/5 returns the original location of the agent, since the agent will not be moving.	
	stay(_Agent_PId,_Type,Loc,_XRange,_YRange)->
		Loc.
%move/5 first finds a new empty coordinate on the grid, and then moves the agent from its current coordinate to the new one, by erasing the presence of the agent from its original coordinate, and putting its presence on the new one. Finally, it returns the new coordinate of the agent to the function caller.
	move(Agent_PId,Type,{Target_X,Target_Y},XRange,YRange)->
		{X,Y}=find_empty_loc(XRange,YRange),
		erase({Target_X,Target_Y}),
		put({X,Y},{Agent_PId,Type}),
		{X,Y}.

%gather_stats/6 calculates for each empty coordinate on the grid whether it belongs to surfers or swimmers, depending on whether there are more swimmers or surfers directly neighboring it.
	gather_stats(XRange,YRange,XRange,YRange,AccA,AccB)->
		{AccA,AccB};
	gather_stats(XRange,Y,XRange,YRange,AccA,AccB)->
		gather_stats(1,Y+1,XRange,YRange,AccA,AccB);
	gather_stats(X,Y,XRange,YRange,AccA,AccB)->
		case get({X,Y}) of
			undefined ->
				Local_State = [get({TX,TY})|| TX <- [X-1,X,X+1],TY<-[Y-1,Y,Y+1]],
				TotSwimmers = lists:sum([1 || {_APId,swimmer} <- Local_State]),
				TotSurfers = lists:sum([1 || {_APId,surfer} <- Local_State]),
				if 
					(TotSurfers > TotSwimmers) -> gather_stats(X+1,Y,XRange,YRange,AccA+1,AccB);
					(TotSurfers < TotSwimmers) -> gather_stats(X+1,Y,XRange,YRange,AccA,AccB+1);
					true -> gather_stats(X+1,Y,XRange,YRange,AccA,AccB)
				end;
			_ ->
				gather_stats(X+1,Y,XRange,YRange,AccA,AccB)
	end.

%swimmer/3 defines the swimmer agent process, it can send the environment/scape agent the request for its local state. Based on this local state and the Ratio it then decides on whether to stay or move, and sends its decision to the scape, awaiting its new grid coordinate.
swimmer(Scape_PId,Loc,Ratio)->
	Scape_PId ! {self(),Loc,get_LocalState},
	receive
		{Scape_PId,Local_State}->
			TotSwimmers = lists:sum([1 || {_APId,swimmer} <- Local_State--[{self(),swimmer}]]),
			Choice = case TotSwimmers >= (8*Ratio) of
				true -> stay;
				false -> move
			end,
			Scape_PId ! {self(),swimmer,Loc,Choice},
			receive {Scape_PId,updated_loc,U_Loc} -> ok end,
			swimmer(Scape_PId,U_Loc,Ratio);
		terminate -> ok
	end.

%surfer/3 works the same way as the swimmer agent does, but can be set up to work completely different, and be based on a completely different set of rules.	
surfer(Scape_PId,Loc,Ratio)->
	Scape_PId ! {self(),Loc,get_LocalState},
	receive
		{Scape_PId,Local_State}->
			TotSurfers = lists:sum([1 || {_APId,surfer} <- Local_State--[{self(),surfer}]]),
			Choice = case TotSurfers >= (8*Ratio) of
				true -> stay;
				false -> move
			end,
			Scape_PId ! {self(),surfer,Loc,Choice},
			receive {Scape_PId,updated_loc,U_Loc} -> ok end,
			surfer(Scape_PId,U_Loc,Ratio);
		terminate -> ok
	end.

-module(sugarscape).
-compile(export_all).
-include("records.hrl").
-define(R,10).
-define(X,49).
-define(Y,49).
-define(TOT_INIT_AGENTS,250).
-define(SRR,1).
-define(INIT_SCAPE_FROM,file).
-define(REPRODUCTION,on).
-define(MAX_AGE,10).
-record(state,{avatars=[],scape,window,canvas,filter={1,100,100},delay=100,time_step=0}).

%Starts two independent processes, a sugarscape(computational system, and through which the agents can be controlled) and a visor (graphical system, through which graphical aspects can be controlled).
start()->
	register(visor,spawn(sugarscape,visor,[])).

%A small graphical system using Erlang's gs module. It creats a canvas on which graphical elements can be generated and moved.	
visor()->
	random:seed(now()),
	GS = gs:start(),
	Window = gs:create(window,GS,[{title,"Visor"},{width,1920},{height,1080}]),
	Canvas = gs:create(canvas,Window,[{width,1920},{height,1080}]),
	put(canvas,Canvas),
	gs:config(Window,{map,true}),
	{Init_Avatars,Scape} = init_sugarscape(Canvas),
	S = #state{avatars=Init_Avatars,scape=Scape,window=Window,canvas=Canvas},
	loop(S).
	
%Sugar scape is located in a hash table, There is a map of coordinates of the sugarscape to sugar level, capacity, and who is currently present there. There is also a list of ids. And there is a reverse map from agent id to agent's loc. The name of the hash table is scape.
	init_sugarscape(Canvas)->
		Tot_Avatars = ?TOT_INIT_AGENTS,
		AvailableLocs = [{X*?R,Y*?R} || X<- lists:seq(0,?X), Y<-lists:seq(0,?Y)],
		Scape = init_Scape(),
		Init_Avatars = init_Avatars(Tot_Avatars,length(AvailableLocs),AvailableLocs,[]),
		U_Avatars=visor:draw_avatars(Canvas,Init_Avatars,[]),
		{U_Avatars,Scape}.
			
		init_Scape()->
			Scape=ets:new(scape,[set,private,named_table]),
			case ?INIT_SCAPE_FROM of
				file->
					read_map();
				random ->
					CoordsP=[{X*?R,Y*?R,SugarCapacity=random:uniform(5)-1} || X<-lists:seq(0,?X),Y<-lists:seq(0,?Y)],
					[ets:insert(scape,{{X,Y},SugarCapacity,SugarCapacity,undefined}) || {X,Y,SugarCapacity}<-CoordsP]
			end,
			Scape.
			
			read_map()->
				{ok,Data} = file:read_file("sugarmap.map"),
				transcribe_map(binary_to_list(Data),0,49,0,[],[]).
	
	transcribe_map([],_0,_MX,Y,Acc,Acc1)->
		[io:format("~p~n",[A]) || A <- lists:reverse([Acc|Acc1])];
	transcribe_map(List,MX,MX,Y,Acc,Acc1)->
		{SugarCap,Remainder} = split_with(10,List),
		%io:format("{~p,~p},~p X:~p Y:~p~n",[X,Y,list_to_integer(SugarCap),X,Y]),
		ets:insert(scape,{{MX*?R,Y*?R},list_to_integer(SugarCap),list_to_integer(SugarCap),undefined}),
		%io:format("Remainder:~p~n",[Remainder]),
		transcribe_map(Remainder,0,MX,Y+1,[],[lists:reverse(Acc)|Acc1]);
	transcribe_map(List,X,MX,Y,Acc,Acc1)->
		%io:format("List:~p~n",[List]),
		{SugarCap,Remainder} = split_with(32,List),
		ets:insert(scape,{{X*?R,Y*?R},list_to_integer(SugarCap),list_to_integer(SugarCap),undefined}),
		%io:format("{~p,~p},~p X:~p Y:~p~n",[X,Y,list_to_integer(SugarCap),X,Y]),
		transcribe_map(Remainder,X+1,MX,Y,[{{X*?R,Y*?R},list_to_integer(SugarCap)}|Acc],Acc1).
		
		split_with(Seperator,List)->
			split_with(Seperator,List,[]).
		
			split_with(Seperator,[Char|List],ValAcc)->
				io:format("Char:~p Seperator:~p~n",[Char,Seperator]),
				case Char of
					Seperator->
						{lists:reverse(ValAcc),List};
					_ ->
						split_with(Seperator,List,[Char|ValAcc])
				end;
			split_with(_Seperator,[],ValAcc)->
				{lists:reverse(ValAcc),[]}.	
			
		init_Avatars(0,_TotLocs,_AvailableLocs,Acc)->
			Acc;
		init_Avatars(Avatar_Index,TotLocs,AvailableLocs,Acc)->
			Loc = lists:nth(random:uniform(TotLocs),AvailableLocs),
			Agent_Id = technome_constructor:generate_UniqueId(),
			Agent = create_agent(automaton,automaton,Agent_Id,{cf,ct,0},Loc),
			[{Loc,SugarLevel,SugarCapacity,undefined}] = ets:lookup(scape,Loc),
			ets:insert(scape,{loc,SugarLevel,SugarCapacity,Agent_Id}),
			init_Avatars(Avatar_Index-1,TotLocs-1,AvailableLocs--[Loc],[Agent|Acc]).
			
	create_agent(Morphology,Specie_Id,Id,{CF,CT,TotNeurons},Loc)->
		Angle = random:uniform()*2*math:pi(),
		Direction = {DX,DY}={0,1},
		{X,Y} = Loc,
		R = ?R/3,
		Color = blue,
		Objects = [{circle,undefined,Color,{X,Y},[{X,Y}],R}],
		#avatar{
			id = Id,
			morphology = Morphology,
			loc = {X,Y},
			direction = Direction,
			r = R,
			energy = random:uniform(20)+5,
			age = 0,
			state={random:uniform(6),random:uniform(4)},%{vision,metabolism}
			objects = Objects
		}.
	
loop(S)->
	receive
		{spawn_avatars,Morphology,Tot_Avatars}->
			U_Avatars = [scape:create_avatar(Morphology,Morphology,technome_constructor:generate_UniqueId(),{cf,ct,0},void) || _<- lists:seq(1,Tot_Avatars)],
			U_Avatars2 = visor:draw_avatars(S#state.canvas,U_Avatars,[]),
			loop(S#state{avatars=U_Avatars2});
		{From,get_all} ->
			From ! {self(),S#state.avatars},
			loop(S);
		{From,set_all,U_Avatars}->
			visor:redraw_avatars(S#state.filter,U_Avatars),
			loop(S#state{avatars=U_Avatars});
		{update_filter,{Zoom,PanX,PanY}}->
			loop(S#state{filter={Zoom,PanX,PanY}});
		terminate ->
			ok
	after S#state.delay ->
		U_Avatars = execute_AgentRules(S,length(S#state.avatars),S#state.avatars,[]),%io:format("1~n"),
		execute_ScapeRules(ets:first(scape)),%io:format("2~n"),
		io:format("Time step:~p Tot agents alive:~p~n",[S#state.time_step,length(U_Avatars)-50]),
		sugarscape:loop(S#state{avatars=U_Avatars,time_step=S#state.time_step+1})
	end.
	
	execute_AgentRules(_S,0,[],Acc)->
		Acc;
	execute_AgentRules(S,TotAvatars,Avatars,Acc)->
		Avatar=lists:nth(random:uniform(TotAvatars),Avatars),
		case (Avatar#avatar.energy < 0) or (Avatar#avatar.age > ?MAX_AGE) of
			true ->%Death/Rebirth
				Agent_Id =Avatar#avatar.id,
				[{Loc,SugarLevel,SugarCapacity,Agent_Id}] = ets:lookup(scape,Avatar#avatar.loc),
				ets:insert(scape,{Loc,SugarLevel,SugarCapacity,undefined}),
				case ?REPRODUCTION of
					off ->
						[gs:destroy(Id) || {_ObjType,Id,_Color,_Pivot,_Coords,_Parameter} <- Avatar#avatar.objects],
						execute_AgentRules(S,TotAvatars-1,Avatars--[Avatar],Acc);
					on ->
						AvailableLocs=find_EmptyLocs(ets:first(scape),[]),
						NewLoc = lists:nth(random:uniform(length(AvailableLocs)),AvailableLocs),
						NewAgent_Id = technome_constructor:generate_UniqueId(),
						NewAgent = create_agent(automaton,automaton,NewAgent_Id,{cf,ct,0},NewLoc),
						[{NewLoc,NewSugarLevel,NewSugarCapacity,undefined}] = ets:lookup(scape,NewLoc),
						ets:insert(scape,{NewLoc,NewSugarLevel,NewSugarCapacity,NewAgent_Id}),
						[U_Avatar]=visor:draw_avatars(S#state.canvas,[NewAgent],[]),
						execute_AgentRules(S,TotAvatars-1,Avatars--[Avatar],[U_Avatar|Acc])
				end;
			false ->%Staying alive
				U_Avatar=move(Avatar),
				%io:format("U_Avatar:~p~n",[U_Avatar]),
				visor:redraw_avatars(S#state.filter,[U_Avatar]),
				execute_AgentRules(S,TotAvatars-1,Avatars--[Avatar],[U_Avatar|Acc])
		end.
		
		find_EmptyLocs('$end_of_table',Acc)->
			Acc;
		find_EmptyLocs(Id,Acc)->
			case ets:lookup_element(scape,Id,4) of
				undefined ->
					find_EmptyLocs(ets:next(scape,Id),[Id|Acc]);
				_ ->
					find_EmptyLocs(ets:next(scape,Id),Acc)
			end.
		
		move(A)->
			{X,Y} = Loc = A#avatar.loc,
			{Vision,Metabolism} = A#avatar.state,
			MovesList = [{mod(X+DX*?R,?X*?R),Y} || DX<-lists:seq(-Vision,Vision)] ++ [{X,mod(Y+DY*?R,?Y*?R)} || DY <-lists:seq(-Vision,Vision)],
			B=[{Loc,SugarLevel,SugarCapacity,Agent_Id}] = ets:lookup(scape,A#avatar.loc),
			{BestLoc,BestSugarLevel} = get_BestMove(MovesList,{{X,Y},SugarLevel},{X,Y}),
			%io:format("Loc:~p~n",[Loc]),
			%io:format("B:~p~n",[B]),
			ets:insert(scape,{Loc,SugarLevel,SugarCapacity,undefined}),
			%io:format("BestLoc:~p BestSuga~p~n",[BestLoc,BestSugarLevel]),
			[{BestLoc,BestSugarLevel,BestSugarCapacity,_}] = ets:lookup(scape,BestLoc),
			ets:insert(scape,{BestLoc,0,BestSugarCapacity,A#avatar.id}),%Agent eats the sugar
			{BX,BY} = BestLoc,
			{DX,DY} = {BX-X,BY-Y},
			translate(A#avatar{energy=A#avatar.energy-Metabolism+BestSugarLevel,age=A#avatar.age+1},{DX,DY}).
			%A#avatar{loc=BestLoc,energy=A#avatar.energy-Metabolism+BestSugarLevel}.%Agent loses energy due to metabolism but gains energy due to sugar eating.
			
			%Translate and move functions allow for the agents to move around the 2d world.	
		translate(Avatar,{DX,DY})->
			{LX,LY} = Avatar#avatar.loc,
			U_Loc = {LX+DX,LY+DY},
			U_Objects=[{ObjName,Id,Color,{PX+DX,PY+DY},[{X+DX,Y+DY}||{X,Y}<-Coords],P}||{ObjName,Id,Color,{PX,PY},Coords,P}<-Avatar#avatar.objects],
			Avatar#avatar{loc = U_Loc,direction={DX,DY},objects=U_Objects}.
			
			get_BestMove([Loc|MoveList],{BestLoc,BestSL},AgentLoc)->
				%io:format("Loc:~p~n",[Loc]),
				[{Loc,SugarLevel,_SugarCapacity,Id}] = ets:lookup(scape,Loc),
				case (SugarLevel > BestSL) and (Id == undefined) of %If sugar level is highest at Loc, select Loc.
					true ->
						get_BestMove(MoveList,{Loc,SugarLevel},AgentLoc);
					false ->
						{AX,AY} = AgentLoc,
						{X,Y} = Loc,
						{BX,BY} = BestLoc,
				%		io:format("AgentLoc:~p Loc:~p BestLoc:~p~n",[AgentLoc,Loc,BestLoc]),
						Distance=math:sqrt(math:pow(AX*X,2)+math:pow(AY*Y,2)),
						BestDistance=math:sqrt(math:pow(AX*BX,2)+math:pow(AY*BY,2)),
						case (SugarLevel == BestSL) and (Distance < BestDistance) and (Id == undefined) of %If sugar level is the same, select closest.
							true ->
								get_BestMove(MoveList,{Loc,SugarLevel},AgentLoc);
							false ->
								get_BestMove(MoveList,{BestLoc,BestSL},AgentLoc)
						end
				end;
			get_BestMove([],{BestLoc,BestSugarLevel},_AgentLoc)->
				{BestLoc,BestSugarLevel}.
		
				mod(X,Y) when X > 0 -> X rem Y;
				mod(X,Y) when X < 0 -> Y + X rem Y;
				mod(0,Y) -> 0.

	
	execute_ScapeRules('$end_of_table')->
		done;
	execute_ScapeRules(Key)->
		%io:format("Key:~p~n",[Key]),
		[{Loc,SugarLevel,SugarCapacity,Agent_Id}] = ets:lookup(scape,Key),
		ets:insert(scape,{Loc,SugarCapacity,SugarCapacity,Agent_Id}),
		execute_ScapeRules(ets:next(scape,Key)).

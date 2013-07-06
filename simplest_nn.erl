-module(simplest_nn).
-compile(export_all).

%create/0 first generates 3 weights, with the 3rd weight being the Bias. The Neuron is spawned first, and is then sent the PIds of the Sensor and Actuator that it's connected with. Then the Cortex element is registered and provided with the PIds of all the elements in the NN system.
create() ->
	Weights = [random:uniform()-0.5,random:uniform()-0.5,random:uniform()-0.5],
	N1_PId = spawn(?MODULE,neuron,[Weights,undefined,undefined]),
	N2_PId = spawn(?MODULE,neuron,[Weights,undefined,undefined]),
	N_PIds = [N1_PId,N2_PId],
	S_PId = spawn(?MODULE,sensor,[N_PIds]),
	A_PId = spawn(?MODULE,actuator,[N_PIds,N_PIds]),
	N1_PId ! {init,S_PId,A_PId},
	N2_PId ! {init,S_PId,A_PId},
	register(cortex,spawn(?MODULE,cortex,[S_PId,[N1_PId,N2_PId],A_PId])).

%neuron/3 expects to first receive the {init,New_SPId,New_APId} message, which sets its sensor and actuator pids. The neuron then expects to receive a vector of length 2 as input, and as soon as the input arrives, the neuron processes the signal and passes the output vector to the outgoing APId.
neuron(Weights,S_PId,A_PId) ->
	receive 
		{S_PId,forward, Input} ->
			io:format("****Thinking****~n Input:~p~n with Weights:~p~n",[Input,Weights]),
			Dot_Product = dot(Input,Weights,0),
			Output = [math:tanh(Dot_Product)],
			A_PId ! {self(),forward,Output},
			neuron(Weights,S_PId,A_PId);
		{init,New_SPId,New_APId} ->
			neuron(Weights,New_SPId,New_APId);
		terminate ->
			ok
	end.

%dot/3 takes a dot product of two vectors, it can operate on a weight vector with and without a bias. When there is no bias in the weight list, both the Input vector and the Weight vector are of the same length. When Bias is present, then when the Input list empties out, the Weights list still has 1 value remaining, its Bias.
	dot([I|Input],[W|Weights],Acc) ->
		dot(Input,Weights,I*W+Acc);
	dot([],[],Acc)->
		Acc;
	dot([],[Bias],Acc)->
		Acc + Bias.

%sensor/1 waits to be triggered by the Cortex element, and then produces a random vector of length 2, which it passes to the connected neuron. In a proper system the sensory signal would not be a random vector but instead would be produced by a function associated with the sensor, a function that for example reads and vector-encodes a signal coming from a GPS attached to a robot.
sensor(N_PIds) ->
	receive
		sync ->
			Sensory_Signal = [random:uniform(),random:uniform()],
			io:format("****Sensing****:~n Signal from the environment ~p~n",[Sensory_Signal]),
			[N_PId ! {self(),forward,Sensory_Signal} || N_PId <- N_PIds],
			sensor(N_PIds);
		terminate ->
			ok
	end.

%actuator/3 function waits for a control signals coming from Neurons. As soon as the signal arrives, the actuator executes its function, pts/1, which prints the value to the screen. 
actuator([N_PId|N_PIds],MNPIds) ->
	receive
		{N_PId,forward,Control_Signal}->
			pts(Control_Signal),
			actuator(N_PIds,MNPIds);
		terminate ->
			ok
	end;
actuator([],MNPIds)->
	actuator(MNPIds,MNPIds).

	pts(Control_Signal)->
		io:format("****Acting****:~n Using:~p to act on environment.~n",[Control_Signal]).

%cortex/3 function triggers the sensor to action when commanded by the user. This process also has all the PIds of the elements in the NN system, so that it can terminate the whole system when requested. Because the cortex element was registered when the function create/0 was executed, you can send it the message to ask the sensor to produce data by executing in the shell: cortex ! sense_think_act. This can be done multiple times, after which to terminate the whole system you can execute: cortex ! terminate.
cortex(Sensor_PId,Neuron_PIds,Actuator_PId)->
	receive
		sense_think_act ->
			Sensor_PId ! sync,
			cortex(Sensor_PId,Neuron_PIds,Actuator_PId);
		terminate ->
			Sensor_PId ! terminate,
			[N_PId ! terminate || N_PId <- Neuron_PIds],
			Actuator_PId ! terminate,
			ok
	end.

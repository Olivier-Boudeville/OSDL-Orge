% Add your standard header here.
-module(class_Camera).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [class_TraceEmitter] ).


% Parameters taken by the constructor ('construct'). 
-define( wooper_construct_parameters, Name, Position, ZoomFactor, 
		VirtualWorldPid, MapSupervisorPid ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/5, new_link/5, 
		synchronous_new/5, synchronous_new_link/5,
		synchronous_timed_new/5, synchronous_timed_new_link/5,
		remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		remote_synchronous_new_link/6, remote_synchronous_timed_new/6,
		remote_synchronous_timed_new_link/6, construct/6, delete/1 ).


% Member method declarations.
-define( wooper_method_export, setName/2, 
		getPosition/1, setAbscissa/2, setOrdinate/2,
		getZoomFactor/1, setZoomFactor/2,
		getFullStatus/1 ).


% Static method declarations.
-define( wooper_static_method_export, ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Orge.Map.Camera").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new map camera.
%
% Construction parameters are:
%
% - Name is the name of this camera, specified as a plain string
%
% - Position = {Xc,Yc} is the camera initial position in the virtual world
%
% - ZoomFactor is the initial zoom of this camera, in cm/px (i.e. expressed in
% virtual world centimeters per pixel
%
construct( State, Name, Position={Xc,Yc}, ZoomFactor, VirtualWorldPid,
		  MapSupervisorPid ) when 
	  is_integer(Xc) andalso is_integer(Yc) andalso is_float(ZoomFactor) ->

	% The attribute of an instance are:
	%
	% - name is the camera name
	%
	% - position={Xc,Yc} is the current camera position (pair of integers)
	%
	% - zoom_factor is the current zoom of this camera, in cm/px (float)
	% 
	% - world_pid is the PID of the virtual world this camera represents
	%
	% - supervisor_pid is the PID of the map supervisor this camera is linked to
	%
	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State, Name ),
	
	VirtualWorldPid ! {getBoundaries,[],self()},
	{TopLeft,BottomRight} = receive
					
					{wooper_result,R} ->
						R

				end,
												 
	% Then the class-specific attributes:
	SetState = setAttributes( TraceState, [
		 {position,Position},
		 {zoom_factor,ZoomFactor},
		 {top_left_point,TopLeft},
		 {bottom_right_point,BottomRight},						
		 {world_pid,VirtualWorldPid},
		 {supervisor_pid,MapSupervisorPid},					
		 {trace_categorization,
		  text_utils:string_to_binary(?TraceEmitterCategorization)} 
							   ] ),
	
	add_info_message( "Camera '~s' starting at location {~B,~B} "
					 "with zoom factor x~.4f, which means that "
					 "1 pixel corresponds to ~s of the virtual world.", 
					 [ Name,Xc,Yc,ZoomFactor,
					  % Expressed as millimeters:
					  text_utils:distance_to_string(10*ZoomFactor)], 
					 SetState ),
	SetState.
	
	
	
delete(State) ->
	State.
	



% Member method section.


% Sets the name of this camera to the one specified as a plain string.
%
% (oneway)
setName( State, NewName ) ->
   	?wooper_return_state_only( setAttribute( State, name, 
							  text_utils:string_to_binary(NewName) ) ).




% Returns the current position of this camera.
%
% (const request)
getPosition( State ) ->
	?wooper_return_state_result( State, ?getAttr(position) ).



% Sets the abscissa of this camera to specified integer value.
%
% (oneway)
setAbscissa( State, NewX ) ->
	{_PreviousX,Y} = ?getAttr(position),
	?wooper_return_state_only( setAttribute( State, position, {NewX,Y} ) ).



% Sets the ordinate of this camera to specified integer value.
%
% (oneway)
setOrdinate( State, NewY ) ->
	{X,_PreviousY} = ?getAttr(position),
	?wooper_return_state_only( setAttribute( State, position, {X,NewY} ) ).




% Returns the current zoom factor of this camera.
%
% (const request)
getZoomFactor( State ) ->
	?wooper_return_state_result( State, ?getAttr(zoom_factor) ).



% Sets the zoom factor of this camera to the specified floating-point value.
%
% (oneway)
setZoomFactor( State, NewZoomFactor ) ->
	?wooper_return_state_only( 
			setAttribute( State, zoom_factor, NewZoomFactor ) ).





% Returns the current status (name, position, zoom factor) of this camera.
%
% (const request)
getFullStatus( State ) ->
	?wooper_return_state_result( State, {?getAttr(name), 
	   	?getAttr(position), ?getAttr(zoom_factor)} ).
	


% Static method section.


% Helper functions section.


% Requests the map supervisor to display specified message.
add_info_message( Message, State ) ->	
	?getAttr(supervisor_pid) ! {add_info_message,Message}.


% Requests the map supervisor to display specified message.
add_info_message( MessageFormat, MessageValue, State ) ->	
	add_info_message( io_lib:format(  MessageFormat, MessageValue ), State ).



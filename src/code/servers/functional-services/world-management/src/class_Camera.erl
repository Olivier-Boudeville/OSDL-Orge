% Add your standard header here.
-module(class_Camera).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [class_TraceEmitter] ).


% Parameters taken by the constructor ('construct'). 
-define( wooper_construct_parameters, Name, Position, ScreenPosition, 
		 ZoomFactor, VirtualWorldPid, MapSupervisorPid ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/6, new_link/6, 
		synchronous_new/6, synchronous_new_link/6,
		synchronous_timed_new/6, synchronous_timed_new_link/6,
		remote_new/7, remote_new_link/7, remote_synchronous_new/7,
		remote_synchronous_new_link/7, remote_synchronous_timed_new/7,
		remote_synchronous_timed_new_link/7, construct/7, delete/1 ).


% Member method declarations.
-define( wooper_method_export, setName/2, 
		 getPosition/1, setAbscissa/2, getAbscissa/1,
		 setOrdinate/2, getOrdinate/1, updateOrdinateWith/2,
		 setOnscreenPosition/2, getZoomFactor/1, setZoomFactor/2,
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
% - Position = {Xc,Yc} is the camera initial position in the virtual world, in
% centimeters
%
% - ScreenPosition = {Xsc,Ysc} is the camera initial onscreen position, in
% pixels
%
% - ZoomFactor is the initial zoom of this camera, in px/cm (i.e. expressed in
% pixel per virtual world centimeter)
%
construct( State, Name, Position={Xc,Yc}, ScreenPosition, ZoomFactor, 
		   VirtualWorldPid, MapSupervisorPid ) when 
	  is_integer(Xc) andalso is_integer(Yc) andalso is_float(ZoomFactor) ->

	% The attribute of an instance are:
	%
	% - name is the camera name
	%
	% - position={Xc,Yc} is the current in-world camera position (pair of
	% integers, expressed in centimeters)
	%
	% - screen_position={Xsc,Ysc} is the current onscreen camera position (pair
	% of pixel coordinates)
	%
	% - zoom_factor is the current zoom of this camera, in px/cm (float)
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
		 {screen_position,ScreenPosition},
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
					  % Takes millimeters:
					  text_utils:distance_to_string(100/ZoomFactor)], 
					 SetState ),
	SetState.
	
	

% Destructor.	
delete(State) ->
	State.
	



% Member method section.


% Sets the name of this camera to the one specified as a plain string.
%
% (oneway)
setName( State, NewName ) ->
   	?wooper_return_state_only( setAttribute( State, name, 
							  text_utils:string_to_binary(NewName) ) ).



% In-world position section.


% Returns the current in-world position of this camera.
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


% Returns the current abscissa of this camera.
%
% (const request)
getAbscissa( State ) ->
	{X,_Y} = ?getAttr(position),
	?wooper_return_state_result( State, X ).



% Sets the ordinate of this camera to specified integer value.
%
% (oneway)
setOrdinate( State, NewY ) ->
	{X,_PreviousY} = ?getAttr(position),
	?wooper_return_state_only( setAttribute( State, position, {X,NewY} ) ).


% Returns the current ordinate of this camera.
%
% (const request)
getOrdinate( State ) ->
	{_X,Y} = ?getAttr(position),
	?wooper_return_state_result( State, Y ).



% Update the current ordinate of this camera by moving it upward by the
% specified number of pixels: this value is multiplied by the current zoom
% factor.
%
% Returns the previous and new ordinates.
%
% (request)
updateOrdinateWith( State, PixelValue ) ->
	{X,Y} = ?getAttr(position),
	NewY = Y + round( PixelValue * ?getAttr(zoom_factor) ),
	?wooper_return_state_result( 
	   setAttribute(State,position,{X,NewY}), 
	   {Y,NewY} ).




% Onscreen position section.


% Updates the screen position of this camera.
%
% (oneway)
setOnscreenPosition( State, NewPosition ) ->
	?wooper_return_state_only( 
	   setAttribute( State, screen_position, NewPosition ) ).




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



% Conversion section.


% Converts the specified screen abscissa offset (in pixels) between the target
% point P={X,Y} and the camera C={Xc,Yc}, i.e. Xsoffset=X-Xc, into an absolute
% in-world abscissa (in centimeters).
%
% Returns the corresponding number of centimeters.
%
screen_offset_to_absolute_world_abscissa( Xsoffset, State ) ->
	{Xworld,_Yworld} = ?getAttr(position),
	Xworld + Xsoffset / ?getAttr(zoom_factor).


% Converts the specified screen ordinate offset (in pixels) between the target
% point P={X,Y} and the camera C={Xc,Yc}, i.e. Ysoffset=Y-Yc, into an absolute
% in-world ordinate (in centimeters).
%
% Returns the corresponding number of centimeters.
%
screen_offset_to_absolute_world_ordinate( Ysoffset, State ) ->
	{_Xworld,Yworld} = ?getAttr(position),
	Yworld - Ysoffset / ?getAttr(zoom_factor).


% Converts the specified screen coordinate offsets (in pixels) between the
% target point P={X,Y} and the camera C={Xc,Yc},
% i.e. {Xsoffset,Ysoffset}={X-Xc,Y-Yc}, into absolute in-world coordinate (in
% centimeters).
%
% Returns the corresponding coordinate pair.
%
screen_offsets_to_absolute_world_coordinates( {Xsoffset,Ysoffset}, State ) ->
	{Xworld,Yworld} = ?getAttr(position),
	Z = ?getAttr(zoom_factor),
	{ Xworld + Xsoffset/Z, Yworld - Ysoffset/Z }.



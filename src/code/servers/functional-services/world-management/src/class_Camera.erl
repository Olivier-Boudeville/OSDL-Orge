% Add your standard header here.
-module(class_Camera).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [class_TraceEmitter] ).


% Parameters taken by the constructor ('construct'). 
-define( wooper_construct_parameters, Name, Position, ZoomFactor ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3, 
		synchronous_new/3, synchronous_new_link/3,
		synchronous_timed_new/3, synchronous_timed_new_link/3,
		remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		remote_synchronous_new_link/4, remote_synchronous_timed_new/4,
		remote_synchronous_timed_new_link/4, construct/4, delete/1 ).


% Member method declarations.
-define( wooper_method_export, ).


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
construct( State, ?wooper_construct_parameters ) ->

	% The attribute of an instance are:
	%
	% - name is the camera name
	% - position={Xc,Yc} is the current camera position
	% - zoom_factor is the current zoom of this camera, in cm/px
	%

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State, Name ),
	
	% Then the class-specific attributes:
	setAttributes( TraceState, [
		 {position,Position},
		 {zoom_factor,ZoomFactor},
		 {trace_categorization,
		  text_utils:string_to_binary(?TraceEmitterCategorization)} 
							   ] ).
	
	
	
delete(State) ->
	State.
	


% Member method section.


% Static method section.


% Helper functions section.

	

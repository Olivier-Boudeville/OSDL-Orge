% Copyright (C) 2003-2010 Olivier Boudeville
%
% This file is part of the Orge library.
%
% The Orge library is free software: you can redistribute it and/or modify
% it under the terms of either the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
%
% The Orge library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License and of the GNU General Public License along with the Orge library.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Created on Sunday, April 4, 2010.
-module(class_MapSupervisor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [class_TraceEmitter] ).


% Parameters taken by the constructor ('construct'). 
-define( wooper_construct_parameters, Name, VirtualWorldPid ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2, 
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronous_timed_new/3,
		 remote_synchronous_timed_new_link/3, construct/3, delete/1 ).


% Member method declarations.
-define( wooper_method_export, enterGUIMainLoop/1 ).


% Static method declarations.
-define( wooper_static_method_export, create_supervisor_for/1 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Orge.Map.Supervisor").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").




% Implementation notes.


% There are virtual-world coordinates and screen coordinates.
%
% Virtual-world coordinates are expressed in an orthonormal basis, abscissa
% increasing onscreen from left to right, ordinate from bottom to top.
%
% Their unit is the virtual-world centimeter. This is not the meter, in order to
% stays as much as possible with integer coordinates. Supposing an integer is
% actually a signed 32 bit integer, coordinates will range in −2,147,483,648 to
% +2,147,483,647, which means a maximum length of 4,294,967,295 cm, thus 42 949
% 673 meters, about 43 kilometers. Therefore, in 32 bit, as long as distances
% will stay within that limit, computations will be effective.
% In 64 bit, distance is 18,446,744,073,709,551,615 cm, thus about
% 184 467 440 737 095 km, which is quite a lot.
%
% Screen coordinates are expressed in an orthonormal basis too, abscissa
% increasing onscreen from left to right, ordinate from top to bottom.
%
% Their unit is the pixel.


% The active map is a rectangular window on the screen which is opened on the
% virtual world.
% 
% Its onscreen size is determined by the layout and size of its containing
% window.
%
% This map is centered on MapOrigin={Xm,Ym}, expressed in virtual-world
% coordinates, thus in centimeters. 
%
% A zoom factor F is defined. It allows to convert screen distances into
% virtual-world distances. It is defined in virtual word centimeters per pixel.
% For example if F = 100 cm/px then a line segment of 10 pixels corresponds to 1
% meter in the virtual world.
% 
% The onscreen size of the map and the zoom factor determine how much of the
% virtual world is shown.


% A difficulty is that we need here to hijack the WOOPER main loop, since GS
% will send messages like '{gs,{3,<0.45.0>},destroy,[],[]}' to the map
% supervisor. As they cannot be mapped to WOOPER messages, they are thus by
% default ignored.

% GS identifiers are actually {Id,GsPid}, even if only Id could be stored for
% each widget..


% Constructs a new map supervisor.
%
% Construction parameters are:
%
% - Name: the name of this instance, specified as a plain string
%
% - VirtualWorldPid is the PID of the virtual world to supervise.
%
construct( State, ?wooper_construct_parameters ) ->

	% The attribute of an instance are:
	%
	% - main_camera is the PID of the main camera
	%
	% - virtual_world_pid is the PID of the virtual world to supervise
	%
	% - gs_pid is the PID of the GS process, most GS messages are based on 
	% a pair made of the container ID and this PID
	%
	% - gs_id is the ID of the GS subsystem
	%
	% - main_win_id is the ID of the main window
	%
	% - canvas_id is the ID of the canvas

	
	% First the direct mother classes:
	
	VirtualWorldPid ! {getName,[],self()},
	WorldName = receive
					
					{wooper_result,N} ->
						text_utils:binary_to_string(N)

				end,
		
	TraceState = class_TraceEmitter:construct( State, Name 
											   ++ " Map Supervisor for world '"
											   ++ WorldName ++ "'" ),
	

	% Starts from the origin of the virtual world, with 1 meter corresponding to
	% 1 pixel:
	MainCameraPid = class_Camera:new_link( "Main camera", _Position={0,0},
										_ZoomFactor=100 ),

	% Then the class-specific attributes:
	InitState = setAttributes( TraceState, [
		 {main_camera_pid,MainCameraPid},
		 {virtual_world_pid,VirtualWorldPid},									
		 {gs_pid,undefined}, 
		 {gs_id,undefined}, 
		 {main_win_id,undefined}, 
		 {canvas_id,undefined}, 
		 {trace_categorization,
		  text_utils:string_to_binary(?TraceEmitterCategorization)} 
							   ] ),

	% Hijack the WOOPER main loop:
	self() ! enterGUIMainLoop,

	% Returns an updated state:
	init_gui( InitState ).
	
	
	
delete(State) ->
	State.

	


% Member method section.


% Enters the GUI main loop.
%
% (oneway)
enterGUIMainLoop(State) ->
	
	%io:format( "State:~n~p~n", [State] ),

	receive

		{gs,_Id,destroy,_Data,[]} ->
			%io:format( "Destroyed!" ),
			self() ! delete,
			?wooper_return_state_only(State);

		{gs,_Id,configure,_Data,[W,H|_]} ->
			% Repack:
			%io:format( "Configured!" ),
			gs:config( packer_win, [{width,W},{height,H}] ), 
			enterGUIMainLoop(State);
		
		Other ->
			io:format( "Message ignored by the map supervisor: '~p'.~n",
					   [Other] ),
			enterGUIMainLoop(State)

	end.
			


% Static method section.


% Creates the default map supervisor for specified virtual world.
create_supervisor_for( VirtualWorldPid ) ->
	new_link( _Name="Orge Default", VirtualWorldPid ).



get_default_main_window_width() ->
	800.

get_default_main_window_height() ->
	600.


get_canvas_width() ->
	640.

get_canvas_height() ->
	480.


% Helper functions section.


% Inits the GUI.
% Returns an updated state.
init_gui( State ) ->

	GsId = gs:start(),

	WindowSize = [ {width,get_default_main_window_width()}, 
				   {height,get_default_main_window_height()} ],
	
	Title = class_TraceEmitter:get_plain_name(State),

	MainWinId = gs:window( GsId, WindowSize ++ [  
								{title,Title},
								{bg,gui:get_color(grey)},
								{configure,true}
												  ]),

	% Setting up camera panel dimensions:
	CameraCellLength = 50,

	CameraPanelWidth  = 3*CameraCellLength,
	CameraPanelHeight = 4*CameraCellLength,

	% The main window is split into four areas:
	%   - first row: empty then canvas
	%   - second row: camera panel then empty
	gs:frame( packer_win, MainWinId, [ 
      {bw,1},
	  {packer_x,[ {fixed,CameraPanelWidth}, {stretch,10} ]},
	  {packer_y,[ {stretch,10},{fixed,CameraPanelHeight} ]}
									 ]),

	% In the main window, canvas is at the top right:
	CanvasId = gs:create( canvas, packer_win, [ 
		{pack_xy,{2,1}},
		{bg,white}, 
		%{hscroll,bottom}, {vscroll,left},
	    {width,get_canvas_width()},
		{height,get_canvas_height()}
		%{scrollregion, {100,200,30,200}}
												] ),

	% In the main frame, the camera frame is at the bottom left:

	CellWidth = CellHeight = {fixed,CameraCellLength},

	gs:frame( packer_cam, packer_win, [ 
      {bw,1}, 
	  {pack_xy,{1,2}},
	  {packer_x,[CellWidth,CellWidth,CellWidth]},
	  {packer_y,[CellHeight,CellHeight,CellHeight,CellHeight]},
	  {width,CameraPanelWidth},
	  {height,CameraPanelHeight} ] ),

	gs:create( label, cam_label, packer_cam, [ {label,{text,"No camera set"}},
      {justify,center}, {bw,1}, {pack_xy,{{1,3},1}} ]),

	gs:create( button, cam_left_button, packer_cam, 
      [ {label,{text,"Left"}}, {pack_xy,{1,3}} ]),

	gs:create( button, cam_right_button, packer_cam, 
	  [ {label,{text,"Right"}}, {pack_xy,{3,3}} ]),

	gs:create( button, cam_center_button, packer_cam, 
      [ {label,{text,"Center"}}, {pack_xy,{2,3}} ]),

	gs:create( button, cam_up_button, packer_cam, 
      [ {label,{text,"Up"}}, {pack_xy,{2,2}} ]),

	gs:create( button, cam_down_button, packer_cam, 
      [ {label,{text,"Down"}}, {pack_xy,{2,4}} ]),
	
    % Sets the GUI to visible and refreshes to initial size:

	gs:config( packer_cam, [ {width,CameraPanelWidth}, 
						 {height,CameraPanelHeight} ] ),

	gs:config( packer_win, WindowSize ),

	gs:config( MainWinId, {map,true} ),

	setAttributes( State, [
						   {gs_id,GsId},
						   {main_win_id,MainWinId},
						   {canvas_id,CanvasId}
						   ] ).

	
	

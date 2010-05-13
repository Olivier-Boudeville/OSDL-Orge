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
-define( wooper_static_method_export, create_supervisor_for/1,
		add_info_message/1, add_info_message/2 ).


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
% will stay within that limit, computations will still be quite effective.
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
% A zoom factor F is defined. It allows to convert screen distances (pixel
% offsets, compared to the center of the screen which is the location of the
% current camera) into virtual-world distances.
%
% It is displayed to the user in pixels per virtual world meter, whereas it is
% managed internally in pixels per virtual world centimeter.
%
% For example, if F is displayed as 100px/m, then each pixel will correspond to
% one centimeter of the virtual world, and it will be stored as 1px/cm.
% 
% The onscreen size of the map and the zoom factor determine how much of the
% virtual world is shown.


% A difficulty is that we need here to hijack the WOOPER main loop, since GS
% will send messages like '{gs,{3,<0.45.0>},destroy,[],[]}' to the map
% supervisor. As they cannot be mapped to WOOPER messages, they are thus by
% default ignored.

% GS identifiers are actually {Id,GsPid}, even if only Id could be stored for
% each widget.


% The coefficient, in [0,1], by which the current zoom factor is multiplied or
% divided when a zooom-in or zoom-out is requested.

% Ex: a coefficient of 0.5 means that a zoom-in from x400 will lead to x600.
-define( zoom_coefficient, 0.5 ).


% Number of pixels corresponding to a move due to a click on a single arrow.
% Note: thus depends on the current zoom factor.
-define( move_offset_single, 5 ).


% Number of pixels corresponding to a move due to a click on a double
% arrow.
% Note: thus depends on the current zoom factor.
-define( move_offset_double, 50 ).



% Constructs a new map supervisor.
%
% Construction parameters are:
%
% - Name: the name of this instance, specified as a plain string
%
% - VirtualWorldPid is the PID of the virtual world to supervise
%
% The supervisor starts with a default main camera.
%
construct( State, ?wooper_construct_parameters ) ->

	% The attribute of an instance are:
	%
	% - current_camera_pid is the PID of the currently selected camera (there
	% must always be one)
	%
	% - camera_list is the list of the PID of all known cameras
	%
	% - virtual_world_pid is the PID of the virtual world to supervise
	%
	% - world_boundaries is a pair of two points that delimits the virtual
	% world: {_TopLeft={X1,Y1},_BottomRight={X2,Y2}}
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
	               ++ " Map Supervisor for world '" ++ WorldName ++ "'" ),

	VirtualWorldPid ! {getBoundaries,[],self()},
	{P1={X1,Y1},P2={X2,Y2}} = receive
							
							{wooper_result,R} ->
								R
									
						end,
	
	Position = linear_2D:get_integer_center( P1, P2 ), 
		
	% Starts from the center of the virtual world, with 1 pixel corresponding to
	% 1 meter:
	MainCameraPid = class_Camera:new_link( "Default camera", Position,
		 % get_onscreen_camera_position() not possible, GUI not created yet:
         {0,0}, _ZoomFactor=100.0, VirtualWorldPid, self() ),


	% Then the class-specific attributes:
	InitState = setAttributes( TraceState, [
		 {current_camera_pid,MainCameraPid},
		 {camera_list,[ MainCameraPid ]},								
		 {virtual_world_pid,VirtualWorldPid},
		 %{world_boundaries,WorldBoundaries},
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
	GuiState = init_gui( InitState ),
	
	add_info_message( "Entering the '~s' virtual world...~n", [WorldName] ),
	
	add_info_message( "World boundaries: top-left corner at {~B,~B}, "
					 "bottom-right one at {~B,~B}.", [X1,Y1,X2,Y2] ),

	add_info_message( "Setting the default camera to the center of the world."),

	GuiState.
	
	
% Overridden destructor.
delete(State) ->
	[ CamPid ! delete || CamPid <- ?getAttr(camera_list) ],
	gs:stop(),
	State.

	


% Member method section.


% Enters the GUI main loop, hijacks the WOOPER one.
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
			% Repacks what needs to (ex: packer_win and other packers)
			%io:format( "Configuring ~w!", [Id] ),
			gs:config( packer_win, [{width,W},{height,H}] ),
			% Updates as well the camera center, needed for screen/world
			% conversions:
			?getAttr(current_camera_pid) ! {setOnscreenPosition,{W,H}},
			enterGUIMainLoop( State );

		{gs,cam_x_location,keypress,[],['Return'|_]} ->
			_CamPid = ?getAttr(current_camera_pid),
			XText = gs:read( cam_x_location, text ),
			
			case string:to_integer(XText) of
				
				{_X,[]} ->
					% Correct input, update:
					ok

			end,
			enterGUIMainLoop(State);
			

		{gs,cam_create_button,click,[],_AddString} ->
			enterGUIMainLoop( add_camera(State) );
		
		% Camera movement section:

		{gs,cam_up_single_button,click,[],_} ->
			enterGUIMainLoop( move_up( State, ?move_offset_single ) );

		{gs,cam_up_double_button,click,[],_} ->
			enterGUIMainLoop( move_up( State, ?move_offset_double ) );


		% Camera zoom section:

		{gs,cam_zoom_in,click,[],_} ->
			enterGUIMainLoop( zoom_in(State) );


		{gs,cam_zoom_out,click,[],_} ->
			enterGUIMainLoop( zoom_out(State) );


		{add_info_message,Message} ->
			add_info_message( Message ),
			enterGUIMainLoop( State );
			

		Other ->
			io:format( "Message ignored by the map supervisor: '~p'.~n",
					   [Other] ),
			enterGUIMainLoop(State)

	end.
			



% Camera movement section.


% Moves the camera up by specified value, in pixels.
% Returns a new state.
move_up( State, Value ) ->

	CamPid = ?getAttr(current_camera_pid),

	CamPid ! {updateOrdinateWith,Value,self()},

	receive 

		{wooper_result,{Y,NewY}} -> 
			gs:config( cam_y_location, {text,io_lib:format("~B",[NewY])} ),
			add_info_message( "Moved upward, of ~s.", 
							  [ text_utils:distance_to_string(NewY-Y) ] ),
			State

	end.
				
	



% Camera zoomw section.


% Manages a zoom-in request.
% Returns a new state.
zoom_in(State) ->
	CamPid = ?getAttr(current_camera_pid),
	CamPid ! {getZoomFactor,[],self()},
	CurrentZoom = receive {wooper_result,Z} -> Z end,
	NewZoom = CurrentZoom * ( 1 + ?zoom_coefficient ) ,
	CamPid ! {setZoomFactor,NewZoom},
	gs:config( cam_factor, {text,io_lib:format("~.2f",[NewZoom])} ),
	add_info_message( "Zoomed in, to factor x~.2f.", [NewZoom] ),
	State.


% Manages a zoom-out request.
% Returns a new state.
zoom_out(State) ->
	CamPid = ?getAttr(current_camera_pid),
	CamPid ! {getZoomFactor,[],self()},
	CurrentZoom = receive {wooper_result,Z} -> Z end,
	NewZoom = CurrentZoom / ( 1 + ?zoom_coefficient ),
	CamPid ! {setZoomFactor,NewZoom},
	gs:config( cam_factor, {text,io_lib:format("~.2f",[NewZoom])} ),
	add_info_message( "Zoomed out, to factor x~.2f.", [NewZoom] ),
	State.
						  
	
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
	
	CameraPanelDimensions = {CameraPanelWidth,CameraPanelHeight} = 
		get_camera_panel_dimensions(),
		
	% The main window is split into four areas:
	%   - first row: empty then canvas
	%   - second row: camera panel then information panel
	gs:frame( packer_win, MainWinId, [ {bw,1},
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

	% In the main frame, the camera panel is at the bottom left:
	
	render_camera_panel( _CamParentId=packer_win, _CamPosition={1,2},
						CameraPanelDimensions, ?getAttr(current_camera_pid) ),
	
	% In the main frame, the information panel is at the bottom right:
	render_information_panel( _InfoParentId=packer_win, _InfoPosition={2,2} ),
	
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



% Returns the length of a unit camera cell, in pixels.
get_camera_cell_length() ->
	40.


% Returns the number of unit camera cells, along the X axis.
get_camera_abscissa_cell_count() ->
	7.


% Returns the number of unit camera cells, along the Y axis.
get_camera_ordinate_cell_count() ->
	7.


% Returns the dimensions in pixels of the camera panel.
get_camera_panel_dimensions() ->	

	% Setting up camera panel dimensions:
	CameraCellLength = get_camera_cell_length(),

	CameraPanelWidth  = get_camera_abscissa_cell_count() * CameraCellLength,
	CameraPanelHeight = get_camera_ordinate_cell_count() * CameraCellLength,

	% Scales are to be taken into account here.
	
	{CameraPanelWidth,CameraPanelHeight}.


	
% Renders the camera panel.	
render_camera_panel( ParentId, Position, 
					_Dimensions={CameraPanelWidth,CameraPanelHeight},
					CameraPid ) ->

	CellWidth = CellHeight = {fixed,get_camera_cell_length()},

	% See diagram for panel layout:
	
	AbscissaLayout = [ CellWidth  || _X <- lists:seq(1,
							    get_camera_abscissa_cell_count() ) ],
	
	OrdinateLayout = [ CellHeight || _Y <- lists:seq(1,
								get_camera_ordinate_cell_count() ) ],

	
	gs:frame( packer_cam, ParentId, [ 
      {bw,1}, 
	  {pack_xy,Position},
	  {packer_x,AbscissaLayout},
	  {packer_y,OrdinateLayout},
	  {width,CameraPanelWidth},
	  {height,CameraPanelHeight} ] ),
	
	
	% Camera status sub-panel (top left one):
	
	CameraPid ! {getFullStatus,[],self()},
	{CameraName,_CameraPosition={X,Y},CameraZoomFactor} = receive 
					 {wooper_result,{N,P,Z}} ->
						{text_utils:binary_to_string(N),P,Z}
				 end,

	gs:create( label, cam_name_label, packer_cam, [ {label,
	  {text,"Camera name:"}},
      {justify,center}, {bw,1}, {pack_xy,{{1,3},1}} ] ),

	gs:create( entry, cam_current_name, packer_cam, [
	  {text,CameraName}, {justify,center}, {bw,1}, {pack_xy,{{1,3},2}} ] ),

	gs:create( button, cam_create_button, packer_cam, [ 
	  {label,{text,"Add"}}, {pack_xy,{{1,3},3}} ]),
	
	
	% Camera location sub-panel (top right one):
	
	gs:create( label, cam_pos_label, packer_cam, [ {label,
	  {text,"Position:"}},
      {justify,center}, {bw,1}, {pack_xy,{{5,7},1}} ] ),

	gs:create( label, cam_x_label, packer_cam, [ {label,
	  {text,"X:"}}, {justify,center}, {bw,1}, {pack_xy,{5,2}} ] ),
	
	gs:create( entry, cam_x_location, packer_cam, [
	  {keypress,true}, {text,io_lib:format("~B",[X])}, {pack_xy,{{6,7},2}} ] ),
	 
	gs:create( label, cam_y_label, packer_cam, [ {label,
	  {text,"Y:"}}, {justify,center}, {bw,1}, {pack_xy,{5,3}} ] ),
	
	gs:create( entry, cam_y_location, packer_cam, [
      {keypress,true}, {text,io_lib:format("~B",[Y])}, {pack_xy,{{6,7},3}} ] ),
	 
	
	
	% Camera zoom sub-panel (bottom right one):
	
	gs:create( label, cam_zoom_label, packer_cam, [ {label,
	  {text,"Zoom factor:"}},
      {justify,center}, {bw,1}, {pack_xy,{{5,7},5}} ] ),
	
	gs:create( label, cam_zoom_factor_label, packer_cam, [ {label,
	  {text,"x"}}, {justify,right}, {bw,1}, {pack_xy,{5,6}} ] ),
	
	gs:create( entry, cam_factor, packer_cam, [
	  {keypress,true}, {text,io_lib:format("~.2f",[CameraZoomFactor])}, 
	  {pack_xy,{{6,7},6}} ] ),

	CameraResourceDir = "../resources/camera",	
	
	gs:create( button, cam_zoom_in, packer_cam, [ 
	  {label,{image,file_utils:join(CameraResourceDir,"button-zoom-in.xbm")}}, 
												  {pack_xy,{6,7}} ] ),
	
	gs:create( button, cam_zoom_out, packer_cam, [ 
	  {label,{image,file_utils:join(CameraResourceDir,"button-zoom-out.xbm")}},
												  {pack_xy,{7,7}} ] ),


	% Camera editor sub-panel (bottom left one):

	gs:create( listbox, cam_editor_list, packer_cam, [ 
			{items,[ CameraName ]},
			{bw,1}, {hscroll,false}, {pack_xy,{{1,3},{5,6}}} ] ),

	gs:create( button, cam_load_button, packer_cam, [ 
	  {label,{text,"Load"}}, {pack_xy,{1,7}} ]),

	gs:create( button, cam_del_button, packer_cam, [ 
	  {label,{text,"Del"}}, {pack_xy,{2,7}} ]),

	gs:create( button, cam_save_button, packer_cam, [ 
	  {label,{text,"Save"}}, {pack_xy,{3,7}} ]),

	
	% Cross of buttons, line by line:
	
	% First we go down from the top:
	
	gs:create( button, cam_up_stop_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-up-stop.xbm")}},
	    {pack_xy,{4,1}} ]),

	gs:create( button, cam_up_double_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-up-double.xbm")}},
	    {pack_xy,{4,2}} ]),
	
	
	gs:create( button, cam_up_single_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-up-single.xbm")}},
	    {pack_xy,{4,3}} ]),
	
	
	% Main button line, from left to right:
	
	gs:create( button, cam_left_stop_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-left-stop.xbm")}},
	    {pack_xy,{1,4}} ]),

	gs:create( button, cam_left_double_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-left-double.xbm")}},
	    {pack_xy,{2,4}} ]),
	
	
	gs:create( button, cam_left_single_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-left-single.xbm")}},
	    {pack_xy,{3,4}} ]),
	
	
	gs:create( button, cam_center_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-center.xbm")}}, 
		{pack_xy,{4,4}} ]),

	
	gs:create( button, cam_right_single_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-right-single.xbm")}},
	    {pack_xy,{5,4}} ]),

	gs:create( button, cam_right_double_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-right-double.xbm")}},
	    {pack_xy,{6,4}} ]),
	
	gs:create( button, cam_right_stop_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-right-stop.xbm")}},
	    {pack_xy,{7,4}} ]),

	% Then we continue below:
	
	gs:create( button, cam_down_single_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-down-single.xbm")}},
	    {pack_xy,{4,5}} ]),
	
	gs:create( button, cam_down_double_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-down-double.xbm")}},
	    {pack_xy,{4,6}} ]),
	
	gs:create( button, cam_down_stop_button, packer_cam, 
      [ {label,{image,
				file_utils:join(CameraResourceDir,"button-down-stop.xbm")}},
	    {pack_xy,{4,7}} ]).



update_cam_list(State) ->
	NameFun = fun(CamPid) ->
				  CamPid ! {getName,[],self()},
				  receive
					  {wooper_result,N} -> text_utils:binary_to_string(N) 
				  end
		  end,
				  
	Names = lists:map( NameFun, ?getAttr(camera_list) ),
	
	gs:config( cam_editor_list, {items,Names} ).	

			  
% records the current camera settings, as a new camera.
add_camera(State) ->
	Name = gs:read( cam_current_name, text ),
	XText = gs:read( cam_x_location, text ),
	YText = gs:read( cam_y_location, text ),
	ZoomText = gs:read( cam_factor, text ),

	io:format( "Name = ~p, X=~w, Y=~w, Zoom=~w.~n", 
			   [Name,XText,YText,ZoomText] ),
	
	{X,[]} = string:to_integer(XText),
	{Y,[]} = string:to_integer(YText),
	{Zoom,[]} = string:to_float(ZoomText),
	
	io:format( "Name = ~p, X=~w, Y=~w, Zoom=~w.~n", [Name,X,Y,Zoom] ),

	NewCameraPid = class_Camera:new_link( Name, {X,Y},
        get_onscreen_camera_position(), Zoom,
		?getAttr(virtual_world_pid), _MapSupervisor=self() ),

	NewState = appendToAttribute( State, camera_list, NewCameraPid ),
	update_cam_list(NewState),
	NewState.


% Renders the information panel.	
render_information_panel( ParentId, Position ) ->
	gs:frame( packer_info, ParentId, [ 
      {bw,1}, 
	  {pack_xy,Position},
	  {packer_x,[{stretch,_Weight=1}]},
	  {packer_y,[ {fixed,20}, {stretch,1}, {fixed,50} ]} ] ),

	_Width = gs:read( packer_info, width ),
	
	gs:scale( scale_x, packer_info, [ {pack_xy,{1,1}}, {width,300}, 
									 {range,{1,500}} ] ),
			 
	gs:editor( info_editor, packer_info, [ {pack_xy,{1,2}}, {width,150},
		{wrap,word}, {vscroll,right}, {enable,false} ] ).
		
			 
			 
% Adds specified message to the information panel editor. 
add_info_message( Message ) ->			 

	gs:config( info_editor, {enable,true} ),

	% Most recent messages are to be displayed first:
	gs:config( info_editor, {insert, {_Top={0,0}, Message ++ "\n" } } ),

	RowCount = gs:read( info_editor, size ),

	% Max number of lines remembered:
	MaxCount = 1000,
	case RowCount of 

		Count when Count > MaxCount ->
			io:format( "Removing oldest lines, from ~B to ~B.~n",
					   [MaxCount,Count] ),
			gs:config( info_editor, {del, {{MaxCount,0},{Count,lineend} } } );
		
		_ ->
			ok

	end,
	%gs:config( info_editor, {insertpos,'end'} ),
	gs:config( info_editor, {enable,false} ).


% Adds specified message to the information panel editor. 
add_info_message( MessageFormat, MessageValues ) ->			 
	add_info_message( io_lib:format( MessageFormat, MessageValues ) ).


% Returns the onscreen position {Xsc,Ysc} of the camera center, i.e. the center
% of the camera frame.
get_onscreen_camera_position() ->
	W = gs:read( packer_cam, width ), 
	H = gs:read( packer_cam, height ),
	{ round(W/2), round(H/2) }.
	

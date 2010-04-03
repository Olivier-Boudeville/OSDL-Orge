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
-module(orge_map).

-export([ start/0 ]).


% Camera management.
-export([ set_camera/3, set_current_camera/2 ]).




% Describes a camera in the virtual world.
-record( camera, {

		   location = {0,0},
		   color = red,
		   name

		  }
	   ).



% Describes a virtual world.   
-record( world, {
		   
		   % Overall rectangular dimensions ({width,height}):
		   dimensions,

		   % World elements:
		   polygons = [],

		   % Known cameras ({camera_id,Camera} pairs):
		   cameras = [],

		   % Current camera (camera_id):
		   current_camera,

		   locations

		  } 
	   ).



% Describes the current state of a GUI.
-record( gui_state, {

		   world,

		   main_win,

		   canvas

		  }
	   ).




% Camera section.



% Sets the location of specified camera.
% Returns the updated camera.
set_camera_location( NewLocation, Camera ) ->
	Camera#camera{ location = NewLocation }.


% Renders specified camera in specified canvas.
render_camera( Camera, Canvas ) ->
	gui:draw_cross( _Location = Camera#camera.location, _EdgeLength = 4,
				_Color = Camera#camera.color, Canvas ).



% Returns a default camera.
create_default_camera() ->
	#camera{ name = "Default" }.



% Returns the specified camera.
get_camera( GuiState, CameraId ) ->

	World = get_world(GuiState),

	case lists:keyfind( CameraId, _Pos=1, World#world.cameras ) of

		false -> 
			throw( {camera_not_found,CameraId} );

		{CameraId,Camera} ->
			Camera

	end.



% Returns the current camera, if any.
get_current_camera( GuiState ) ->

	World = get_world(GuiState),

	case World#world.current_camera of

		undefined ->
			throw( no_current_camera );

		CurrentCameraId ->
			get_camera(GuiState,CurrentCameraId)

	end.



% Sets specified camera identifier to specified camera state, possibly updating
% a previously existing camera pair.
% Returns an updated GUI state.
set_camera( CameraId, Camera, GuiState ) ->

	World = get_world(GuiState),

	NewCameras = lists:keyreplace( CameraId, _Pos=1,  World#world.cameras, 
								   {CameraId,Camera} ),
	
	GuiState#gui_state{ world = World#world{ cameras = NewCameras } }.



% Sets current camera, possibly updated the previously current camera.
% Returns an updated GUI state.
set_current_camera( Camera, GuiState ) ->

	World = get_world(GuiState),

	case  World#world.current_camera of

		undefined ->
			throw( no_current_camera );

		CurrentCameraId ->
			set_camera( CurrentCameraId, Camera, GuiState )

	end.



% World section.


% Returns the world recorded in the specified GUI state.
get_world( GuiState ) ->
	GuiState#gui_state.world.


% Creates an example world.
create_example_world() ->

	WorldWidth  = 800,
	WorldHeight = 600,

	WorldDimensions = {WorldWidth,WorldHeight},

	% Centers the default camera:
	DefaultCamera = set_camera_location( {WorldWidth div 2,WorldHeight div 2},
								  create_default_camera() ),

	RandomPoints = [ {random:uniform(300)+500,random:uniform(300)+200}
					 || _Count <- lists:seq(1,20) ],

	io:format( "Random points: ~w.~n", [RandomPoints] ),

	#world{ 
			dimensions = WorldDimensions,
			polygons = [ get_triangle(), get_upright_square() ],
			cameras  = [ {default_camera,DefaultCamera} ],
			current_camera = default_camera,
			locations = RandomPoints
		  }.



% Renders specified world in specified canvas.
render_world( World, Canvas ) ->
	[ render_polygon( P, Canvas ) || P <- World#world.polygons ],
	[ render_camera(  Cam, Canvas ) || {_CamId,Cam} <- World#world.cameras ],
	[ gui:draw_cross( Loc, Canvas ) || Loc <- World#world.locations ],

	Pivot = find_pivot( World#world.locations ),

	io:format( "Pivot: ~w.~n", [Pivot] ),
	gui:draw_cross( Pivot, 10, blue, Canvas ).



% Graphical User Interface management section.


	
get_main_window_width() ->
	1024.


get_main_window_height() ->
	768.


get_canvas_width() ->
	800.


get_canvas_height() ->
	600.




% Creates the initial GUI.
% Returns {MainWin,Canvas}:
create_initial_gui( GsId ) ->

	WindowSize = [ {width,get_main_window_width()}, 
				   {height,get_main_window_height()} ],

	MainWin = gs:window( GsId, WindowSize ++ [  
								{title,"Orge Map Viewer"},
								{bg,black} ]),

	CameraCellLength = 50,

	CameraPanelWidth  = 3*CameraCellLength,
	CameraPanelHeight = 4*CameraCellLength,


	% Main window split into four areas:
	%   - first row: empty then canvas
	%   - second row: camera panel then empty
	gs:frame( packer_win, MainWin, [ 
      {bw,1},
	  {packer_x,[ {fixed,CameraPanelWidth}, {stretch,10} ]},
	  {packer_y,[ {stretch,10},{fixed,CameraPanelHeight} ]}
							   ] ),	
	% In the main frame, canvas is at the top right:
	Canvas = gs:create( canvas, packer_win, [ 
		{pack_xy,{2,1}},
		{bg,white}, 
		%{hscroll,bottom}, {vscroll,left},
	    {width,get_canvas_width()},
		{height,get_canvas_height()}
		% Centers canvas:
	    %{x,(get_main_window_width()-get_canvas_width()) / 2},
		%{y,(get_main_window_height()-get_canvas_height()) / 2}
		%{scrollregion, {100,200,30,200}}
										 ] ),
	% In the main frame, the camera frame is at the bottom left:
	CellWidth = {fixed,CameraCellLength},
	CellHeight = CellWidth,
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
	
	gs:config( packer_cam, [ {width,CameraPanelWidth}, 
						 {height,CameraPanelHeight} ] ),

	gs:config( packer_win, WindowSize ),
	{MainWin,Canvas}.



update_camera_panel( GuiState ) ->
	Camera = get_current_camera( GuiState ),
	{X,Y} = Camera#camera.location,
	Text = io_lib:format( "~s camera\nAt [~B;~B]", [Camera#camera.name,X,Y] ),
	gs:config( cam_label, [ {label,{text,Text}} ] ).



% Starts the GUI.
start() ->

	GsId = gs:start(),

	{MainWin,Canvas} = create_initial_gui( GsId ),

	InitialWorld = create_example_world(),

	InitialGuiState = #gui_state{ 
	  world    = InitialWorld,
	  main_win = MainWin,
	  canvas   = Canvas
	 },

	render_world( InitialWorld, Canvas ),
	
	update_camera_panel( InitialGuiState ),

    % Sets the GUI to visible:
	gs:config( MainWin, {map,true} ),

	map_view_loop( InitialGuiState ),

	gs:stop().




% Main loop of the GUI.
map_view_loop( GuiState ) ->
	
	receive
        
		{gs,cam_left_button,click,_Data,_Arg} ->
			io:format( "Click left!~n" ),
			%CurrentCam = GuiState#gui_state.current_camera,
			map_view_loop( GuiState );

		{gs,_Pair,destroy,[],[]} ->
			io:format( "Quitting Orge Map!~n" ),
			erlang:halt();
		
		X ->
            io:format("Got '~w' (ignored).~n",[X]),
			map_view_loop( GuiState )
	
	end.




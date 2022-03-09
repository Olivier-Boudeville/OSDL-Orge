% Copyright (C) 2003-2013 Olivier Boudeville
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
% Created on Monday, April 5, 2010.
-module(class_VirtualWorld).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [class_TraceEmitter] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, Name, Boundaries ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		synchronous_new/2, synchronous_new_link/2,
		synchronous_timed_new/2, synchronous_timed_new_link/2,
		remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		remote_synchronous_new_link/3, remote_synchronous_timed_new/3,
		remote_synchronous_timed_new_link/3, construct/3, delete/1 ).


% Member method declarations.
-define( wooper_method_export, getBoundaries/1, getCenter/1,
		getElementsInDisc/3 ).


% Static method declarations.
-define( wooper_static_method_export, ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Orge.World").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new virtual world.
%
% Construction parameters are:
%
% - Name: the name of this world, specified as a plain string
%
% - Boundaries={{Xmin,Ymin},{Xmax,Ymax}} is a pair of integer points, the
% top-left and the bottom-right one, which determine the rectangle that delimits
% this virtual world
%
construct( State, Name, _Boundaries={PTopLeft,PBottomRight} ) ->

	% The attribute of an instance are:
	%
	% - top_left_point: the point in the virtual world which corresponds to the
	% top-left corner (integer coordinates)
	%
	% - bottom_right_point: the point in the virtual world which corresponds to
	% the bottom-right corner (integer coordinates)
	%
	% - top_level_elements: the list of the top-level elements in this world
	%

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State, Name ),

	% Then the class-specific attributes:
	setAttributes( TraceState, [
		 {top_left_point,PTopLeft},
		 {bottom_right_point,PBottomRight},
		 {top_level_elements,[]},
		 {trace_categorization,
		  text_utils:string_to_binary(?TraceEmitterCategorization)}
							   ] ).



delete(State) ->
	State.




% Member method section.


% Returns a pair of points corresponding to the boundaries of the virtual world,
% the top-left one and the bottom-right one.
%
% (const request) (integer coordinates)
getBoundaries( State ) ->
	?wooper_return_state_result( State,
			  {?getAttr(top_left_point),?getAttr(bottom_right_point)} ).


% Returns the coordinates of the center of the world, based on its boundaries.
%
% (const request) (integer coordinates)
getCenter( State ) ->
	?wooper_return_state_result( State, linear_2D:get_integer_center(
			?getAttr(top_left_point), ?getAttr(bottom_right_point) ) ).


% Returns all elements of this world that may be included, partially or totally,
% in the specified disc.
%
% The exact list may actually be a subset of the returned one, as the selection
% is based on bounding-boxes.
%
% (const request)
getElementsInDisc( State, Center, Radius ) ->
	io:format( "getElementsInDisc: center = ~w, radius = ~w m.~n",
			 [Center,math:sqrt(Radius)/100] ),
	TopLevelElements = ?getAttr(top_level_elements),
	?wooper_return_state_result( State,	TopLevelElements ).



% Static method section.


% Helper functions section.

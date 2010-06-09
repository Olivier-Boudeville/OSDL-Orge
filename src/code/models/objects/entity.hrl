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



% An entity is the most global abstraction for anything that can be located into
% a virtual world, while being simple enough and having no spontaneous behaviour
% of its own so that it does deserve being a world element (see
% class_WorldElement).
%
% As a result entities are just data-structures (records), not active object
% (processes). They are thus cheaper yet more limited.
%
% To nevertheless give them some polymorphic abilities (code that handle all the
% same, regardless of their particular record type), by conventions the first
% effective field of any entity record is 'location', a {X,Y} tuple.


% As an example, the simplest (mostly useless) entity is:
-record( abstract_entity, {

		   % The entity location, {X,Y}, both being in-world integer
		   % coordinates:
		   location = {0,0}

		  }

).


% An instance of such a record can be seen also as a tuple:
% {abstract_entity,{X,Y}}.


% We can define, once for all, generic all-purpose functions operating on all
% kinds of entities:

-define( define_translate_for(EntityType),

		 % Translates specific entity of vector V.
		 translate( Entity, _V={Vx,Vy} ) when is_record(Entity,EntityType) ->
			   {X,Y} = Entity#EntityType.location,
			   Entity#EntityType{ location={X+Vx,Y+Vy} }

).

% To be added in all source files defining entities (see entity_test.erl for an
% example):
%?define_translate_for(abstract_entity),

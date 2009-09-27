% Copyright (C) 2009 Olivier Boudeville
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


% Learns a language from a representative word set.
-module(orge_language_learner_app).



% For trace facilities:
-include("traces.hrl").


-export( [ exec/0 ] ).
	
	


get_words( _LanguageManagerPid, _Variation, 0, Acc ) ->
	Acc;
	
get_words( LanguageManagerPid, Variation, WordCount, Acc ) ->
	LanguageManagerPid ! {generate,Variation,self()},
	receive
	
		{wooper_result,{generation_success,Word} } ->
			get_words( LanguageManagerPid, Variation, WordCount-1, [Word|Acc] )
	
	end.
	


evaluate_probabilities( _LanguageManagerPid, _Variation, [] ) ->
	ok;
	
evaluate_probabilities( LanguageManagerPid, Variation, [Word|T] ) ->
	LanguageManagerPid ! {evaluate,[Word,Variation],self()},
	receive
	
		{wooper_result,Probability} ->
			?emit_info([ io_lib:format( "The probability that the word '~s' "
				%"belongs to the variation '~w' is ~.5f %.", 
				"belongs to the variation '~w' is ~f %.", 
				[Word,Variation,Probability*100] ) ]),
			evaluate_probabilities( LanguageManagerPid, Variation, T )
	
	end.
	
	

exec() ->

	?traces_start,
	?init_trace_supervisor,

	Language = "modern-greek",
	Variations = [ 'female-names', 'male-names', surnames ],
	Options = [ generate_original_only, generate_capitalized_words,
		prohibited_index ],
	
	?emit_info([ io_lib:format( "Testing a language manager for language '~s',"
		" with variations ~w and options ~w.", 
		[ Language, Variations, Options ] ) ]),
	
	FirstLanguageManagerPid = class_LanguageManager:synchronous_new_link(
		Language, Variations, _MarkovOrder = 2, Options ),
	
	FirstLanguageManagerPid ! {learn,[],self()},
	receive
	
		{wooper_result,learning_success} ->
			?emit_info([ "Learning succeeded." ]);
			
		{wooper_result,{learning_failure,Reason}} ->
			?emit_info([ io_lib:format( "Learning failed, reason: ~p.",
				[Reason] ) ])
			
	end,

	WordCount = 20,

	FirstTestVariation = 'female-names',
	
	?emit_info([ io_lib:format(	"Requesting the generation of ~B words "
		"from the '~s' variation.", [WordCount,FirstTestVariation] ) ]),
		 
	FirstWords = get_words( FirstLanguageManagerPid, FirstTestVariation,
		WordCount, [] ),
	
	?emit_info([ io_lib:format(	"Generated following words: ~s.",
		[ basic_utils:string_list_to_string(FirstWords)] ) ]),
	
	OriginalWords = [ "Artemisia", "Aspasia", "Aspa", "Aster", "Atalante",
		"Athena", "Basilea", "Vasiliki", "Berenice" ], 

	OtherWords = [ "Tourmente", "Apocalypse", "Ladder", "Boat",
		"Doppel", "Augen", "Pasta", "Arte" ],
		
	evaluate_probabilities( FirstLanguageManagerPid, FirstTestVariation, 
		 FirstWords ++ OtherWords ++ OriginalWords ),
		
	FirstLanguageManagerPid ! delete,
	
	?traces_stop.



	
	
	
				

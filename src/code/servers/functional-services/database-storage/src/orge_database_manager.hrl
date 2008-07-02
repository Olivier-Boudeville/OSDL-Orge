

% Describes the Mnesia entry type (fields) for Orge users:
-record( orge_user_settings, 
	{
		first_name,
		last_name,
		date_of_birth,
		address_line_1,
		address_line_2,
		city,
		state,
		country,
		postal_code,
		home_telephone,
		mobile_telephone,
		email_address,
		account_login,
		account_password,
		security_question,
		security_answer
	}
).




 
% Internal record that stores the current state of the Orge database:
-record( database_state, {
	current_user_id
} ).


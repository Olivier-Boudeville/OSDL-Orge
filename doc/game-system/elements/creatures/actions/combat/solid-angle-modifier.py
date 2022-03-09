#!/usr/bin/env python


from math import *

neutral_solid_angle = 0.08

mu = 1

mu_alpha = 1
mu_beta = 9

def compute_solid_angle_for(surface,distance):
	"""Surface in m^2, distance in m."""
	return 25 * surface / ( pi*(distance**2))


def modifier( solid_angle ):
	if solid_angle > neutral_solid_angle:
		return 1-exp(mu_alpha*(neutral_solid_angle-solid_angle)) 
	else:
		return max(-1,1-exp(mu_beta*(neutral_solid_angle-solid_angle)))


def solid_angle_to_string(angle):
	return "%0.2f" % (angle,)
	
    
def modifier_to_string(modifier):
	return "%0.1f %%" % (modifier*100,)
	

def display_for(surface,distance):
	angle = compute_solid_angle_for(surface,distance)
	print "Surface = %.0f m^2, distance = %.0f m -> angle = %s, to-hit modifier = %s." % ( 
    	surface,
    	distance,
    	solid_angle_to_string( angle ),
    	modifier_to_string(modifier(angle)) )

def display_verbose_for(surface,distance):
	angle = compute_solid_angle_for(surface,distance)
	print "For a surface of %.1f m^2 and a distance of %.1f m, the solid angle is %s, which corresponds to a to-hit modifier of %s." % ( 
    	surface,
    	distance,
    	solid_angle_to_string( angle ),
    	modifier_to_string(modifier(angle)) )


def display_for_angle(angle):
	print "For a solid angle of %s, modifier is %s." % ( 
		solid_angle_to_string( angle ),
		modifier_to_string(modifier(angle)) )
	


def display_examples_for(surface):    
	display_for(surface,1)
	display_for(surface,2)
	display_for(surface,5)
	display_for(surface,10)
	display_for(surface,15)
	display_for(surface,20)
	display_for(surface,25)
	display_for(surface,30)
	display_for(surface,40)
	display_for(surface,50)

display_examples_for(1)
print 
display_examples_for(2)



display_for_angle( 0.0  )
display_for_angle( 0.02 )
display_for_angle( 0.04 )
display_for_angle( 0.06 )
display_for_angle( 0.08 )
display_for_angle( 0.10 )
display_for_angle( 0.16 )
display_for_angle( 2  )


plot_file = open( "solid-angle-modifier.dat", "w" )

first_angle = 0

#last_angle = 10
#last_angle = 5
last_angle = 5 * neutral_solid_angle
#last_angle = neutral_solid_angle

angle_increment = (last_angle-first_angle)/100.0

angle = first_angle

while angle < last_angle:
	angle += angle_increment
	plot_file.write( "%s %s\n" % ( angle, 100*modifier(angle) ) )


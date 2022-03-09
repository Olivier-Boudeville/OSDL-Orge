#!/usr/bin/env python

# All probabilities are in [0,1].

from math import *

# This coefficient is chosen so that, with a base probability of 50%, a 
# modifier of p % leads roughly to an overall probability of 50+p %.
modifier_coeff=4.2


def logit_inv(x):
	return 1/(1+exp(-x/100.0))


def generate_modifier_abacus():
	plot_file = open( "modifier-abacus.dat", "w" )
	m_offset_for_50_percent = logit_inv(50)
	for pnew in range(0,100):
		m = logit_inv( pnew ) - m_offset_for_50_percent
		plot_file.write( "%s %s\n" % ( pnew, 100 * m * modifier_coeff ) )

generate_modifier_abacus()


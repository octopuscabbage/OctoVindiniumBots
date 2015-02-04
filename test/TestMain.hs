module Main where 


import Pathfinding_test
import Utils_test
import DistanceCalcs_test
import HeatMapTest

import Test.Hspec


main = hspec $ parallel $do 
        utilts_tests		
	distancecalcs_test
	pathfinding_tests
        heatmap_tests
	

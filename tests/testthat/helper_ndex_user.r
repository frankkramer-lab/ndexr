## Set the default user and password used for testing
ndexTestConf=list(	user='testacc', password='testacc', 			# username and password to test the server connection
			uuidPublicNetwork='9ed0cd55-9ac0-11e4-9499-000c29202374',	# UUID of a public network (usually from "ndextutorials") to perform some read-only tests
			uuidPrivateNetwork='9f1ca938-0e1f-11e7-ab16-0ac135e8bacf')	# UUID of a private network (owned by the defined user) to perform some (network changing) tests on
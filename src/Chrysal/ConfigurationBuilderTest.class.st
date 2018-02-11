Class {
	#name : #ConfigurationBuilderTest,
	#superclass : #TestCase,
	#category : #'Chrysal-Tests'
}

{ #category : #tests }
ConfigurationBuilderTest >> testBuilder [
	"this should be probably turned into a resources"
	
	ConfigurationBuilder new 
		configurationClass: ConfigurationForXX;
		with: ConfigurationDescriptionForXX new.
		
	self assert: (ConfigurationForXX selectors includes: #convertAttribution:).
	self assert: (ConfigurationForXX selectors includes: #attribution:).
	self assert: (ConfigurationForXX selectors includes: #attribution).
]

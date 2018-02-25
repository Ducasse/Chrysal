Class {
	#name : #ConfigurationBuilderTest,
	#superclass : #TestCase,
	#category : #'Chrysal-Tests'
}

{ #category : #tests }
ConfigurationBuilderTest >> testBuilder [
	"this should be probably turned into a resources"
	
	ChrysalConfigurationBuilder new 
		configurationClass: ConfigurationForXX;
		with: ConfigurationDescriptionForXX new.
		
	self assert: (ConfigurationForXX selectors includes: #convertAttribution:).
	self assert: (ConfigurationForXX selectors includes: #attribution:).
	self assert: (ConfigurationForXX selectors includes: #attribution).
	
	self assert: (ConfigurationForXX >> #attribution) sourceCode equals: 'attribution
	"generated code"
	^ self propertyAt: #attribution ifAbsent: [''me, myself and I'']'.
	
	self assert: (ConfigurationForXX >> #latexWriter) sourceCode equals: 'latexWriter
	"generated code"
	^ self propertyAt: #latexWriter ifAbsent: [#latex:sbabook]'

]

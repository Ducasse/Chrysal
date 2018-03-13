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
		withDescriptionItems: ChrysalConfigurationDescription itemDescriptionForXX.
	self assert: (ConfigurationForXX selectors includes: #convertAttribution:).
	self assert: (ConfigurationForXX selectors includes: #attribution:).
	self assert: (ConfigurationForXX selectors includes: #attribution).
	self
		assert: (ConfigurationForXX >> #attribution) sourceCode
		equals:
			'attribution
	"generated code"
	^ self propertyAt: #attribution ifAbsent: [''me, myself and I'']'.
	self
		assert: (ConfigurationForXX >> #latexWriter) sourceCode
		equals:
			'latexWriter
	"generated code"
	^ self propertyAt: #latexWriter ifAbsent: [#latex:sbabook]'
]

{ #category : #tests }
ConfigurationBuilderTest >> testDescriptionBuilder [
	
	| builder string items |
	builder := ChrysalConfigurationBuilder new.
	items := ChrysalConfigurationDescription itemDescriptionForXX.
	self assert: items size equals: 13.
	string := String
		streamContents: [ :s |  builder 
										addDescriptionBodyIn: s 
										forItems:  items ].
	self 
		assert: (STON fromString: string) size 
		equals: items size
]

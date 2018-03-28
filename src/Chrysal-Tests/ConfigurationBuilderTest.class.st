Class {
	#name : #ConfigurationBuilderTest,
	#superclass : #TestCase,
	#category : #'Chrysal-Tests'
}

{ #category : #tests }
ConfigurationBuilderTest class >> ensureConfigurationForXXExist [
	Smalltalk globals at: #ConfigurationForXX ifAbsent: [  
		ChrysalConfigurationBuilder new
			defineConfigurationClassNamed: #ConfigurationForXX packagedIn: 'Chrysal-Tests';
			withDescriptionItems: ChrysalConfigurationBuilder itemDescriptionForXX ].
	^ Smalltalk globals at: #ConfigurationForXX
]

{ #category : #tests }
ConfigurationBuilderTest >> ensureConfigurationForXXExist [

	^ self class ensureConfigurationForXXExist
]

{ #category : #tests }
ConfigurationBuilderTest >> testBuilder [
	"this should be probably turned into a resources"

	self assert: (self ensureConfigurationForXXExist selectors includes: #convertAttribution:).
	self assert: (self ensureConfigurationForXXExist selectors includes: #attribution:).
	self assert: (self ensureConfigurationForXXExist selectors includes: #attribution).
	self
		assert: (self ensureConfigurationForXXExist >> #attribution) sourceCode
		equals:
			'attribution
	"generated code"
	^ self propertyAt: #attribution ifAbsent: [''me, myself and I'']'.
	self
		assert: (self ensureConfigurationForXXExist >> #latexWriter) sourceCode
		equals:
			'latexWriter
	"generated code"
	^ self propertyAt: #latexWriter ifAbsent: [#latex:sbabook]'
]

{ #category : #tests }
ConfigurationBuilderTest >> testDescriptionBuilder [
	
	| builder string items |
	builder := ChrysalConfigurationBuilder new.
	items := ChrysalConfigurationBuilder itemDescriptionForXX.
	self assert: items size equals: 16.
	string := String
		streamContents: [ :s |  builder 
										addDescriptionBodyIn: s 
										forItems:  items ].
	self 
		assert: (STON fromString: (Compiler evaluate: string)) size 
		equals: items size
]

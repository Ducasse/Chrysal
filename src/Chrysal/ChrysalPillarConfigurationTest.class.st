Class {
	#name : #ChrysalPillarConfigurationTest,
	#superclass : #TestCase,
	#category : #'Chrysal-ToBeMovedElsewhere'
}

{ #category : #'tests - from cocoon' }
ChrysalPillarConfigurationTest >> configurationClass [
	^ ConfigurationForPillar
]

{ #category : #'tests - from cocoon' }
ChrysalPillarConfigurationTest >> testReadingDefaultScriptLanguage [
	|  config |
	config := self configurationClass newFromDictionary:  (Dictionary with: 'defaultScriptLanguage' -> 'smalltalk').
	self assert: config defaultScriptLanguage equals: (PRAbstractScriptLanguage for: 'smalltalk').
	
	config := self configurationClass newFromDictionary:  (Dictionary with: 'defaultScriptLanguage' -> 'css').
	self assert: config defaultScriptLanguage equals: (PRAbstractScriptLanguage for: 'css')
]

{ #category : #'tests - from cocoon' }
ChrysalPillarConfigurationTest >> testReadingHeadingLevelOffset [
	| dictionary config |
	dictionary := Dictionary with: 'headingLevelOffset' -> 1.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config headingLevelOffset equals: 1
]

{ #category : #'tests - from cocoon' }
ChrysalPillarConfigurationTest >> testReadingNewLine [
	| dictionary config |
	dictionary := Dictionary with: 'newLine' -> #unix.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config newLine equals: UnixPlatform new lineEnding
]

{ #category : #'tests - from cocoon' }
ChrysalPillarConfigurationTest >> testReadingRenderStructureAsSlide [
	| dictionary config |
	dictionary := Dictionary with: 'renderStructureAsSlide' -> false.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config renderStructureAsSlide equals: false
]

{ #category : #'tests - from cocoon' }
ChrysalPillarConfigurationTest >> testReadingScrambledEmailAddresses [
	| dictionary config |
	dictionary := STON fromStream: '{ "scrambledEmailAddresses" : true }' readStream.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config scrambledEmailAddresses equals: true
]

{ #category : #'tests - from cocoon' }
ChrysalPillarConfigurationTest >> testReadingSeparateOutputFiles [
	| dictionary config |
	dictionary := Dictionary with: 'separateOutputFiles' -> false.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config separateOutputFiles equals: false
]

{ #category : #'tests - from cocoon' }
ChrysalPillarConfigurationTest >> testReadingTitle [
	| dictionary config |
	dictionary := Dictionary with: 'title' -> 'foo'.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config title equals: 'foo'
]

{ #category : #'tests - from cocoon' }
ChrysalPillarConfigurationTest >> testReadingVerbose [
	| dictionary config |
	dictionary := Dictionary with: 'verbose' -> false.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: (config propertyAt: #verbose) equals: false
]

{ #category : #'tests - from cocoon' }
ChrysalPillarConfigurationTest >> testSeparateOutputFiles [
	| dictionary config |
	dictionary := STON
		fromStream: '{  "separateOutputFiles" : false }' readStream.
	config := self configurationClass
		newFromDictionary: dictionary.
	self assert: config separateOutputFiles equals: false
	

]
